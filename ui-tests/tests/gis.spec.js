const fs = require("fs");
const path = require("path");
const { test, expect, galata } = require("@jupyterlab/galata");

const fileName = "gis.ipynb";
const jgisFileName = "france_hiking.jGIS";
const examplesDir = path.resolve(__dirname, "../../examples");
const notebookPath = path.join(examplesDir, fileName);
const jgisPath = path.join(examplesDir, jgisFileName);

test.use({ tmpPath: "r-jupytergis-test" });

/** Parse the example notebook with all cell outputs cleared. */
function loadClearedNotebook() {
  const nb = JSON.parse(fs.readFileSync(notebookPath, "utf8"));
  for (const cell of nb.cells) {
    if (cell.cell_type === "code") {
      cell.outputs = [];
      cell.execution_count = null;
    }
  }
  return nb;
}

/**
 * Galata has no "find cell by content" helper, and getCellTextInput reads the
 * editor via the clipboard (flaky/empty on some browsers). Since we own the
 * notebook, read its JSON directly: the cell order matches the rendered order,
 * so the index is deterministic and needs no browser interaction.
 */
function cellIndexBySource(nb, snippet) {
  const index = nb.cells.findIndex((cell) => [].concat(cell.source).join("").includes(snippet));
  if (index < 0) {
    throw new Error(`No cell found containing: ${snippet}`);
  }
  return index;
}

/** Run a cell and assert it executed without error output. */
async function runCellOk(page, cellIndex) {
  expect(await page.notebook.runCell(cellIndex)).toBe(true);

  const cell = await page.notebook.getCellLocator(cellIndex);
  // It got an execution count, i.e. the kernel actually ran it.
  await expect(cell.locator(".jp-InputPrompt")).toHaveText(/\[\d+\]/);
  // No error/traceback output.
  await expect(cell.locator('[data-mime-type="application/vnd.jupyter.error"]')).toHaveCount(0);
  return cell;
}

/** Locator for a cell's rendered output area. */
function cellOutput(cell) {
  return cell.locator(".jp-OutputArea-output");
}

test.describe("examples/gis.ipynb", () => {
  test.beforeEach(async ({ request, tmpPath }) => {
    const contents = galata.newContentsHelper(request);
    // Upload a copy with outputs cleared so assertions reflect this run only.
    await contents.uploadContent(
      JSON.stringify(loadClearedNotebook()),
      "text",
      `${tmpPath}/${fileName}`,
    );
    // The GISDocument loads this file from the server contents, so it must
    // live next to the notebook in the test directory.
    await contents.uploadContent(
      fs.readFileSync(jgisPath, "utf8"),
      "text",
      `${tmpPath}/${jgisFileName}`,
    );
  });

  test.afterEach(async ({ request, tmpPath }) => {
    const contents = galata.newContentsHelper(request);
    await contents.deleteDirectory(tmpPath);
  });

  test("creates and displays a GISDocument map", async ({ page, tmpPath }) => {
    await page.notebook.openByPath(`${tmpPath}/${fileName}`);
    await page.notebook.activate(fileName);

    const nb = loadClearedNotebook();

    // Mandatory loading
    await runCellOk(page, cellIndexBySource(nb, 'library("jupytergis")'));

    // Expect a rendered GIS document
    const displayCell = await runCellOk(page, cellIndexBySource(nb, "GISDocument$new"));
    await expect(cellOutput(displayCell)).toBeVisible();
    // JupyterGIS renders the map with OpenLayers, which mounts a canvas.
    await expect(displayCell.locator(".ol-viewport canvas").first()).toBeVisible({
      timeout: 60_000,
    });
  });

  test("adds layers from R and reads their parameters back", async ({ page, tmpPath }) => {
    await page.notebook.openByPath(`${tmpPath}/${fileName}`);
    await page.notebook.activate(fileName);

    const nb = loadClearedNotebook();

    await runCellOk(page, cellIndexBySource(nb, 'library("jupytergis")'));
    const displayCell = await runCellOk(page, cellIndexBySource(nb, "GISDocument$new"));
    await expect(displayCell.locator(".ol-viewport canvas").first()).toBeVisible({
      timeout: 60_000,
    });

    // Add a raster layer with opacity 0.6 from R.
    await runCellOk(page, cellIndexBySource(nb, "doc$add_raster_layer"));

    // Reading the layer's opacity back through the CRDT yields the value we set.
    const opacityCell = await runCellOk(page, cellIndexBySource(nb, "$parameters$opacity"));
    await expect(cellOutput(opacityCell)).toContainText("0.6");

    // Add OpenStreetMap raster, vector-tile and GeoJSON layers.
    await runCellOk(page, cellIndexBySource(nb, "doc$add_vectortile_layer"));
    await runCellOk(page, cellIndexBySource(nb, "doc$add_geojson_layer"));

    // The GeoJSON layer name is derived from its file name.
    const nameCell = await runCellOk(page, cellIndexBySource(nb, "doc$layers$get(t, roads)$name"));
    await expect(cellOutput(nameCell)).toContainText("ne_10m_roads");

    // The map is still rendered after mutating the document from R.
    await expect(displayCell.locator(".ol-viewport canvas").first()).toBeVisible();
  });

  test("edits a layer's opacity from the UI and reads it back from R", async ({
    page,
    tmpPath,
  }) => {
    await page.notebook.openByPath(`${tmpPath}/${fileName}`);
    await page.notebook.activate(fileName);

    await page.evaluate(() => {
      document.body.style.zoom = "0.5";
    });

    const nb = loadClearedNotebook();

    await runCellOk(page, cellIndexBySource(nb, 'library("jupytergis")'));
    const displayCell = await runCellOk(page, cellIndexBySource(nb, "GISDocument$new"));
    await expect(displayCell.locator(".ol-viewport canvas").first()).toBeVisible({
      timeout: 60_000,
    });

    // Add the Google Satellite raster layer (opacity 0.6) from R.
    await runCellOk(page, cellIndexBySource(nb, "doc$add_raster_layer"));

    // The side panels render inside the GISDocument output, overlaying the map.
    const output = cellOutput(displayCell);

    await expect(output.locator(".ol-viewport canvas").first()).toBeVisible();

    // Adding the layer from R auto-selects it, so we don't click it in the tree
    // (clicking the already-selected row would toggle the selection off). With
    // the page zoomed out the side panels collapse into a single tab bar, so
    // the selected layer's properties live behind the Object Properties tab;
    // open it to reveal the opacity control.
    await output.getByRole("tab", { name: "Object Properties" }).click();

    // Get opacity of Google layer
    const opacityInput = output
      .getByRole("slider")
      .locator('xpath=following-sibling::input[@type="number"]');
    await expect(opacityInput).toHaveValue("0.6");

    // Change opacity from the UI
    await opacityInput.fill("0.3");
    await opacityInput.press("Enter");
    await opacityInput.blur();
    // Wait to check CRDT opacity has changed in R
    const opacityCellIndex = cellIndexBySource(nb, "$parameters$opacity");
    await expect(async () => {
      const opacityCell = await runCellOk(page, opacityCellIndex);
      await expect(cellOutput(opacityCell)).toContainText("0.3", { timeout: 1_000 });
    }).toPass({ timeout: 20_000 });
  });
});
