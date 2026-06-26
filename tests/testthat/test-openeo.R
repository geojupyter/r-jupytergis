# Tests for the openEO tile layer source builder. These exercise
# `.openeo_tile_source()` with fakes so they need neither the `openeo`
# package nor a running backend / Jupyter kernel.

# A stand-in for an openeo::Graph: it carries the "Graph" class so the
# coercion branch is skipped, and a $serialize() returning a flat node map.
fake_graph <- function(node_map) {
  structure(
    list(serialize = function() node_map),
    class = "Graph"
  )
}

# A stand-in for an openeo::OpenEOClient exposing the two accessors the
# source builder reads.
fake_connection <- function(host, bearer) {
  list(
    getHost = function() host,
    getAuthClient = function() list(access_token = bearer)
  )
}

test_that(".openeo_tile_source builds the expected source definition", {
  node_map <- list(
    loadco1 = list(
      process_id = "load_collection",
      arguments = list(id = "S2", bands = list("B04", "B08"))
    ),
    saveres1 = list(
      process_id = "save_result",
      arguments = list(format = "PNG"),
      result = TRUE
    )
  )

  src <- jupytergis:::.openeo_tile_source(
    graph = fake_graph(node_map),
    connection = fake_connection(
      "http://localhost:8080",
      "basic//abc123"
    ),
    name = "My Layer"
  )

  expect_equal(src$type, "OpenEOTileSource")
  expect_equal(src$name, "My Layer Source")
  expect_equal(src$parameters$processGraph, node_map)
  expect_equal(src$parameters$serverUrl, "http://localhost:8080")
  # The bearer is forwarded verbatim, including the basic auth prefix.
  expect_equal(src$parameters$authBearer, "basic//abc123")
})

test_that(".openeo_tile_source coerces integer graph args to doubles", {
  # yr::Prelim$any turns R integers into empty maps {} in the CRDT, which
  # corrupts the process graph. Integer leaves must come out as doubles.
  node_map <- list(
    loadco1 = list(
      process_id = "load_collection",
      arguments = list(width = 1024L, tile_buffer = 0L, west = 7.5)
    )
  )

  src <- jupytergis:::.openeo_tile_source(
    graph = fake_graph(node_map),
    connection = fake_connection("http://localhost:8080", "basic//x"),
    name = "x"
  )

  args <- src$parameters$processGraph$loadco1$arguments
  expect_type(args$width, "double")
  expect_type(args$tile_buffer, "double")
  expect_identical(args$width, 1024)
  expect_identical(args$tile_buffer, 0)
  # Non-integer values are left untouched.
  expect_identical(args$west, 7.5)
})

test_that(".openeo_tile_source errors without a connection", {
  expect_error(
    jupytergis:::.openeo_tile_source(
      graph = fake_graph(list()),
      connection = NULL,
      name = "x"
    )
  )
})

test_that(".openeo_spatial_extent finds a nested bounding box", {
  node_map <- list(
    loadco1 = list(
      process_id = "load_collection",
      arguments = list(
        id = "S2",
        spatial_extent = list(
          west = 7.5,
          south = 51.9,
          east = 7.6,
          north = 52.0
        )
      )
    ),
    saveres1 = list(
      process_id = "save_result",
      arguments = list(format = "PNG")
    )
  )

  ext <- jupytergis:::.openeo_spatial_extent(node_map)
  expect_equal(ext, c(west = 7.5, south = 51.9, east = 7.6, north = 52.0))
})

test_that(".openeo_spatial_extent returns NULL when no extent is declared", {
  expect_null(jupytergis:::.openeo_spatial_extent(list(
    a = 1,
    b = list(c = "x")
  )))
  expect_null(jupytergis:::.openeo_spatial_extent("not a list"))
})

test_that(".openeo_view_extent reprojects the bbox to EPSG:3857 as a list", {
  ext <- c(west = 7.5, south = 51.9, east = 7.6, north = 52.0)
  view <- jupytergis:::.openeo_view_extent(ext)

  # A list (not a numeric vector): yr::Prelim$any cannot insert a bare vector.
  expect_type(view, "list")
  expect_length(view, 4)

  # Web Mercator: x grows with longitude, y with latitude; min corner first.
  expect_equal(view[[1]], 7.5 * pi / 180 * 6378137, tolerance = 1)
  expect_lt(view[[1]], view[[3]]) # minX < maxX
  expect_lt(view[[2]], view[[4]]) # minY < maxY
})
