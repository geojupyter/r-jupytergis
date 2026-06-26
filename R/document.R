# JupyterGIS document widget, mirroring the Python `GISDocument`

#' Generate a random RFC 4122-style UUID string.
#'
#' @return A length-one character vector containing a 36-character UUID.
#' @noRd
.uuid <- function() {
  bytes <- sample.int(256L, 16L, replace = TRUE) - 1L
  hex <- sprintf("%02x", bytes)
  paste0(
    paste(hex[1:4], collapse = ""),
    "-",
    paste(hex[5:6], collapse = ""),
    "-",
    paste(hex[7:8], collapse = ""),
    "-",
    paste(hex[9:10], collapse = ""),
    "-",
    paste(hex[11:16], collapse = "")
  )
}

#' Build comm metadata for the JupyterGIS widget.
#'
#' @param path Path to a `.jGIS`, `.qgz`, or `.qgs` file, or `NULL` for an
#'   ephemeral in-memory document.
#' @return A named list of comm metadata fields consumed by the frontend.
#' @noRd
.make_comm_metadata <- function(path) {
  if (is.null(path)) {
    return(list(
      ymodel_name = "@jupytergis:widget",
      path = NULL,
      format = NULL,
      contentType = NULL,
      create_ydoc = TRUE
    ))
  }

  ext <- tolower(tools::file_ext(path))
  if (ext == "jgis") {
    format <- "text"
    contentType <- "jgis"
  } else if (ext == "qgz") {
    format <- "base64"
    contentType <- "QGZ"
  } else if (ext == "qgs") {
    format <- "base64"
    contentType <- "QGS"
  } else {
    stop("File extension is not supported: ", ext)
  }

  list(
    ymodel_name = "@jupytergis:widget",
    path = path,
    format = format,
    contentType = contentType,
    create_ydoc = FALSE
  )
}

#' Recursively coerce integer leaves to doubles.
#'
#' `yr::Prelim$any` serializes R integers as empty maps `{}` (see
#' `add_raster_layer()`), which would corrupt integer process-graph args such
#' as `tile_buffer`. JSON has one number type, so `0` and `0.0` are equivalent.
#' @noRd
.ints_to_doubles <- function(x) {
  if (is.list(x)) {
    return(lapply(x, .ints_to_doubles))
  }
  if (is.integer(x)) {
    return(as.double(x))
  }
  x
}

#' Build the `OpenEOTileSource` definition for an openEO process graph.
#'
#' Resolves the connection, coerces `graph` to an `openeo::Graph` and
#' serializes it to the flat process-graph node map (the R equivalent of the
#' Python client's `graph.flat_graph()`), then packages it with the backend
#' url and bearer token.
#'
#' @param graph A `Graph`, or a datacube / result `ProcessNode` coercible to a
#'   `Graph` via `as(graph, "Graph")`.
#' @param connection An `OpenEOClient`, or `NULL` to use
#'   `openeo::active_connection()`.
#' @param name Display name used to derive the source name.
#' @return A named list with `type`, `name` and `parameters`
#'   (`processGraph`, `serverUrl`, `authBearer`).
#' @noRd
.openeo_tile_source <- function(graph, connection, name) {
  if (is.null(connection)) {
    if (!requireNamespace("openeo", quietly = TRUE)) {
      stop(
        "Install 'openeo' (install.packages(\"openeo\")) or pass `connection`."
      )
    }
    connection <- openeo::active_connection()
  }
  if (is.null(connection)) {
    stop(
      "No openEO connection. Pass `connection` or call openeo::login() first."
    )
  }

  g <- if (inherits(graph, "Graph")) {
    graph
  } else {
    if (!requireNamespace("openeo", quietly = TRUE)) {
      stop("Install 'openeo' (install.packages(\"openeo\")) or pass a Graph.")
    }
    methods::as(graph, "Graph")
  }

  list(
    type = "OpenEOTileSource",
    name = paste0(name, " Source"),
    parameters = list(
      processGraph = .ints_to_doubles(g$serialize()),
      serverUrl = connection$getHost(),
      # Prefixed bearer ("basic//..." / "oidc/..."), as the backend expects.
      authBearer = connection$getAuthClient()$access_token
    )
  )
}

#' Find the `spatial_extent` of an openEO (flat) process graph.
#'
#' Recursively searches the serialized graph for the first list carrying
#' `west`/`south`/`east`/`north` keys (the openEO bounding box, in EPSG:4326).
#' @param x A serialized process graph (nested list) or any sub-node.
#' @return A named numeric vector `c(west, south, east, north)`, or `NULL` if
#'   the graph declares no spatial extent.
#' @noRd
.openeo_spatial_extent <- function(x) {
  if (!is.list(x)) {
    return(NULL)
  }
  corners <- c("west", "south", "east", "north")
  if (all(corners %in% names(x))) {
    return(vapply(corners, function(k) as.double(x[[k]]), double(1)))
  }
  for (el in x) {
    found <- .openeo_spatial_extent(el)
    if (!is.null(found)) {
      return(found)
    }
  }
  NULL
}

#' Project a WGS84 lon/lat point to Web Mercator (EPSG:3857).
#'
#' Spherical Mercator, matching OpenLayers' default view projection. Latitude is
#' clamped to the projection's valid range.
#' @param lon,lat Longitude and latitude in degrees.
#' @return A numeric vector `c(x, y)` in metres.
#' @noRd
.lonlat_to_webmercator <- function(lon, lat) {
  radius <- 6378137
  lat <- max(min(lat, 85.06), -85.06)
  c(radius * lon * pi / 180, radius * log(tan(pi / 4 + lat * pi / 360)))
}

#' Web Mercator view extent for an openEO spatial extent.
#'
#' @param extent A named numeric `c(west, south, east, north)` in EPSG:4326.
#' @return A list `[minX, minY, maxX, maxY]` in EPSG:3857, as OpenLayers'
#'   `View.fit()` expects. A list (not a numeric vector) so `yr::Prelim$any`
#'   serializes it as a JSON array — inserting a bare numeric vector errors.
#' @noRd
.openeo_view_extent <- function(extent) {
  sw <- .lonlat_to_webmercator(extent[["west"]], extent[["south"]])
  ne <- .lonlat_to_webmercator(extent[["east"]], extent[["north"]])
  as.list(as.double(c(sw[1], sw[2], ne[1], ne[2])))
}

#' JupyterGIS document
#'
#' Comm-backed widget mirroring `jupytergis_lab.GISDocument`. Exposes the
#' document's CRDT roots (`layers`, `sources`, `options`, `layerTree`,
#' `metadata`) as fields, and provides `add_raster_layer()` and
#' `add_openeo_tile_layer()`.
#'
#' @export
GISDocument <- R6::R6Class(
  "GISDocument",
  inherit = ywidgets::CommRootWidget,

  public = list(
    #' @field layers Shared `yr::Map` of layer definitions keyed by layer id.
    layers = NULL,
    #' @field sources Shared `yr::Map` of source definitions keyed by source id.
    sources = NULL,
    #' @field options Shared `yr::Map` of document-level options.
    options = NULL,
    #' @field layerTree Shared `yr::Array` describing layer ordering and grouping.
    layerTree = NULL,
    #' @field metadata Shared `yr::Map` of document metadata.
    metadata = NULL,

    #' @description Create a new GISDocument.
    #' @param path Path to a `.jGIS`, `.qgz`, or `.qgs` file. `NULL` creates an
    #'   ephemeral in-memory document.
    #' @param ydoc Optional existing `yr::Doc` to adopt.
    initialize = function(path = NULL, ydoc = NULL) {
      super$initialize(
        ydoc = ydoc,
        comm_metadata = .make_comm_metadata(path)
      )

      self$layers <- self$register_storage(
        "layers",
        yr::Prelim$map(list())
      )$read()
      self$sources <- self$register_storage(
        "sources",
        yr::Prelim$map(list())
      )$read()
      self$options <- self$register_storage(
        "options",
        yr::Prelim$map(list())
      )$read()
      self$layerTree <- self$register_storage(
        "layerTree",
        yr::Prelim$array(list(), recursive = FALSE)
      )$read()
      self$metadata <- self$register_storage(
        "metadata",
        yr::Prelim$map(list())
      )$read()

      # Seed the view defaults, mirroring the Python GISDocument
      # (jupytergis_lab gis_document.py). These must be *written into* the
      # options map, not just passed as a Prelim: register_storage's
      # `get_or_insert_map()` keeps only the prelim's type and discards its
      # contents, so the map would otherwise sync to the frontend empty. The
      # frontend gates the whole map view on an observed change to `options`
      # (`initialSyncReady` resolves from the options observer), so an empty
      # options map means the MainView (OpenLayers canvas) never mounts.
      # Numeric literals are doubles, not integers (`yr::Prelim$any`
      # serializes R integers as `{}`; see add_raster_layer()).
      self$with_write(function(trans) {
        defaults <- list(
          latitude = 0,
          longitude = 0,
          zoom = 0,
          bearing = 0,
          pitch = 0,
          projection = "EPSG:3857",
          storyMapPresentationMode = FALSE
        )
        for (key in names(defaults)) {
          self$options$insert(trans, key, yr::Prelim$any(defaults[[key]]))
        }
      })
    },

    #' @description Add a Raster Layer to the document.
    #' @param url Tiles URL.
    #' @param name Display name for the layer.
    #' @param attribution Attribution text.
    #' @param opacity Layer opacity in [0, 1].
    #' @param url_parameters Extra URL parameters for tile requests.
    #' @return The new layer id.
    add_raster_layer = function(
      url,
      name = "Raster Layer",
      attribution = "",
      opacity = 1,
      url_parameters = NULL
    ) {
      source_id <- .uuid()
      layer_id <- .uuid()

      source <- list(
        type = "RasterSource",
        name = paste0(name, " Source"),
        parameters = list(
          url = url,
          # Doubles, not integers: yr::Prelim$any does not serialize R
          # integer vectors (`0L`/`24L`) — they become an empty map `{}`,
          # which breaks OpenLayers' tile grid and renders a blank layer.
          minZoom = 0,
          maxZoom = 24,
          attribution = attribution,
          htmlAttribution = attribution,
          provider = "",
          bounds = list(),
          urlParameters = if (is.null(url_parameters)) {
            structure(list(), names = character(0))
          } else {
            url_parameters
          }
        )
      )

      layer <- list(
        type = "RasterLayer",
        name = name,
        visible = TRUE,
        parameters = list(
          source = source_id,
          opacity = opacity,
          color = structure(list(), names = character(0))
        )
      )

      # Source, layer and layer-tree entries must be written in separate
      # transactions: the JupyterGIS frontend decides whether to *add* or
      # *update* a layer by checking if it is already in the layer tree, so a
      # layer and its tree entry arriving in one transaction make it take the
      # update path on a layer that was never added (mainView `_onLayersChanged`).
      self$with_write(function(trans) {
        self$sources$insert(trans, source_id, yr::Prelim$any(source))
      })
      self$with_write(function(trans) {
        self$layers$insert(trans, layer_id, yr::Prelim$any(layer))
      })
      self$with_write(function(trans) {
        self$layerTree$insert(
          trans,
          self$layerTree$len(trans),
          yr::Prelim$any(layer_id)
        )
      })

      layer_id
    },

    #' @description Add an openEO process-graph tile layer to the document.
    #'
    #' Mirrors `jupytergis_lab.GISDocument.add_openeo_tile_layer`: the source
    #' carries the flat process graph plus the backend url and session bearer
    #' token, rendered via titiler-openeo on the frontend.
    #' @param graph An openEO `Graph`, or a datacube / result `ProcessNode`
    #'   (e.g. from `openeo::save_result()`) coercible to a `Graph`.
    #' @param connection An `OpenEOClient` (`openeo::connect()`/`login()`).
    #'   Defaults to `openeo::active_connection()`; passed separately because an
    #'   R process graph, unlike the Python one, does not carry its connection.
    #' @param name Display name for the layer.
    #' @param opacity Layer opacity in [0, 1].
    #' @param zoom_to_extent Whether to fit the map view to the process graph's
    #'   `spatial_extent`. The titiler-openeo backend serves tiles only within
    #'   the requested extent and returns HTTP 404 ("no data for the given
    #'   extents") for tiles outside it — which is what a zoomed-out initial
    #'   view requests. Fitting to the extent makes the layer load on open.
    #'   Ignored when the graph declares no spatial extent.
    #' @return The new layer id.
    add_openeo_tile_layer = function(
      graph,
      connection = NULL,
      name = "OpenEO Tiles",
      opacity = 1,
      zoom_to_extent = TRUE
    ) {
      source <- .openeo_tile_source(graph, connection, name)

      source_id <- .uuid()
      layer_id <- .uuid()

      layer <- list(
        type = "OpenEOTileLayer",
        name = name,
        visible = TRUE,
        parameters = list(
          source = source_id,
          opacity = opacity
        )
      )

      # Written in separate transactions for the same reason as
      # add_raster_layer(): the frontend uses the layer tree to decide between
      # the add and update paths.
      self$with_write(function(trans) {
        self$sources$insert(trans, source_id, yr::Prelim$any(source))
      })
      self$with_write(function(trans) {
        self$layers$insert(trans, layer_id, yr::Prelim$any(layer))
      })
      self$with_write(function(trans) {
        self$layerTree$insert(
          trans,
          self$layerTree$len(trans),
          yr::Prelim$any(layer_id)
        )
      })

      # Fit the view to the data extent. The frontend's view follows
      # `options.useExtent` + `options.extent` (in the EPSG:3857 view
      # projection), calling `View.fit(extent)`. Without this the view stays at
      # the default world zoom, where the openEO backend has no tiles to serve.
      if (isTRUE(zoom_to_extent)) {
        extent <- .openeo_spatial_extent(source$parameters$processGraph)
        if (!is.null(extent)) {
          view_extent <- .openeo_view_extent(extent)
          self$with_write(function(trans) {
            self$options$insert(trans, "extent", yr::Prelim$any(view_extent))
            self$options$insert(trans, "useExtent", yr::Prelim$any(TRUE))
          })
        }
      }

      layer_id
    }
  )
)

#' `hera::mime_types` method for `GISDocument`.
#'
#' @param x A `GISDocument`.
#' @return A character vector of supported MIME types.
#' @noRd
mime_types.GISDocument <- function(x) {
  c("text/plain", "application/vnd.jupyter.ywidget-view+json")
}

#' `hera::mime_bundle` method for `GISDocument`.
#'
#' @param x A `GISDocument`.
#' @param mimetypes MIME types to include in the bundle.
#' @param ... Unused.
#' @return A list with `data` and `metadata` entries suitable for Jupyter display.
#' @noRd
mime_bundle.GISDocument <- function(x, mimetypes = hera::mime_types(x), ...) {
  list(
    data = list(
      "text/plain" = "",
      "application/vnd.jupyter.ywidget-view+json" = list(
        version_major = 2L,
        version_minor = 0L,
        model_id = x$comm_id()
      )
    ),
    metadata = structure(list(), names = character(0))
  )
}

registerS3method(
  "mime_types",
  "GISDocument",
  mime_types.GISDocument,
  envir = asNamespace("hera")
)
registerS3method(
  "mime_bundle",
  "GISDocument",
  mime_bundle.GISDocument,
  envir = asNamespace("hera")
)
