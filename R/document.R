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

#' @description Extract a meaningful layer name from a file path or URL.
#' @param path A file path or URL string.
#' @return A character string suitable for a default layer name.
#' @noRd
.extract_layer_name <- function(path) {
  if (inherits(path, "Path")) {
    path <- as.character(path)
  }

  has_scheme <- grepl("^[a-zA-Z][a-zA-Z0-9+.-]*://", path)

  if (has_scheme && grepl("\\{[zxy]\\}", path)) {
    path_no_scheme <- sub("^[a-zA-Z][a-zA-Z0-9+.-]*://", "", path)
    return(sub("/.*$", "", path_no_scheme))
  }

  filename <- sub(".*/", "", sub("/+$", "", path))
  name_without_ext <- sub("\\.[^.]*$", "", filename)

  if (nzchar(name_without_ext)) name_without_ext else filename
}

#' Normalize image corner coordinates.
#
#' @param coordinates Image corners as either:
#'   - an `n x 2` matrix (`[lon, lat]` per row), or
#'   - a list of `[lon, lat]` pairs.
#' @return A list of coordinate pairs, each as `list(lon, lat)`.
#' @noRd
.normalize_coordinates <- function(coordinates) {
  if (is.matrix(coordinates)) {
    coordinates <- lapply(seq_len(nrow(coordinates)), function(i) {
      coordinates[i, ]
    })
  } else if (!is.list(coordinates)) {
    stop(
      "`coordinates` must be a list of [lon, lat] pairs or an n x 2 matrix"
    )
  }
  lapply(coordinates, function(pair) {
    pair <- as.numeric(pair)
    # force doubles (avoid BigInt for ints)
    if (length(pair) != 2L || anyNA(pair)) {
      stop("Each coordinate must be a numeric [lon, lat] pair")
    }
    list(pair[[1]], pair[[2]])
    # list of 2 scalars -> JSON [lon, lat]
  })
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
      # (jupytergis_lab gis_document.py). Without this the map never renders:
      # the frontend only mounts its OpenLayers canvas after it observes a
      # change on the `options` map (`initialSyncReady` resolves from the
      # options observer), so an empty `options` leaves a toolbar but no map.
      #
      # We must *write* the defaults in a transaction rather than pass them as
      # the storage prelim above: register_storage's `get_or_insert_map()`
      # keeps only the prelim's type and drops its contents, so a seeded prelim
      # would still sync empty.
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

    #' @description Write a source, layer, and layerTree entry to the document.
    #' @param source_id The ID to use for the source.
    #' @param source The source object to store.
    #' @param layer_id The ID to use for the layer.
    #' @param layer The layer object to store.
    #' @return The layer id.
    .add_source_layer = function(source_id, source, layer_id, layer) {
      # Write source, layer, and layerTree in separate transactions so the frontend
      # treats the layer as newly added rather than updating a layer that does not
      # yet exist in the tree.
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

    #' @description Add a Raster Layer to the document.
    #' @param url Tiles URL.
    #' @param name Display name for the layer.
    #' @param attribution Attribution text.
    #' @param opacity Layer opacity in [0, 1].
    #' @param url_parameters Extra URL parameters for tile requests.
    #' @return The new layer id.
    add_raster_layer = function(
      url,
      name = NULL,
      attribution = "",
      opacity = 1,
      url_parameters = NULL
    ) {
      source_id <- .uuid()
      layer_id <- .uuid()

      if (is.null(name)) {
        name <- .extract_layer_name(url)
      }

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

      self$.add_source_layer(source_id, source, layer_id, layer)
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

      self$.add_source_layer(source_id, source, layer_id, layer)

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
    },

    #' @description Add a Vectortile Layer to the document.
    #' @param url Tiles URL.
    #' @param name Display name for the layer.
    #' @param attribution Attribution text.
    #' @param min_zoom smallest size of the marker.
    #' @param max_zoom largest size of the marker.
    #' @param opacity Layer opacity in [0, 1].
    #' @param url_parameters Extra URL parameters for tile requests.
    #' @return The new layer id.
    add_vectortile_layer = function(
      url,
      name = NULL,
      attribution = "",
      min_zoom = 0,
      max_zoom = 24,
      opacity = 1,
      url_parameters = NULL
    ) {
      source_id <- .uuid()
      layer_id <- .uuid()

      if (is.null(name)) {
        name <- .extract_layer_name(url)
      }

      source <- list(
        type = "VectorTileSource",
        name = paste0(name, " Source"),
        parameters = list(
          url = url,
          minZoom = min_zoom,
          maxZoom = max_zoom,
          attribution = attribution,
          htmlAttribution = attribution,
          provider = "",
          bounds = list(),
          urlParameters = list()
        )
      )

      layer <- list(
        type = "VectorTileLayer",
        name = name,
        visible = TRUE,
        parameters = list(
          source = source_id,
          opacity = opacity
        )
      )

      self$.add_source_layer(source_id, source, layer_id, layer)
    },

    #' @description Add a GeoJSON Layer to the document.
    #' @param path The path to the JSON file or URL to embed into the jGIS file.
    #' @param name The name that will be used for the object in the document.
    #' @param data The raw GeoJSON data to embed into the jGIS file
    #' @param opacity Layer opacity in [0, 1].
    #' @return The new layer id.
    add_geojson_layer = function(
      path = NULL,
      data = NULL,
      name = NULL,
      opacity = 1
    ) {
      if (inherits(path, "Path")) {
        path <- as.character(path)
      }

      parameters <- list()

      if (!is.null(path)) {
        if (startsWith(path, "http://") || startsWith(path, "https://")) {
          if (requireNamespace("httr", quietly = TRUE)) {
            resp <- httr::GET(path)
            httr::stop_for_status(resp)
          }
          parameters$path <- path
        } else {
          parameters$data <- jsonlite::fromJSON(path)
        }
      }

      if (!is.null(data)) {
        parameters$data <- data
      }

      # Extract name from path if not provided
      if (is.null(name) & !is.null(path)) {
        name <- .extract_layer_name(path)
      }

      # Fallback if still missing
      if (is.null(name)) {
        name <- "GeoJSON"
      }

      source_id <- .uuid()
      layer_id <- .uuid()

      source <- list(
        type = "GeoJSONSource",
        name = paste0(name, " Source"),
        parameters = parameters
      )

      layer <- list(
        type = "VectorLayer",
        name = name,
        visible = TRUE,
        parameters = list(
          source = source_id,
          opacity = opacity
        )
      )

      self$.add_source_layer(source_id, source, layer_id, layer)
    },

    #' @description Add a Image Layer to the document.
    #' @param url Image URL.
    #' @param coordinates Corners of image specified in longitude, latitude pairs.
    #' @param name Display name for the layer.
    #' @param opacity Layer opacity in [0, 1].
    #' @return The new layer id.
    add_image_layer = function(
      url,
      coordinates,
      name = NULL,
      opacity = 1
    ) {
      coordinates <- .normalize_coordinates(coordinates)

      source_id <- .uuid()
      layer_id <- .uuid()

      if (is.null(name)) {
        name <- .extract_layer_name(url)
      }

      source <- list(
        type = "ImageSource",
        name = paste0(name, " Source"),
        parameters = list(
          path = url,
          coordinates = coordinates
        )
      )

      layer <- list(
        type = "ImageLayer",
        name = name,
        visible = TRUE,
        parameters = list(
          source = source_id,
          opacity = opacity
        )
      )

      self$.add_source_layer(source_id, source, layer_id, layer)
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
