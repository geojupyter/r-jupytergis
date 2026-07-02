# Helpers for openEO process-graph tile layers, used by
# `GISDocument$add_openeo_tile_layer()` (see R/document.R).

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
