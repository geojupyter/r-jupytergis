<p align="center"><img width="100" src="https://raw.githubusercontent.com/geojupyter/jupytergis/main/packages/base/style/icons/logo.svg"></p>
<p align="center"><sub>Logo by <a href="https://github.com/IsabelParedes">Isabel Paredes</a></sub></p>
<h1 align="center">JupyterGIS</h1>

This is the R client for [JupyterGIS](https://github.com/geojupyter/jupytergis/).
It shares the same JS frontend as the Python client, as well as the same underlying
[CRDT](https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type) library
[Yrs](https://github.com/y-crdt/y-crdt) via its R bindinds
[Yr](https://github.com/y-crdt/yr).

The main interface is a `GISDocument` widget implemented via
[r-ywdigets](https://github.com/QuantStack/r-ywidgets).

## Installation
This project only works in JupyterLab with the [xeus-r](https://github.com/jupyter-xeus/xeus-r)
kernel.
The official installation method is based on Conda-Forge via (`mamba`/`conda`/`pixi`).

```bash
mamba install jupyterlab xeus-r r-ywidgets
```

## Getting started

The main interface is via the ``GISDocument`` class.
See [the example notebook](examples/gis.ipynb) for details.

```R
doc <- GISDocument$new("france_hiking.jGIS")
layer <- doc$add_raster_layer(
    url = "https://mt1.google.com/vt/lyrs=y&x={x}&y={y}&z={z}",
    name = "Google Satellite",
    attribution = "Google",
    opacity = 0.6
)
```

## See also

- [r-ywdigets](https://github.com/QuantStack/r-ywidgets): The R collaborative widget library.
- [JupyterGIS](https://github.com/geojupyter/jupytergis): The Jupyter frontend for the widgets
  and Python API to control them.
