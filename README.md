# `hexscape`

An R package for aggregation of spatial data into hexagonal patches using simple features

## Installation

```r
# install.packages("remotes")
remotes::install_github("ku-awdc/hexscape")
```

In order to use `load_map` with NUTS and LAU datasets, it is important to download these
locally yourself. There is a function in `hexscape` that facilitates the downloading and
storing of these datasets for you. Run the following once the package is installed

```r
hexscape::download_maps()
```

From now now on, any use of `load_map` would use the retrieved NUTS and LAU data.
