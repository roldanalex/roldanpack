
# roldanpack

<!-- badges: start -->
<!-- badges: end -->

Welcome to my personal package with functions that I use daily.

## Installation

You can install the development version of roldanpack like so:

``` r
# Install the package from Github
remotes::install_github('roldanalex/roldanpack')
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(roldanpack)

download_data(name, data)
download_plot(name, plot)
plot_roc(tag, score, model_name = NA, subtitle = NA, interval = 0.2, plotly = FALSE)
value_box(value, subtitle, icon, color, width = 3)
```

