
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mbte.vis

[![Lifecycle](https://img.shields.io/badge/lifecycle-active-green.svg)]()
[![Travis-CI Build
Status](https://travis-ci.org/mkerschbaumer/mbte.vis.svg?branch=master)](https://travis-ci.org/mkerschbaumer/mbte.vis)

The goal of mbte is to combine the
[mbte](https://mkerschbaumer.github.io/mbte)-package with the
[trelliscopejs](https://github.com/hafen/trelliscopejs)-package. The
package is mainly intended to visualize fits created by the
mbte-package. Using *trend modules* provided in this package, trends can
be detected and filtered by model coefficients.

## Installation

You can install mbte.vis from github with:

``` r
devtools::install_github("mkerschbaumer/mbte.vis")
```

## Example

``` r
# Load example dataset from mbte-package
library(mbte)
library(mbte.vis)
library(dplyr)
data(filtered_signals)
```

Detect linear and logistic trends using trend modules:

``` r
fits <- mbte_coef_fit(
  filtered_signals,
  lin = tm_linear(),
  sig = tm_logistic()
)

fits
#> # A tibble: 37 x 5
#>    mv    signal_nr signal            fits              coefficients     
#>    <chr>     <int> <list>            <list>            <list>           
#>  1 mv1           1 <tibble [22 × 2]> <tibble [22 × 2]> <tibble [1 × 12]>
#>  2 mv3           2 <tibble [22 × 2]> <tibble [22 × 2]> <tibble [1 × 12]>
#>  3 mv5           1 <tibble [30 × 2]> <tibble [30 × 2]> <tibble [1 × 12]>
#>  4 mv6           1 <tibble [21 × 2]> <tibble [21 × 2]> <tibble [1 × 12]>
#>  5 mv6           3 <tibble [30 × 2]> <tibble [30 × 2]> <tibble [1 × 12]>
#>  6 mv7           1 <tibble [56 × 2]> <tibble [56 × 2]> <tibble [1 × 12]>
#>  7 mv8           1 <tibble [29 × 2]> <tibble [29 × 2]> <tibble [1 × 12]>
#>  8 mv8           2 <tibble [26 × 2]> <tibble [26 × 2]> <tibble [1 × 12]>
#>  9 mv9           1 <tibble [63 × 2]> <tibble [63 × 2]> <tibble [1 × 12]>
#> 10 mv10          1 <tibble [25 × 2]> <tibble [25 × 2]> <tibble [1 × 12]>
#> # … with 27 more rows
```

Compute error metric for each fit (to sort fits later on):

``` r
# compute normalized root mean squared error
nrmse <- function(pred, obs) {
  sqrt(mean((pred - obs)^2)) / (max(obs) - min(obs))
}

# compute minimum score for each (sub)signal - see mbte package
metrics <- fits %>%
  mbte_compute_metrics(nrmse = nrmse(.pred, .obs)) %>% 
  group_by(mv, signal_nr) %>%
  summarise(score = min(result, na.rm = TRUE))
metrics
#> # A tibble: 37 x 3
#> # Groups:   mv [?]
#>    mv    signal_nr score
#>    <chr>     <int> <dbl>
#>  1 mv1           1 0.166
#>  2 mv10          1 0.156
#>  3 mv11          2 0.241
#>  4 mv12          3 0.164
#>  5 mv13          1 0.118
#>  6 mv15          1 0.144
#>  7 mv16          1 0.134
#>  8 mv17          1 0.234
#>  9 mv17          2 0.129
#> 10 mv18          1 0.152
#> # … with 27 more rows
```

Join the fitted signals with the computed scores:

``` r
combined <- fits %>%
  left_join(metrics, by = c("mv", "signal_nr")) %>%
  mbte_reconstruct(fits) # currently needed to preserve needed attributes
combined
#> # A tibble: 37 x 6
#>    mv    signal_nr signal           fits            coefficients      score
#>    <chr>     <int> <list>           <list>          <list>            <dbl>
#>  1 mv1           1 <tibble [22 × 2… <tibble [22 × … <tibble [1 × 12… 0.166 
#>  2 mv3           2 <tibble [22 × 2… <tibble [22 × … <tibble [1 × 12… 0.0879
#>  3 mv5           1 <tibble [30 × 2… <tibble [30 × … <tibble [1 × 12… 0.237 
#>  4 mv6           1 <tibble [21 × 2… <tibble [21 × … <tibble [1 × 12… 0.246 
#>  5 mv6           3 <tibble [30 × 2… <tibble [30 × … <tibble [1 × 12… 0.0933
#>  6 mv7           1 <tibble [56 × 2… <tibble [56 × … <tibble [1 × 12… 0.246 
#>  7 mv8           1 <tibble [29 × 2… <tibble [29 × … <tibble [1 × 12… 0.222 
#>  8 mv8           2 <tibble [26 × 2… <tibble [26 × … <tibble [1 × 12… 0.221 
#>  9 mv9           1 <tibble [63 × 2… <tibble [63 × … <tibble [1 × 12… 0.243 
#> 10 mv10          1 <tibble [25 × 2… <tibble [25 × … <tibble [1 × 12… 0.156 
#> # … with 27 more rows
```

The table above can be visualized via the trelliscopejs package using
the following wrapper function:

``` r
mbte_visualize_coef(combined, mv, signal_nr)
```

Trelliscopejs allows to filter/sort the data to visualize. In this
example, sorting by the score in ascending order is applied. A low error
score indicates, that a trend module was able to generalize the
underlying trend:

![](images/trelliscopejs-visualisation.png)
