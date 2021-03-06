
<!-- README.md is generated from README.Rmd. Please edit that file -->
ejoints
=======

The goal of ejoints is to work with empirical joint distributions.

Installation
------------

You can install ejoints from github with:

``` r
# install.packages("devtools")
devtools::install_github("AnthonyEbert/ejoints")
```

Example
-------

``` r
library(ejoints)


# Suppose we have the following 2D data points 

input <- matrix(
  c(1, 1, 2, 2, 2, 2.1, 5, 5, 3, 2, 10, 3), 
  ncol = 2, 
  byrow = TRUE
)

input
#>      [,1] [,2]
#> [1,]    1  1.0
#> [2,]    2  2.0
#> [3,]    2  2.1
#> [4,]    5  5.0
#> [5,]    3  2.0
#> [6,]   10  3.0


# Sample 5 values from this empirical density

rejoint(5, input = input)
#>          [,1]     [,2]     [,3]      [,4]     [,5]
#> [1,] 2.667644 3.478254 1.470917 0.5717271 1.977306
#> [2,] 2.722370 2.602672 3.587081 1.6119524 1.701695

# Compute empirical density at some point 4, 5

dejoint(c(4,5), input = input)
#> [1] 0.016361
```

Let's use ggplot2 to create a plot

``` r

library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)

x = seq(-10, 15, by = 0.25)
input_df <- dplyr::as_data_frame(expand.grid(x,x))
input_df$z = apply(input_df, 1, dejoint, input = input)

ggplot(input_df) + aes(x = Var1, y = Var2, z = z) + geom_contour()
```

![](README-plots-1.png)

``` r

ggplot(input_df) + aes(x = Var1, y = Var2, fill = z) + geom_raster()
```

![](README-plots-2.png)
