
<!-- README.md is generated from README.Rmd. Please edit that file -->
esvis
=====

R Package for effect size visualizations.

[![Travis-CI Build Status](https://travis-ci.org/DJAnderson07/esvis.svg?branch=master)](https://travis-ci.org/DJAnderson07/esvis) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/DJAnderson07/esvis?branch=master&svg=true)](https://ci.appveyor.com/project/DJAnderson07/esvis) [![codecov](https://codecov.io/gh/DJAnderson07/esvis/branch/master/graph/badge.svg)](https://codecov.io/gh/DJAnderson07/esvis) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/esvis)](https://cran.r-project.org/package=esvis)

This package is designed to visually compare two or more distributions across the entirety of the scale, rather than only by measures of central tendency (e.g., means). There are also some functions for estimating effect size, including Cohen's *d*, Hedges' *g*, percentage above a cut, transformed (normalized) percentage above a cut, the area under the curve (conceptually equivalent to the probability that a randomly selected individual from Distribution A has a higher value than a randomly selected individual from Distribution B), and the *V* statistic, which essentially transforms the area under the curve to standard deviation units (see [Ho, 2009](https://www.jstor.org/stable/40263526?seq=1#page_scan_tab_contents)).

Installation
------------

Install directly from CRAN with

``` r
install.packages("esvis")
```

Or the development version from from github with:

``` r
# install.packages("devtools")
devtools::install_github("DJAnderson07/esvis")
```

Plotting methods
----------------

There are three primary data visualizations: (a) binned effect size plots, (b)probability-probability plots, and (c) empirical cumulative distribution functions. All plots should be fully manipulable with calls to the base plotting functions.

At present, the binned effect size plot can only be produced with Cohen's *d*, although future development will allow the user to select the type of effect size. The binned effect size plot splits the distribution into quantiles specified by the user (defaults to lower, middle, and upper thirds), calculates the mean difference between groups within each quantile bin, and produces an effect size for each bin by dividing by the overall pooled standard deviation (i.e., not by quantile). For example

``` r
library(esvis)
binned_plot(math ~ ell, benchmarks)
```

![binned\_plot](inst/image/README-binned_plot-1.png)

Note that in this plot one can clearly see that the magnitude of the differences between the two three groups depends upon scale location (i.e., low achieving students versus average or high achieving students). Both the reference group and the quantiles used can be changed. For example `binned_plot(math ~ ell, benchmarks, ref_group = "Non-ELL", qtiles = seq(0, 1, .2))` would produce the same plot but binned by quintiles, with students who did not receive English language services (Non-ELL) as the reference group.

A probability-probability plot can be produced with a call to `pp_plot` and an equivalent argument structure. In this case, we're visualizing the difference in reading achievement by race/ethnicity. By default, the distribution with the highest mean serves as the reference group, in this case students identifying as White.

``` r
pp_plot(reading ~ ethnicity, benchmarks)
```

![pp\_plot1](inst/image/README-pp_plot1-1.png)

If the grouping factor has only two levels, the area under the PP curve will be shaded, with the AUC an *V* statistics annotated onto the plot.

``` r
pp_plot(reading ~ frl, benchmarks)
```

![pp\_plot2](inst/image/README-pp_plot2-1.png)

The shading and annotations are optional and can be removed. The colors and all other plot features are also fully customizable.

Finally, the `ecdf_plot` function essentially dresses up the base `plot.ecdf` function, but also adds some nice referencing features through additional, optional arguments. Below, I have included the optional `hor_ref = TRUE` argument such that horizontal reference lines appear, relative to the cuts provided.

``` r
ecdf_plot(math ~ season, benchmarks, 
    ref_cut = c(190, 200, 215), 
    hor_ref = TRUE)
```

![ecdf\_plot](inst/image/README-ecdf_plot-1.png)

Estimation Methods
------------------

Compute effect sizes for all possible pairwise comparisons.

``` r
coh_d(mean ~ subject, seda)
#>   ref_group foc_group   estimate
#> 1      math       ela  0.8312519
#> 2       ela      math -0.8312519
```

Or specify a reference group

``` r
coh_d(mean ~ grade, seda, ref_group = 8)
#>   ref_group foc_group estimate
#> 1         8         7 0.593485
#> 2         8         6 1.165106
#> 3         8         5 1.819459
#> 4         8         4 2.416754
#> 5         8         3 3.004039
```

Other effect sizes are estimated equivalently. For example, compute *V* ([Ho, 2009](https://www.jstor.org/stable/40263526?seq=1#page_scan_tab_contents)) with

``` r
v(mean ~ grade, seda, ref_group = 8)
#>   ref_group foc_group estimate
#> 1         8         7 0.605855
#> 2         8         6 1.202515
#> 3         8         5 1.912094
#> 4         8         4 2.577780
#> 5         8         3 3.225021
```

or *AUC* with

``` r
auc(mean ~ grade, seda, ref_group = 8)
#>   ref_group foc_group  estimate
#> 1         8         7 0.6658216
#> 2         8         6 0.8024226
#> 3         8         5 0.9118211
#> 4         8         4 0.9658305
#> 5         8         3 0.9887090
```
