
<!-- README.md is generated from README.Rmd. Please edit that file -->

# esvis

R Package for effect size visualization and estimation.

[![Build
Status](https://travis-ci.org/datalorax/esvis.svg?branch=master)](https://travis-ci.org/datalorax/esvis)

[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/DJAnderson07/esvis?branch=master&svg=true)](https://ci.appveyor.com/project/DJAnderson07/esvis)
[![codecov](https://codecov.io/gh/DJAnderson07/esvis/branch/master/graph/badge.svg)](https://codecov.io/gh/DJAnderson07/esvis)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/esvis)](https://cran.r-project.org/package=esvis)

This package is designed to help you very quickly estimate and visualize
distributional differences by categorical factors (e.g., the effect of
treatment by gender and income category). Emphasis is placed on
evaluating distributional differences across the entirety of the scale,
rather than only by measures of central tendency (e.g., means).

## Installation

Install directly from CRAN with

``` r
install.packages("esvis")
```

Or the development version (heavily updated, a new CRAN release is
planned soon) from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("datalorax/esvis")
```

## Plotting methods

There are three primary data visualizations: (a) binned effect size
plots, (b) probability-probability plots, and (c) empirical cumulative
distribution functions. All plots use the
[ggplot2](http://ggplot2.tidyverse.org) package and are fully
manipulable after creation using standard ggplot commands (e.g.,
changing the theme, labels, etc.). These plots were all produced by
first running `library(ggplot2); theme_set(theme_minimal())` to produce
the plots with the minimal theme, but no theme structure is imposed on
any of the plots.

### Binned ES Plot

At present, the binned effect size plot can only be produced with
Cohen’s *d*, although future development will allow the user to select
the type of effect size. The binned effect size plot splits the
distribution into quantiles specified by the user (defaults to lower,
middle, and upper thirds), calculates the mean difference between groups
within each quantile bin, and produces an effect size for each bin by
dividing by the overall pooled standard deviation (i.e., not by
quantile). For example

``` r
library(esvis)
binned_plot(benchmarks, math ~ ell)
```

![](README-binned_plot1-1.png)<!-- -->

Note that in this plot one can clearly see that the magnitude of the
differences between the groups depends upon scale location, as evidence
by the reversal of the effect (negative to positive) for the Non-ELL
(non-English Language Learners) group. We could also change the
reference group, change the level of quantile binning, and evaluate the
effect within other factors. For example, we can look by season
eligibility for free or reduced price lunch, with quintiles binning, and
non-ELL students as the reference group with

``` r
binned_plot(benchmarks, 
            math ~ ell + frl + season, 
            ref_group = "Non-ELL",
            qtile_groups = 5)
```

![](README-binned_plot2-1.png)<!-- -->

The `ref_group` argument can also supplied as a formula.

### PP Plots

Probability-probability plot can be produced with a call to `pp_plot`
and an equivalent argument structure. In this case, we’re visualizing
the difference in reading achievement by race/ethnicity by season.

``` r
pp_plot(benchmarks, reading ~ ethnicity + season)
```

![](README-pp_plot1-1.png)<!-- -->

Essentially, the empirical cummulative distribution function (ECDF) for
the reference group (by default, the highest performing group) is mapped
against the ECDF for each corresponding group. The magnitude of the
achievement gap is then displayed by the distance from the diagonal
reference line, representing, essentially, the ECDF for the reference
group.

By default, the area under the curve is shaded, which itself is an
effect-size like measure, but this is also manipulable.

### ECDF Plot

Finally, the `ecdf_plot` function essentially dresses up the base
`plot.ecdf` function, but also adds some nice referencing features
through additional, optional arguments. Below, I have included the
optional `hor_ref = TRUE` argument such that horizontal reference lines
appear, relative to the cuts provided.

``` r
ecdf_plot(benchmarks, math ~ season, 
    cuts = c(190, 200, 215))
```

![](README-ecdf_plot-1.png)<!-- -->

These are the curves that go into the PP-Plot, but occasionally can be
useful on their own.

## Estimation Methods

Compute effect sizes for all possible pairwise comparisons.

``` r
coh_d(benchmarks, math ~ season + frl)
#> # A tibble: 30 x 6
#>    season_ref frl_ref season_foc frl_foc  coh_d coh_se
#>    <chr>      <chr>   <chr>      <chr>    <dbl>  <dbl>
#>  1 Fall       FRL     Fall       Non-FRL  0.744 0.0706
#>  2 Fall       FRL     Spring     FRL      1.32  0.0496
#>  3 Fall       FRL     Spring     Non-FRL  2.01  0.0787
#>  4 Fall       FRL     Winter     FRL      0.625 0.0472
#>  5 Fall       FRL     Winter     Non-FRL  1.30  0.0733
#>  6 Fall       Non-FRL Fall       FRL     -0.744 0.0706
#>  7 Fall       Non-FRL Spring     FRL      0.550 0.0694
#>  8 Fall       Non-FRL Spring     Non-FRL  1.14  0.0919
#>  9 Fall       Non-FRL Winter     FRL     -0.127 0.0693
#> 10 Fall       Non-FRL Winter     Non-FRL  0.501 0.0872
#> # ... with 20 more rows
```

Or specify a reference group. In this case, I’ve used the formula-based
interface, but a string vector specifiying the specific reference group
could also be supplied.

``` r
coh_d(benchmarks, 
      math ~ season + frl, 
      ref_group = ~Fall + `Non-FRL`)
#> # A tibble: 5 x 6
#>   season_ref frl_ref season_foc frl_foc  coh_d coh_se
#>   <chr>      <chr>   <chr>      <chr>    <dbl>  <dbl>
#> 1 Fall       Non-FRL Fall       FRL     -0.744 0.0706
#> 2 Fall       Non-FRL Spring     FRL      0.550 0.0694
#> 3 Fall       Non-FRL Spring     Non-FRL  1.14  0.0919
#> 4 Fall       Non-FRL Winter     FRL     -0.127 0.0693
#> 5 Fall       Non-FRL Winter     Non-FRL  0.501 0.0872
```

Notice that the reference to Non-FRL is wrapped in back-ticks, which
should be used anytime there are spaces or other non-standard
characters.

Other effect sizes are estimated equivalently. For example, compute *V*
([Ho, 2009](https://www.jstor.org/stable/40263526?seq=1#page_scan_tab_contents))
can be estimated with

``` r
v(benchmarks, 
  math ~ season + frl, 
  ref_group = ~Fall + `Non-FRL`)
#> # A tibble: 5 x 5
#>   season_ref frl_ref season_foc frl_foc      v
#>   <chr>      <chr>   <chr>      <chr>    <dbl>
#> 1 Fall       Non-FRL Winter     Non-FRL -0.509
#> 2 Fall       Non-FRL Spring     FRL     -0.547
#> 3 Fall       Non-FRL Winter     FRL      0.110
#> 4 Fall       Non-FRL Spring     Non-FRL -1.15 
#> 5 Fall       Non-FRL Fall       FRL      0.702
```

or *AUC* with

``` r
auc(benchmarks, 
    math ~ season + frl, 
    ref_group = ~Fall + `Non-FRL`)
#> # A tibble: 5 x 5
#>   season_ref frl_ref season_foc frl_foc   auc
#>   <chr>      <chr>   <chr>      <chr>   <dbl>
#> 1 Fall       Non-FRL Winter     Non-FRL 0.360
#> 2 Fall       Non-FRL Spring     FRL     0.349
#> 3 Fall       Non-FRL Winter     FRL     0.531
#> 4 Fall       Non-FRL Spring     Non-FRL 0.209
#> 5 Fall       Non-FRL Fall       FRL     0.690
```
