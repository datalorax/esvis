# esvis
R Package for effect size visualizations.

[![Travis-CI Build Status](https://travis-ci.org/DJAnderson07/esvis.svg?branch=master)](https://travis-ci.org/DJAnderson07/esvis) 
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/DJAnderson07/esvis?branch=master&svg=true)](https://ci.appveyor.com/project/DJAnderson07/esvis) 
[![codecov](https://codecov.io/gh/DJAnderson07/esvis/branch/master/graph/badge.svg)](https://codecov.io/gh/DJAnderson07/esvis)

This package was designed primarily for visualizing achievement differences (achievement gaps) between student groups in educational data. However, it is generally designed to compare two or more distributions visually and could be used in any context where there is a need to compare distributions (e.g., treatment effects). A particular strength of the visualizations is the ability to evaluate overall differences between the distributions at all points on the scale, rather than only by measures of central tendency (e.g., means). There are also some functions for estimating effect size, including the area under the curve (conceptually equivalent to the probability that a randomly selected individual in distribution a has a higher value than a randomly selected individual from distribution b), and the *V* statistic, which essentailly transforms the area under the curve to standard deviation units (see [Ho, 2009](https://www.jstor.org/stable/40263526?seq=1#page_scan_tab_contents)).

## Installation
Install with *devtools* with


```r
# install devtools, if not already installed
# install.packages("devtools")

# install esvis
devtools::install_github("DJAnderson07/esvis")
```

## Basic Usage

Compute effect sizes for all possible pairwise comparisons.


```r
library(esvis)
coh_d(mean ~ subject, seda)
```

```
##   ref_group foc_group   estimate
## 1      math       ela  0.8312519
## 2       ela      math -0.8312519
```

Or specify a reference group


```r
coh_d(mean ~ grade, seda, 8)
```

```
##   ref_group foc_group estimate
## 1         8         7 0.593485
## 2         8         6 1.165106
## 3         8         5 1.819459
## 4         8         4 2.416754
## 5         8         3 3.004039
```

Currently implemented effect size calculations include Cohen's *d*, Hedge's *g*, percent above the cut, transformed percent above the cut, area under the curve, and the *V* statistic, as outlined by [Ho & Reardon, 2012](http://journals.sagepub.com/doi/abs/10.3102/1076998611411918).

Currently, the `pp_plot` function is the most fully developed visualization.


```r
pp_plot(mean ~ subject, seda)
```

![pp_plot1](https://photos-5.dropbox.com/t/2/AABjE8-TEoQlE90lZgQYLWnqgR9UXdqaSB3WmTgk1biGXQ/12/33227330/png/32x32/1/_/1/2/pp_plot1-1.png/ELXOmBkY66AYIAIoAg/JZu_noW3loJBTK-JMXhnbMqoAEYAj7Kf0PO-rsNcHTs?size=1280x960&size_mode=3)
When comparing more than one group, the reference group (which can be changed) is plotted along the x-axis.


```r
pp_plot(mean ~ grade, seda)
```
![pp_plot2](https://photos-4.dropbox.com/t/2/AAAP2e05onDssz5yGWwVC9ImUAm5H91ORHSjB7Irdf0BDw/12/33227330/png/32x32/1/_/1/2/pp_plot2-1.png/ELXOmBkY66AYIAIoAg/BohaD3AXvBS6RpLfpLp2U8WCfW29QbOUEnw14PGmeRs?size=1280x960&size_mode=3)