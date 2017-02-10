# esvis
R Package for effect size visualizations.

[![Travis-CI Build Status](https://travis-ci.org/DJAnderson07/esvis.svg?branch=master)](https://travis-ci.org/DJAnderson07/esvis) 
[![codecov](https://codecov.io/gh/DJAnderson07/esvis/branch/master/graph/badge.svg)](https://codecov.io/gh/DJAnderson07/esvis)

This package was designed primarily for visualizing achievement differences (achievement gaps) between student groups in educational data. However, it is generally designed to compare two or more distributions visually and could be used in any context where there is a need to compare distributions (e.g., treatment effects). A particular strength of the visualizations is the ability to evaluate overall differences between the distributions at all points on the scale, rather than only by measures of central tendency (e.g., means). There are also some functions for estimating effect size, including the area under the curve (conceptually equivalent to the probability that a randomly selected individual in distribution a has a higher value than a randomly selected individual from distribution b), and the *V* statistic, which essentailly transforms the area under the curve to standard deviation units (see [Ho, 2009](https://www.jstor.org/stable/40263526?seq=1#page_scan_tab_contents)).

## Installation
Install with *devtools* with

```{r }
# install devtools, if not already installed
# install.packages(devtools)

# install esvis
devtools::install_github("DJAnderson07/esvis")
```
