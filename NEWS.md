# esvis 0.3.1.0000
Minor update to fix breaking changes with the release of dplyr 1.0

# esvis 0.3.0.0000
This is a major update that:

* Moves all the plottingfunctionality to ggplot2. 

* Extends the plotting by allowing faceting with `+` in the
  formula. Up to two additional variables are possible.
  
* Changed the API so the data argument comes first, followed 
  by the formula.

# esvis 0.2.1.0000 (no submitted to CRAN)
This releases fixes bugs introduced from dependency updates.

# esvis 0.2.0.0000

This release is mostly about reformatting code and minor bug fixes. A few changes:

* The [viridisLite](https://CRAN.R-project.org/package=viridisLite) package is now listed as a `suggests`, and there are options for the plots to be produced with these color schemes, if the package is installed.

* A few of the effect sizes were reversed in 0.1, relative to the focal/reference groups. Those have been fixed.

* There is now a `theme` function that is extensible and allows for custom themes, rather than just the "standard" and "dark" themes.
