#' Synthetic benchmark screening data
#' 
#' Across the country many schools engage in seasonal benchmark screenings to 
#' monitor to progress of their students. These are relatively brief
#' assessments administered to "check-in" on students' progress throughout
#' the year. This dataset was simulated from a real dataset from one large
#' school district using the terrific 
#' \href{https://CRAN.R-project.org/package=synthpop}{synthpop}
#' R package. Overall characteristics of the synthetic data are remarkably
#' similar to the real data.
#' 
#' @format A data frame with 10240 rows and 9 columns.
#'   \describe{
#'     \item{sid}{Integer. Student identifier.}
#' 	   \item{cohort}{Integer. Identifies the cohort from which the student was
#' 			sampled (1-3).}
#'     \item{sped}{Character. Special Education status: "Non-Sped" or "Sped"}
#' 	   \item{ethnicity}{Character. The race/ethnicity to which the student
#' 			identified. Takes on one of seven values: "Am. Indian", "Asian",
#' 			"Black", "Hispanic", "Native Am.", "Two or More", and "White"}
#' 	   \item{frl}{Character. Student's eligibility for free or reduced price
#' 			lunch. Takes on the values "FRL" and "Non-FRL".}
#' 	   \item{ell}{Character. Students' English language learner status. Takes 
#' 			on one of values: "Active", "Monitor", and "Non-ELL". Students
#' 			coded "Active" were actively receiving English language services
#' 			at the time of testing. Students coded "Monitor" had previously 
#' 			received services, but not at the time of testing. Students coded
#' 			"Non-ELL" did not receive services at any time.}
#'     \item{season}{Character. The season during which the assessment was
#' 			administered: "Fall", "Winter", or "Spring"}
#'     \item{reading}{Integer. Reading scale score.}
#'     \item{math}{Integer. Mathematics scale score.}
#' }

"benchmarks"