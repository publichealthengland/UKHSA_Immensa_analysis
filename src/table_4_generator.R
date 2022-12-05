#' table_4_generator
#'
#' `table_4_generator()` Produces all graphs used for the preprint
#'
#' @param surrounding_areas List of affected areas and their counterfactuals
#' 
#' @param labels list - A list of titles for detailing the proportion of tests by affected area
#'
table_4_generator <- function(surrounding_areas, labels) {
  x <- labels %>% select(Affected_area, prop_tests_in_area, prop_tests_lab_x)

  for (col in c("prop_tests_in_area", "prop_tests_lab_x")) {
    df <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df) <- colnames(x)
    if (col == "prop_tests_in_area") {
      df[nrow(df) + 1, ] <- list("", x[1, ][[col]], "")
      x1 <- rbind(x[2:nrow(x), ], df)
    } else {
      df[nrow(df) + 1, ] <- list("", "", x[1, ][[col]])
      x <- rbind(x1, df)
    }
  }

  colnames(x) <- c(
    "UTLA", "Percentage of tests in UTLA processed by the affected lab",
    "Percentage of all tests processed by the affected lab"
  )

  areas <- initialise_areas(surrounding_areas)

  x$UTLA <- c(
    names(areas),
    "Mean of nine most affected areas", "Total of nine most affected areas"
  )
  rownames(x) <- NULL

  return(x)
}
