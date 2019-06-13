#' Create Statistical Significance
#'
#' Create a star indicator based on the p.value.
#'
#' Statistical tests return p-values (in a column called
#' p.value - if you use the `broom` package) and
#' this creates the variable `significance` that
#' has the stars.
#'
#' @param df The data frame containing `p.value`.
#' @return The data frame with a `significance` column
#'   added.
#' @export
create_statistical_significance <- function(df) {
  df %>%
    mutate(significance = ifelse(p.value < .001, '***',
                                 ifelse(p.value < .01, '**',
                                        ifelse(p.value < .05, '*', ''))))
}
