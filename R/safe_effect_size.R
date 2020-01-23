#' Safe Effect Size
#'
#' Safely Calculate Effect Size by Group
#'
#' Calculate effect size on a set of data not worrying about
#' errors. This will return the results in a friendly
#' manner using the `broom` package -- or it will return
#' `NA`.
#'
#' @param df The data frame.
#' @param outcome The name of a numeric column that is the
#'   outcome you are testing. (Default `score`).
#' @param group A column in the data frame that will separate
#'   the data into two separate groups. (Default `value`).
#' @export
safe_effect_size <- function(df) {
  safe_cohensD <- purrr::safely(lsr::cohensD)
  res <- safe_cohensD(SCORE~value, data = df)
  if (is.null(res$result)) {
    return(tibble(x = NA_real_))
  } else {
    return(broom::tidy(res$result))
  }
}
