#' Safe T-Test
#'
#' Safely Calculate T tests by Group
#'
#' Calculate t-tests on a set of data not worrying about
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
safe_ttest <- function(df) {
  safe_t.test <- purrr::safely(t.test)
  res <- safe_t.test(score~value, data = df)
  if (is.null(res$result)) {
    #warning(df)
    #warning(res$error)
    return(dplyr::tibble(estimate = NA,
                  estimate1 = NA,
                  estimate2 = NA,
                  statistic = NA,
                  p.value = NA,
                  conf.low = NA,
                  conf.high = NA,
                  estimate1name = NA,
                  estimate2name = NA))
  } else {
    group_names = names(res$result$estimate)
    names_tib = dplyr::tibble(estimate1name = group_names[1],
                              estimate2name = group_names[2])
    return(dplyr::bind_cols(broom::tidy(res$result), names_tib))
  }
}
