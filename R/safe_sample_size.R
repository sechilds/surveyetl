#' Get Sample Size
#'
#' Get sample sizes for two groups.
#'
#' This function finds the sample size for two
#' groups that are under comparison. It currently
#' uses `value` to separate out the two groups.
#'
#' @param df The data frame containg the survey data.
#' @param group The column in the data frame that splits
#'   the two groups.
#' @return A data frame with the count of the two groups.
#' @export
get_sample_size <- function(df, group = value) {
  df %>%
    group_by(group) %>%
    summarise(count = n()) %>%
    spread(value, count) %>%
    rename(n_comparison = `0`,
           n_group = `1`)
}

#' Safe Sample Size
#'
#' Safely get sample sizes for two groups.
#'
#' This function gets the sample size for two
#' groups and is robust to errors (such as one
#' of the groups not existing).
#'
#' @param df The survey data frame.
#' @return A tibble with the sample size.
#' @export
safe_sample_size <- function(df) {
  safe_get_sample_size <- purrr::safely(get_sample_size)
  res <- safe_get_sample_size(df)
  if (is.null(res$result)) {
    return(dplyr::tibble(n_comparison = NA,
                  n_group = NA))
  } else {
    return(res$result)
  }
}
