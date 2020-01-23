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
get_sample_size <- function(df) {
  df %>%
    dplyr::group_by(value) %>%
    dplyr::summarise(count = n()) %>%
    tidyr::spread(value, count) -> counts
  group_names <- names(counts)
  group_names <- tibble(group1 = group_names[1],
                        group2 = group_names[2])
  names(counts) <- c('n1', 'n2')
  bind_cols(counts, group_names)
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
    return(dplyr::tibble(n1 = NA_integer_,
                  n2 = NA_integer_,
                  group1 = NA_character_,
                  group2 = NA_character_))
  } else {
    return(res$result)
  }
}
