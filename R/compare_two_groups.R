#' Compare Two Groups
#'
#' Compare two groups within the data.
#'
#' This function statistically compares two groups
#' and saves that information into a data frame.
#'
#' @param df The survey data frame
#' @param comp_id The column of the data frame that separates the
#'   data into two groups.
#' @param ... The variables to group the dataset by.
#' @return A data frame with the statistical comparisons.
#' @export
compare_two_groups <- function(df, comp_id, ...) {
  comp_id <- dplyr::enquo(comp_id)
  group_vars <- dplyr::enquos(...)
  df %>%
    select(field_name, score, !!comp_id, !!!group_vars) %>%
    mutate(value = !!comp_id) %>%
    group_by(!!!group_vars, field_name) %>%
    nest() %>%
    mutate(result = purrr::map(data, safe_stats)) %>%
    select(-data) %>%
    unnest(result)
}
