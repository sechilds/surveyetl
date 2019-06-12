#' Compare Two Groups
#'
#' Compare two groups within the data.
#'
#' This function statistically compares two groups
#' and saves that information into a data frame.
#'
#' @param df The survey data frame
#' @param fac The faculty code of the first faculty you are comparing.
#' @param fac2 The faculty code you are comparing `fac` with.
#' @param ... The variables to group the dataset by.
#' @return A data frame with the statistical comparisons.
#' @export
compare_two_groups <- function(df, fac, fac2, ...) {
  group_vars <- quos(...)
  print(paste(fac, 'vs', fac2))
  df %>%
    select(field_name, score, positive, group1, !!!group_vars) %>%
    filter(group1 %in% c(fac2, fac)) %>%
    collect() %>%
    mutate(key = fac,
           value = if_else(group1 == fac, 1, 0)) %>%
    group_by(!!!group_vars, field_name) %>%
    nest() %>%
    mutate(result = purrr::map(data, safe_stats)) %>%
    select(-data) %>%
    unnest(result) %>%
    mutate(faculty = fac, comp_fac = fac2)
}
