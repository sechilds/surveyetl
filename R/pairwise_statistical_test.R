#' Pairwise Statistical Tests
#'
#' Run statistical test between each combination of groups
#'
#' Given a grouping variable (`comp_id`), go through all
#' possible combinations of pairs and perform statistical
#' tests on each one.
#'
#' @param df The data frame containing the survey data.
#' @param comp_id The column which identifies the groups
#'   in the data that you want to compare against each
#'   other.
#' @return A data frame of all the statistical comparisons.
#' @export
pairwise_statistical_tests <- function(df, comp_id) {
  comp_id <- dplyr::enquo(comp_id)
  groups <- df %>%
    dplyr::select(!! comp_id) %>%
    dplyr::distinct() %>%
    dplyr::pull(!! comp_id)
  group_pairs <- combinat::combn(groups, 2, simplify = FALSE)
  group_pairs %>%
    purrr::map_dfr(function(x) {pairwise_test(df, comp_id, x)}) -> res
  bind_rows(res, reverse_pairwise_comparison(res))
}


#' Pairwise Test
#'
#' Run a Single Pairwise test
#'
#' A helper function for `pairwise_statistical_tests`.
#'
#' @param df the data frame
#' @param comp_id the field used to break up the data into groups
#' @param pair a list of two groups that we want to compare
#' @return a data frame containing the statistics
#' @export
pairwise_test <- function(df, comp_id, pair) {
  df %>%
    dplyr::filter(!! comp_id %in% pair) %>%
    compare_two_groups(!! comp_id)
}

swap_fields <- function(df, field1, field2) {
  field1 <- dplyr::enquo(field1)
  field2 <- dplyr::enquo(field2)
  df %>%
    mutate(temp_field := !! field1) %>%
    mutate(!!field1 := !! field2) %>%
    mutate(!!field2 := temp_field) %>%
    select(-temp_field)
}

reverse_triangle <- function(df) {
  df %>%
    dplyr::mutate(triangle = dplyr::case_when(triangle == 'US' ~ 'DS',
                                              triangle == 'UH' ~ 'DH',
                                              triangle == 'DS' ~ 'US',
                                              triangle == 'DH' ~ 'UH',
                                              is.null(triangle) ~ '',
                                              is.na(triangle) ~ '',
                                              TRUE ~ triangle))
}

#' @export
reverse_pairwise_comparison <- function(df) {
  df %>%
    swap_fields(estimate1, estimate2) %>%
    swap_fields(estimate1name, estimate2name) %>%
    swap_fields(n1, n2) %>%
    swap_fields(group1, group2) %>%
    mutate(estimate = -estimate) %>%
    reverse_triangle()
}
