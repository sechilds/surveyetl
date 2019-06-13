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
    select(!! comp_id) %>%
    unique() %>%
    pull(!! comp_id)
  group_pairs <- combinat::combn(groups, 2, simplify = FALSE)
  group_pairs %>%
    map_dfr(function(x) {pairwise_test(df, !! comp_id, x)})
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
    filter(!! comp_id %in% pair) %>%
    compare_two_groups(!! comp_id)
}
