#' Safe Statistics
#'
#' Generate a set of comparison statistics safely
#'
#' Comparison statistics ready.
#'
#' @param df Survey data frame.
#' @return statistics
#' @export
safe_stats <- function(df) {
  dplyr::bind_cols(list(safe_ttest(df),
                 safe_effect_size(df),
                 safe_sample_size(df))) %>%
    create_statistical_significance() %>%
    rename(group_mean = estimate2,
           comparison_mean = estimate1,
           effect_size = x) %>%
    create_triangle_field()
}
