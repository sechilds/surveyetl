#' Create Statistical Significance
#'
#' Create a star indicator based on the p.value.
#'
#' Statistical tests return p-values (in a column called
#' p.value - if you use the `broom` package) and
#' we also create `effect_size` using the `lsr` package.
#' this creates the variable `triangle` that
#' shows both the direction, the statistical significance
#' and the effect size.
#'
#' @param df The data frame containing `p.value` and `effect_size`.
#' @return The data frame with a `significance` column
#'   added.
#' @export
create_triangle_field <- function(df) {
  up_hollow = 'UH'
  up_solid = 'US'
  down_hollow = 'DH'
  down_solid = 'DS'

  df %>%
    mutate(triangle = if_else(p.value < .05,
                             if_else(estimate1 > estimate2,
                                    if_else(abs(effect_size) < .3, up_hollow, up_solid),
                                    if_else(abs(effect_size) < .3, down_hollow, down_solid)), '',
                             missing = NA_character_))
}
