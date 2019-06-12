get_sample_size <- function(df) {
  df %>%
    group_by(value) %>%
    summarise(count = n()) %>%
    spread(value, count) %>%
    rename(n_comparison = `0`,
           n_group = `1`)
}

safe_sample_size <- function(df) {
  safe_get_sample_size <- safely(get_sample_size)
  res <- safe_get_sample_size(df)
  if (is.null(res$result)) {
    return(tibble(n_comparison = NA,
                  n_group = NA))
  } else {
    return(res$result)
  }
}
