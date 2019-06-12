#' Question Table
#'
#' Extract the text of each question from the labelled
#' data loaded from the SPSS file.
question_table <- function(df, struct) {
  questions <- df %>%
    var_label %>%
    Filter(. %>% is.null %>% `!`, .)
  data <- tibble(field_name = names(questions),
                 question_text = questions) %>%
    unnest()
  data <- tibble::rowid_to_column(data, "question_id")
  data <- left_join(data, struct, by='field_name')
  as.tibble(data)
}
