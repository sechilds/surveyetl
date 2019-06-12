#' Question Table
#'
#' Create a table that includes the full text of each survey question.
#'
#' Extract the text of each question from the labelled
#' data loaded from the SPSS file.
#'
#' @param df The survey data frame. It should be labelled data.
#' @param struct A data frame holding the structure of the survey.
#'   This should include the field name and other information on
#'   each question that gets merged into the question table.
#' @return A tibble containing the field name, the full text of the
#'   question and any survey structure information.
#' @examples
#' question_table(cgpss, data_frame(field_name=character()))
#'
#' question_table(nsse, nsse_structure)
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
