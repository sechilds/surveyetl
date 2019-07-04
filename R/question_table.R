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
#' @param excluded_fields A list of fields that are not questions,
#'   so shouldn't be saved to the question list.
#' @return A tibble containing the field name, the full text of the
#'   question and any survey structure information.
#' @examples
#' question_table(cgpss, data_frame(field_name=character()))
#'
#' question_table(nsse, nsse_structure)
#' @export
question_table <- function(df, struct, excluded_fields = NULL) {
  if (is.null(excluded_fields) == FALSE) {
  questions <- df %>%
    dplyr::select(-one_of(excluded_fields))
  } else {
    questions <- df
  }
  questions %>%
    var_label %>%
    Filter(. %>% is.null %>% `!`, .) -> questions
  data <- tibble(field_name = names(questions),
                 question_text = questions) %>%
    unnest()
  data <- tibble::rowid_to_column(data, "question_file_order")
  data <- left_join(data, struct, by='field_name')
  as.tibble(data)
}
