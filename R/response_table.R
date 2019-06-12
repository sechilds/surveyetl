#' Response Table
#'
#' Create a table with one row for each response.
#'
#' Extract the answers/responses from the survey file.
#' Then we create a table where there is one row for
#' each question per respondent -- assuming they answered
#' the question.
#'
#' @param df The survey data frame. It should have variable labels.
#' @param excluded_fields A list of field names that are not
#'   questions and shouldn't be included.
#' @param respondent_id_field The field in the data frame that
#'   uniquely identifies each respondent within that cycle of
#'   the survey.
#' @param survey_year_field the field in the data frame that
#'   identifies the year of the survey.
#' @param survey_id A string representing the name of the survey.
#' @return A tibble containing all the responses of the survey,
#'   with the actual response, a character label and a score.
#' @examples
#' response_table(cgpss,
#'                survey_id = 'CGPSS',
#'                respondent_id_field = UNIV_RecordNum,
#'                survey_year_field = SURVEY_YEAR,
#'                excluded_fields = c('LANGUAGE', 'UNIV', 'PROGRAM_NAME', 'CIP'))
#' @export
response_table <- function(df,
                           respondent_id_field = NULL,
                           survey_year_field = NULL,
                           excluded_fields,
                           survey_id = NULL) {
  respondent_id_field = dplyr::enquo(respondent_id_field)
  survey_year_field = dplyr::enquo(survey_year_field)

  df %>%
    select(-one_of(excluded_fields)) %>%
    mutate(survey_id = survey_id,
           respondent_id = !! respondent_id_field,
           survey_year = !! survey_year_field) %>%
    select(-!! respondent_id_field) %>%
    select(-!! survey_year_field) %>%
    gather(field_name, response, -survey_id, -survey_year, -respondent_id) -> res1

  df %>%
    select(-one_of(excluded_fields)) %>%
    mutate(survey_id = survey_id,
           respondent_id = !! respondent_id_field,
           survey_year = !! survey_year_field) %>%
    mutate_at(vars(-respondent_id, -survey_year, -survey_id), to_character) %>%
    select(-!! respondent_id_field, -!! survey_year_field) %>%
    gather(field_name, label, -survey_id, -survey_year, -respondent_id) -> res2

  res3 <- res1 %>%
    inner_join(res2,
               by = c('survey_id', 'survey_year', 'respondent_id', 'field_name')) %>%
    mutate(score = as.numeric(iconv(response, from = 'latin1', to = 'utf8')))

  res3
}
