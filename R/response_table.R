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
#' @param survey_id A string representing the name of the survey.
#' @param respondent_id_field The field in the data frame that
#'   uniquely identifies each respondent within that cycle of
#'   the survey.
#' @param survey_year_field the field in the data frame that
#'   identifies the year of the survey.
#' @return A tibble containing all the responses of the survey,
#'   with the actual response, a character label and a score.
#' @examples
#' response_table(cgpss,
#'                c('LANGUAGE, 'UNIV', 'PROGRAM_NAME', 'CIP'),
#'                survey_id = 'CGPSS',
#'                respondent_id_field = UNIV_RecordNum,
#'                survey_year_field = SURVEY_YEAR)
#' @export
response_table <- function(df,
                           excluded_fields,
                           survey_id = NULL,
                           respondent_id_field = NULL,
                           survey_year_field = NULL)
  respondent_id_field = enquo(respondent_id_field)
  survey_year_field = enquo(survey_year_field)

  df %>%
    select(-one_of(exclued_fields)) %>%
    mutate(survey_id = survey_id,
           respondent_id = !! respondent_id_field,
           survey_year = !! survey_year_field) %>%
    select(-!! respondent_id_field) %>%
    select(-!! survey_year_field) %>%
    gather(field_name, response, -survey_id, -survey_year, -respondent_id) -> res1

  df %>%
    select(-one_of(exclued_fields)) %>%
    mutate(survey_id = survey_id,
           respondent_id = !! respondent_id_field,
           survey_year = !! survey_year_field) %>%
    mutate_all(to_character) %>%
    select(-!! respondent_id_field, -!! survey_year_field) %>%
    gather(field_name, label, -survey_id, -survey_year, -respondent_id) -> res2

  res3 <- res1 %>%
    inner_join(res2,
               by = c('survey_id', 'survey_year', 'respondent_id')) %>%
    mutate(score = as.numeric(iconv(response, from = 'latin1', to = 'utf8')))

  res3
