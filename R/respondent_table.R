#' Respondent Table
#'
#' Create a table with one row for each respondent.
#'
#' Extract respondent data from the survey file.
#' Then we make sure there is enough information to
#' uniquely identify each respondent.
#'
#' @param df The survey data frame. It should be labelled data.
#' @param respondent_characteristic_fields A list of field names
#'   that represent characteristics of the respondent. These are
#'   fields that you wish to include in the respondent table.
#' @param survey_id A string representing the name of the survey.
#'   (e.g. "CGPSS")
#' @param respondent_id_field The field in the data frame that
#'   uniquely identifies each respondent within that cycle of
#'   the survey.
#' @param survey_year_field The field in the data frame that
#'   identifies the year of the survey.
#' @return A tibble containing the characteristics of each respondent
#'   of the survey. Each respondent will be uniquely identified by
#'   the ID of the survey, the year of the survey and the respondent
#'   identifier.
#' @examples
#' respondent_table(cgpss,
#'                  c('LANGUAGE', 'UNIV', 'PROGRAM_NAME', 'CIP'),
#'                  survey_id = 'CGPSS',
#'                  respondent_id_field = UNIV_RecordNum,
#'                  survey_year_field = SURVEY_YEAR)
#' @export
respondent_table <- function(df,
                             respondent_characteristic_fields,
                             survey_id = NULL,
                             respondent_id_field = NULL,
                             survey_year_field = NULL) {
  respondent_id_field = enquo(respondent_id_field)
  survey_year_field = enquo(survey_year_field)

  df %>%
    select(one_of(respondent_characteristic_fields),
           !! respondent_id_field,
           !! survey_year_field
           ) %>%
    mutate(survey_id = survey_id,
           respondent_id = !! respondent_id_field,
           survey_year = !! survey_year_field)
}
