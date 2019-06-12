#' CGPSS Respondent Characteristics
#'
#' Return a list of fields describing CGPSS respondents
#' @export
cgpss_respondent_characteristics <- function() {
  c(cgpss_excluded_fields(),
  c( 'UNIV_RecordNum',
     'SURVEY_YEAR',
     'GENDER',
     'AGE',
     'RESIDENCE',
     'MARITAL',
     'CHILDREN',
     'CITIZEN',
     'CITIZENOther_Specified',
     'gBlack',
     'gEastAsia',
     'gSouthAsia',
     'gSouthEastAsia',
     'gWestAsia',
     'gLatinAm',
     'gMixed',
     'None',
     'gMixedOther',
     'ABORIGINAL',
     'DisablSelfID',
     'Disabl_Sense',
     'Disabl_Mobil',
     'Disabl_Learn',
     'Disabl_Mental',
     'Disabl_Autism',
     'Disabl_Chronic',
     'Disabl_Else',
     'Disabl_Else_Specified',
     'Disabl_NotRespond'))
}

#' CGPSS Excluded Fields
#'
#' Return a list of non-question CGPSS fields.
#'
#' In order to exclude non-question bits from the response
#' table, we need a list of fields that aren't questions.
#' @export
cgpss_excluded_fields <- function() {
  c('LANGUAGE',
    'UNIV',
    'PROGRAM_NAME',
    'CIP',
    'IMMIGRATION',
    'UNIV_YEARSTUDY',
    'UNIV_GENDER',
    'UNIV_PROGRAM_LEVEL',
    'UNIV_ACADLOAD',
    'UNIV_FACULTY',
    'UNIV_ACRONYM',
    'UNIV_ACRONYM_DESC',
    'UNIV_IMPOSE_STREAM',
    'CUSTOM1',
    'CUSTOM2',
    'CUSTOM3',
    'CUSTOM4',
    'CUSTOM5',
    'UNIV_DISCIPLINE',
    'UNIV_DISCIPLINE_DIM_FR',
    'UNIV_DISCIPLINE_DIM_EN',
    'UNIV_CLUSTER',
    'UNIV_CLUSTER_DIM_FR',
    'UNIV_CLUSTER_DIM_EN',
    'ACTUAL_STREAM',
    'ACTUAL_STREAM_DIM',
    'UNIV_DEGREE',
    'UNIV_DEGREE_DIM',
    'THESIS',
    'DEGREE_SELF_REPORT',
    'DISCIPLINE_COMBINED',
    'DISCIPLINE_SPECIFIED',
    'YEARSTUDY',
    'STARTED',
    'COMPLETED',
    'MODIFIED')
}
