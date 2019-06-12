library(tidyverse)
library(haven)
library(labelled)
library(surveyetl)

cgpss <- read_sav('H:/GROUPS/Surveys/CGPSS/CGPSS-2016/Results/YORK_CGPSS_2016_SPSS_DATA.sav')

cgpss_structure <- read_csv('cgpss_2016_structure.csv',
                            col_types = 'icciic'
                            ) %>%
  select(-question_file_order, -question_text)

qtable <- question_table(cgpss, cgpss_structure,
                         excluded_fields = cgpss_excluded_fields())

write_csv(qtable, 'cgpss_2016_questions.csv', append = FALSE)

respondents <- respondent_table(cgpss,
                                cgpss_respondent_characteristics(),
                                survey_id = 'CGPSS',
                                respondent_id_field = UNIV_RecordNum,
                                survey_year_field = SURVEY_YEAR)

responses <- response_table(cgpss,
                            respondent_id_field = UNIV_RecordNum,
                            survey_year_field = SURVEY_YEAR,
                            survey_id = 'CGPSS',
                            excluded_fields = cgpss_excluded_fields())

responses %>%
  left_join(respondents, by = c('survey_id', 'survey_year', 'respondent_id')) %>%
  left_join(qtable, by = ('field_name')) -> full_survey

