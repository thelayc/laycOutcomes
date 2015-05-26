# library(devtools)
# install_github('thelayc/laycUtils')
# install_github('thelayc/laycEnrollment')
library(laycUtils)
library(laycEnrollment)
library(stringr)
library(dplyr)
library(extrafont)

list.files()
list.files('../temp_data/fy14')

enroll_file <- ('../temp_data/fy14/raw_enrollment_report.txt')
pos_file <- ('../temp_data/fy14/raw_pos_report.txt')
tp_file <- ('../temp_data/fy14/raw_touchpoint_report_detailed.txt')
asmt_file <- ('../temp_data/fy14/raw_assessment_report.txt')
ref_file <- ('../temp_data/fy14/raw_referrals_report.txt')
ptype_file <- ('../temp_data/fy14/program_type.txt')

# Deal with pos data ------------------------------------------------------

# Load data
# enroll <- load_txt(enroll_file)
# enroll <- clean_data(enroll)
# ptype <- load_txt(ptype_file)
# ptype <- clean_data(ptype)
pos <- load_txt(pos_file)
pos <- clean_data(pos)

# check_attend <- unique(pos$pos_name)

## Identify type of information tracked by pos: attendance, scores, notes, etc.
# Identify pos tracking a contact with participants (as opposed to tracking scores, grades, etc.)
contact_pos <- 'attend|^case|^\\[promotor|contact'
score_pos <- 'score|level|credit'

pos$info_type <- 'other'
pos$info_type[str_detect(pos$pos_name, contact_pos)] <- 'contact'
pos$info_type[str_detect(pos$pos_name, score_pos)] <- 'score'

# Keep only records tracking attendance
pos %>%
  filter(info_type == 'contact') %>%
  filter(pos_value != 'no') %>%
  mutate(tool_id = pos_id,
         tool_name = pos_name,
         tool_type = 'pos') %>%
  select(subject_id, program_id, program_name, tool_type,
         tool_id, tool_name, date, time) ->
  test


# Deal with touchpoint data -----------------------------------------------
tp <- load_txt(tp_file)
tp <- clean_data(tp)

