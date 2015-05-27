# library(devtools)
# install_github('thelayc/laycUtils')
# install_github('thelayc/laycEnrollment')
library(laycUtils)
library(laycEnrollment)
library(stringr)
library(dplyr)
library(extrafont)



# 1 - Load data -----------------------------------------------------------

# list.files()
# list.files('../temp_data/fy14', full.names = TRUE)

enroll_file <- ('../temp_data/fy14/raw_enrollment_report.txt')
pos_file <- ('../temp_data/fy14/raw_pos_report.txt')
tp_file <- ('../temp_data/fy14/raw_touchpoint_report_detailed.txt')
job_file <-  "../temp_data/fy14/raw_job_report.txt"
# asmt_file <- ('../temp_data/fy14/raw_assessment_report.txt')
# ref_file <- ('../temp_data/fy14/raw_referrals_report.txt')
ptype_file <- ('../temp_data/fy14/program_type.txt')

pos <- load_txt(pos_file)
pos <- format_data(pos)
enroll <- load_txt(enroll_file)
enroll <- format_data(enroll)
ptype <- load_txt(ptype_file)
ptype <- format_data(ptype)
tp <- load_txt(tp_file)
tp <- format_data(tp)
job <- load_txt(job_file)
job <- format_data(job)

# 2 - Workforce total-------------------------------------------------------
workforce_programs <- c("ss - ccorps projects", "ss - ged", "ss - job placement", "ss - job readiness",
                        "pg - employment case management", "pg - employment ged", "pg - employment job placement",
                        "pg - employment job training", "dc - wise ged", "dc - wise job placement",
                        "dc - wise job training")

laycEnrollment::get_enroll(enroll, eto_programs = workforce_programs)

# 3 - ged -----------------------------------------------------------------
ged_enroll(enroll)
ged_gains(pos)
ged_take(pos)
ged_pass(enroll)

# 4 - JRT -----------------------------------------------------------------

jrt_enroll(enroll)
jrt_gains(tp)

# 5 - Job placement --------------------------------------------------------
job_enroll(enroll)
job_placed(enroll, job)
job_retention(enroll, job)


jrt <- data.frame(
  measure=c("% enrolled in JRT", "% assessed on soft skills", "% gaining soft-skills"),
  target=c(.75, .9, .72),
  value=c(232, 50, 36),
  n = c(407, 232, 50)
)

# Data cleaning and transformation
df$value_scaled <- round(df$value / df$n, 3)
df$y_title <- paste(df$measure, '\nn =', df$n)
df$target_met[df$value_scaled < df$target / 2] <- 'low'
df$target_met[df$value_scaled >= df$target / 2 & df$value_scaled < df$target] <- 'medium'
df$target_met[df$value_scaled >= df$target] <- 'high'
df$target_met <- ordered(df$target_met)
df$target_met <- ordered(df$target_met, levels = c("low", "medium", "high"))
