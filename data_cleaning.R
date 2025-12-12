library(tidyverse)
library(haven)
library(fixest)

# filepaths
root_dir <- getwd()
data_raw <- read_dta(file.path(root_dir, "Replication Files Plos", "tables.dta")) 

# create raw wage difference variables
data_with_wage_diffs <- data_with_dummies %>%
  # male minus female
  mutate(diff_wage = ifelse(!is.na(wage) & !is.na(wage_female), wage - wage_female, NA_real_)) %>%
  # female minus male-- NOTE: orginal names have _OR
  mutate(diff_wage_OR = ifelse(!is.na(wage) & !is.na(wage_female), wage_female - wage, NA_real_)) %>%
  # d50 wage variables (woman earns more than man) -- NOTE: original cond don't have _OR version
  mutate(d50_wage = ifelse(!is.na(diff_wage_OR), diff_wage_OR < 0, NA),
         d50_pglabgro = ifelse(!is.na(diff_pglabgro_OR), diff_pglabgro_OR < 0, NA),
         d50_ijob1 = ifelse(!is.na(diff_ijob1_OR), diff_ijob1_OR < 0, NA),
         d50_plc0013_h = ifelse(!is.na(diff_plc0013_h_OR), diff_plc0013_h_OR < 0, NA)) %>%
  # wage difference bins
  mutate(bins_diff_wage = round(diff_wage/1000, 0)) %>%
  # wage difference if women > man
  mutate(dint = diff_wage * d50_wage) %>%
  # mean weighting factor by couple 
  group_by(pid) %>%
  mutate(wm = mean(phrf, na.rm=T)) %>%
  ungroup() %>%
  # share of household income  that is male
  mutate(share = wage/(wage_female+wage))

# key running variable centered at the cutoff
data_center_running_var <- data_scale_mcs %>%
  mutate(recode_diff_wage_OR = diff_wage_OR/1000,
         recode_diff_wage_OR_int = round(diff_wage_OR/1000, 0),
         recode_diff_wage = diff_wage/1000,
         recode_female_wage = wage_female/1000,
         recode_male_wage_OR=wage/1000,
         # create treatment var
         treatment = (diff_wage_OR>0) * 1
  )


# local variables for later
running = "recode_diff_wage_OR"
running_or = "recode_diff_wage_OR_int"
outcomes = c("overalllifesat", "worksat", "m11125", "std_mcs", "std_pcs", "overalllifesat_female", "worksat_female", "m11125_female", "std_mcs_female", "std_pcs_female")

# filter according to missing covariates and age limitations
data_male_sample <- data_center_running_var %>%
  # age constraints for males
  filter(age > 17, age < 65) %>%
  # keep partnered males, since female data already added in columns and dataset at partner level
  filter(partner == 1) %>%
  filter(female == 0) %>%
  # filter if missing education of either partner
  filter(!is.na(edu_isced), !is.na(edu_isced_female)) %>%
  # filter if missing number of children for either partner
  filter(!is.na(nchildren), !is.na(nchildren_female)) %>%
  # filter if missing running variable
  filter(!is.na(recode_diff_wage_OR))

# standardized health outcomes for male and female
data_std_outcomes <- data_male_sample %>%
  mutate(across(c('mcs', 'pcs', 'mcs_female', 'pcs_female'), scale, .names='std_{.col}'))





