library(tidyverse)
library(haven)
library(fixest)

# filepaths
root_dir <- getwd()
data_raw <- read_dta(file.path(root_dir, "Replication Files Plos", "tables.dta")) 

# convert categorical variables with value labels to dummy variables
data_with_dummies <- data_raw %>%
  # remove value label for numerics
  mutate(
    across(
      starts_with(c("partner", "pglabgro", "plc0013_h", "plc0016", "ijob1", "overalllifesat", "worksat", "m11125")),
      zap_labels
    )
  ) %>%
  # convert value labels to factors
  mutate(
    across(
      where(haven::is.labelled),
      haven::as_factor
    )
  ) %>%
  # create dummy variables
  mutate(college = (edu_isced == "College or More") * 1,
         college_female = (edu_isced_female == "College or More") * 1
         )

# create raw wage difference variables
data_with_wage_diffs <- data_with_dummies %>%
  # male minus female
  mutate(diff_wage = ifelse(!is.na(wage) & !is.na(wage_female), wage - wage_female, NA_real_)) %>%
  # female minus male-- NOTE: orginal names have _OR
  mutate(diff_wage_OR = ifelse(!is.na(wage) & !is.na(wage_female), wage_female - wage, NA_real_),
         diff_pglabgro_OR = ifelse(!is.na(pglabgro) & !is.na(pglabgro_female), pglabgro_female - pglabgro, NA_real_),
         diff_ijob1_OR = ifelse(!is.na(ijob1) & !is.na(ijob1_female), ijob1_female - ijob1, NA_real_),
         diff_plc0013_h_OR = ifelse(!is.na(plc0013_h) & !is.na(plc0013_h_female), plc0013_h_female - plc0013_h, NA_real_)
  ) %>%
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

# new mental health for if MH is higher or lower than median
data_scale_mcs <- data_with_wage_diffs %>%
  mutate(med_mcs = cut(mcs, breaks=quantile(mcs, seq(0,1,0.5), na.rm=T, include=T, labels=F),
                       labels=c(0,1),
                       right=F,
                       include.lowest=T))

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

## calculate residuals for partners to reduce eliminate time variance

# fitting regression on participant ID
for (outcome in outcomes) {
  # regress outcome ~ 1 | pid (intercept + individual FE)
  fe_model <- feols(as.formula(paste(outcome, "~ 1 | pid")), data = data_std_outcomes)
  
  # get the exact sample
  model_data <- fixest_data(fe_model, sample = "estimation")
  model_rows <- rownames(model_data)
  
  # create residual variable for each outcome
  data_std_outcomes[[paste0(outcome, "_res")]] <- NA
  data_std_outcomes[model_rows, paste0(outcome, "_res")] <- resid(fe_model)
  
}

# save RDS
saveRDS(data_std_outcomes, file.path(root_dir, "Intermediate data files", "analysis_data.RDS"))

