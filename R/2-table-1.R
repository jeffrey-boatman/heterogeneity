library(tidyverse)
library(tableone)
library(labelled)

load("../RData/analysis.RData")
# load("../RData/complete_case.RData")

# pasted ----
set.seed(123)

load("../RData/analysis.RData")
analysis <- analysis %>% rename(trt = teh_treatment)

with(analysis, table(gender, trt, study))

# log-transform biomarkers
# biomarkers <- c("tne_bsl", "nnal_visit0", "phet_visit0", "cema_visit0", "pgem_visit0", 
#   "iso_visit0" , "nnal_visit20" , "phet_visit20", "cema_visit20", 
#   "pgem_visit20", "iso_visit20", "tne_20") 
# 
# 
# if (any(analysis[, biomarkers] < 0, na.rm = TRUE))
#   stop("can't log transform biomarkers")
# 
# analysis[, biomarkers] <- log(analysis[, biomarkers])


# prefix biomarkers with 'l' to indicate log
# names(analysis)[match(biomarkers, names(analysis))] <- paste0("l", biomarkers)


analysis <- analysis %>%
  mutate(cohort = ifelse(trt == 0, 1, ifelse(!is.na(tne_20) & tne_20 < 6.41, 2, 3)),
    menthol = ifelse(menthol == 0, "No", "Yes")) %>% 
  filter(study == "P2")


# primary analysis ----



com <- analysis
nn <- sort(names(com))

# t1 <- select(com, age, gender, race, edu, cpd_bsl, co_bsl, tne_bsl, ftnd_w_cpd,
#   ftnd_wo_cpd, wisdm_ts_bsl, wisdm_pdm_bsl, wisdm_sdm_bsl, menthol, trt)
t1 <- select(com, age, gender, race, edu, cpd_bsl, co_bsl, tne_bsl, ftnd_w_cpd,
  ftnd_wo_cpd, wisdm_ts_bsl, wisdm_pdm_bsl, wisdm_sdm_bsl, menthol, cohort)

# age
# sex
# race
# hispanic: don't have
# Education
# Employment: don't have
# CPD
# Years of regular smoking: don't have
# TNE
# NMR: don't have
# FTND
# FTND wo CPD
# WISDM total score
# WISDM primary motives
# WISDM Secondary motives
# Menthol
# Other tobacco products: don't have
# Previous quit attempts: don't have
# longest cigarette free interval: don't have



# --- Doesn't work yet --- #
t1 <- set_variable_labels(t1, 
  "age"           = "Age, mean(SD)", 
  "gender"        = "Female, n(%)", 
  "race"          = "Race, n(%)", 
  "edu"           = "Education n(%)", 
  "cpd_bsl"       = "Cigarettes per day, mean(SD)", 
  "co_bsl"        = "Carbon Monoxide, mean(SD), ppm",
  "tne_bsl"       = "TNE, nmol/mg, median(25, 75th percentile)", 
  "ftnd_w_cpd"    = "FTND, mean(SD)",
  "ftnd_wo_cpd"   = "FTND without cigarettes per day, mean(SD)", 
  "wisdm_ts_bsl"  = "WISDM total score, mean(SD)", 
  "wisdm_pdm_bsl" = "WISDM, primary motives, mean(SD)", 
  "wisdm_sdm_bsl" = "WISDM, secondary motives, mean(SD)", 
  "menthol"       = "Menthol Cigarettes, n(%)"
)


tab <- CreateTableOne(data = t1, 
  strata = "cohort", 
  test   = FALSE)

tab2 <- print(tab, 
  nonnormal  = "tne_bsl", 
  explain    = FALSE, 
  contDigits = 1, 
  noSpaces   = TRUE, 
  varLabels  = TRUE)
# print(xtable(tab2))

write.table(as.data.frame(tab2),
  quote = FALSE,
  sep = " & ")

with(analysis, {
  tot <- sum(cohort %in% c(2, 3))
  ncom <- sum(cohort == 2)
  pcom <- ncom / tot
  cat("--- Compliance Summary ---\n")
  cat(sprintf(" - Total in Immediate Reduction Group: %i\n", tot))
  cat(sprintf(" - Number Compliant: %i\n", ncom))
  cat(sprintf(" - Percent Compliant: %.1f%%", pcom * 100))
})
