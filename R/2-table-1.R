library(tidyverse)
library(tableone)
library(labelled)

load("../RData/analysis.RData")
load("../RData/complete_case.RData")

# restrict to compliant participants
analysis <- analysis %>%
  rename(trt = teh_treatment) %>%
  mutate(menthol = ifelse(menthol == 0, "No", "Yes")) %>%
  filter(trt == 0 | trt == 1 & log(tne_20) < log(6.41)) %>%
  filter(study == "P2")
com <- analysis[complete_case, ]

nn <- sort(names(com))

t1 <- select(com, age, gender, race, edu, cpd_bsl, co_bsl, tne_bsl, ftnd_w_cpd,
  ftnd_wo_cpd, wisdm_ts_bsl, wisdm_pdm_bsl, wisdm_sdm_bsl, menthol, trt)

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
  strata = "trt", 
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
