library(tidyverse)
library(tableone)

load("../RData/analysis.RData")

# --- Doesn't work yet --- #
all <- set_variable_labels(all, 
  "Menthol" = "Use of Menthol Cigarettes",
  "BSL_co" = "Baseline Expired Carbon Monoxide (ppm)",        
  "BSL_TNE" = "Baseline Total Nicotine Equivalents",
  "BSL_totalcpd" = "Baseline Total Cigarettes/Day",
  "CES_aversion" = "CES Aversion",
  "CES_cravingreduction" = "CES Craving Reduction",
  "CES_enjoy" = "CES Enjoyment of Sensation",
  "CES_reward" = "CES Psychological Reward",
  "CES_satisfaction" = "CES Satisfaction",
  "CESD" = "CESD",      
  "FTND"  = "FTND",        
  "MNWS2_score" = "MNWS2 Score",
  "PANAS_neg" = "PANAS (Negative)",
  "PANAS_pos" = "PANAS (Positive)",
  "QSU_U_f1_BSL" = "Baseline QSU f1", 
  "QSU_U_f2_BSL" = "Baseline QSU f2",
  "QSU_U_tot_BSL" = "Baseline Total QSU",
  "WISDM_affective" = "WISDM Affective Enhancement",
  "WISDM_affiliative" = "WISDM Affiliative Attachment",   
  "WISDM_auto" = "WISDM Automaticity",
  "WISDM_cog"  = "WISDM Cognitive Enhancement",
  "WISDM_craving"  = "WISDM Craving", 
  "WISDM_cue" = "WISDM Cue Exposure",
  "WISDM_lossofcontrol" = "WISDM Loss of Control", 
  "WISDM_p"  = "WISDM Primary Dependence Motives",     
  "WISDM_s"  = "WISDM Secondary Dependence Motives",  
  "WISDM_social" = "WISDM Social Goads",   
  "WISDM_t" = "WISDM Total Score",      
  "WISDM_taste" = "WISDM Taste",
  "WISDM_tolerance" = "WISDM Tolerance",
  "WISDM_weight" = "WISDM Weight Control" )

tab <- CreateTableOne(data = select(analysis, -id), 
  strata = "study", 
  test   = FALSE)

tab2 <- print(tab, nonnormal=TRUE, explain = FALSE, contDigits = 0, noSpaces = TRUE, varLabels = TRUE)
print(xtable(tab2))
