knitr::opts_chunk$set(echo = FALSE)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(reshape2)
library(tableone)
library(xtable)
library(labelled)
set.seed(1234)
#Eric portion
smoke <- read.csv(file = "C:/Users/Chuyu/Desktop/Research/ForestData.csv", header=TRUE)
setnames(smoke, 
         old = c("treatment_forChuyu", "Demo_9_SCR", "Demo_2_1_TEXT_SCR", "Demo_3_SCR", "race_3cat", "menthol", "CES_enjoyment_BSL2", "CES_cravingreduction_BSL2", "MNWS2_score_BSL2", "QSU_U_f1_BSL2", "QSU_U_f2_BSL2", "QSU_U_tot_BSL2", "CES_satisfaction_BSL2", "CES_reward_BSL2", "CES_aversion_BSL2", "WISDM_affat_BSL1", "WISDM_aut_BSL1", "WISDM_loc_BSL1", "WISDM_ce_BSL1", "WISDM_crav_BSL1", "WISDM_ceap_BSL1", "WISDM_seg_BSL1", "WISDM_taste_BSL1", "WISDM_tol_BSL1", "WISDM_wc_BSL1", "WISDM_ae_BSL1", "WISDM_pdm_BSL1", "WISDM_sdm_BSL1", "WISDM_ts_BSL1", "PANAS1_pos_BSL1", "PANAS1_neg_BSL1", "FTND_score_SCR", "CESD_score_BSL1", "co_BSL2", "tne_nmolperml_BSL2", "baseline_cpd"), 
         new = c("Treatment", "Education", "Age", "Gender", "Race", "Menthol", "CES_enjoy", "CES_cravingreduction", "MNWS2_score", "QSU_U_f1_BSL", "QSU_U_f2_BSL", "QSU_U_tot_BSL", "CES_satisfaction", "CES_reward", "CES_aversion", "WISDM_affiliative", "WISDM_auto", "WISDM_lossofcontrol", "WISDM_cog", "WISDM_craving", "WISDM_cue", "WISDM_social", "WISDM_taste", "WISDM_tolerance", "WISDM_weight", "WISDM_affective", "WISDM_p", "WISDM_s", "WISDM_t", "PANAS_pos", "PANAS_neg", "FTND", "CESD", "BSL_co", "BSL_TNE", "BSL_totalcpd"))
smoke <- subset(smoke,smoke$BSL_totalcpd<9000 & smoke$BSL_TNE<9000 & !is.na(smoke$PANAS_neg))
smoke <- subset(smoke, select = -c(treatment_label, treatment, CENIC_subject_id, tot_nnal_pmolperml_belowLOD_week6, cannabis, baseline_drinks_tlfb, CESD_score_week6))
smoke$tne_nmolperml_week6 <- log(smoke$tne_nmolperml_week6)
smoke$tot_nnal_pmolperml_week6 <- log(smoke$tot_nnal_pmolperml_week6)
cols <- c(3, 4, 5, 6)
smoke[,cols] <- lapply(smoke[,cols], factor)
levels(smoke$Education) <- c("1", "1", "2", "3", "3", "3")
#gender: 1 is male, 2 is female
#race: 0 is white, 1 is black, 2 is other
#menthol: 0 is non menthol, 1 is menthol (?)
eric <- smoke



#Dorothy portion
smoke <- read.csv(file = "C:/Users/Chuyu/Desktop/Research/CENIC1P2 data/CENIC1P2_wide_2017_11_22.csv", header=TRUE)
smoke <- smoke[, c(4, 9, 135, 136, 138, 139, 147, 148, 149, 150, 151, 152, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 175, 176, 177, 178, 271, 274, 283, 285, 288, 567, 569, 572, 576, 579, 583, 586, 617, 620)]
smoke$nnal_visit8 <- log(smoke$nnal_visit8)
smoke$tne_visit8 <- log(smoke$tne_visit8)
setnames(smoke, 
         old = c("demo_9_3c", "AGE_CLEAN", "GENDER_CLEAN", "race_3c", "Menthol_Status_num", "ces1_3_visit0", "ces1_11_visit0", "mnws_total_visit0", "qsu_u_factor1_visit0", "qsu_u_factor2_visit0", "qsu_u_total_visit0", "ces1_satisfaction_visit0", "ces1_psych_reward_visit0", "ces1_aversion_visit0", "WISDM_Affiliative_visit91", "WISDM_Automaticity_visit91", "WISDM_Loss_of_Control_visit91", "WISDM_Cognitive_visit91", "WISDM_Craving_visit91", "WISDM_Cue_visit91", "WISDM_Social_visit91", "WISDM_Taste_visit91", "WISDM_Tolerance_visit91", "WISDM_Weight_visit91", "WISDM_Affective_visit91", "WISDM_p_visit91", "WISDM_s_visit91", "WISDM_t_visit91", "PANAS_positive_visit91", "PANAS_negative_visit91", "ftndwcpd_visit92", "CESD_total_visit92", "co_visit0", "tne_visit0", "totalcpd_visit0"), 
         new = c("Education", "Age", "Gender", "Race", "Menthol", "CES_enjoy", "CES_cravingreduction", "MNWS2_score", "QSU_U_f1_BSL", "QSU_U_f2_BSL", "QSU_U_tot_BSL", "CES_satisfaction", "CES_reward", "CES_aversion", "WISDM_affiliative", "WISDM_auto", "WISDM_lossofcontrol", "WISDM_cog", "WISDM_craving", "WISDM_cue", "WISDM_social", "WISDM_taste", "WISDM_tolerance", "WISDM_weight", "WISDM_affective", "WISDM_p", "WISDM_s", "WISDM_t", "PANAS_pos", "PANAS_neg", "FTND", "CESD", "BSL_co", "BSL_TNE", "BSL_totalcpd"))
smk <- subset(smoke,!is.na(Race) & !is.na(WISDM_affiliative) & !is.na(PANAS_neg) & !is.na(PANAS_pos) & !is.na(BSL_totalcpd))
cols <- c(2, 4, 5, 6)
smk[,cols] <- lapply(smk[,cols], factor)
levels(smk$Race) <- c("0", "1", "2")
levels(smk$Gender) <- c("2", "1")
smk$Treatment <- as.integer(smk$Treatment)

grad <- subset(smk, Treatment!=2)
grad$Treatment[grad$Treatment==3] <- 0 
immed <- subset(smk, Treatment!=1)
immed$Treatment[immed$Treatment==2] <- 1
immed$Treatment[immed$Treatment==3] <- 0

####removing outliers and missing:
immed <- subset(immed, immed$totalcpd_visit8<150)
eric <- subset(eric, eric$total_cpd_week6<5000)
#subsetting out only compliant subjects: 
doro <- subset(immed, Treatment==0 | Treatment==1 & tne_visit8<log(6.41))

#eric is 661 by 42
#compliant is 429 by 50

eric$Treatment <- as.factor(eric$Treatment)
doro$Treatment <- as.factor(doro$Treatment)
immed$Treatment <- as.factor(immed$Treatment)
levels(eric$Gender) <- levels(doro$Gender) <- levels(immed$Gender) <- c("Male", "Female")
levels(eric$Race) <- levels(doro$Race) <- levels(immed$Race) <- c("White", "Black", "Other")
levels(eric$Education) <- levels(doro$Education) <- levels(immed$Education) <- c("HS or less", "HS Grad", "College or more")
levels(eric$Treatment) <- levels(doro$Treatment) <- levels(immed$Treatment) <- c("Usual Nicotine", "Low Nicotine")
levels(eric$Menthol) <- levels(doro$Menthol) <- levels(immed$Menthol) <- c("No", "Yes")

eric <- subset(eric, select = -c(total_cpd_week6,
                                     study_cpd_week6, 
                                     non_study_cpd_week6, 
                                     tne_nmolperml_week6, 
                                     tot_nnal_pmolperml_week6, 
                                     FTND_score_week6))

doro <- subset(doro, select = -c(nnal_visit8,
                                     tne_visit8, 
                                     totalcpd_visit8, 
                                     studycpd_visit8, 
                                     nscpd_visit8, 
                                     ftndwcpd_visit8, 
                                     SMAST_total_visit92, 
                                     DAST_total_visit92, 
                                     nnal_visit20,
                                     tne_visit20, 
                                     totalcpd_visit20, 
                                     studycpd_visit20, 
                                     nscpd_visit20, 
                                     ftndwcpd_visit20 
                                     ))
immed <- subset(immed, select = -c(nnal_visit8,
                                     tne_visit8, 
                                     totalcpd_visit8, 
                                     studycpd_visit8, 
                                     nscpd_visit8, 
                                     ftndwcpd_visit8, 
                                     SMAST_total_visit92, 
                                     DAST_total_visit92, 
                                     nnal_visit20,
                                     tne_visit20, 
                                     totalcpd_visit20, 
                                     studycpd_visit20, 
                                     nscpd_visit20, 
                                     ftndwcpd_visit20 
                                     ))
eric$dat <- "Eric"
doro$dat <- "Dorothy Compliant"
immed$dat <- "Dorothy"
eric <- eric[,order(names(eric))]
doro <- doro[,order(names(doro))]
immed <- immed[,order(names(immed))]
all <- rbind(eric,doro, immed)
all <- all %>% 
  select(Treatment, Age, Gender, Education, Menthol, Race, WISDM_t, WISDM_p, WISDM_auto, WISDM_lossofcontrol, WISDM_tolerance, WISDM_craving, WISDM_s, WISDM_taste, WISDM_cog, WISDM_weight, WISDM_cue, WISDM_affective, WISDM_social, WISDM_affiliative, everything()) %>%
  select(-WISDM_t, -WISDM_p, -WISDM_auto, -WISDM_lossofcontrol, -WISDM_tolerance, -WISDM_craving, -WISDM_s, -WISDM_taste, -WISDM_cog, -WISDM_weight, -WISDM_cue, -WISDM_affective, -WISDM_social, -WISDM_affiliative, everything())
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

#Center for Epidemiological Studies Depression Scale
#Fagerström Test for Nicotine Dependence
tab <- CreateTableOne(data=all, strata = "dat", test=FALSE)
tab2 <- print(tab, nonnormal=TRUE, explain = FALSE, contDigits = 0, noSpaces = TRUE, varLabels = TRUE)
print(xtable(tab2))
