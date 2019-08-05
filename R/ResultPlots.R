knitr::opts_chunk$set(echo = FALSE)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(randomForestSRC)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(reshape2)
library(MethComp)
library(glmnet)
library(rattle)
library(tableone)
set.seed(1234)
#Eric portion
smoke <- read.csv(file = "C:/Users/Chuyu/Desktop/Research/ForestData.csv", header=TRUE)
setnames(smoke, 
         old = c("treatment_forChuyu", "Demo_9_SCR", "Demo_2_1_TEXT_SCR", "Demo_3_SCR", "race_3cat", "menthol", "CES_enjoyment_BSL2", "CES_cravingreduction_BSL2", "MNWS2_score_BSL2", "QSU_U_f1_BSL2", "QSU_U_f2_BSL2", "QSU_U_tot_BSL2", "CES_satisfaction_BSL2", "CES_reward_BSL2", "CES_aversion_BSL2", "WISDM_affat_BSL1", "WISDM_aut_BSL1", "WISDM_loc_BSL1", "WISDM_ce_BSL1", "WISDM_crav_BSL1", "WISDM_ceap_BSL1", "WISDM_seg_BSL1", "WISDM_taste_BSL1", "WISDM_tol_BSL1", "WISDM_wc_BSL1", "WISDM_ae_BSL1", "WISDM_pdm_BSL1", "WISDM_sdm_BSL1", "WISDM_ts_BSL1", "PANAS1_pos_BSL1", "PANAS1_neg_BSL1", "FTND_score_SCR", "CESD_score_BSL1", "co_BSL2", "tne_nmolperml_BSL2", "baseline_cpd"), 
         new = c("trt", "edu", "age", "gender", "race", "menthol", "CES_enjoy", "CES_cravingreduction", "MNWS2_score", "QSU_U_f1_BSL", "QSU_U_f2_BSL", "QSU_U_tot_BSL", "CES_satisfaction", "CES_reward", "CES_aversion", "WISDM_affiliative", "WISDM_auto", "WISDM_lossofcontrol", "WISDM_cog", "WISDM_craving", "WISDM_cue", "WISDM_social", "WISDM_taste", "WISDM_tolerance", "WISDM_weight", "WISDM_affective", "WISDM_p", "WISDM_s", "WISDM_t", "PANAS_pos", "PANAS_neg", "FTND", "CESD", "BSL_co", "BSL_TNE", "BSL_totalcpd"))
smoke <- subset(smoke,smoke$BSL_totalcpd<9000 & smoke$BSL_TNE<9000 & !is.na(smoke$PANAS_neg))
smoke <- subset(smoke, select = -c(treatment_label, treatment, CENIC_subject_id, tot_nnal_pmolperml_belowLOD_week6, cannabis, baseline_drinks_tlfb, CESD_score_week6))
smoke$tne_nmolperml_week6 <- log(smoke$tne_nmolperml_week6)
smoke$tot_nnal_pmolperml_week6 <- log(smoke$tot_nnal_pmolperml_week6)
cols <- c(3, 4, 5, 6)
smoke[,cols] <- lapply(smoke[,cols], factor)
levels(smoke$edu) <- c("1", "1", "2", "3", "3", "3")
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
         new = c("edu", "age", "gender", "race", "menthol", "CES_enjoy", "CES_cravingreduction", "MNWS2_score", "QSU_U_f1_BSL", "QSU_U_f2_BSL", "QSU_U_tot_BSL", "CES_satisfaction", "CES_reward", "CES_aversion", "WISDM_affiliative", "WISDM_auto", "WISDM_lossofcontrol", "WISDM_cog", "WISDM_craving", "WISDM_cue", "WISDM_social", "WISDM_taste", "WISDM_tolerance", "WISDM_weight", "WISDM_affective", "WISDM_p", "WISDM_s", "WISDM_t", "PANAS_pos", "PANAS_neg", "FTND", "CESD", "BSL_co", "BSL_TNE", "BSL_totalcpd"))
smk <- subset(smoke,!is.na(race) & !is.na(WISDM_affiliative) & !is.na(PANAS_neg) & !is.na(PANAS_pos) & !is.na(BSL_totalcpd))
cols <- c(2, 4, 5, 6)
smk[,cols] <- lapply(smk[,cols], factor)
levels(smk$race) <- c("0", "1", "2")
levels(smk$gender) <- c("2", "1")
smk <- dplyr::rename(smk, trt=Treatment)
smk$trt <- as.integer(smk$trt)

grad <- subset(smk, trt!=2)
grad$trt[grad$trt==3] <- 0 
immed <- subset(smk, trt!=1)
immed$trt[immed$trt==2] <- 1
immed$trt[immed$trt==3] <- 0

####removing outliers and missing:
immed <- subset(immed, immed$totalcpd_visit8<150)
eric <- subset(eric, eric$total_cpd_week6<5000)

#subsetting out only compliant subjects: 
doro <- subset(immed, trt==0 | trt==1 & tne_visit8<log(6.41))

#eric is 661 by 42
#immed is 642 by 50
#compliant is 429 by 50
#putting everything in the same order
eric <- eric[,order(names(eric))]
immed <- immed[,order(names(immed))]

trainE <- function(dat, outcome){
  outcome <- dat[[outcome]]
  smk <- subset(dat, select = -c(total_cpd_week6,
                                     study_cpd_week6, 
                                     non_study_cpd_week6, 
                                     tne_nmolperml_week6, 
                                     tot_nnal_pmolperml_week6, 
                                     FTND_score_week6))
  smk <- cbind(smk,outcome)
  smk0 <- subset(smk, smk$trt==0)
  smk1 <- subset(smk, smk$trt==1)
  m0 <- cv.glmnet(x = data.matrix(subset(smk0, select = -outcome)), 
                  y = smk0$outcome, 
                  family = "gaussian", alpha = 1, standardize = TRUE)
  m1 <- cv.glmnet(x = data.matrix(subset(smk1, select = -outcome)), 
                  y = smk1$outcome, 
                  family = "gaussian", alpha = 1, standardize = TRUE)
  return(list(m0=m0, m1=m1, smk=smk))
}


trainD <- function(dat, outcome){
  outcome <- dat[[outcome]]
  smk <- subset(dat, select = -c(nnal_visit8,
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
  smk <- cbind(smk,outcome)
  smk0 <- subset(smk, smk$trt==0)
  smk1 <- subset(smk, smk$trt==1)
  m0 <- cv.glmnet(x = data.matrix(subset(smk0, select = -outcome)), 
                  y = smk0$outcome, 
                  family = "gaussian", alpha = 1, standardize = TRUE)
  m1 <- cv.glmnet(x = data.matrix(subset(smk1, select = -outcome)), 
                  y = smk1$outcome, 
                  family = "gaussian", alpha = 1, standardize = TRUE)
  return(list(m0=m0, m1=m1, smk=smk))
}

test_diff <- function(smk, m0, m1){
    pred0 <- pred1 <- rep(0, nrow(smk))
    pred0 <- predict(m0, newx = data.matrix(subset(smk, select = -outcome)), s = "lambda.min")
    pred1 <- predict(m1, newx = data.matrix(subset(smk, select = -outcome)), s = "lambda.min")
    Z <- pred0-pred1
    return(list(pred0=pred0, pred1=pred1, Z=Z))
}

esmk <- trainE(eric, "total_cpd_week6")
dsmk <- trainD(immed, "totalcpd_visit8")
#########need LASSO plots############ -----------------------
plot(esmk$m0)
coef(esmk$m0, s = "lambda.min")

#Eric train, eric data
EE <- test_diff(esmk$smk, esmk$m0, esmk$m1) #####changed all to model based
Z_EE <- EE$Z %>% as.vector()
pred0_EE <- EE$pred0
pred1_EE <- EE$pred1

#Dorothy train, Dorothy data
DD <- test_diff(dsmk$smk, dsmk$m0, dsmk$m1)
Z_DD <- DD$Z %>% as.vector()
pred0_DD <- DD$pred0
pred1_DD <- DD$pred1

#Eric train, dorothy data
ED <- test_diff(dsmk$smk, esmk$m0, esmk$m1)
Z_ED <- ED$Z %>% as.vector()
pred0_ED <- ED$pred0
pred1_ED <- ED$pred1

#Dorothy train, Eric data
DE <- test_diff(esmk$smk, dsmk$m0, dsmk$m1)
Z_DE <- DE$Z %>% as.vector()
pred0_DE <- DE$pred0
pred1_DE <- DE$pred1

#to look at the lasso coefs for each model:
#coef(esmk$m0, s = "lambda.min")
###########PLOT 1##########
plot.new()
png("hist1.png",width = 8, height=3.5, units='in',res=300)
par(mfrow=c(1, 2))
hist(Z_EE, main="Total CPD Trained using Eric's data \n Eric's data plugged in", sub=paste("SD = ",round(sd(Z_EE), 2)), xlab = paste("Mean = ", round(mean(Z_EE), 2) ))
hist(Z_ED, main="Total CPD Trained using Eric's data \n Dorothy's data plugged in", sub=paste("SD = ",round(sd(Z_ED), 2)), xlab = paste("Mean = ", round(mean(Z_ED), 2) ), breaks = 14)
dev.off()

# trt_eff <- data.frame(val=c(Z_EE, Z_ED))
# trt_eff$group <- c(rep("EE", length(Z_EE)), rep("ED", length(Z_ED)))
# ggplot(trt_eff, aes(val, fill = group)) + geom_histogram(alpha = 0.5, position = 'identity', bins = 20) + labs(title = "Total CPD Treatment Effect Estimates")

Esmk <- subset(esmk$smk, select=-c(outcome))
rtree <- function(znum, train){
  train$Z <- znum
#training tree with train
  fit <- rpart(Z~., dat=train, method = "anova",  control = rpart.control(maxdepth = 3))
#fixing names
  split.fun <- function(x, labs, digits, varlen, faclen){
  labs <- gsub("^BSL_co", "Baseline Expired Carbon Monoxide (ppm)", labs)
  labs <- gsub("^WISDM_to", "WISDM Tolerance", labs)
  labs <- gsub("^WISDM_so", "WISDM Social Goads", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width=20), collapse="\n")
  }
  labs
  }
#plot the tree
  prp(fit, type = 3, extra = 101, fallen.leaves = TRUE, split.fun = split.fun)
  return(list(onetree=fit))
}
###########PLOT 2##########
plot.new()
png("tree1.png",width = 7, height=4, units='in',res=300)
tree <- rtree(Z_EE, Esmk)
dev.off()
depth <- tree$onetree$control$maxdepth
t <- capture.output(asRules(tree$onetree))
rule <- matrix(rep("", (depth+1)*(2^depth)), ncol=depth+1)
index <- grep("Rule", t)
for(i in 1:length(index)){
  rule[i,] <- t[(index[i]):(index[i]+depth)]
}
#getting original order
bits <- unlist(strsplit(rule[, 1], "="))
i <- grep("cover", bits)
ms <- bits[i]
order1 <- sub("cover", "", ms)
#get rid of all rules bits
ind <- grep("Rule", rule)
rule[ind] <- ""
rule <- as.data.table(rule)
rule <- subset(rule, V2!="")
rule$V1 <- order1

#getting order of terminal nodes from tree
want <- grep("[*]$", capture.output(tree$onetree), value = TRUE)
bits <- unlist(strsplit(want, " "))
i <- grep("[*]", bits)
order2 <- bits[i-1] %>% as.data.frame()
setnames(order2, ".", "V1")
#merging
rule$V1 <- round(as.numeric(rule$V1),3)
order2$V1 <- round(as.numeric(as.character(order2$V1)), 3)
merged <- merge(x = order2, y = rule, by = "V1", sort = FALSE, all = T)
rules <- subset(merged, V2!="")[1:depth+1]
nn <- nrow(rules)

make_tree <- function(dat, Z, nn){
  tab <- sapply(1:nn , function(n){
    vals <- subset(dat, eval(parse(text=gsub("&+$",'',unite(rules, sep="&")[n,]))), select = c(Z,"trt"))
    c <- vals[[Z]][vals$trt==0]
    t <- vals[[Z]][vals$trt==1]
    me <- mean(vals[[Z]])
    per <- 100*dim(vals)[1]/nrow(dat)
    mar <- qt(.975, df=dim(vals)[1]-2)*sqrt( (var(c)/length(c)) + (var(t)/length(t)) )
    rbind(me, per, mar)})
  newtab <- t(tab)%>% as.data.frame()
  setnames(newtab, c("V1", "V2", "V3"), c("means", "percents", "margin"))
  }

make_tree2 <- function(dat, nn){
  tab <- sapply(1:nn , function(n){
    vals <- subset(dat, eval(parse(text=gsub("&+$",'',unite(rules, sep="&")[n,]))), select = c("outcome","trt"))
    c <- vals$outcome[vals$trt==0]
    t <- vals$outcome[vals$trt==1]
    me <- mean(c)-mean(t)
    per <- 100*dim(vals)[1]/nrow(dat)
    mar <- qt(.975, df=dim(vals)[1]-2)*sqrt( (var(c)/length(c)) + (var(t)/length(t)) )
    rbind(me, per, mar)})
  newtab <- t(tab)%>% as.data.frame()
  setnames(newtab, c("V1", "V2", "V3"), c("means", "percents", "margin"))
  }

Etab <- make_tree(cbind(Z_EE, Esmk), "Z_EE", nn) %>% as.data.frame()
Dtab <- make_tree2(dsmk$smk, nn) %>% as.data.frame()

#pvalue for trt by subgroup interaction for dorothy's
# dat <- dsmk$smk
# f <- function(n){
#     dat <- subset(dat, eval(parse(text=gsub("&+$",'',unite(rules, sep="&")[n,]))))
#     dat$subgroup <- n
#     dat}
# listsg <- lapply(1:nn, f)
# new <- do.call(rbind, listsg)
# m0 <- lm(outcome ~ trt+factor(subgroup), data = new)
# m1 <- lm(data = new, formula = outcome~trt+factor(subgroup)+trt:factor(subgroup))
# pval <- anova(m0,m1)$`Pr(>F)`[2]
#interaction btw trt and subgroup does not reduce the SSE significantly 
x <- y <- rep(NA, nn)
x[1:nn] <- Etab$percents
y[1:nn] <- Dtab$percents
barplot(x, ylab="Percents", main = "Eric's data" , xlab = "Node Averages", names.arg = round(Etab[,1], 2))
barplot(y, ylab="Percents", main = "Dorothy's Data", xlab = "Node Averages", names.arg = round(Dtab[,1], 2))
###########PLOT 3##########
plot.new()
png("validate.png",width = 7, height=5, units='in',res=300)
offs <- 0.1
plot((1:nn)-offs, Dtab$means,
     ylim = range(Etab$means-Etab$margin, Etab$means+Etab$margin, Dtab$means-Dtab$margin, Dtab$means+Dtab$margin),
    pch=19, xlab="Node", ylab="Mean +/- 95% CI",
    main="Eric's tree using Eric (red) vs Dorothy (black) data", xlim = c(0.5, 7.5))
arrows((1:nn)-offs, Dtab$means-Dtab$margin, (1:nn)-offs, Dtab$means+Dtab$margin, length=0.05, angle=90, code=3)
points((1:nn)+offs, Etab$means, pch=19, col="red")
#text(x=2.5, y=13, labels = paste("treatment/subgroup interaction pval=",round(pval, 2)))
dev.off()
#putting everything in the same order
eric <- eric[,order(names(eric))]
doro <- doro[,order(names(doro))]

#using the functions:
esmk <- trainE(eric, "total_cpd_week6")
dsmk <- trainD(doro, "totalcpd_visit8")

#Eric train, eric data
EE <- test_diff(esmk$smk, esmk$m0, esmk$m1)
Z_EE <- EE$Z
pred0_EE <- EE$pred0
pred1_EE <- EE$pred1

#Dorothy train, Dorothy data
DD <- test_diff(dsmk$smk, dsmk$m0, dsmk$m1)
Z_DD <- DD$Z
pred0_DD <- DD$pred0
pred1_DD <- DD$pred1

#Eric train, dorothy data
ED <- test_diff(dsmk$smk, esmk$m0, esmk$m1)
Z_ED <- ED$Z
pred0_ED <- ED$pred0
pred1_ED <- ED$pred1

#Dorothy train, Eric data
DE <- test_diff(esmk$smk, dsmk$m0, dsmk$m1)
Z_DE <- DE$Z
pred0_DE <- DE$pred0
pred1_DE <- DE$pred1
###########PLOT 4##########
plot.new()
png("hist2.png",width = 4.5, height=3.5, units='in',res=300)
hist(Z_DD, main="Total CPD Trained with Dorothy's data \n Dorothy's data plugged in", sub=paste("SD = ",round(sd(Z_DD), 2)), xlab = round(mean(Z_DD), 2) )
dev.off()
Dsmk <- subset(dsmk$smk, select=-c(outcome))
rtree <- function(znum, train){
  train$Z <- znum
#fixing names
split.fun <- function(x, labs, digits, varlen, faclen){
  labs <- gsub("^age", "Age", labs)
  labs <- gsub("^MNWS2_sc", "MNWS2 Score", labs)
  labs <- gsub("^BSL_TNE", "Baseline Total Nicotine Equivalents", labs)
  labs <- gsub("^QSU_U_f1", "Baseline QSU f1", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width=20), collapse="\n")
  }
  labs
}
  #training tree with train
  fit <- rpart(Z~., dat=train, method = "anova",  control = rpart.control(maxdepth = 3))
  #plot the tree
  prp(fit, type = 3, extra = 101, fallen.leaves = TRUE, split.fun = split.fun)
  return(list(onetree=fit))
}
###########PLOT 5##########
plot.new()
png("tree2.png",width = 7, height=4, units='in',res=300)
tree <- rtree(Z_DD, Dsmk)
dev.off()
