################################################################################
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(rpart)
library(rpart.plot)
library(reshape2)
library(MethComp)
library(glmnet)
library(rattle)

set.seed(1234)

#Dorothy portion
smoke <- read.csv(file = "../data/CENIC1P2_wide_2017_11_22.csv", 
  header=TRUE)
# smoke <- read.csv(file = "../data/causal_data_20161021.csv", 
#   header=TRUE)
smoke <- smoke[, c(4, 9, 135, 136, 138, 139, 147, 148, 149, 150, 151, 
  152, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 
  167, 168, 169, 170, 171, 172, 173, 175, 176, 177, 178, 271, 274, 283, 
  285, 288, 567, 569, 572, 576, 579, 583, 586, 617, 620)]

smoke$nnal_visit8 <- log(smoke$nnal_visit8)
smoke$tne_visit8  <- log(smoke$tne_visit8)

old_names <- c("demo_9_3c", "AGE_CLEAN", "GENDER_CLEAN", "race_3c", 
  "Menthol_Status_num", "ces1_3_visit0", "ces1_11_visit0", 
  "mnws_total_visit0", "qsu_u_factor1_visit0", "qsu_u_factor2_visit0", 
  "qsu_u_total_visit0", "ces1_satisfaction_visit0", "ces1_psych_reward_visit0", 
  "ces1_aversion_visit0", "WISDM_Affiliative_visit91", 
  "WISDM_Automaticity_visit91", "WISDM_Loss_of_Control_visit91", 
  "WISDM_Cognitive_visit91", "WISDM_Craving_visit91", "WISDM_Cue_visit91", 
  "WISDM_Social_visit91", "WISDM_Taste_visit91", "WISDM_Tolerance_visit91", 
  "WISDM_Weight_visit91", "WISDM_Affective_visit91", "WISDM_p_visit91", 
  "WISDM_s_visit91", "WISDM_t_visit91", "PANAS_positive_visit91", 
  "PANAS_negative_visit91", "ftndwcpd_visit92", "CESD_total_visit92", 
  "co_visit0", "tne_visit0", "totalcpd_visit0")
new_names <- c("edu", "age", "gender", "race", "menthol", "CES_enjoy", 
  "CES_cravingreduction", "MNWS2_score", "QSU_U_f1_BSL", "QSU_U_f2_BSL", 
  "QSU_U_tot_BSL", "CES_satisfaction", "CES_reward", "CES_aversion", 
  "WISDM_affiliative", "WISDM_auto", "WISDM_lossofcontrol", "WISDM_cog", 
  "WISDM_craving", "WISDM_cue", "WISDM_social", "WISDM_taste", 
  "WISDM_tolerance", "WISDM_weight", "WISDM_affective", "WISDM_p", "WISDM_s", 
  "WISDM_t", "PANAS_pos", "PANAS_neg", "FTND", "CESD", "BSL_co", "BSL_TNE", 
  "BSL_totalcpd")
# cbind(old_names, new_names)
setnames(smoke, old = old_names, new = new_names)
rm(old_names, new_names)

smk <- subset(smoke,!is.na(race) & !is.na(WISDM_affiliative) & 
    !is.na(PANAS_neg) & !is.na(PANAS_pos) & !is.na(BSL_totalcpd))
#gender: 0 is female, 1 is male
#menthol: 0 is non menthol, 1 is menthol
smk[,c("race", "edu")] <- lapply(smk[,c("race", "edu")], factor)
levels(smk$race) <- c("1", "2", "3")
#race: 1 is white, 2 is black, 3 is other
smk <- cbind(
  subset(smk, select = -c(race, edu)), 
  model.matrix( ~race+edu, smk)[,-1])
smk <- dplyr::rename(smk, trt=Treatment)
smk$trt <- as.integer(smk$trt)

grad <- subset(smk, trt!=2)
grad$trt[grad$trt==3] <- 0 
immed <- subset(smk, trt!=1)
immed$trt[immed$trt==2] <- 1
immed$trt[immed$trt==3] <- 0

#removing outliers and missing:
immed <- subset(immed, immed$totalcpd_visit8 < 150)

#putting columns in order
immed <- immed[,order(names(immed))]


#subsetting out only compliant subjects: 
doro <- subset(immed, trt == 0 | trt == 1 & tne_visit8 < log(6.41))
#immed is 642 by 5
#doro is 429 by 50 (compliant from immed)

immed <- as_tibble(immed)
doro  <- as_tibble(doro)

# unused variables in lasso
cols_to_drop <- c(
  "nnal_visit8",
  "tne_visit8", 
  "totalcpd_visit8",  
  "studycpd_visit8", 
  "nscpd_visit8", 
  "ftndwcpd_visit8", 
  "SMAST_total_visit92", 
  "DAST_total_visit92", 
  "nnal_visit20",
  "tne_visit20", 
  "totalcpd_visit20", 
  "studycpd_visit20", 
  "nscpd_visit20", 
  "ftndwcpd_visit20")

# cols_to_drop <- match(cols_to_drop, names(doro), nomatch = 0)

if (!all(names(immed) == names(doro)))
  stop("names in 'doro' and 'immed' don't match! code won't work!")

#functions for LASSO training and estimating 
trainD <- function(dat, outcome){
  Y <- dat[[outcome]]
  
  tdata <- dat[, -match(cols_to_drop, names(dat), nomatch = NULL)]
  #tdata <- cbind(tdata, Y)
  
  # tdata0 <- subset(tdata, tdata$trt==0)
  # tdata1 <- subset(tdata, tdata$trt==1)
  trt_0 <- tdata$trt == 0
  trt_1 <- tdata$trt == 1
  
  m0 <- cv.glmnet(x = data.matrix(tdata[trt_0, ]), 
    y           = Y[trt_0], 
    family      = "gaussian", 
    alpha       = 1, 
    standardize = TRUE)
  m1 <- cv.glmnet(x = data.matrix(tdata[trt_1, ]), 
    y           = Y[trt_1], 
    family      = "gaussian", 
    alpha       = 1, 
    standardize = TRUE)
  # return(list(m0=m0, m1=m1, smk=tdata))
  
  #tdata[, outcome] <- Y
  
  out <- list()
  
  out$m0      <- m0
  out$m1      <- m1
  out$x       <- tdata
  out$y       <- Y
  out$outcome <- outcome
  
  out
}

test_diff <- function(x, m0, m1, ...) {
    # pred0 <- pred1 <- rep(0, nrow(smk))
    # pred0 <- predict(m0, newx = data.matrix(subset(smk, select = -outcome)), s = "lambda.min")
    # pred1 <- predict(m1, newx = data.matrix(subset(smk, select = -outcome)), s = "lambda.min")
    # Z <- pred0-pred1
    # return(list(pred0=pred0, pred1=pred1, Z=Z))

    pred0 <- predict(m0, newx = data.matrix(x), s = "lambda.min")
    pred1 <- predict(m1, newx = data.matrix(x), s = "lambda.min")
    Z     <- pred0 - pred1
    
    out <- list()
    
    out$pred0 <- pred0
    out$pred1 <- pred1
    out$Z     <- Z

    out
}

#training and getting estimates for both dorothy and compliant
# debugonce(trainD)
dsmk <- trainD(immed, "totalcpd_visit8")
csmk <- trainD(doro, "totalcpd_visit8")

#Dorothy 
# DD <- test_diff(dsmk$smk, dsmk$m0, dsmk$m1)
DD <- do.call(test_diff, dsmk)
Z_DD <- DD$Z %>% as.vector()

#Dorothy Compliant 
# CC <- test_diff(csmk$smk, csmk$m0, csmk$m1)
CC <- do.call(test_diff, csmk)
Z_CC <- CC$Z %>% as.vector()

#Taking out WISDOM and CES (and all the diff outcomes):
more_cols_to_drop <- c(
  "WISDM_affective", 
  "WISDM_affiliative", 
  "WISDM_auto", 
  "WISDM_cog", 
  "WISDM_cue", 
  "WISDM_craving", 
  "WISDM_lossofcontrol", 
  "WISDM_social", 
  "WISDM_taste", 
  "WISDM_tolerance", 
  "WISDM_weight", 
  "CES_satisfaction", 
  "CES_reward", 
  "CES_aversion", 
  "CES_enjoy", 
  "CES_cravingreduction")
more_cols_to_drop <- match(more_cols_to_drop, names(dsmk$x), nomatch = NULL)

if (!all(names(dsmk$x) == names(csmk$x)))
  stop("names in 'dsmk$x' and 'csmk$x' don't match! code won't work!")


Dsmk <- dsmk$x[, -more_cols_to_drop]
Csmk <- csmk$x[, -more_cols_to_drop]

#Function for making the tree:
rtree <- function(znum, train){
  train$Z <- znum
  fit <- rpart(Z ~ ., dat = train, method = "anova", 
    control = rpart.control(maxdepth = 4), model = TRUE)
  fit
}

# function for fixing names in plotted tree
split.fun <- function(x, labs, digits, varlen, faclen) {
  labs <- gsub("^BSL_co", "Baseline Expired Carbon Monoxide (ppm)", labs)
  labs <- gsub("^WISDM_to", "WISDM Tolerance", labs)
  labs <- gsub("^WISDM_so", "WISDM Social Goads", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width=20), collapse="\n")
  }
  labs
}



DD_tree <- rtree(Z_DD, Dsmk)
CC_tree <- rtree(Z_CC, Csmk)

#plot the trees ----
prp(DD_tree, type = 3, extra = 101, fallen.leaves = TRUE, split.fun = split.fun)
prp(CC_tree, type= 3, extra = 101, fallen.leaves = TRUE, split.fun = split.fun)



# # verification that we can compute means on a 2nd outcome
# # using the fitted regression tree. We'll test the code by
# # computing the means on the original outcome variable
# testdata <- na.omit(doro)
# #Y <- testdata$studycpd_visit8
# #X <- select(testdata, -studycpd_visit8)
# fit <- rpart(age ~ ., 
#   dat = testdata, 
#   method = "anova", 
#   # control = rpart.control(maxdepth = 4), 
#   model = TRUE)
# testdata$fitted <- predict(fit)
# testdata <- testdata %>%
#   group_by(fitted) %>%
#   mutate(testfitted = mean(fitted)) %>%
#   ungroup()
# 
# testdata %>%
#   select(contains("fitted"))
# 
# # TRUE indicates that it works
# all(testdata$fitted == testdata$testfitted)

# --- end testing --- #


# Drepeats <- function(s){
#   set.seed(s)
#   Pimmed <- immed
#   Pimmed$trt <- Pimmed$trt[sample(nrow(Pimmed))]
#   pdsmk <- trainD(Pimmed, "totalcpd_visit8")
#   DDP <- test_diff(pdsmk$smk, pdsmk$m0, pdsmk$m1) 
#   Z_DDP <- DDP$Z %>% as.vector()
#   sd(Z_DDP)
# }
# 
# Crepeats <- function(s){
#   set.seed(s)
#   Pdoro <- doro
#   Pdoro$trt <- Pdoro$trt[sample(nrow(Pdoro))]
#   pcsmk <- trainD(Pdoro, "totalcpd_visit8")
#   CCP <- test_diff(pcsmk$smk, pcsmk$m0, pcsmk$m1) 
#   Z_CCP <- CCP$Z %>% as.vector()
#   sd(Z_CCP)
# }

permute <- function(s, data) {
  set.seed(s)
  data$trt <- data$trt[sample(nrow(data))]
  psmk <- trainD(data, "totalcpd_visit8")
  DDP <- do.call(test_diff, psmk)
  Z_DDP <- DDP$Z %>% as.vector()
  sd(Z_DDP)
}


# --- Jeff added  --- #
# n_perm <- 1000
n_perm <- 100
# ------------------- #

Dsd1000 <- sapply(seq_len(n_perm), permute, data = immed) %>% unlist()
Csd1000 <- sapply(seq_len(n_perm), permute, data = doro)  %>% unlist()


hist(Dsd1000, 
  main   = "SD of Dorothy Treatment Effects", 
  sub    = sprintf("SD = %.2f", sd(Dsd1000)), 
  xlab   = sprintf("Mean = %.2f", mean(Dsd1000)), 
  breaks = 20)
abline(v = sd(Z_DD), col="blue")
legend("topright", 
  col    = c("blue"), 
  legend = c("Original data SD"), 
  lty    = 1, 
  title  = sprintf("P-value = %.3f", mean(Dsd1000 > sd(Z_DD))))

hist(Csd1000, 
  main   = "SD of Compliant Dorothy Treatment Effects", 
  sub    = sprintf("SD = %.2f", sd(Csd1000)), 
  xlab   = sprintf("Mean = %.2f", mean(Csd1000)), 
  breaks = 30)
abline(v = sd(Z_CC), col="blue")
legend("topright", 
  col    = c("blue"), 
  legend = c("Original data SD"), 
  lty    = 1, 
  title  = sprintf("P-value = %.3f", mean(Csd1000> sd(Z_CC))))

CrossVal <- function (i, dorodata, DEPTH) {
   # folds <- 4
   #  DEPTH <- 5
   #  dorodata <- immed
   #  i <- split(seq_len(nrow(dorodata)), sample(rep(seq_len(folds), length.out = nrow(dorodata))))[[1]] 
  
  #CV datasets
  trainingData <- slice(dorodata, -i)
  testingData <- slice(dorodata, i)
  
  #-------------------------------------------------------------------------training data part    
  ########################LASSO######################
  
  #putting columns in the same order
  trainingData <- trainingData[,order(names(trainingData))]
  
  #functions for LASSO training and estimating 
  
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
  
  #actually training and getting estimates
  dsmk <- trainD(trainingData, "totalcpd_visit8")
  
  #Doro train, doro data
  DD <- test_diff(dsmk$smk, dsmk$m0, dsmk$m1) 
  Z_DD <- DD$Z %>% as.vector()
  
  
  
  #000000######00000000#####making the zero depth:

    #putting columns in the same order
    testingData0 <- testingData[,order(names(testingData))]
    #actually training and getting estimates
    dsmkT0 <- trainD(testingData, "totalcpd_visit8")
    #Doro train, doro data
    DT0 <- test_diff(dsmkT0$smk, dsmkT0$m0, dsmkT0$m1)
    Z_DT0 <- DT0$Z %>% as.vector()
    depth0 <- mean((Z_DT0-mean(Z_DD))^2)
    
  #000000######00000000#####
    
    
    ########################REGRESSION TREE#########################
    
    #Taking out WISDOM and CES (and all the diff outcomes):
    Doro <- subset(trainingData, select = -c(
      WISDM_affective, WISDM_affiliative, WISDM_auto, WISDM_cog, WISDM_cue, WISDM_craving, WISDM_lossofcontrol, WISDM_social, WISDM_taste, WISDM_tolerance, WISDM_weight, 
      CES_satisfaction, CES_reward, CES_aversion, CES_enjoy, CES_cravingreduction, 
      nnal_visit8,tne_visit8, totalcpd_visit8, studycpd_visit8, nscpd_visit8, ftndwcpd_visit8, SMAST_total_visit92, DAST_total_visit92, nnal_visit20,tne_visit20, totalcpd_visit20, studycpd_visit20, nscpd_visit20, ftndwcpd_visit20 ))
    
    #Function for making the tree:
    rtree <- function(znum, train, depth){
      train$Z <- znum
      #training tree with train
      fit <- rpart(Z~., dat=train, method = "anova",  control = rpart.control(maxdepth = depth))
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
      #prp(fit, type = 3, extra = 101, fallen.leaves = TRUE, split.fun = split.fun)
      return(list(onetree=fit))
    }
    
    trees <- lapply(1:DEPTH, rtree, znum=Z_DD, train=Doro)
    
    ########################EXTRACT TREE RULES###################
    getrules <- function(x){
      tree <- trees[[x]]
        #Making tables of means/perc/margins from trees:
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
        #get rid of wrap around rules
        rule[is.na(rule)] <- ""
        #need to get rid of stuff after blanks
        ind2 <- which(rule=="", arr.ind = TRUE) %>% as.data.frame()
        ind3 <- ind2[ind2[,2]!=ncol(rule),] 
        if(dim(ind3)[1] != 0){
          cind <- ind3[,2]+1
          rind <- ind3[,1]
          for(i in 1:nrow(ind3)){rule[rind[i], cind[i]] <- ""}
        }
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
        rules <- subset(merged, V2!="")[1:depth+1] #isolated the splits into a table
        nn <- nrow(rules) #number of nodes
        return(list(rules, nn))
    }
    
    dep <- lapply(1:DEPTH, getrules)

    
    ########################NODE MEANS###################
    
    node_means <- function(x, dep, dat, Z){
      rules <- dep[[x]][[1]]
      nn <- dep[[x]][[2]]
      # tab <- sapply(1:nn , function(n){
      #   vals <- subset(dat, eval(parse(text=
      #                                    gsub("^(.*)(race==\\d)(,)(\\d)(.*)$", "\\1\\2|race==\\4\\5",
      #                                         gsub("^(.*)(edu==\\d)(,)(\\d)(.*)$", "\\1\\2|edu==\\4\\5", 
      #                                              gsub("edu=", "edu==", 
      #                                                   gsub("race=", "race==", 
      #                                                        gsub("gender=", "gender==",
      #                                                             gsub("menthol=", "menthol==",
      #                                                                  gsub("&+", "&",
      #                                                                       gsub("&+$",'',unite(rules, sep="&")[n,]))))))))
      #   )), select = c(Z,"trt"))
      #   me <- mean(vals[[Z]])
      # })
      srep1 <- function(n) {
        vals <- subset(dat, 
          eval(
            parse(
              text = gsub("^(.*)(race==\\d)(,)(\\d)(.*)$", "\\1\\2|race==\\4\\5",
                gsub("^(.*)(edu==\\d)(,)(\\d)(.*)$", "\\1\\2|edu==\\4\\5", 
                gsub("edu=", "edu==", 
                gsub("race=", "race==", 
                gsub("gender=", "gender==",
                gsub("menthol=", "menthol==",
                gsub("&+", "&",
                gsub("&+$",'', unite(rules, "temp_str", sep="&")[n,]))))))))
            )
          ), 
          select = c(Z,"trt")
        )
        me <- mean(vals[[Z]])
      }
      # debug(srep)
      tab <- sapply(1:nn, srep1)
    }
    #means of each node from testing data
    #debug(node_means)
    Nmeans <- lapply(1:DEPTH, node_means, dep=dep, dat=cbind(Z_DD, Doro), Z="Z_DD")
    
    
    #-------------------------------------------------------------------------testing data part
    
    ########################LASSO######################
    
    #putting columns in the same order
    testingData <- testingData[,order(names(testingData))]
    #actually training and getting estimates
    dsmkT <- trainD(testingData, "totalcpd_visit8")
    #Doro train, doro data
    DT <- test_diff(dsmkT$smk, dsmkT$m0, dsmkT$m1)
    Z_DT <- DT$Z %>% as.vector()
    
    ########################OVERALL MEAN OF MSEs######################
    
    #putting the new LASSO estimates into the tree we made earlier
    node_mses <- function(x, dep, dat, Z){
      rules <- dep[[x]][[1]]
      nn <- dep[[x]][[2]]
      # tab <- sapply(1:nn , function(n){
      #   vals <- subset(dat, eval(parse(text=
      #                                    gsub("^(.*)(race==\\d)(,)(\\d)(.*)$", "\\1\\2|race==\\4\\5",
      #                                         gsub("^(.*)(edu==\\d)(,)(\\d)(.*)$", "\\1\\2|edu==\\4\\5", 
      #                                              gsub("edu=", "edu==", 
      #                                                   gsub("race=", "race==",
      #                                                        gsub("gender=", "gender==",
      #                                                             gsub("menthol=", "menthol==",
      #                                                                  gsub("&+", "&",
      #                                                                       gsub("&+$",'',unite(rules, sep="&")[n,]))))))))
      #   )), select = c(Z,"trt"))
      #   (vals[[Z]]-Nmeans[[x]][n])^2
      # })
      srep2 <- function(n) {
        vals <- subset(dat, 
          eval(
            parse(text = gsub("^(.*)(race==\\d)(,)(\\d)(.*)$", "\\1\\2|race==\\4\\5",
              gsub("^(.*)(edu==\\d)(,)(\\d)(.*)$", "\\1\\2|edu==\\4\\5", 
              gsub("edu=", "edu==", 
              gsub("race=", "race==",
              gsub("gender=", "gender==",
              gsub("menthol=", "menthol==",
              gsub("&+", "&",
              gsub("&+$",'',unite(rules, "temp_str", sep="&")[n,]))))))))
            )
          ), select = c(Z,"trt")
        )
        (vals[[Z]]-Nmeans[[x]][n])^2        
      }
      tab <- sapply(1:nn, srep2)
      return(tab)
    }
    
    getmeans <- function(x){
      SumMses <- node_mses(x, dep=dep, dat=cbind(Z_DT, testingData), Z="Z_DT")
      SSMses <- unlist(SumMses) %>% sum()
      OverallMean <- SSMses/nrow(testingData)
      return(OverallMean)
    }
    
    Omeans <- lapply(1:DEPTH, getmeans) %>% unlist()
    return(c(depth0, Omeans))
}


repeats <- function(s, DEPTH, dordat){
  set.seed(s)
  folds <- 4
  splits <- split(seq_len(nrow(dordat)), sample(rep(seq_len(folds), length.out = nrow(dordat))))
  results <- lapply(splits, CrossVal, dordat, DEPTH)
  return(results)
}





# debugonce(CrossVal)
# debugonce(repeats)

n_sim <- 1e1
D100 <- lapply(seq_len(n_sim), repeats, DEPTH=5, dordat=immed) %>% unlist(, recursive=FALSE) %>% as.data.frame()

# medD100 <- sapply(1:6, function(x) {return(median(as.numeric(D100[x,])))})
medD100 <- apply(D100, 1, median)

plot(c(0, 1, 2, 3, 4, 5), sqrt(rowMeans(D100)), 
  xlab = "Depths", 
  ylab = "Mean RMSE", 
  pch  = 16, 
  main = sprintf("Dorothy %i Simulations RMSE", n_sim))
plot(c(0, 1, 2, 3, 4, 5), sqrt(medD100), 
  xlab = "Depths", 
  ylab = "Median RMSE", 
  pch  = 16, 
  main = sprintf("Dorothy %i Simulations RMSE", n_sim))


C100 <- lapply(seq_len(n_sim), repeats, DEPTH=5, dordat=doro) %>% unlist(, recursive=FALSE) %>% as.data.frame()
# medC100 <- sapply(1:6, function(x) {return(median(as.numeric(C100[x,])))})
medC100 <- apply(C100, 1, median)

plot(c(0, 1, 2, 3, 4, 5), sqrt(rowMeans(C100)), 
  xlab = "Depths", 
  ylab = "Mean RMSE", 
  pch  = 16, 
  main = sprintf("Compliant %i Simulations RMSE", n_sim)) 
plot(c(0, 1, 2, 3, 4, 5), sqrt(medC100), 
  xlab = "Depths", 
  ylab = "Median RMSE", 
  pch  = 16, 
  main = sprintf("Compliant %i Simulations RMSE", n_sim))
