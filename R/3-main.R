# R command for batch: 
# "C:\Program Files\R\R-3.6.1\bin\R.exe" CMD BATCH --vanilla R\3-main.R


library(tidyverse)
library(xtable)
source("R/functions.R")

# --- global variables --- #
estimation <- "lasso"
# estimation <- "random_forest"
# cohort <- "ITT" # use all participants, or compliant ones?
# cohort <- "compliant"
# ------------------------ #
begin <- Sys.time()
for (cohort in c("compliant", "ITT")) {
  # set.seed(123)
  # set.seed(321)
  load("../RData/analysis.RData")
  analysis <- analysis %>% rename(trt = teh_treatment)
  # analysis <- analysis %>% 
  #   mutate(tne_nmolperml_bsl = log(tne_nmolperml_bsl)) %>%
  #   rename("ltne" = "tne_nmolperml_bsl")
  # analysis <- analysis %>%  
  #   mutate(ltne_bsl = log(tne_bsl)) %>%
  #   select(-tne_bsl)

  # log-transform biomarkers
  biomarkers <- c("tne_bsl", "nnal_visit0", "phet_visit0", "cema_visit0", "pgem_visit0", 
    "iso_visit0" , "nnal_visit20" , "phet_visit20", "cema_visit20", 
    "pgem_visit20", "iso_visit20", "tne_20") 


  if (any(analysis[, biomarkers] < 0, na.rm = TRUE))
    stop("can't log transform biomarkers")

  analysis[, biomarkers] <- log(analysis[, biomarkers])


  # prefix biomarkers with 'l' to indicate log
  names(analysis)[match(biomarkers, names(analysis))] <- paste0("l", biomarkers)


  if (cohort == "compliant") {
    # analysis <- analysis %>%
    #   filter(trt == 0 | trt == 1 & ltne_20 < log(6.41))
    analysis <- analysis %>%
      mutate(compliant = 1 * (trt == 0 | trt == 1 & ltne_20 < log(6.41)))
  } else {
    analysis <- analysis %>%
      mutate(compliant = 1)
  }


  # primary analysis ----

  # drop week 6/8 outcomes and any other unnecessaries
  to_drop <- c("study_cpd", "total_cpd", "cesd") 
  train <- analysis
  train[, to_drop] <- NULL

  # outcomes that will be used throughout analysis.
  # cesd will be treated as both numeric and binary.
  # bcesd_20 >= 16 means depressed
  # train <- train %>%
  #   mutate(bcesd_20 = 1 * (cesd_20 >= 16))
  outcomes <- c("total_cpd_20", "cesd_20"
    #, "bcesd_20"
    , "co_20", "ltne_20", 
    "lnnal_visit20", "lphet_visit20", "lcema_visit20", "lpgem_visit20", 
    "liso_visit20", "weight_gain")

  # training data is CENIC-P2
  train <- train %>% filter(study == "P2")

  # matrix of predictors, outcomes.
  # drop variables that will not be used as predictors or as outcomes
  vars_to_drop <- c("id", "study", "trt")
  Ytrain <- train %>% select(outcomes)
  Xtrain <- train %>% select(-outcomes, -vars_to_drop)

  # must have complete set of potential predictors for inclusion.
  # missing Y values are handled separately for each variable
  complete_case <- apply(Xtrain, 1, none_are_na)
  # export complete_case for use in other files, e.g., 2-table-1
  if (cohort == "compliant")
    save(list = "complete_case", file = "../RData/complete_case.RData")
  Xtrain <- Xtrain[complete_case, ]
  Ytrain <- Ytrain[complete_case, ]

  Ytrain_missing <- as_tibble(is.na(Ytrain))

  trt <- as.logical(train$trt) # treatment indicator, only for P2!
  trt <- trt[complete_case]
  com <- Xtrain[, 'compliant', drop = TRUE]
 

  # Xtrain <- model.matrix(~ 0 + ., Xtrain)
  # fm is ~ . -compliant since we don't want to include
  # compliance as a predictor
  Xtrain <- model.matrix(~ . -compliant, Xtrain)
  Xtrain <- Xtrain[, -1]

  # cbind(colnames(Xtrain))

  # debug(estimate_trt_diff)

  # random number seeds to ensure control groups are the same
  # rnseeds <- array(dim = c(2, 2, length(outcomes)),
  #   dimnames  = list(
  #     cohort  = c("ITT", "compliant"),
  #     group   = c("con", "trt"),
  #     outcome = outcomes))
  # rnseeds()
  # ~ treatment effects ----
  trt_diffs_list <- list()
  for (outcome in outcomes) {
    is_binomial <- ifelse(outcome == "bcesd_20", TRUE, FALSE)
    trt_diffs_list[[outcome]] <- estimate_trt_diff(X = Xtrain,
      X0 = Xtrain[!trt & com, ], 
      X1 = Xtrain[trt  & com, ], 
      Y0 = Ytrain[!trt & com, outcome, drop = TRUE],
      Y1 = Ytrain[trt  & com, outcome, drop = TRUE],
      estimation = estimation,
      is_binomial = is_binomial)
  }
  # set.seed(321)

  # create matrix with estimated treatment effects
  trt_diffs <- sapply(trt_diffs_list, '[[', "trt_diff")
  trt_diffs <- as_tibble(trt_diffs)

  # missing Y values have missing values for treatment effect
  for (outcome in outcomes) {
    trt_diffs[Ytrain_missing[, outcome, drop = TRUE], outcome] <- NA
  }

  # ~ output histogram data ----
  histolist <- list()
  for (outcome in outcomes) {
    x <- trt_diffs[, outcome, drop = TRUE]
    h <- hist(x, plot = FALSE)
    h$mean <- mean(x, na.rm = TRUE)
    histolist[[outcome]] <- h
  }

  save(list = "histolist", file = sprintf("../RData/histolist-%s.RData", cohort))

  # ~ permutation tests ----
  n_perm <- 10000
  n <- nrow(Xtrain)
  stddevs <- array(dim = c(n_perm, length(outcomes)),
    dimnames = list(permutation = seq_len(n_perm), outcome = outcomes))

  for(outcome in outcomes) {
    y_perm <- Ytrain[, outcome, drop = TRUE]
    if(outcome != "bcesd_20") {
      y_perm[trt] <-  y_perm[trt] - (mean(y_perm[trt], na.rm = TRUE) - 
        mean(y_perm[!trt], na.rm = TRUE))
    }
    #complete_temp <- !is.na(y_perm)
    #x_perm <- Xtrain[complete_temp, ]
    #y_perm <- y_perm[complete_temp]
    #trt_temp <- trt[complete_temp]
    # n <- length(y_perm)
    for(ii in seq_len(n_perm)) {
      index <- seq_len(n)
      index <- sample(index)
      trt_perm <- trt[index]
      com_perm <- com[index]
      trt_diff_perm <- estimate_trt_diff(X = Xtrain,
        X0 = Xtrain[!trt_perm & com_perm, ], 
        X1 = Xtrain[trt_perm  & com_perm, ], 
        Y0 = y_perm[!trt_perm & com_perm],
        Y1 = y_perm[trt_perm  & com_perm],
        estimation = estimation,
        is_binomial = ifelse(outcome == "bcesd_20", TRUE, FALSE))
      tdp <- trt_diff_perm$trt_diff
      tdp[Ytrain_missing[, outcome, drop = TRUE]] <- NA
      stddevs[ii, outcome] <- sd(tdp, na.rm = TRUE)
      if(ii %% 100 == 0)
        message(outcome, ": ", ii)
    }
  }

    # pvals:
  pvals <- list()
  for(outcome in outcomes)
    pvals[[outcome]] <- mean(stddevs[, outcome] > sd(trt_diffs[, outcome, drop = TRUE], na.rm = TRUE))
  pvals <- do.call(c, pvals)

  pvals

  sink(sprintf("tables/%s/%s/permutaion-pvals.txt", estimation, cohort))
  print(pvals)
  sink()



  # ~ cross validation for tree depth ----
  # set.seed(11350)
  # depths <- 0:8
  # k <- 5 # number of folds
  # n_mc <- 10
  # mse <- array(dim = c(length(depths), length(outcomes), n_mc),
  #   dimnames = list(depth = depths, outcome = outcomes, 
  #     iteration = seq_len(n_mc)))

  # for debugging/updating
  # outcome <- outcomes[1]
  # depth <- 0
  # fold <- 1
  # mc <- 2

  # for (mc in seq_len(n_mc)) {
  #   # loop over outcomes
  #   for (outcome in outcomes) {
  #     # testdata <- as.data.frame(Xtrain)
  #     # testdata[outcome] <- Ytrain[outcome]
  #     # testdata <- na.omit(testdata)
  #     cY <- Ytrain[, outcome, drop = TRUE]
  #     cX <- Xtrain[!is.na(cY), ]
  #     ctrt <- trt[!is.na(cY)]
  #     cY <- cY[!is.na(cY)]
  #     folds <- rep(seq_len(k), length.out = nrow(cX))
  #     folds <- sample(folds)
  #     # loop over depth
  #     for (depth in depths) {
  #       e <- rep(NA, nrow(cX)) # prediction error vector
  #       # loop over csed20
  #       for(fold in seq_len(k)) {
  #         msg <- sprintf("mc %i, outcome %s, depth %i, fold %i",
  #           mc, outcome, depth, fold)
  #         message(msg)
  #         try({
  #           # lasso, train
  #           is_binomial <- ifelse(outcome == "bcesd_20", TRUE, FALSE)
  #           tdl_train <- estimate_trt_diff(X = cX[folds != fold, ],
  #             X0 = cX[!ctrt & folds != fold, ], 
  #             X1 = cX[ctrt  & folds != fold, ], 
  #             Y0 = cY[!ctrt & folds != fold],
  #             Y1 = cY[ctrt  & folds != fold],
  #             estimation = estimation,
  #             is_binomial = is_binomial)
  #           td_train <- tdl_train$trt_diff
  #           # tree, train
  #           if (depth > 0) {
  #             train_tree <- rtree(cX[folds != fold, ], td_train, 
  #               maxdepth  = depth,
  #               minbucket = 8,
  #               cp        = 0)
  #           }
  #           tdl_test <- estimate_trt_diff(X = cX[folds == fold, ],
  #             X0 = cX[!ctrt & folds == fold, ],
  #             X1 = cX[ctrt  & folds == fold, ],
  #             Y0 = cY[!ctrt & folds == fold],
  #             Y1 = cY[ctrt  & folds == fold],
  #             estimation = estimation,
  #             is_binomial = is_binomial)
  #           y <- tdl_test$trt_diff
  #           # y <- trt_diffs[fold == folds, outcome, drop = TRUE]
  #           yhat <- if (depth > 0) {
  #             unname(predict(train_tree,
  #               newdata = as.data.frame(cX[folds == fold, ]))) 
  #           } else {
  #             mean(tdl_train$trt_diff)
  #           }
  #           e[folds == fold] <- y - yhat
  #         })
  #       }
  #       mse[match(depth, depths), outcome, mc] <- mean(e ^ 2, na.rm = TRUE)
  #     }
  #   }
  # }

  # average over mc iterations
  # mmse <- apply(mse, c(1, 2), mean)
  # smse <- apply(mse, c(1, 2), sd)

  # min_mse_depth <- depths[apply(mmse, 2, which.min)]

  # cat("Max tree depth giving minimum MSE: \n")
  # for(i in seq_along(outcomes))
  #   cat(outcomes[i], ":", min_mse_depth[i], "\n")
  # names(min_mse_depth) <- outcomes

  # was previously finding depth with min mse, then
  # increasing depth by one the mse was within 1 sd of
  # the min. not doing this any more.

  # is next depth within one sd of min?
  # matchmin <- match(min_mse_depth, depths)
  # within_one <- mmse[cbind(matchmin + 1, c(1, 2, 3))] < 
  #   mmse[cbind(matchmin, c(1, 2, 3))] + 
  #   smse[cbind(matchmin, c(1, 2, 3))]
  # min_mse_depth <- (min_mse_depth + 1) * within_one + 
  #   min_mse_depth * (1 - within_one)

  # ~ cv mse plot ----
  # cols <- c("steelblue2", "seagreen3", "gold")
  # cols <- rainbow(length(outcomes))
  # title_names <- c("Total CPD", "CESD", "CESD (Binary)", "CO", 
  #   "NNAL", "PheT", "CEMA", "PGEM", "ISO", "Weight Gain")
  # pdf(sprintf("plots/%s/%s/cv-mse.pdf", estimation, cohort))
  # par(mar=c(5, 4, 4, 8) + 0.1, xpd = TRUE)
  # plot(mmse[, 1] ~ depths,
  #   ylim = range(mmse),
  #   pch = 20,
  #   col = cols[1],
  #   ylab = "MSE",
  #   xlab = "Maximum Tree Depth",
  #   main = "Cross Validation Error",
  #   cex = 1.5)
  # lines(mmse[, 1] ~ depths,
  #   lwd = 2,
  #   col = cols[1])
  # # points(mmse[, 2] ~ depths, 
  # #   col = cols[2],
  # #   pch = 16,
  # #   cex = 1.5)
  # # points(mmse[, 3] ~ depths, 
  # #   col = cols[3],
  # #   pch = 17,
  # #   cex = 1.5)
  # for(ii in 2:length(outcomes)) {
  #   points(mmse[, ii] ~ depths,
  #     col = cols[ii],
  #     pch = 20,
  #     cex = 1.5)
  #   lines(mmse[, ii] ~ depths,
  #     col = cols[ii],
  #     lwd = 2)
  # }
  # # for(i in 1:3)
  #   # lines(mmse[, i] ~ depths, col = cols[i], lwd = 2)
  # legend("topright",
  #   legend = title_names,
  #   lty = 1,
  #   col = cols,
  #   pch = 20,
  #   bty = "n",
  #   inset = c(-0.3625, 0))
  # dev.off()

  # set all tree depths to 2.
  min_mse_depth <- rep(2, length(outcomes))
  names(min_mse_depth) <- outcomes

  # create the tree for each column in treat_diffs
  # debug(rtree)
  if (any(min_mse_depth == 0))
    stop("min mse at depth 0")
  tree_list <- list()
  for (outcome in outcomes) {
    tdt <- trt_diffs[, outcome, drop = TRUE]
    xtemp <- Xtrain[!is.na(tdt), ]
    tdt <- tdt[!is.na(tdt)]
    tree_list[[outcome]] <- rtree(xtemp, tdt,
      maxdepth = min_mse_depth[outcome], minbucket = 8, cp = 0)  
  }

  # fitted treat diffs
  fitted_trt_diffs <- lapply(tree_list, predict)
  ftd <- array(dim = dim(trt_diffs))
  colnames(ftd) <- outcomes
  for(outcome in outcomes) {
    ftd[!Ytrain_missing[, outcome, drop = TRUE], outcome] <- fitted_trt_diffs[[outcome]]
  }
  fitted_trt_diffs <- as_tibble(ftd)

  # 'where' matrix showing which obs belong to the same node.
  # where_tib <- sapply(tree_list, '[[', 'where')
  # where_tib <- as_tibble(where_tib)
  where_tib <- lapply(tree_list, '[[', 'where')
  wtb <- array(dim = dim(trt_diffs))
  colnames(wtb) <- outcomes
  for(outcome in outcomes) {
    wtb[!Ytrain_missing[, outcome, drop = TRUE], outcome] <- where_tib[[outcome]]
  }
  where_tib <- as_tibble(wtb)


  where_names      <- paste0("where_", names(where_tib))
  names(where_tib) <- where_names

  # check to make sure that we can get fitted values within
  # each terminal node by computing means within each value
  # of the 'where' vector. (The where vector is used by the
  # rpart package to determine node membership.) This code
  # checks to make sure that we get the correct answer with 
  # this method. 
  check <- list()

  for (outcome in outcomes) {
    where_name <- where_names[match(outcome, outcomes)]
    where      <- where_tib[, where_name, drop = TRUE]
    node_means <- tapply(trt_diffs[, outcome, drop = TRUE], where, mean)
    nodes      <- as.numeric(names(node_means))
    est        <- node_means[match(where, nodes)]

    check[[outcome]] <- est
  }

  check <- as_tibble(check)

  # if TRUE, then proceed and compute means using this method.
  # If not, debug until TRUE.
  eq <- all_equal(round(check, 8), round(fitted_trt_diffs, 8))
  if (!eq)
    stop("check failed. re-program method for finding means within terminal nodes")

  # compute means within each where group.
  trt_diffs_w_where <- bind_cols(trt_diffs, where_tib)
  trt_diffs_w_where

  fitted_means <- list()
  fitted_SEMs  <- list()

  for(outcome in outcomes) {
   where_col <- where_names[match(outcome, outcomes)] 
   means <- aggregate(trt_diffs_w_where, 
     by  = list(trt_diffs_w_where[, where_col, drop = TRUE]), 
     FUN = mean,
     na.rm = TRUE)
   SEMs <- aggregate(trt_diffs_w_where, 
     by  = list(trt_diffs_w_where[, where_col, drop = TRUE]), 
     FUN = SEM)
   means <- means[, c(where_col, outcomes)]
   SEMs  <- SEMs[, c(where_col, outcomes)]
   names(means)[1] <- "node"
   names(SEMs)[1]  <- "node"
   fitted_means[[outcome]] <- means 
   fitted_SEMs[[outcome]]  <- SEMs
  }

  # ~ tables ----

  # ~~ lasso coefficients ----
  if (estimation == "lasso") {
    con_coefs <- lapply(trt_diffs_list, '[[', 'm0')
    con_coefs <- lapply(con_coefs, coef)

    trt_coefs <- lapply(trt_diffs_list, '[[', 'm1')
    trt_coefs <- lapply(trt_coefs, coef)

    coefs <- mapply(cbind, con_coefs, trt_coefs)
    coefs <- do.call(rbind, coefs)
    coefs <- as.matrix(coefs)

    rownames(coefs) <- paste0(rep(outcomes, sapply(con_coefs, nrow)), " & ",
      rownames(coefs))

    # keep rows with at least 1 non-zero coefficient:
    coefs <- coefs[abs(rowSums(coefs)) > 0, ]

    # format for printing
    colnames(coefs) <- c("Control", "Treatment")

    coefs  <- as.data.frame(coefs)

    coefs[] <- lapply(coefs, function(x) sprintf("%.2f", x))
    coefs[] <- lapply(coefs, function(x) gsub("0.00", "-", x))

    write.table(coefs,
      file      = sprintf("tables/%s/%s/lasso-coefs.txt", estimation, cohort),
      sep       = " & ",
      quote     = FALSE)

  }

  # ~~ trt heterogeneity ----

  for (outcome in outcomes) {
    mt <- fitted_means[[outcome]]
    st <- fitted_SEMs[[outcome]]
    dm <- dim(mt)
    dn <- dimnames(mt)

    mt <- as.list(c(as.matrix(mt)))
    st <- as.list(c(as.matrix(st)))

    tab <- mapply(function(x, y) sprintf("%0.2f (%0.2f)", x, y),
      mt, st)
    tab <- array(tab, dim = dm, dimnames = dn)
    tab <- t(tab)
    tab <- as.data.frame(tab)
    tab <- tab[-1, ]
    colnames(tab) <- paste0("node", colnames(tab))

    sink(file = sprintf("tables/%s/%s/tree-%s.txt", estimation, cohort, outcome))
    print(tab)
    sink()
  }

  # n_lines <- sapply(fitted_means, nrow)
  # 
  # tab    <- do.call(rbind, fitted_means)
  # rnames <- rownames(tab)
  # dtab   <- dim(tab)
  # tab    <- as.numeric(sprintf("%.2f", as.matrix(tab)))
  # tab    <- array(tab, dim = dtab)
  # 
  # tab <- apply(tab, 2, function(x) sprintf("%0.2f", x))
  # 
  # colnames(tab) <- c("node", outcomes)
  # # rownames(tab) <- rnames
  # 
  # tab <- as.data.frame(tab)
  # 
  # tab$node <- rnames
  # 
  # write.table(tab,
  #   file      = sprintf("tables/%s/%s/tree-outcomes.txt", estimation, cohort),
  #   sep       = " & ",
  #   quote     = FALSE,
  #   row.names = FALSE)

  # ~ plots ----

  # ~~ histograms ----
  # title_names <- c("Total CPD", "CESD", "CO")
  # pdf(sprintf("plots/%s/%s/trt-diff-histograms.pdf", estimation, cohort))
  # par(mfrow = c(3, 4))
  # for(outcome in outcomes) {
  #   hist(trt_diffs[, outcome, drop = TRUE],
  #     main = title_names[match(outcome, outcomes)],
  #     xlab = title_names[match(outcome, outcomes)],
  #     sub = sprintf("p = %0.3f", pvals[outcome]))
  #   # legend("topleft", bty = "n", 
  #   #   legend = sprintf("p = %0.3f", pvals[outcome]))
  # }
  # dev.off()

  # for (outcome in outcomes) {
  #   pdf(sprintf("plots/%s/%s/trt-diff-histograms-%s.pdf", estimation, cohort, outcome))
  #   hist(trt_diffs[, outcome, drop = TRUE],
  #     main = title_names[match(outcome, outcomes)],
  #     xlab = title_names[match(outcome, outcomes)],
  #     #,sub = sprintf("p = %0.3f", pvals[outcome])
  #     sub = sprintf("%s Analysis", ifelse(cohort == "compliant", "Compliers", "ITT")),
  #     prob = TRUE)
  #   dev.off()
  # }

  # ~~ trees ----

  # debug(split_fun)

  for(outcome in outcomes) {
    pdf(sprintf("plots/%s/%s/tree-%s.pdf", estimation, cohort, outcome))
    plot_tree(tree_list[[outcome]])
    dev.off()
  }



  # validation ----

  if (cohort == "ITT") {
    # if not, don't bother with validation
    # drop week 20 and non-baseline variables

    #vars_to_drop <- c("total_cpd_20", "study_cpd", "cesd", "cesd_20", "tne_20", "co_20", "study_cpd", "study_cpd")
    #train <- analysis[, -match(vars_to_drop, names(analysis), NULL)]

    bios <- paste0('l', biomarkers)
    bios <- setdiff(bios, "ltne_bsl")
    train <- analysis[, -match(outcomes, colnames(analysis), 0L)]
    train <- train[, -match(bios, colnames(train), 0L)]
    train$weight_visit0 <- NULL

    train <- na.omit(train)
    test <- train %>% filter(study == "P1S1")
    train <- train %>% filter(study == "P2")


    # treatment indicators
    trt_train <- as.logical(train$trt)
    trt_test  <- as.logical(test$trt)

    # predictor matrices and outcome 
    Ytrain <- train$total_cpd
    Ytest  <- test$total_cpd

    vars_to_drop <- c("id", "study", "trt", "total_cpd", "study_cpd")
    Xtrain <- train %>% select(-vars_to_drop)
    Xtest  <- test  %>% select(-vars_to_drop)

    # cbind(names(Xtrain))
    # cbind(names(Xtest))

    # Xtrain <- model.matrix(~ 0 + ., Xtrain)
    # Xtest  <- model.matrix(~ 0 + ., Xtest)
    Xtrain <- model.matrix(~ ., Xtrain)
    Xtest  <- model.matrix(~ ., Xtest)

    Xtrain <- Xtrain[, -1]
    Xtest  <- Xtest[, -1]

    # debugonce(estimate_trt_diff)

    # ~ estimated treatment effects ----
    tdiffs_train <- estimate_trt_diff(X = Xtrain,
      X0 = Xtrain[!trt_train, ], 
      X1 = Xtrain[trt_train, ], 
      Y0 = Ytrain[!trt_train],
      Y1 = Ytrain[trt_train],
      estimation = estimation)

    tdiffs_test <- estimate_trt_diff(X = Xtest,
      X0 = Xtest[!trt_test, ], 
      X1 = Xtest[trt_test, ], 
      Y0 = Ytest[!trt_test],
      Y1 = Ytest[trt_test],
      estimation = estimation)

    #  estimated treatment effects
    trt_diffs_train <- tdiffs_train$trt_diff
    trt_diffs_test  <- tdiffs_test$trt_diff

    # ~ regression tree ----

    # ~~ cross validation for tree depth ----

    # set.seed(19450508) depths <- 0:8 n_mc <- 10 mse <- array(dim =
    # c(length(depths), n_mc), dimnames = list(depth = depths, iteration =
    # seq_len(n_mc)))
    #
    # k <- 5 # number of folds folds <- rep(seq_len(k), length.out = nrow(Xtrain))
    # folds <- sample(folds)

    # for debugging/updating
    # depth <- 5
    # fold <- 1

    # testdata <- as.data.frame(Xtrain)
    # testdata$total_cpd <- Ytrain
    # for(mc in seq_len(n_mc)) {
    #   # loop over depth
    #   for (depth in depths) {
    #     e <- numeric(nrow(Xtrain)) # prediction error vector
    #     # loop over folds
    #     for(fold in seq_len(k)) {
    #       message(sprintf("mc %i, depth %i, fold %i", mc, depth, fold))
    #       # lasso, train
    #       tdl_train <- estimate_trt_diff(X = Xtrain[folds != fold, ],
    #         X0 = Xtrain[!trt & folds != fold, ], 
    #         X1 = Xtrain[trt  & folds != fold, ], 
    #         Y0 = Ytrain[!trt & folds != fold],
    #         Y1 = Ytrain[trt  & folds != fold],
    #         estimation = estimation)
    #       td_train <- tdl_train$trt_diff
    #       # tree, train
    #       if (depth > 0) {
    #         train_tree <- rtree(Xtrain[folds != fold, ], td_train, 
    #           maxdepth  = depth,
    #           minbucket = 8,
    #           cp        = 0)
    #       }
    #       tdl_test <- estimate_trt_diff(X = Xtrain[folds == fold, ],
    #         X0 = Xtrain[!trt & folds == fold, ],
    #         X1 = Xtrain[trt  & folds == fold, ],
    #         Y0 = Ytrain[!trt & folds == fold],
    #         Y1 = Ytrain[trt  & folds == fold],
    #         estimation = estimation)
    #       y <- tdl_test$trt_diff
    #       # y <- trt_diffs_train[fold == folds]
    #       yhat <- if (depth > 0) {
    #         unname(predict(train_tree,
    #           newdata = as.data.frame(Xtrain[folds == fold, ]))) 
    #       } else {
    #         mean(tdl_train$trt_diff)
    #       }
    #       e[folds == fold] <- y - yhat
    #     }
    #     mse[match(depth, depths), mc] <- mean(e ^ 2)
    #   }
    # }
    # 
    # # average over iterations
    # mmse <- rowMeans(mse)
    # smse <- apply(mse, 1, sd)
    # 
    # md <- depths[which.min(mmse)]
    # ds <- sprintf("Depth with minimum MSE for validation: %i\n", md)
    # cat(ds)
    # 
    # matchmin <- match(md, depths)
    # within_one <- mmse[matchmin + 1] < 
    #   mmse[matchmin] + 
    #   smse[matchmin]
    # md <- (md + 1) * within_one + 
    #   md * (1 - within_one)



    # debugonce(rtree)
    tree <- rtree(Xtrain, trt_diffs_train, maxdepth = 4,
      cp        = 0,
      minbucket = 8)

    # ~ test group means ----

    # predicted means from tree

    if (unique(length(tree$frame$yval)) < length(tree$frame$yval))
      stop("yval doesn't contain unique values. Use of 'match' is invalid")

    pred_test <- predict(tree, as.data.frame(Xtest))
    pred_node <- match(pred_test, tree$frame$yval)

    means_by_tree  <- tapply(pred_test, pred_node, mean)

    # observed means using tree covariate partition
    means_by_group <- tapply(trt_diffs_test, pred_node, mean)

    if (!all(names(means_by_group) == names(means_by_tree)))
      stop("check sorting of means in means_by_group and means_by_tree")

    # ~ plots ----


    # ~~ treatment effects ----

    pdf(sprintf("plots/%s/%s/validation-trt-diff-histograms.pdf", estimation, cohort),
      height = 5,
      width = 10)
    par(mfrow = c(1, 2))
    hist(trt_diffs_train, prob = TRUE,
      main = "CENIC-P2",
      xlab = "Estimated Treatment Effects")
    hist(trt_diffs_test, prob = TRUE,
      main = "CENIC-P1S1",
      xlab = "Estimated Treatment Effects")
    dev.off()

    # ~~ comparing means ----

    cols = c("steelblue2", "seagreen3")
    pdf(sprintf("plots/%s/%s/validation-means-plot.pdf", estimation, cohort))
    plot(means_by_tree ~ seq_along(sort(unique(pred_node))),
      pch = 16,
      xlab = "Terminal Node",
      ylab = "Treatment Effect",
      ylim = range(means_by_group, means_by_tree),
      col = cols[1])
    points(means_by_group ~ seq_along(sort(unique(pred_node))),
      pch = 17,
      col = cols[2])
    legend("topleft",
      bty = "n",
      pch = c(16, 17),
      col = cols,
      legend = c("Predicted Mean", "Observed Means"))
    dev.off()

    pdf(sprintf("plots/%s/%s/validation-scatter-plot.pdf", estimation, cohort))
    plot(means_by_group ~ means_by_tree,
      pch = 16,
      ylab = "Observed Means",
      xlab = "Predicted Means",
      sub  = "(Control - Treatment)",)
    legend("topleft",
      legend = sprintf("r = %.3f", cor(means_by_group, means_by_tree)),
      bty = "n")
    abline(a = 0, b = 1, lty = 2)
    dev.off()


    # ~ tables ----

    # ~~ lasso coefficients ----
    if (estimation == "lasso") {
      con_coefs <- lapply(list(tdiffs_train, tdiffs_test), '[[', 'm0')
      con_coefs <- lapply(con_coefs, coef)
      con_coefs <- do.call(cbind, con_coefs)

      trt_coefs <- lapply(list(tdiffs_train, tdiffs_test), '[[', 'm1')
      trt_coefs <- lapply(trt_coefs, coef)
      trt_coefs <- do.call(cbind, trt_coefs)

      coefs <- rbind(con_coefs, trt_coefs)
      coefs <- as.matrix(coefs)

      rownames(coefs) <- paste0(rep(c("con", "trt"), each = nrow(con_coefs)), " & ",
        rownames(coefs))

      # keep rows with at least 1 non-zero coefficient:
      coefs <- coefs[rowSums(coefs) > 0, ]

      # format for printing
      colnames(coefs) <- c("Train", "Test")

      coefs  <- as.data.frame(coefs)

      coefs[] <- lapply(coefs, function(x) sprintf("%.2f", x))
      coefs[] <- lapply(coefs, function(x) gsub("0.00", "-", x))

      write.table(coefs,
        file      = sprintf("tables/%s/%s/validation-lasso-coefs.txt", estimation, cohort),
        sep       = " & ",
        quote     = FALSE)

    }
  }
}
end <- Sys.time()
end - begin


# this code was used to reformat the tables. The 
# reformatting is now done earlier in the code.
# ot <- read.table(sprintf("tables/%s/%s/tree-outcomes.txt", estimation, cohort),
#   as.is = TRUE, header = TRUE)
# ot <- as_tibble(ot)
# ot <- ot %>% select(-starts_with("X"))
# ot <- ot %>% 
#   separate(node, sep = "\\.", into = c("outcome", "node"))
# ot$node <- as.double(ot$node)
# 
# # loop over outcomes
# for (outcome in outcomes){
#   tot <- ot[ot$outcome == outcome, ]
#   tot <- as.matrix(tot[, -1])
#   tot <- t(tot)
#   tot <- as.data.frame(tot)
#   tot <- tot[-1, ]
#   colnames(tot) <- seq_len(ncol(tot))
#   write.table(tot,
#     file = sprintf("tables/%s/%s/tree-%s.txt", estimation, cohort, outcome),
#     sep = " & & ",
#     quote = FALSE)
# }

# stuff to create heatmap for figures
# apply(trt_diffs, 2, range)
# apply(trt_diffs, 2, median)
# 
# (4.93 - -1.227949) / (7.7200636 - -1.227949) * 255
# (7.6 - -1.227949) / (7.7200636 - -1.227949) * 255
# (17.735944 - 8.74) / (17.735944 - 7.7200636) * 255
# 
# 
# rgb <- function(val, vals) {
#   lo <- min(vals)
#   hi <- max(vals)
#   med <- median(vals)
#   los <- round((val - lo) / (med - lo) * 255)
#   his <- round((hi - val) / (hi - med) * 255)
#   ltmed <- val < med
#   if (ltmed) 
#     msg <- sprintf("r: %i, g: %i, b: %i", los, los, 255)
#   else
#     msg <- sprintf("r: %i, g: %i, b: %i", 255, his, his)
#   msg
# }
# 
# rgb(4.93, trt_diffs$total_cpd_20)
# rgb(7.6, trt_diffs$total_cpd_20)
# 
# lapply(c(4.77, 5.98, 6.86, 7.41), rgb, vals = trt_diffs$co_20)
# 
# lapply(c( 8.67, 8.75, 7.49, 7.14), rgb, vals = trt_diffs$total_cpd_20)
# lapply(c( -3.05, 1.39, -0.2, 2.5), rgb, vals = trt_diffs$cesd_20)
# lapply(c( 6.34, 5.82, 6.33, 5.21), rgb, vals = trt_diffs$co_20)
# 
# lapply(c(6.46, 9.88, 4.79, 8.51, 8.87, 7.65, 10.28, 8.63), rgb, vals = trt_diffs$total_cpd_20)
# lapply(c(0.16, -0.14, -0.11, -0.68, 0, -0.71, -0.53, -0.65), rgb, vals = trt_diffs$cesd_20)
# lapply(c(2.63, 4.86, 5.12, 7.1, 6.4, 8.79, 9.68, 12.07), rgb, vals = trt_diffs$co_20)
