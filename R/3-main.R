# R command for batch: 
# "C:\Program Files\R\R-3.6.1\bin\R.exe" CMD BATCH --vanilla R\3-main.R


library(tidyverse)
library(xtable)
source("R/functions.R")

# --- global variables dictating output and cohort --- #
output_dir <- "all"
cohort <- "all"
# ---------------------------------------------------- #

set.seed(123)

load("../RData/analysis.RData")
analysis <- analysis %>% rename(trt = teh_treatment)
analysis <- analysis %>% 
  mutate(tne_nmolperml_bsl = log(tne_nmolperml_bsl)) %>%
  rename("ltne" = "tne_nmolperml_bsl")


# validation ----

# drop week 20 and non-baseline variables

vars_to_drop <- c("total_cpd_20", "study_cpd", "cesd", "cesd_20", "tne_20", "co_20", "study_cpd", "study_cpd")
train <- analysis[, -match(vars_to_drop, names(analysis), NULL)]

test <- train %>% filter(study == "P1S1")
train <- train %>% filter(study == "P2")


# treatment indicators
trt_train <- as.logical(train$trt)
trt_test  <- as.logical(test$trt)

# predictor matrices and outcome 
Ytrain <- train$total_cpd
Ytest  <- test$total_cpd

vars_to_drop <- c("id", "study", "trt", "total_cpd")
Xtrain <- train %>% select(-vars_to_drop)
Xtest  <- test  %>% select(-vars_to_drop)

Xtrain <- model.matrix(~ 0 + ., Xtrain)
Xtest  <- model.matrix(~ 0 + ., Xtest)


# debug(estimate_trt_diff)

# ~ estimated treatment effects ----
tdiffs_train <- estimate_trt_diff(X = Xtrain,
  X0 = Xtrain[!trt_train, ], 
  X1 = Xtrain[trt_train, ], 
  Y0 = Ytrain[!trt_train],
  Y1 = Ytrain[trt_train])

tdiffs_test <- estimate_trt_diff(X = Xtest,
  X0 = Xtest[!trt_test, ], 
  X1 = Xtest[trt_test, ], 
  Y0 = Ytest[!trt_test],
  Y1 = Ytest[trt_test])

# create matrix with estimated treatment effects
trt_diffs_train <- tdiffs_train$trt_diff
trt_diffs_test  <- tdiffs_test$trt_diff

# ~ regression tree ----
tree <- rtree(Xtrain, trt_diffs_train, maxdepth = 5, minbucket = 8)

# ~ test group means ----

# from tree
pred_test <- predict(tree, as.data.frame(Xtest))
pred_node <- match(pred_test, tree$frame$yval)

means_by_tree  <- tapply(pred_test,      pred_node, mean)
means_by_group <- tapply(trt_diffs_test, pred_node, mean)

# ~ plots ----


# ~~ treatment effects ----

pdf(sprintf("plots/%s/validation-trt-diff-histograms.pdf", output_dir),
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
pdf(sprintf("plots/%s/validation-means-plot.pdf", output_dir),
  height = 5,
  width = 10)
plot(means_by_tree ~ seq_along(unique(pred_node)),
  pch = 16,
  xlab = "Terminal Node",
  ylab = "Treatment Effect",
  ylim = range(means_by_group, means_by_tree),
  col = cols[1])
points(means_by_group ~ seq_along(unique(pred_node)),
  pch = 16,
  col = cols[2])
legend("topleft",
  bty = "n",
  pch = 16,
  col = cols,
  legend = c("Predicted Mean", "Observed Means"))
dev.off()

pdf(sprintf("plots/%s/validation-scatter-plot.pdf", output_dir))
plot(means_by_tree ~ means_by_group,
  pch = 16,
  xlab = "Observed Means",
  ylab = "Predicted Means")
legend("topleft",
  legend = sprintf("r = %.3f", cor(means_by_group, means_by_tree)),
  bty = "n")
dev.off()


# ~ tables ----

# ~~ lasso coefficients ----
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
  file      = sprintf("tables/%s/validation-lasso-coefs.txt", output_dir),
  sep       = " & ",
  quote     = FALSE)

# ~ trt heterogeneity ----


# main analysis ----

# drop week 6/8 outcomes
to_drop <- c("tne_20", "study_cpd", "total_cpd", "cesd")
analysis[, to_drop] <- NULL

# outcomes that will be used throughout analysis.
outcomes <- c("total_cpd_20", "cesd_20", "co_20")

# training data is CENIC-P2
train <- analysis %>% filter(study == "P2")
trt   <- as.logical(train$trt) # treatment indicator, only for P2!


# matrix of predictors, outcomes.
# drop variables that will not be used as predictors or as outcomes
vars_to_drop <- c("id", "study", "trt")
Ytrain <- train %>% select(outcomes)
Xtrain <- train %>% select(-outcomes, -vars_to_drop)
Xtrain <- model.matrix(~ 0 + ., Xtrain)


# debug(estimate_trt_diff)

# loop over outcomes, get estimated treatment effects
trt_diffs_list <- list()
for (outcome in outcomes) {
  trt_diffs_list[[outcome]] <- estimate_trt_diff(X = Xtrain,
    X0 = Xtrain[!trt, ], 
    X1 = Xtrain[trt, ], 
    Y0 = Ytrain[, outcome, drop = TRUE][!trt],
    Y1 = Ytrain[, outcome, drop = TRUE][trt])
}

# create matrix with estimated treatment effects
trt_diffs <- sapply(trt_diffs_list, '[[', "trt_diff")
trt_diffs <- as_tibble(trt_diffs)

# ~ cross validation for tree depth ----
depths <- 1:8
mse <- array(dim = c(length(depths), length(outcomes)),
  dimnames = list(depth = depths, outcome = outcomes))

k <- 10 # number of folds
folds <- rep(seq_len(k), length.out = nrow(Xtrain))
folds <- sample(folds)

# loop over outcomes
for (outcome in outcomes) {
  # loop over depth
  for (depth in depths) {
    e <- numeric(nrow(Xtrain))
    # loop over folds
    for(fold in seq_len(k)) {
      message(outcome, ", ", depth, ", ", fold)
      train_tree <- rtree(Xtrain[folds != fold, ], 
        trt_diffs[folds != fold, outcome, drop = TRUE], 
        maxdepth = depth,
        minbucket = 8)
      yhat <- unname(predict(train_tree, 
        newdata = as.data.frame(Xtrain[folds == fold, ])))
      y <- trt_diffs[folds == fold, outcome, drop = TRUE]
      e[folds == fold] <- y - yhat
    }
    mse[depth, outcome] <- mean(e ^ 2)
  }
}

apply(mse, 2, which.min)

# ~ cv mse plot ----
cols <- c("steelblue2", "seagreen3", "gold")
pdf(sprintf("plots/%s/cv-mse.pdf", output_dir))
plot(mse[, 1] ~ depths,
  ylim = range(mse),
  pch = 15,
  col = cols[1],
  ylab = "MSE",
  xlab = "Maximum Tree Depth",
  main = "Cross Validation Error")
points(mse[, 2] ~ depths, 
  col = cols[2],
  pch = 16)
points(mse[, 3] ~ depths, 
  col = cols[3],
  pch = 17)
for(i in 1:3)
  lines(mse[, i] ~ depths, col = cols[i])
legend("topright",
  legend = outcomes,
  lty = 1,
  col = cols,
  pch = 15:17,
  bty = "n")
dev.off()

# create the tree for each column in treat_diffs
# debug(rtree)
tree_list <- list()
for (outcome in outcomes)
  tree_list[[outcome]] <- rtree(Xtrain, trt_diffs[, outcome, drop = TRUE],
    maxdepth = which.min(mse[, outcome]), minbucket = 8)  

# fitted treat diffs
fitted_trt_diffs <- sapply(tree_list, predict)
fitted_trt_diffs <- as_tibble(fitted_trt_diffs)

# 'where' matrix showing which obs belong to the same node.
where_tib <- sapply(tree_list, '[[', 'where')
where_tib <- as_tibble(where_tib)

where_names      <- paste0("where_", names(where_tib))
names(where_tib) <- where_names

# now we want to compute means within terminal nodes based
# on tree built for other outcomes. To do this, we only need to
# know which observations belong to the same terminal node.
# Then we can compute means within these groups. To do this,
# use the 'where' matrix to compute fitted values. To check that
# this works, test it by using the method to compute values
# on the original outcome. These should match the predicted
# values obtainred from the tree predict function.
# make sure this passes before proceeding. 

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
all_equal(round(check, 8), round(fitted_trt_diffs, 8))

# compute means within each where group.
trt_diffs_w_where <- bind_cols(trt_diffs, where_tib)
trt_diffs_w_where

fitted_means <- list()

for(outcome in outcomes) {
 where_col <- where_names[match(outcome, outcomes)] 
 means <- aggregate(trt_diffs_w_where, 
   by  = list(trt_diffs_w_where[, where_col, drop = TRUE]), 
   FUN = mean)
 means <- means[, c(where_col, outcomes)]
 names(means)[1] <- "node"
 fitted_means[[outcome]] <- means 
}

# ~ tables ----

# ~~ lasso coefficients ----
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
coefs <- coefs[rowSums(coefs) > 0, ]

# format for printing
colnames(coefs) <- c("Control", "Treatment")

coefs  <- as.data.frame(coefs)

coefs[] <- lapply(coefs, function(x) sprintf("%.2f", x))
coefs[] <- lapply(coefs, function(x) gsub("0.00", "-", x))

write.table(coefs,
  file      = sprintf("tables/%s/lasso-coefs.txt", output_dir),
  sep       = " & ",
  quote     = FALSE)

# ~~ trt heterogeneity ----

n_lines <- sapply(fitted_means, nrow)

tab    <- do.call(rbind, fitted_means)
rnames <- rownames(tab)
dtab   <- dim(tab)
tab    <- as.numeric(sprintf("%.2f", as.matrix(tab)))
tab    <- array(tab, dim = dtab)

tab <- apply(tab, 2, function(x) sprintf("%0.2f", x))

colnames(tab) <- c("node", outcomes)
# rownames(tab) <- rnames

tab <- as.data.frame(tab)

tab$node <- rnames

write.table(tab,
  file      = sprintf("tables/%s/tree-outomes.txt", output_dir),
  sep       = " & ",
  quote     = FALSE,
  row.names = FALSE)

# ~ plots ----

# ~~ histograms ----
title_names <- c("Total CPD", "CESD", "CO")
pdf(sprintf("plots/%s/trt-diff-histograms.pdf", output_dir))
par(mfrow = c(2, 2))
for(outcome in outcomes) {
  hist(trt_diffs[, outcome, drop = TRUE],
    main = title_names[match(outcome, outcomes)],
    xlab = title_names[match(outcome, outcomes)])
}
dev.off()

# ~~ trees ----
for(outcome in outcomes) {
  pdf(sprintf("plots/%s/tree-%s.pdf", output_dir, outcome))
  plot_tree(tree_list[[outcome]])
  dev.off()
}



