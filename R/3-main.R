library(tidyverse)
library(xtable)
source("R/functions.R")

# --- global variables dictating output and cohort --- #
table_dir <- "all"
# ---------------------------------------------------- #

set.seed(123)

load("../RData/analysis.RData")
analysis <- analysis %>% rename(trt = teh_treatment)

# outcomes that will be used throughout analysis.
outcomes <- c("total_cpd", "cesd")

# training data is CENIC-P2
train <- analysis %>% filter(study == "P2")
trt   <- as.logical(train$trt) # treatment indicator, only for P2!


# matrix of predictors, outcomes.
# drop variables that will not be used as predictors or as outcomes
vars_to_drop <- c("id", "study_cpd", "study", "trt", "teh_control")
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

# create the tree for each column in treat_diffs
# debug(rtree)
tree_list <- list()
for (outcome in outcomes)
  tree_list[[outcome]] <- rtree(Xtrain, trt_diffs[, outcome, drop = TRUE])  

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

# tables ----

# ~ lasso coefficients ----
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
  file      = sprintf("tables/%s/lasso-coefs.txt", table_dir),
  sep       = " & ",
  quote     = FALSE)

# ~ trt heterogeneity ----

n_lines <- sapply(fitted_means, nrow)

tab    <- do.call(rbind, fitted_means)
rnames <- rownames(tab)
dtab   <- dim(tab)
tab    <- as.numeric(sprintf("%.2f", as.matrix(tab)))
tab    <- array(tab, dim = dtab)

colnames(tab) <- c("node", outcomes)
rownames(tab) <- rnames

write.table(tab,
  file      = "tables/tree-table.txt",
  sep       = " & ",
  quote     = FALSE)
