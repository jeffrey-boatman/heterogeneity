estimate_trt_diff <- function(X, X0, X1, Y0, Y1, 
  estimation = c("lasso", "random_forest"), is_binomial = FALSE) {
  
  complete_0 <- !is.na(Y0)
  complete_1 <- !is.na(Y1)
  
  X0 <- X0[complete_0, ]
  X1 <- X1[complete_1, ]
  Y0 <- Y0[complete_0]
  Y1 <- Y1[complete_1]
  
  estimation <- match.arg(estimation)
  
  family <- ifelse(is_binomial, "binomial", "gaussian")
  
  if (estimation == "lasso") {
    require(glmnet)

    m0 <- cv.glmnet(x = X0,
      y           = Y0,
      family      = family,
      alpha       = 1,
      standardize = TRUE)
    m1 <- cv.glmnet(x = X1,
      y           = Y1,
      family      = family,
      alpha       = 1,
      standardize = TRUE)
    p0 <- c(predict(m0, newx = X, s = "lambda.min", type = "response"))
    p1 <- c(predict(m1, newx = X, s = "lambda.min", type = "response"))
    trt_diff <- p0 - p1

    nonzero_x0 <- rownames(coef(m0))[which(!(coef(m0) == 0))]
    nonzero_x1 <- rownames(coef(m1))[which(!(coef(m1) == 0))]

    nonzero_x0 <- setdiff(nonzero_x0, "(Intercept)")
    nonzero_x1 <- setdiff(nonzero_x1, "(Intercept)")

    out <- list()

    out$m0       <- m0
    out$m1       <- m1
    out$trt_diff <- trt_diff
    out$nonzero_x0 <- nonzero_x0
    out$nonzero_x1 <- nonzero_x1
  } else if (estimation == "random_forest") {

    stop('not yet implented for binomial outcomes')
    require(randomForest)

    m0 <- randomForest(x = X0, y = Y0)
    m1 <- randomForest(x = X1, y = Y1)
    p0 <- c(predict(m0, newdata = as.data.frame(X)))
    p1 <- c(predict(m1, newdata = as.data.frame(X)))
    trt_diff <- p0 - p1

    out <- list()

    out$m0       <- m0
    out$m1       <- m1
    out$trt_diff <- trt_diff

  }

  out
}

rtree <- function(X, Y, maxdepth = 4, minbucket = 2, cp = 0.01){
  require(rpart)
  data  <- as.data.frame(cbind(Y, X))
  fit <- rpart(Y ~ ., dat = data, 
    method  = "anova", 
    control = rpart.control(maxdepth = maxdepth, minbucket = minbucket, cp = cp), 
    model   = TRUE)
  fit
}

# function for plotting trees
plot_tree <- function(tree) {
  require(rpart.plot)
  prp(tree, type = 3, extra = 101, fallen.leaves = TRUE, split.fun = split_fun)  
}


# function for fixing names in plotted tree
split_fun <- function(x, labs, digits, varlen, faclen) {
  require(tibble)
  ll <- tribble(
    ~ old,          ~ new,
     "ftnd_w_c",     "FTND",
     "ftnd_wo_ ",    "FTND (No CPD)",
     "mnws_bsl",     "MNWS",
     "age",          "Age",
     #"ltne_nmo",     "TNE",
     "qsu_f1_b",     "QSU Factor 1",
     "co_bsl",       "Baseline CO",
     "cesd_bsl",     "Baseline CESD",
     "wisdm_se",     "WISDM Social/Environmental Goads",
     "wisdm_sd",     "WISDM Secondary Dependence Motive",
     "cpd_bsl",      "Baseline CPD",
     "ltne",         "Baseline log TNE",
     "ces_enjo",     "CES Enjoyment",
     "genderma = 0", "Male = 0",
     "genderfe = 1", "Female = 1",
     "lpgem_vi",     "log PGEM",
     "eduHSgra",     "HS Grad",
     "liso_vis",     "log ISO",
     "ces_aver ",    "CES Aversion",
     "wisdm_ce",     "WISDM Cue exposure/associative processes",
     "wisdm_af",     "WISDM Affiliative attachment",
     "wisdm_pd",     "WISDM Primary Dependence Motive",
     "pss_bsl",      "Perceived Stress Scale",
     "qsu_tot_",     "QSU"
  )
  olds <- ll$old
  news <- ll$new
  for (i in seq_along(olds)) {
    labs <- gsub(olds[i], news[i], labs)
  }
  # labs <- gsub("^BSL_co", "Baseline Expired Carbon Monoxide (ppm)", labs)
  # labs <- gsub("^WISDM_to", "WISDM Tolerance", labs)
  # labs <- gsub("^WISDM_so", "WISDM Social Goads", labs)
  for(i in seq_along(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width=20), collapse="\n")
  }
  labs
}

any_is_na <- function(x) any(is.na(x))

none_are_na <- Negate(any_is_na)
