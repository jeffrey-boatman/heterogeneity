estimate_trt_diff <- function(X, X0, X1, Y0, Y1){
  require(glmnet)
  
  m0 <- cv.glmnet(x = X0, 
    y           = Y0, 
    family      = "gaussian", 
    alpha       = 1, 
    standardize = TRUE)
  m1 <- cv.glmnet(x = X1, 
    y           = Y1, 
    family      = "gaussian", 
    alpha       = 1, 
    standardize = TRUE)
  p0 <- c(predict(m0, newx = X, s = "lambda.min"))
  p1 <- c(predict(m1, newx = X, s = "lambda.min"))
  trt_diff <- p0 - p1

  out <- list()
  
  out$m0       <- m0
  out$m1       <- m1
  out$trt_diff <- trt_diff
  out
}

rtree <- function(X, Y, maxdepth = 4, minbucket = 2){
  require(rpart)
  data  <- as.data.frame(cbind(Y, X))
  fit <- rpart(Y ~ ., dat = data, 
    method  = "anova", 
    control = rpart.control(maxdepth = maxdepth, minbucket = minbucket), 
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
     "ltne_nmo",     "TNE",
     "qsu_f1_b",     "QSU Factor 1",
     "co_bsl",       "CO",
     "cesd_bsl",     "CESD",
     "wisdm_se",     "WISDM Social/Environmental Goads"
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

