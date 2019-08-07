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

rtree <- function(X, Y){
  require(rpart)
  data  <- as.data.frame(cbind(Y, X))
  fit <- rpart(Y ~ ., dat = data, 
    method  = "anova", 
    control = rpart.control(maxdepth = 4), 
    model   = TRUE)
  fit
}

