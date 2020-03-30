library(parallel)


sim <- function(seed, coefs, n_perm = 1000) {
  set.seed(seed)
  n  <- 1e2
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n)
  a  <- rbinom(n, 1, 1 / 2)

  fm <- ~ a * (x1 + x2)
  dm <- model.matrix(fm)
  y <- rnorm(n, mean = c(dm %*% coefs))

  pe <- chuyu_est(y, a, x1, x2, FALSE)
  
  chuyu_pd <- replicate(n_perm, chuyu_est(y, a, x1, x2, TRUE))
  jeff_pd  <- replicate(n_perm, jeff_est(y, a, x1, x2, TRUE))
  david_pd <- replicate(n_perm, david_est(y, a, x1, x2, TRUE))

  out <- list()
  out$chuyu_pval  <- mean(chuyu_pd > pe)
  out$jeff_pval   <- mean(jeff_pd  > pe)
  out$david_pval  <- mean(david_pd > pe)
  unlist(out)
}

jeff_est <- function(y, a, x1, x2, permute = FALSE) {
  n <- length(y)
  index <- seq_len(n)
  if (permute)
    index <- sample(index)
  data <- data.frame(y = y, a = a, x1 = x1[index], x2 = x2[index])
  d0 <- data[data$a == 0, ]
  d1 <- data[data$a == 1, ]
  m0 <- lm(y ~ x1 + x2, data = d0)
  m1 <- lm(y ~ x1 + x2, data = d1)
  p0 <- predict(m0, newdata = data)
  p1 <- predict(m1, newdata = data)
  z  <- p1 - p0
  sd(z)
}

chuyu_est <- function(y, a, x1, x2, permute = FALSE) {
  n <- length(y)
  index <- seq_len(n)
  if (permute)
    index <- sample(index)
  data <- data.frame(y = y, a = a[index], x1 = x1, x2 = x2)
  d0 <- data[data$a == 0, ]
  d1 <- data[data$a == 1, ]
  m0 <- lm(y ~ x1 + x2, data = d0)
  m1 <- lm(y ~ x1 + x2, data = d1)
  p0 <- predict(m0, newdata = data)
  p1 <- predict(m1, newdata = data)
  z  <- p1 - p0
  sd(z)
}

david_est <- function(y, a, x1, x2, permute = FALSE) {
  n <- length(y)
  index <- seq_len(n)
  data <- data.frame(y = y, a = a, x1 = x1, x2 = x2)  

  if (permute) {
    index <- sample(index)    
    data$y[data$a == 1] <-  data$y[data$a == 1] - 
      (mean(data$y[data$a == 1]) - mean(data$y[data$a == 0]))
    #data <- data.frame(y = y, a = a[index], x1 = x1, x2 = x2)
    data$a <- data$a[index]
  }

  d0 <- data[data$a == 0, ]
  d1 <- data[data$a == 1, ]
  m0 <- lm(y ~ x1 + x2, data = d0)
  m1 <- lm(y ~ x1 + x2, data = d1)
  p0 <- predict(m0, newdata = data)
  p1 <- predict(m1, newdata = data)
  z  <- p1 - p0
  sd(z)
}

# type I error ----
coefs <- c(1, 1, 1, 1, 0, 0)
# debugonce(sim)
# sim(1, coefs, 10)

out <- mclapply(seq_len(1000),
  FUN      = sim,
  coefs    = coefs,
  n_perm   = 1000,
  mc.cores = 24)
out <- do.call(rbind, out)
apply(out, 2, function(x) mean(x < 0.05))

# power ----
coefs <- c(1, 1, 1, 1, 1 / 4, 1 / 4)

out <- mclapply(seq_len(1000),
  FUN      = sim,
  coefs    = coefs,
  n_perm   = 1000,
  mc.cores = 24)
out <- do.call(rbind, out)
apply(out, 2, function(x) mean(x < 0.05))
