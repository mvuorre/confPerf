#' Cut ratings to a desired number of bins
#'
#' @param x vector of ratings
#' @param min minimum value of rating scale
#' @param max maximum value of rating scale
#' @param n_bins desired number of bins
#' @param ... Not used
#'
#' @return Vector of ratings as an ordered factor
#' @export
#'
#' @examples
bin_ratings <- function(x, min, max, n_bins, ...) {
  # To [0, 1] interval
  unit <- (x - min) / (max - min)
  # To factor, ensuring levels exist
  bp <- c(-Inf, seq(0, 1, length = n_bins))
  factor(cut(unit, breaks = bp, labels = FALSE), levels = 1:n_bins)
}


#' Create an SDT table of cumulative proportions
#'
#' @param x Binary predictive variable (e.g. accuracy or stimulus)
#' @param y Ordinal ratings
#' @param constant Constant to add to rates
#' @param ... Passed to bin_ratings (min, max, n_bins)
#'
#' @return
#' @export
#'
#' @examples
sdt_roc <- function(x, y, constant, ...) {
  
  xcut <- factor(x, levels = 0:1)
  ycut <- bin_ratings(y, ...)
  
  tab <- table(xcut, ycut) # Frequency table
  tab <- tab + constant  # Add constant? (if it is zero no constant is added)
  tab <- tab / rowSums(tab) # Proportions
  # Cumulate proportions from high rating to low rating
  roc <- apply(tab, 1, function(x) rev(cumsum(rev(x))))
  roc
}

#' Calculate resolution metrics
#'
#' @param data A data frame
#' @param x Binary predictive variable, unquoted
#' @param y Raw ratings, unquoted
#' @param constant Constant added to rates
#' @param ... Passed to bin_ratings (min, max, n_bins)
#'
#' @return Data frame of resolution metrics
#' @export
#'
#' @examples
metrics <- function(data, x, y, constant, ...) {
  x <- enquo(x)
  y <- enquo(y)
  xv <- pull(data, !!x)
  yv <- pull(data, !!y)
  
  proportion_correct <- mean(xv, na.rm = T)
  # stopifnot(proportion_correct < 1)
  mean_rating <- mean(yv, na.rm = T)
  n_trials <- length(xv)
  
  # Measures using raw Y
  phi <- tryCatch({
    cor.test(xv, cut(yv, 2, labels = FALSE))$estimate
  }, error = function(e) {
    return(NA)
  })
  gamma_old <- tryCatch({
    vcdExtra::GKgamma(table(xv, yv))$gamma
  }, error = function(e) {
    return(NA)
  })
  pearson_r <- tryCatch({
    cor.test(xv, yv)$estimate
  }, error = function(e) {
    return(NA)
  })
  
  # Linear regression of zROC
  roc <- sdt_roc(xv, yv, constant, ...)
  zroc <- data.frame(qnorm(roc))
  zroc_ <- tryCatch({
    # Remove rows where zrate is infinite
    zroc <- subset(zroc, (!X0 %in% c(-Inf, Inf) & !X1 %in% c(-Inf, Inf)))
    zroc_lm <- lm(X1 ~ X0, data = zroc)
    zroc_d <- coef(zroc_lm)[["(Intercept)"]]
    zroc_m <- coef(zroc_lm)[["X0"]]
    zroc_s <- 1 / zroc_m
    zroc_da <- (sqrt(2) * zroc_d) / sqrt(1 + zroc_m^2)
    # Gamma using zROC (Higham & Higham, 2018)
    zroc_auc <- pnorm(zroc_d / sqrt(1 + (1 / zroc_s)^2))
    zroc_auc_gamma <- 2 * zroc_auc - 1
    tibble(zroc_d, zroc_da, zroc_s, zroc_auc, zroc_auc_gamma)
  }, error = function(e) {
    return(
      tibble(
        zroc_d = NA, 
        zroc_da = NA, 
        zroc_s = NA, 
        zroc_auc = NA, 
        zroc_auc_gamma = NA
      )
    )
  })
  
  # Ordinal regression on table of counts to allow adding constant
  ord_data <- tibble(
    xv = factor(xv, levels = 0:1), 
    yv = bin_ratings(yv, ...)
  )
  ord_data <- count(ord_data, xv, yv, .drop = FALSE)
  ord_data$n <- ord_data$n + constant
  ord_ <- tryCatch({
    fit <- ordinal::clm(
      ordered(yv) ~ factor(xv),
      link = "probit",
      scale = ~ factor(xv),
      weights = n,
      control = list(maxIter = 500, convergence = "stop"),
      data = ord_data
    )
    ord_d <- fit$beta
    ord_s <- exp(fit$zeta)
    ord_m <- 1 / ord_s
    ord_da <- (sqrt(2) * (ord_d / ord_s)) / sqrt(1 + ord_m^2)
    # Gamma using ordinal model's zROC
    ord_auc <- pnorm(ord_d / sqrt(1 + (1 / ord_s)^2))
    ord_auc_gamma <- 2 * ord_auc - 1
    tibble(ord_d, ord_da, ord_s, ord_auc, ord_auc_gamma)
  }, error = function(e) {
    return(tibble(ord_d = NA, ord_da = NA, ord_s = NA, ord_auc = NA, ord_auc_gamma = NA))
  })
  
  
  # Gamma using trapezoidal rule to approximate AUC
  trap_auc <- DescTools::AUC(roc[, "0"], roc[, "1"], method = "trapezoid")
  # auc_step <- DescTools::AUC(roc[,"0"], roc[,"1"], method = "step")
  # auc_spline <- DescTools::AUC(roc[,"0"], roc[,"1"], method = "spline")
  trap_auc_gamma <- 2 * trap_auc - 1
  
  out <- tibble(
    proportion_correct, mean_rating, n_trials,
    phi, pearson_r, gamma_old,
    trap_auc, trap_auc_gamma
  )
  bind_cols(out, zroc_, ord_)
}


# Simulation --------------------------------------------------------------

sim_run <- function(p_know = NA, 
                    n_trials = 100, 
                    p_guess = 0,
                    d_gen = 1, 
                    s_gen = 1,
                    tau_gen = c(-Inf, -1, -.5, 0, .5, 1, Inf),
                    constant = 0,
                    min = 1,
                    max = 6,
                    n_bins = 6,
                    out = "pars",
                    ...) {
  
  n_bins <- length(tau_gen) - 1
  
  # Performance (p(know) to number of known items)
  stopifnot(p_know <= 1 & p_know >= 0)
  n_know <- ceiling(p_know * n_trials)
  
  # Number of known items to number of accurate answers
  accuracy <- c(
    rbinom(n_trials - n_know, 1, p_guess),
    rep(1, times = n_know)
  )
  
  # Metacognition evidence values
  evidence <- c(
    rnorm(n_trials - n_know, 0, 1),
    rnorm(n_know, d_gen, s_gen)
  )
  
  # Evidence values to confidence ratings
  rating <- cut(
    evidence, 
    breaks = tau_gen, 
    labels = FALSE
  )
  # Ratings to 0 - 5, as in math data
  # rating <- rating-1
  
  # Return data or parameters
  dat <- data.frame(accuracy, rating)
  if (out == "pars") {
    metrics(
      data = dat, 
      x = accuracy, 
      y = rating, 
      constant = constant,
      min = min,
      max = max,
      n_bins = n_bins
    )
  } else if (out == "data") {
    dat
  }
}

