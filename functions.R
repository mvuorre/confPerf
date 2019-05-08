# Function used in data analysis and simulations

#' metrics: Function to estimate performance and resolution metrics
#'
#' @param data A data.frame (or equivalent)
#' @param accuracy Name of column in `data` that contains
#' 0s and 1s denoting accuracy of each response, string
#' @param confidence Name of column in `data` that contains
#' continuous raw ratings of confidence, string
#' @param confidenceK Name of column in `data` that contains
#' categorical ratings of confidence, string
#' @param ordinal Should parameters of ordinal regression be returned, logical
#' @param levels Number of confidence levels (0 -> levels-1), integer
#' @param ... Not used
#'
#' @return data frame with resolution metrics
#'
metrics <- function(data,
                    accuracy = "accuracy",
                    confidenceK = "con6",
                    confidence = "con",
                    ordinal = FALSE,
                    levels = 6,
                    ...) {
  accuracy <- data[[accuracy]]
  conK <- data[[confidenceK]]

  # If continuous confidence provided, use it
  if (!is.null(confidence)) {
    con <- data[[confidence]]
    gamma <- tryCatch({
      Hmisc::rcorr.cens(accuracy, con, outx = TRUE)[2]
    },
    error = function(e) return(NA)
    )
    r <- tryCatch({
      cor.test(accuracy, con, method = "pearson")$estimate
    },
    error = function(e) return(NA)
    )
    con_m <- mean(con, na.rm = T)
  } else {
    # Otherwise use ordinal variable
    gamma <- tryCatch({
      vcdExtra::GKgamma(table(accuracy, conK))$gamma
    },
    error = function(e) return(NA)
    )
    r <- tryCatch({
      cor.test(accuracy, conK, method = "pearson")$estimate
    },
    error = function(e) return(NA)
    )
    con_m <- mean(as.integer(conK), na.rm = T)
  }


  p <- mean(accuracy, na.rm = T)
  n <- length(accuracy)

  # Get SDT measures with linear regression of zROC
  # Correct for zero observations in empty cells by ensuring all cells
  # exist (forcing factors ensures that all levels are enumerated)
  sdt <- data.frame(
    accuracy = factor(accuracy, levels = 0:1), 
    conK = factor(conK, levels = 0:(levels-1))
  )
  sdt <- data.frame(table(sdt))
  # ... and then add .5 to all cells
  sdt$Freq <- sdt$Freq + .5
  # Rates and cumulative zRates
  hits <- sdt$Freq[sdt$accuracy == 1]
  fas <- sdt$Freq[sdt$accuracy == 0]
  hr <- cumsum(rev(hits) / sum(hits))
  far <- cumsum(rev(fas) / sum(fas))
  zhr <- qnorm(hr)
  zfar <- qnorm(far)

  sdt_metrics <- tryCatch({
    coefs <- coef(lm(zhr[-length(zhr)] ~ zfar[-length(zfar)]))
    y0 <- coefs[[1]]
    m <- coefs[[2]]
    s <- 1 / m
    da <- (sqrt(2) * y0) / sqrt(1 + m^2)
    cbind(da, y0, s)
  }, error = function(e) {
    return(cbind(da = NA, y0 = NA, s = NA))
  })

  # SDT measures using ordinal regression
  if (ordinal) {
    sdt_metrics_ord <- tryCatch({
      fit <- ordinal::clm(
        ordered(conK) ~ factor(accuracy),
        link = "probit",
        scale = ~accuracy,
        control = list(maxIter = 100, convergence = "stop")
      )
      d_ord <- fit$beta
      s_ord <- exp(fit$zeta)
      m <- 1 / s
      da_ord <- (sqrt(2) * (d_ord / s_ord)) / sqrt(1 + m^2)
      cbind(da_ord, d_ord, s_ord)
    }, error = function(x) {
      return(cbind(da_ord = NA, d_ord = NA, s_ord = NA))
    })
    sdt_metrics <- cbind(sdt_metrics, sdt_metrics_ord)
  }
  
  # Gamma using ROC (Higham & Higham, 2018)
  az_zroc <- pnorm(y0 / sqrt(1 + (1/s)^2 ))
  gz <- 2 * az_zroc - 1
  # Gamma using trapezoidal rule to approximate AUC
  gt <- 2 * DescTools::AUC(far, hr, method = "trapezoid") - 1
  
  data.frame(gamma, gz, gt, sdt_metrics, r, p, n, con_m, row.names = NULL)
}

library(ggstance)
#' Quasi-brinley plots for resolution-performance scatterplots
#'
#' @param data 
#' @param x 
#' @param y 
#'
#' @return
#' @export
#'
#' @examples
brinley <- function(data, x, y) {
  x <- enquo(x)
  y <- enquo(y)
  xv <- pull(data, !!x)
  yv <- pull(data, !!y)
  xs <- Hmisc::smean.cl.boot(xv)
  ys <- Hmisc::smean.cl.boot(yv)
  xmin <- min(xv) - .05
  ymin <- min(yv) - .05
  ymax <- max(yv) + .05
  p <- ggplot(data, aes(!!x, !!y)) +
    geom_hline(yintercept = 0, lty = 2, size = .33) +
    geom_point(shape = 21, fill = "white", alpha = .75, stroke = .8) +
    geom_smooth(method = lm, col = "black", size = .75, alpha = .33) +
    scale_x_continuous(
      breaks = c(.25, .5, .75, 1), 
      limits = c(xmin, 1.05), expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(ymin, ymax), expand = c(0, 0)
    ) +
    labs(
      x = "Proportion correct",
      y = "Gamma"
    ) +
    coord_cartesian(clip = "off") +
    theme(aspect.ratio = 1, panel.border = element_rect(size = .33))
  p +
    annotate(
      x = xs[1], xmin = xs[2], xmax = xs[3], 
      y = ymin, geom = "pointrangeh", fatten = 2.25, size = 1.3
    ) +
    annotate(
      y = ys[1], ymin = ys[2], ymax = ys[3], 
      x = xmin, geom = "pointrange", fatten = 2.25, size = 1.3
    )
}
