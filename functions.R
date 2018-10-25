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
  # exist...
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
  zhr <- qnorm(cumsum(rev(hits) / sum(hits)))
  zfar <- qnorm(cumsum(rev(fas) / sum(fas)))

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

  data.frame(gamma, sdt_metrics, r, p, n, con_m, row.names = NULL)
}
