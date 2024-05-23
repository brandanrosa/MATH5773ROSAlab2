#' mygd
#'
#' A function which produces three plots based on Gradient Descent Theory. The plots estimate, you guessed it, the Beta's
#'
#'
#' @param binit initial estimate of beta's
#' @param s learning rate
#' @param iter number of iterations
#' @param ylm a pre-populated `lm()`
#' @param x a numeric vector with the explanatory variable
#' @param y a numeric vector with the response variable
#'
#' @return Three plots and an invisible list containing the tail data of the iterated estimates of the Intercept and Slope
#' @export
#'
#' @examples \dontrun{mygd(binit=c(10,4), s=0.001, iter=500, ylm, x, y)}
mygd <- function(binit = c(), s = 0.00001, iter = 500, ylm = ylmf, x, y){

  # Matrices
  x = model.matrix(ylm)
  y = as.matrix(y)

  # BetaHat
  betahat <- solve(t(x) %*% x) %*% t(x) %*% y

  # ylm
  sm <- summary(lm(y ~ x[, 2]))

  # estimated Beta's as matrix
  binit <- as.matrix(binit)

  # build stuff
  beta <- matrix(NA, nrow = 2, ncol = iter + 1, dimnames = list(c("B0", "B1")))
  beta[,1] <- binit
  sslope <- vector(mode = "numeric", length = iter + 1)

  for(i in 1:iter){
    slope = -2 * t(x) %*% (y - x %*% beta[,i])
    sslope[i] <- sqrt(t(slope) %*% slope)
    beta[,i + 1] <- beta[,i] - s * slope
  }

  beta <- t(beta)

  t0 <- as.vector(tail(beta)[,1])
  t1 <- as.vector(tail(beta)[,2])

  Index = c(1:(iter + 1))

  beta.df <- data.frame(Index, beta[,1], beta[,2])
  names(beta.df) <- c("Index", "Intercept", "Slope")

  par(mfrow = c(1, 3))

  p1 <- plot(x = beta.df$Index,
             y = beta.df$Intercept,
             pch = 21,
             col = 'hotpink',
             bg = 'white',
             ylab = "Intercept",
             xlab = "Index",
             type = "b",
             cex = 1.2
  )

  abline(h = ylm$coefficients[1], col = 'blue')

  p2 <- plot(x = beta.df$Index,
             y = beta.df$Slope,
             pch = 21,
             col = "hotpink",
             bg = "white",
             ylab = "Slope",
             xlab = "Index",
             type = "b",
             cex = 1.2
  )

  abline(h = ylm$coefficients[2], col = "blue")

  p3 <- plot(x = beta.df$Index,
             y = sslope,
             type = "l",
             col = "hotpink",
             lwd = 1.5,
             ylab = "Gradient of RSS",
             xlab = "Index"
  )

  invisible(list(B0.hat_tail = tail(beta.df$Intercept), B1.hat_tail = tail(beta.df$Slope), compare_to_ylm=coef(ylm)))
}
