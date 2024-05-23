#' mylm
#'
#' This function produces a plot of the data with fitted LSRL. There is also a list containing: beta estimates and confidence intervals, the design and response matrices, and sample variance, s^2.
#'
#' @param x a numeric vector with the explanatory variable
#' @param y a numeric vector with the response variable
#' @param alpha the alpha level for the ci
#'
#' @return a scatter plot with LSRL and an invisible list with above named components
#' @export
#'
#' @examples \dontrun{mylm(x = spruce$BHDiameter, y = spruce$Height)}
mylm <- function(x, y, alpha=0.05) { # x and y are vectors
  x0 <- rep(1, times = length(x))
  df <- data.frame(x0, x)
  dat <- data.frame(x0, x, y)

  X <- as.matrix(df)
  Y <- matrix(y, nrow = length(y), ncol = 1)

  cc <- solve(t(X) %*% X)

  betahat <- cc %*% t(X) %*% Y
  row.names(betahat) <- c("beta0", "beta1")

  degfree <- length(x) - 2
  RSS <- t(Y) %*% Y - t(betahat) %*% t(X) %*% Y

  ssq <- RSS / degfree
  s <- sqrt(ssq)
  tt <- qt(1-alpha/2, df = degfree)

  me0 <- tt*s*sqrt(cc[1,1])
  me1 <- tt*s*sqrt(cc[2,2])

  l0 <- betahat[1] - me0
  u0 <- betahat[1] + me0
  ci_beta0 <- c(l0, u0)

  l1 <- betahat[2] - me1
  u1 <- betahat[2] + me1
  ci_beta1 <- c(l1, u1)

  g <-   ggplot(data = dat, aes(x = x, y = y)) +
    geom_point(size = 3.5, color = "green") +
    stat_smooth(method = "lm", formula = y ~ x) +
    stat_poly_eq(
      formula = y ~ x,
      aes(label = paste(after_stat(eq.label), sep = "~~~~")),
      parse = TRUE,
      cex = 5,
      color = "blue") +
    labs(title = "Scatterplot with LSRL")
  print(g)

  invisible(
    list(betahat=betahat,
         ci_beta0=ci_beta0,
         ci_beta1=ci_beta1,
         X=X,
         Y=Y,
         ssq=ssq
    )
  )
}
