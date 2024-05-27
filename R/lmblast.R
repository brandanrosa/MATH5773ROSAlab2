#' lmblast
#'
#' This function is exploding with useful regression data produced from the `lm()` function
#'
#' @param obj use `lm()` to create the obj
#' @param alpha the alpha level
#'
#' @return a named list containing the following: R-Squared, a matrix of the residuals, the design and response matrices, the AIC, sample variance, and a confidence interval for Beta's.
#' @export
#'
#' @examples \dontrun{lmblast}
lmblast <- function(obj, alpha = 0.05) { # obj is an lm()

  # Summary of lm()
  sm <- summary(obj)

  # R-Squared
  rsq <- sm$r.squared

  # Residuals
  resid <- as.matrix(sm$residuals)
  colnames(resid) <- "Residuals"

  # Design Mat (X)
  X <- cbind(1, as.matrix(obj$model[,1]))

  # Response Mat (Y)
  Y <- as.matrix(obj$model[,2])

  # AIC (Akaike Info Criterion)
  aic <- stats::extractAIC(obj)

  # S-Squared (point est for pop var)
  s <- sm$sigma
  ssq <- s^2

  # 100(1 - alpha)% ci for Beta Vector
  ci <- stats::confint(obj, level = 1-alpha)
  ci

  # List
  list(R.Squared=rsq, Residuals=round(resid,4), Design.Mat.X=X, Response.Mat.Y=Y, AIC=aic, S.Squared=ssq, Conf.Int=ci)
}
