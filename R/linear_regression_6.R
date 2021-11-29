## Linear Regression

# defining the cost function
minimize = function(y, X, beta0) {
  R <- sum((y - X%*%beta0)^2)
}

#' @title Linear Regression
#'
#' @description Estimates a vector for the model coefficients. It also computes model evaluation metrics.
#' @param y A \code{vector} of length \code{n} that represents the response variable of interest that we would like to explain/predict.
#' @param X A \code{matrix} of dimension \code{n x p} that represents the independent (explanatory) variables.
#' @param alpha A \code{numeric} (double) used to denote the significance level.
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{beta}{Estimated coefficient vector}
#'      \item{sigma2}{Estimated variance}
#'      \item{variance_beta}{Estimated variance for each beta}
#'      \item{r_squared}{Coefficient of determination}
#'      \item{mallow}{Mallow's \code{C_p}}
#'      \item{f_stat}{F-statistic}
#'      \item{p_value}{Determined by F-statistic}
#'      \item{ci}{Interval of values based on \code{alpha}}
#'      \item{res}{Model Residuals}
#'      \item{preds}{Model Predictions}
#' }
#' @author Clayton Haley
#' @author Ukamaka Victoria Nnyaba
#' @author Robert Fett
#' @importFrom stats optim
#' @export
#' @examples
#' x <- matrix(rnorm(400), ncol = 4) # so this is a 100x4 predictor matrix
#' y <- rnorm(100)
#' model <- our_lm(y, x, alpha = 0.05)
#' model
our_lm = function(y, X, alpha = 0.05) {

  # Make sure data formats are appropriate
  y <- as.vector(y)
  covariates <- as.matrix(X)

  # Define parameters
  n <- length(y)
  p <- dim(X)[2]
  df <- n - p

  beta0 <- rep(NA, ncol(X))
  beta0[1] <- mean(y)

  for (i in 2:ncol(X)){
    beta0[i] <- cov(X[,i],y)/var(X[,i])
  }

  # Estimate beta
  m <- optim(par = beta0, fn = minimize, y = y, X = X)
  beta.hat <- m$par

  # Checking fitness of the model
  y_hat  <- X%*%beta.hat
  SSE <- sum((y - y_hat)^2)
  SST <- sum((y - mean(y))^2)
  R_sq <- 1 - (SSE/SST)  #coefficient of determination

  #computing the residuals
  resid <- y - X%*%as.matrix(beta.hat)
  sigma2.hat <- (i/df) * t(resid) %*% resid
  Cp <- SSE + 2*p*sigma2.hat #Mallow's Cp

  var.beta <- drop(sigma2.hat) * solve(t(X)%*%X)

  # Confidence intervals for beta
  quant <- 1 - alpha/2
  ci.beta <- c(beta.hat - qnorm(p = quant)*sqrt(var.beta), beta.hat + qnorm(p = quant)*sqrt(var.beta))

  # F-test
  DFM <- p - 1
  DFE <- n - p
  SSM <- sum((y_hat - mean(y))^2)
  MSM <- SSM/DFM
  MSE <- SSE / DFE

  F_star <- MSM/MSE
  p <- pf(F_star, DFM, DFE, lower.tail = FALSE)

  # Return all estimated values
  return(list(beta = beta.hat, sigma2 = sigma2.hat, variance_beta = var.beta,
              r_squared = R_sq, mallow = Cp, f_stat = F_star, p_value = p,
              ci = ci.beta, res = resid, preds = y_hat))
}


#' @title Residuals vs fitted-values
#'
#' @description Creates a plot for the residuals and fitted values.
#' @param res A \code{vector} of length \code{n} that represents the residuals.
#' @param preds A \code{vector} of length \code{n} that represents the predictions.
#' @author Clayton Haley
#' @author Ukamaka Victoria Nnyaba
#' @author Robert Fett
#' @export
#' @examples
#' res_plot(model$res, model$preds)
res_plot <- function(res, preds){
  plot(preds, res, main = "Residuals vs Fitted Values",
       xlab="Fitted Values", ylab="Residuals")
  abline(0,0)
}


#' @title qq-plot of residuals
#'
#' @description Creates a qq-plot for the residuals.
#' @param res A \code{vector} of length \code{n} that represents the residuals.
#' @author Clayton Haley
#' @author Ukamaka Victoria Nnyaba
#' @author Robert Fett
#' @export
#' @examples
#' qq_plot(model$res)
qq_plot <- function(res){
  qqnorm(res)
  qqline(res)
}


#' @title Histogram of residuals
#'
#' @description Creates a histogram for the residuals.
#' @param res A \code{vector} of length \code{n} that represents the residuals.
#' @author Clayton Haley
#' @author Ukamaka Victoria Nnyaba
#' @author Robert Fett
#' @export
#' @examples
#' histogram(model$res)
histogram <- function(res){
  hist(res, main = "Histogram of Residuals", xlab = "Residuals")
}
