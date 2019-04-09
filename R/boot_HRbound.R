#' Bootstrap Harm Rate Bound
#'
#' Calculate bootstrap confidence interval for the bound of harm rate from the treatment to the outcome.
#' @param Tr Treatment variable, a vector taking value 1 or 0.
#' @param S Surrogate variable, a vector taking value 1 or 0.
#' @param Y Outcome variable, a vector taking value 1 or 0.
#' @param c1 0<=c1<=1, representing the harm rate from the treatment to the surrogate.
#' @param c3 0<=c3<=1, representing the upper bound for the propotion of the individuals that do not conform to causal necessity.
#' @param nboot Number of bootstrap samples. A positive integer.
#' @param conf.level Confidence level of the interval.
#' @return Calculate confidence interval for the bound of the hard rate from the treatment to the outcome.
#' @export
#' @details This function computes the bootstrap confidence interval for the sharp bound of harm rate from the treatment to the surrogate.
#' @examples
#' boot_HRbound(Tr = ACCORD_eye[, 1], S = ACCORD_eye[, 2], Y = ACCORD_eye[, 3], n = 1000)

boot_HRbound <- function(Tr, S, Y, c1 = 0, c3 = 1, nboot, conf.level = 0.95){
  boot.LB <- numeric(nboot)
  boot.UB <- numeric(nboot)
  for (i in 1:nboot) {
    index <- sample(length(Tr), replace = T)
    Tr_temp <- Tr[index]
    S_temp <- S[index]
    Y_temp <- Y[index]
    temp <- HRbound(Tr_temp, S_temp, Y_temp, c1, c3)
    boot.LB[i] <- temp$LowerBound
    boot.UB[i] <- temp$UpperBound
  }
  c(quantile(boot.LB, (1 - conf.level)/2), quantile(boot.UB, 1 - (1 - conf.level)/2))
}

