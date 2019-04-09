#' Plot Harm Rate Bounds
#'
#' Present a plot for the bounds of harm rate from the treatment to the outcome.
#' @param Tr Treatment variable, a vector taking value 1 or 0.
#' @param S Surrogate variable, a vector taking value 1 or 0.
#' @param Y Outcome variable, a vector taking value 1 or 0.
#' @param c3 0<=c3<=1, representing the upper bound for the propotion of the individuals that do not conform to causal necessity.
#' @return Lower and upper bound of the harm rate from the treatment to the outcome, and whether the individual surrogate paradox manifests.
#' @details This function plots the lower and upper sharp bounds of harm rate from the treatment to the surrogate when c1 varies.
#' @export
#' @examples
#' plot_bound(Tr = ACCORD_eye[, 1], S = ACCORD_eye[, 2], Y = ACCORD_eye[, 3], c3 = 0)
#' @references Ma, L., Yin, Y., Liu, L. and Geng, Z. On the Individual Surrogate Paradox. Submitted

plot_bound <- function(Tr, S, Y, c3 = 1){
  c1 <- seq(0, 1, 0.001)
  LB <- numeric(1001)
  UB <- numeric(1001)
  for (i in 1:1001) {
    temp <- HRbound(Tr, S, Y, c1[i], c3)
    LB[i] <- temp$LowerBound
    UB[i] <- temp$UpperBound
  }
  plot(c1, UB, type = "l", col = "blue", ylab = "Sharp bounds", ylim = c(0, 1.75 * max(UB)))
  lines(c1, LB, col = "red")
  legend("topright", legend = c("UB", "LB"), col = c("blue", "red"), lty = c(1, 1))
}


