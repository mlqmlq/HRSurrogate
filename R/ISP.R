#' Individual Surrogate Paradox
#'
#' Check whether individual surrogate paradox manifests by calculating the bound of harm rate from the treatment to the outcome.
#' @param Tr Treatment variable, a vector taking value 1 or 0.
#' @param S Surrogate variable, a vector taking value 1 or 0.
#' @param Y Outcome variable, a vector taking value 1 or 0.
#' @param c1 0<=c1<=1, representing the harm rate from the treatment to the surrogate.
#' @param c3 0<=c3<=1, representing the upper bound for the propotion of the individuals that do not conform to causal necessity.
#' @return Lower and upper bound of the harm rate from the treatment to the outcome, and whether the individual surrogate paradox manifests.
#' @details This function calculates the sharp bounds of harm rate from the treatment to the surrogate, and determine whether individual surrogate paradox exists using the method proposed in Ma et al.
#' @export
#' @examples
#' ISP(Tr = ACCORD_eye[, 1], S = ACCORD_eye[, 2], Y = ACCORD_eye[, 3], c1 = 0.5)
#' @references Ma, L., Yin, Y., Liu, L. and Geng, Z. On the Individual Surrogate Paradox. Submitted

ISP <- function(Tr, S, Y, c1, c3 = 1){
  if(length(Tr) != length(S) | length(Tr) != length(Y) | length(Y) != length(S))
    return("The length of the dataset is incompatible.")
  if(sum(is.na(Tr)) + sum(is.na(S)) + sum(is.na(Y)) > 0)
    return("Missingness is not allowed.")
  if(sum(Tr == 1) + sum(Tr == 0) < length(Tr) | sum(S == 1) + sum(S == 0) < length(Tr) | sum(Y == 1) + sum(Y == 0) < length(Tr))
    return("Please input numeric vectors containing only 0 and 1.")
  temp <- HRbound(Tr, S, Y, c1, c3)
  if(temp$LowerBound > c1){
    print("Individual surrogate paradox manifests.")
    return(temp)
  }
  if(temp$UpperBound < c1){
    print("Individual surrogate paradox is absent.")
    return(temp)
  }
  print("Whether individual surrogate paradox manifests can not be determined.")
  temp
}
