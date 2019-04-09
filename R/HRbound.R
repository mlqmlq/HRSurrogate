#' Harm Rate Bounds
#'
#' Calculate the bounds of harm rate from the treatment to the outcome.
#' @param Tr Treatment variable, a vector taking value 1 or 0.
#' @param S Surrogate variable, a vector taking value 1 or 0.
#' @param Y Outcome variable, a vector taking value 1 or 0.
#' @param c1 0<=c1<=1, representing the harm rate from the treatment to the surrogate.
#' @param c3 0<=c3<=1, representing the upper bound for the propotion of the individuals that do not conform to causal necessity.
#' @return Lower and upper bound of the harm rate from the treatment to the outcome.
#' @details This function calculates the lower and upper sharp bounds of harm rate from the treatment to the surrogate, using the method proposed in Ma et al.
#' @export
#' @examples
#' HRbound(Tr = ACCORD_eye[, 1], S = ACCORD_eye[, 2], Y = ACCORD_eye[, 3])
#' @references Ma, L., Yin, Y., Liu, L. and Geng, Z. On the Individual Surrogate Paradox. Submitted

HRbound <- function(Tr, S, Y, c1 = 0, c3 = 1){
  if(length(Tr) != length(S) | length(Tr) != length(Y) | length(Y) != length(S))
    return("The length of the dataset is incompatible.")
  if(sum(is.na(Tr)) + sum(is.na(S)) + sum(is.na(Y)) > 0)
    return("Missingness is not allowed.")
  if(sum(Tr == 1) + sum(Tr == 0) < length(Tr) | sum(S == 1) + sum(S == 0) < length(Tr) | sum(Y == 1) + sum(Y == 0) < length(Tr))
    return("Please input numeric vectors containing only 0 and 1.")
  T_1 <- which(Tr == 1)
  T_0 <- which(Tr == 0)
  S_1 <- which(S == 1)
  S_0 <- which(S == 0)
  Y_1 <- which(Y == 1)
  Y_0 <- which(Y == 0)
  total = length(Y)
  P_Y_1 <- length(Y_1) / total
  P_Y_0 <- 1-P_Y_1
  P_T_1 <- length(T_1) / total
  P_T_0 <- 1-P_T_1
  P_S_1 <- length(S_1) / total
  P_S_0 <- 1-P_S_1
  #-----------------------------------------P(Y=1,S=0|T=0)--------------------------------------
  SY_01<-S_0[which(S_0 %in% Y_1)]
  TSY_001<-SY_01[which(SY_01 %in% T_0)]
  P_TSY_001<-length(TSY_001)/total
  P_SY_T_010<-P_TSY_001/P_T_0
  #-----------------------------------------P(Y=0,S=0|T=0)---------------------------------------
  #First we calculate P(S=0|T=0)
  TS_00<-S_0[which(S_0 %in% T_0)]
  P_TS_00<-length(TS_00)/total
  P_S_T_00<-P_TS_00/P_T_0
  P_SY_T_000<-P_S_T_00-P_SY_T_010
  #-----------------------------------------P(Y=0,S=1|T=0)---------------------------------------
  TY_01<-Y_1[which(Y_1 %in% T_0)]
  P_TY_01<-length(TY_01)/total
  P_TY_00<-P_T_0-P_TY_01
  P_Y_T_00<-P_TY_00/P_T_0
  P_SY_T_100<-P_Y_T_00-P_SY_T_000
  #-----------------------------------------P(Y=1,S=0|T=1)--------------------------------------
  P_SY_01<-length(SY_01)/total
  P_TSY_101<-P_SY_01-P_TSY_001
  P_SY_T_011<-P_TSY_101/P_T_1
  #-----------------------------------------P(Y=0,S=0|T=1)--------------------------------------
  P_S_T_01<-(P_S_0-P_TS_00)/P_T_1
  P_SY_T_001<-P_S_T_01-P_SY_T_011
  #-----------------------------------------P(Y=0,S=1|T=1)--------------------------------------
  P_Y_T_01<-(P_Y_0-P_TY_00)/P_T_1
  P_SY_T_101<-P_Y_T_01-P_SY_T_001
  #Lower bound:
  L1<-0
  L2<-P_SY_T_101 + P_SY_T_001 + P_SY_T_011 - P_SY_T_100 - P_SY_T_000 - P_SY_T_010 - c1
  L3<-P_Y_T_01 - P_Y_T_00
  L4<-P_SY_T_001 - P_SY_T_000 - c1
  LB<-max(L1,L2,L3,L4)
  #upper bound:
  f1 <- c(P_SY_T_000, P_SY_T_100, P_SY_T_010, P_SY_T_001, P_SY_T_101, P_SY_T_011, 1, c1, c3)
  e <- c(0,1,0,0,-1,0,0,-1,-1,1,1,1,0,-1,0,-1,0,-1,0,0,0,-1,-1,0,0,0,0,0,0.5,-0.5,0,-0.5,0.5,0,-1,-0.5,1  , 1,    0,    0  ,  0  ,  0,   -1,  0    ,0,0.5 ,  1,  0.5, -0.5  , -1, -0.5 ,-0.5,  0 ,-0.5,0,   0 ,  -1 ,   0 ,   0 ,   1 ,   0 ,-1   ,-1,0 ,  0  , -1  , -1,    0  ,  0  ,  0  ,0  , -1,0   ,1,    0,   -1,   -1,   -1,    0,  0,   -1,0 ,0.5, -0.5,   -1, -0.5, -0.5 ,   0  ,0, -0.5,1, 0.5  ,0.5,    0, -0.5  ,0.5,   -1  ,0 ,-0.5,0,   0 ,  -1,    0 ,  -1,    0 ,   0 ,-1 ,   0,1   ,0,    0,    0,    0  ,  1  , -1,  0  , -1,0  , 1,    0 ,   0 ,   0 ,   1 ,  -1, -1,    0,0.5,   0 ,-0.5, -0.5,    0,  0.5, -0.5  ,0 ,-0.5)
  d <- matrix(e,nrow = 15,byrow = TRUE)
  d <- -d
  UB <- min(d %*% f1)
  UB <- max(LB, UB)
  rt <- list(LB, UB)
  names(rt) <- c("LowerBound", "UpperBound")
  return(rt)
}


