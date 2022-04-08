
# RA: the matrix of runs scored on a transition with no outs
RA <- matrix(1:64, nrow = 8, ncol = 8)
RA <- cbind(c(1,2,2,2,3,3,3,4),c(0,1,1,1,2,2,2,3),c(0,1,1,1,2,2,2,3),c(0,1,1,1,2,2,2,3),c(0,0,0,0,1,1,1,2),c(0,0,0,0,1,1,1,2),c(0,0,0,0,1,1,1,2),c(0,0,0,0,0,0,0,1))

# RB: the matrix of runs scored on a transition resulting in one more out
RB <- matrix(1:64, nrow = 8, ncol = 8)
RB <- cbind(c(0,1,1,1,2,2,2,3),c(0,0,0,0,1,1,1,2),c(0,0,0,0,1,1,1,2),c(0,0,0,0,1,1,1,2),c(0,0,0,0,0,0,0,1),c(0,0,0,0,0,0,0,1),c(0,0,0,0,0,0,0,1),c(0,0,0,0,0,0,0,0))

# R0: a matrix of 0 runs scored
R0 <- matrix(0, nrow = 8, ncol = 8)

# Rout - matrix of inning end, no runs scored
Rout <- matrix(0, nrow = 8, ncol = 1)

# 25 x 25 R matrix - hard-coded based on number of runs scored in each transition
R_matrix <- rbind(
  cbind(RA, RB, R0, Rout),
  cbind(R0, RA, RB, Rout),
  cbind(R0, R0, RA, Rout),
  matrix(0, nrow = 1, ncol = 25)
)
