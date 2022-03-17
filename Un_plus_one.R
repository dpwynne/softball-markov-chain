Un_plus_one <- function(Un, P, R){
  # Un: the 21 x 25 Un matrix from the previous iteration
  # The j^th row of Un indicates that (j-1) runs have scored this inning
  # P: the 25 x 25 transition matrix for the batter
  # R: the 25 x 25 "runs" matrix indicating how many runs scored on a transition
  
  # First we subset the P matrix into P0, P1, P2, P3, P4
  # where Pk is a matrix with the entry of P if the corresponding entry of R is k and 0 otherwise
  
  # These should get the correct entries but the resulting matrices might be shaped wrong
  P0 <- P * (R == 0)
  P1 <- P * (R == 1)
  P2 <- P * (R == 2)
  P3 <- P * (R == 3)
  P4 <- P * (R == 4)
  
  # Now we create the U_(n+1) matrix
  
  Unplus <- matrix(0, nrow = 21, ncol = 25)
  
  # The first 4 rows of the matrix have only partial sums
  
  # We do indeed right-multiply by the transition matrix so this is right
  # There's a chance that dimensions drop somewhere and this doesn't work
  Unplus[1,] <- Un[1,] %*% P0
  Unplus[2,] <- Un[2,] %*% P0 + Un[1,] %*% P1
  Unplus[3,] <- Un[3,] %*% P0 + Un[2,] %*% P1 + Un[1,] %*% P2
  Unplus[4,] <- Un[4,] %*% P0 + Un[3,] %*% P1 + Un[2,] %*% P2 + Un[1,] %*% P3
  
  # The last 17 rows of the matrix should have full sums
  
  # This part should work if the previous section of code works
  for (j in 5:21){
    Unplus[j,] <- Un[j,] %*% P0 + Un[j-1, ] %*% P1 + Un[j-2, ] %*% P2 + Un[j-3, ] %*% P3 + Un[j-4, ] %*% P4
  }
  
  return(Unplus)
  
}