

Simulated_inning <- function(Y,P1,P2,P3,P4,P5,P6,P7,P8,P9){
# Function to calculate different player's U matrices.
# Y: a data frame containing the number of at-bats, hits, doubles, triples, home runs and walks for each player.
# P1-P9: integers - the player number to compute the U matrix for.


  # Create a 25 x 25 x 9 array for the player transition matrices
  # Check here to make sure that each 3rd dimension (25 x 25 matrix) has the right numbers
P <- array(c(
  player_transition_matrix(P1, Y),
  player_transition_matrix(P2, Y),
  player_transition_matrix(P3, Y),
  player_transition_matrix(P4, Y),
  player_transition_matrix(P5, Y),
  player_transition_matrix(P6, Y),
  player_transition_matrix(P7, Y),
  player_transition_matrix(P8, Y),
  player_transition_matrix(P9, Y)
  ),
  dim = c(25, 25, 9))  


# flip between U0 and U1 - U0 = Un and U1 = UnPlusOne
U0 <- matrix(data = 0L, ncol = 25, nrow = 21)
U1 <- U0
U0[1,1] <- 1

# for iteration
i <- 0

# while loop to reiterate the function until the 25th column = 1.
# while Un after batter has less than 99.9% chance of being end of inning 
# U0 is the inning state at start of the at-bat
# U1 is the inning state at the end of the at-bat
while(sum(U1[,25]) < 0.999){
  player_num <- (i %% 9) + 1 # get the next player in the lineup, when i = 0 it's P1
  U1 <- Un_plus_one(U0, P[,,player_num], R_matrix) # get the updated Un_plus_one
  U0 <- U1 # set the new Un_plus_one to the new Un
  i <- i+1 # increment i
}

# R_matrix: The 25 x 25 "runs" matrix indicating how many runs scored on a transition

return(U1[,25])

}