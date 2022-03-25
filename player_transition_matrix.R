player_transition_matrix <- function(X,Y){
  # computes the P transition matrix for a given player
  # P is used in the Un_plus_one function
  # X: an integer - the player number to compute the matrix for
  # Y: a data frame containing the number of at-bats, hits, doubles, triples, home runs and walks for each player
  
  # Set up the P matrix
  # right now we have to keep it as a data frame - will debut to make sure it works with just matrix
  df <- data.frame(matrix(data = 0L, ncol = 25, nrow = 25))
  
  # Filter the data frame to get just the player number
  Y2 <- Y %>% filter(NO. == X)
  
  # Convert everything to integers and replace - with 0
  Y2[Y2 == "-"] <- "0"
  Y2$HR <- as.integer(Y2$HR)
  Y2$AB <- as.integer(Y2$AB)
  Y2$H <- as.integer(Y2$H)
  Y2$"2B" <- as.integer(Y2$"2B")
  Y2$"3B" <- as.integer(Y2$"3B")
  Y2$BB <- as.integer(Y2$BB)
  
  # Check to make sure at-bats is positive
  # There's a better way to do this, but we'll just hard-code the stop for now
  if(Y2$AB <= 0){
    stop("Player has no at-bats")
  }
  
  # compute the probabilities of each event
  PA <- Y2$AB + Y2$BB # number of plate appearance including walks
  p_HR <- Y2$HR/PA # probability of HR on plate appearance
  p_SO <- ((Y2$H-(Y2$"2B"+Y2$"3B"+Y2$HR))/PA) + (Y2$BB/PA) # prob of single OR walk
  p_1B <- ((Y2$H-(Y2$"2B"+Y2$"3B"+Y2$HR))/PA) # prob of single
  p_2B <- Y2$"2B"/PA # prob of double
  p_3B <- Y2$"3B"/PA # prob of triple
  p_BB <- Y2$BB/PA # prob of walk
  P_OUT <- 1 - p_SO - p_2B - p_3B - p_HR # prob of out
  
  # end of inning state
  df[25,25] <- 1
  
  # A matrix - plays that do not increase the outs
  df[1:8,1] <- p_HR
  df[1,2] <- df[2,5] <- p_SO
  df[3,2] <- df[4,2] <- df[7,2] <- df[5,5] <- df[6,5] <- df[8,5] <- p_1B
  df[1,3] <- df[3,3] <- df[4,3] <- df[7,3] <- df[2,7] <- df[5,7] <- df[6,7] <- df[8,7] <- p_2B
  df[1:8,4] <- p_3B
  df[3,5] <- df[4,6] <- df[5:8,8] <- p_BB
  
  # A matrix again
  df[9:16,9] <- p_HR
  df[9,10] <- df[10,13] <- p_SO
  df[11,10] <- df[12,10] <- df[15,10] <- df[13,13] <- df[14,13] <- df[16,13] <- p_1B
  df[9,11] <- df[11,11] <- df[12,11] <- df[15,11] <- df[10,15] <- df[13,15] <- df[14,15] <- df[16,15] <- p_2B
  df[9:16,12] <- p_3B
  df[11,13] <- df[12,14] <- df[13:16,16] <- p_BB
  
  # A matrix again
  df[17:24,17] <- p_HR
  df[17,18] <- df[18,21] <- p_SO
  df[19,18] <- df[20,18] <- df[23,18] <- df[21,21] <- df[22,21] <- df[24,21] <- p_1B
  df[17,19] <- df[19,19] <- df[20,19] <- df[23,19] <- df[18,23] <- df[21,23] <- df[22,23] <- df[24,23] <- p_2B
  df[17:24,20] <- p_3B
  df[19,21] <- df[20,22] <- df[21:24,24] <- p_BB
  
  # B matrix
  df[1,9] <- df[2,10] <- df[3,11] <- df[4,12] <- df[5,13] <- df[6,14] <- df[7,15] <- df[8,16] <- df[9,17] <- df[10,18] <- df[11,19] <- df[12,20] <- df[13,21] <- df[14,22] <- df[15,23] <- df[16,24] <- P_OUT
  
  # F matrix
  df[17:24,25] <- P_OUT
  
  # output the final matrix
  df
  
}
