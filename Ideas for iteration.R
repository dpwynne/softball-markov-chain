

Simulated_inning <- function(Y,P1,P2,P3,P4,P5,P6,P7,P8,P9){
# Function to calculate different player's U matrices.
# Y: a data frame containing the number of at-bats, hits, doubles, triples, home runs and walks for each player.
# P1-P9: integers - the player number to compute the U matrix for.

U1 <- Un_plus_one(U0,player_transition_matrix(P1,Y),R_matrix)
U2 <- Un_plus_one(U1,player_transition_matrix(P2,Y),R_matrix)
U3 <- Un_plus_one(U2,player_transition_matrix(P3,Y),R_matrix)
U4 <- Un_plus_one(U3,player_transition_matrix(P4,Y),R_matrix)
U5 <- Un_plus_one(U4,player_transition_matrix(P5,Y),R_matrix)
U6 <- Un_plus_one(U5,player_transition_matrix(P6,Y),R_matrix)
U7 <- Un_plus_one(U6,player_transition_matrix(P7,Y),R_matrix)
U8 <- Un_plus_one(U7,player_transition_matrix(P8,Y),R_matrix)
U9 <- Un_plus_one(U8,player_transition_matrix(P9,Y),R_matrix)
U10 <- Un_plus_one(U9,player_transition_matrix(P1,Y),R_matrix)
U11 <- Un_plus_one(U10,player_transition_matrix(P2,Y),R_matrix)
U12 <- Un_plus_one(U11,player_transition_matrix(P3,Y),R_matrix)
U13 <- Un_plus_one(U12,player_transition_matrix(P4,Y),R_matrix)
U14 <- Un_plus_one(U13,player_transition_matrix(P5,Y),R_matrix)
U15 <- Un_plus_one(U14,player_transition_matrix(P6,Y),R_matrix)
U16 <- Un_plus_one(U15,player_transition_matrix(P7,Y),R_matrix)
U17 <- Un_plus_one(U16,player_transition_matrix(P8,Y),R_matrix)
U18 <- Un_plus_one(U17,player_transition_matrix(P9,Y),R_matrix)
U19 <- Un_plus_one(U18,player_transition_matrix(P1,Y),R_matrix)
U20 <- Un_plus_one(U19,player_transition_matrix(P2,Y),R_matrix)


# if function to stop iteration when sum of 25th column is 1.
if(sum(U1[,25]) > 0.999){
return(U1[,25])
stop
}
if(sum(U2[,25]) > 0.999){
return(U2[,25])
stop
}
if(sum(U3[,25]) > 0.999){
return(U3[,25])
stop
}
if(sum(U4[,25]) > 0.999){
return(U4[,25])
stop
}
if(sum(U5[,25]) > 0.999){
return(U5[,25])
stop
}
if(sum(U6[,25]) > 0.999){
return(U6[,25])
stop
}
if(sum(U7[,25]) > 0.999){
return(U7[,25])
stop
}
if(sum(U8[,25]) > 0.999){
return(U8[,25])
stop
}
if(sum(U9[,25]) > 0.999){
return(U9[,25])
stop
}
if(sum(U10[,25]) > 0.999){
return(U10[,25])
stop
}
if(sum(U11[,25]) > 0.999){
return(U11[,25])
stop
}
if(sum(U12[,25]) > 0.999){
return(U12[,25])
stop
}
if(sum(U13[,25]) > 0.999){
return(U13[,25])
stop
}
if(sum(U14[,25]) > 0.999){
return(U14[,25])
stop
}
if(sum(U15[,25]) > 0.999){
return(U15[,25])
stop
}
if(sum(U16[,25]) > 0.999){
return(U16[,25])
stop
}
if(sum(U17[,25]) > 0.999){
return(U17[,25])
stop
}
if(sum(U18[,25]) > 0.999){
return(U18[,25])
stop
}
if(sum(U19[,25]) > 0.999){
return(U19[,25])
stop
}
if(sum(U20[,25]) > 0.999){
return(U20[,25])
stop
}
}

# potential idea for the iteration loop.


Simulated_inning <- function(Y,P1,P2,P3,P4,P5,P6,P7,P8,P9){
# Function to calculate different player's U matrices.
# Y: a data frame containing the number of at-bats, hits, doubles, triples, home runs and walks for each player.
# P1-P9: integers - the player number to compute the U matrix for.

# while loop to reiterate the function until the 25th column = 1.
  
for(j in 1:9){
  
  while (sum(Uj[,25]) < 0.999) {
  
Uj <- Un_plus_one(Uj-1,player_transition_matrix(Pj,Y),R_matrix)

# player_transition_matrix: computes P transition matrix for given player.
# R_matrix: The 25 x 25 "runs" matrix indicating how many runs scored on a transition


}
}
}