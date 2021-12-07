library(dplyr)
two_games = read.csv("BW_2021_pbp_with_fielders.csv",h=T)
#OCT31,2021 VERSION 2: ADDED MORE BASES (3RD BASE)
#NOV13TH,2021 VERSION 3: FINISHED STEP 2, CHANGE VARIABLE NAME, CLEANED COMMENTS
#Goal: Figure out whether the first batter got out
#Need: outs, outs_end,runs_scored -> some way to identify half-inning
# -> gameID,batting_team,inning, or pitching_team



#lets pick one to do at a time to figure out whether the first batter got out

#STEP 1: Figure out whether 1st batter get out
#if the first batter is out, doesn't that mean there's nobody
#on second base on the first inning? It means there nobody on any of the bases...

###NOTE: this is ultimately for the Markov Chain Model, not really useful for dataviz
#SHould have 8
BW1st <- two_games %>% mutate(
  runner_1st = !is.na(On1B), # runner on 1st = TRUE, 1st base empty = FALSE
  runner_2nd = !is.na(On2B),  # runner on 2nd = TRUE, 2nd base empty = FALSE
  runner_3rd = !is.na(On3B), # runner on 3rd = TRUE, 3rd base empty = FALSE
  first_runners = case_when( #statement on left want to evaluate if T or F
  # first runner is safe if theyre on the second base after first inning
    #Need a statement for when runner_1st but not runner_2nd -> return a 1 (for someone on base 1)
    !runner_1st & !runner_2nd & !runner_3rd ~ "0",#no runner on 1st and 2nd and 3rd
    runner_1st &!runner_2nd & !runner_3rd ~ "1", #runner on 1st, but not on 2nd and 3rd
    !runner_1st & runner_2nd & !runner_3rd ~"2", #runner on 2nd, but not on 1st and 3rd
    !runner_1st & !runner_2nd & runner_3rd ~"3", #runner on 3rd, but not 1st and 2nd
    runner_1st & runner_2nd & !runner_3rd ~ "12",#runner on 1st and 2nd, but not 3rd
    runner_1st & !runner_2nd & runner_3rd ~ "13",#runner on 1st and 3rd, but not 2nd
    !runner_1st & runner_2nd & runner_3rd ~ "23",#runner on 2nd and 3rd, but not 1st
    runner_1st & runner_2nd & runner_3rd ~ "123" #runner on 1st and 2nd and 3rd
  )
)%>% 
  
# if outs and outs_end are equal, batter did not get out
# otherwise, there was at least one out on the play
mutate(batter_out = if_else(outs == outs_end, "N", "Y"))


#VANESSA Fix this
#Get rid of all the plays that don't change the batter
BW1st <- BW1st %>% filter(!(outcome %in% c("Substitution")))

summarise(BW1st)

#mutate() to add variable indicating if batter get out
#group_by to get each half inning
#summarize() did the first batter get out?

#NEXT WEEK:
#Use lead function where the next play starts
#Use lag



#Step 2: Figure out how many runs were scored in that half inning
#group_by some step in step 2 figure out how many runs were scored in the half inning
BWinnings <- BW1st %>% group_by(game_id,batting_team,inning,pitching_team)%>%summarise(
  away_score_begin = first(away_score),
  home_score_begin = first(home_score),
  away_end = last(away_score_end),
  home_end = last(home_score_end),
  total_runs = sum(runs_scored),
  first_batter_out = first(batter_out)
)

View(BWinnings)
#Things to fix:
# 1. Fix the CSUN_END DOUBLE VARIABLE TO LOOK LIKE AWAY_SCORE AND HOME_SCORE_END bc its not always CSUN
# 2. Figure out how to get the total number of runs for away and home teams
# 3. Figure out how to get whether the first batter was out or not


#Step 3: Regroup_by if batter was out and summarize number of run scored
#plot the distribution of run scored, etc


## Example plotting and summarizing commands
library(ggplot2)
ggplot(BWinnings, aes(x = total_runs)) + geom_histogram(center = 0, binwidth = 1) + facet_grid(first_batter_out~pitching_team)

BWinnings %>% group_by(batting_team, first_batter_out) %>% summarise(
  avg.runs.scored = mean(total_runs),
  sd.runs.scored = sd(total_runs),
  median.runs.scored = median(total_runs)
)

ggplot(BWinnings, aes(x = total_runs)) +geom_histogram(center = 0, binwidth = 1) + facet_grid(first_batter_out~batting_team)
ggplot(BWinnings, aes(x = total_runs)) +geom_histogram(center = 0, binwidth = 1)

ggplot(BWinnings, aes(x = total_runs)) +geom_histogram(center = 0, binwidth = 1) + facet_grid(.~first_batter_out)

CSUF_pitching <- BWinnings %>% filter(pitching_team == "Cal St. Fullerton")
CSUF_batting <- BWinnings %>% filter(batting_team == "Cal St. Fullerton")

ggplot(CSUF_pitching, aes(x = total_runs)) +geom_histogram(center = 0, binwidth = 1) + facet_grid(first_batter_out~.)

xtabs(~first_batter_out + total_runs, data = BWinnings)  ## for all teams in BW
xtabs(~first_batter_out + total_runs + pitching_team, data = BWinnings)

ggplot(BWinnings, aes(x = total_runs)) +geom_histogram(center = 0, binwidth = 1) + facet_grid(first_batter_out~pitching_team)
