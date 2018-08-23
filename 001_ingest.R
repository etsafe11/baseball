#-------------------------- 
# This script produces play-by-play data for a particular year and team. Later we want it to ingest data for
# all teams and all years iteratively.
# Download this folder: https://www.retrosheet.org/events/2017eve.zip
# in command prompt, navigate to 2017eve folder you downloaded using "cd" command etc, and type:): 
# C:\Users\ethompson\Desktop\baseball\2017eve>bevent -y 2017 2017ANA.eva >2017.txt
# for example, for 2017 LAA play-by-play data
# then you can run this script
#-------------------------- 


# ingest data - currently only ingesting 2017 LA Angels home games
ANA2017 <- as.data.frame(read.csv("2017eve/2017ANA.txt", stringsAsFactors=TRUE, header = FALSE))
header <- as.data.frame(read.csv("2017eve/header.csv", stringsAsFactors=FALSE, header = FALSE))

# add header
names(ANA2017) <- header[,1]

# create runs variable which is sum of home and visitor teams' scores at each plate appearance

ANA2017$runs <- with(ANA2017, vis_score + home_score)

# create key for each half-inning of the season
ANA2017$half_inning <- with(ANA2017, paste(game_id, inning, batting_team))

# total runs scored during half-inning

ANA2017$runs_scored <- with(ANA2017, (batter_dest > 3) + 
                              (runner_on_1st_dest > 3) + 
                              (runner_on_2nd_dest > 3) + 
                              (runner_on_3rd_dest > 3))

runs_scored_inning <- aggregate(ANA2017$runs_scored, list(half_inning = ANA2017$half_inning), sum)

# total runs scored at start of inning
runs_scored_start <- aggregate(ANA2017$runs, list(half_inning = ANA2017$half_inning), "[", 1)

# create max of runs in the half inning (starting runs plus runs scored)
max <- data.frame(half_inning = runs_scored_start$half_inning)

max$x <- runs_scored_inning$x + runs_scored_start$x

ANA2017 <- merge(ANA2017, max)

n <- ncol(ANA2017)

names(ANA2017)[n] <- "max_runs"

# runs scored in remainder of inning
ANA2017$runs_roi <- with(ANA2017, max_runs - runs)

# binary variable to indicate whether base is occupied or not
runner1 <- ifelse(as.character(ANA2017[ , "first_runner"]) == "", 0, 1)
runner2 <- ifelse(as.character(ANA2017[ , "second_runner"]) == "", 0, 1)
runner3 <- ifelse(as.character(ANA2017[ , "third_runner"]) == "", 0, 1)

# get.state() function creates something like 010 to indicate a runner on second base only or 111 for bases loaded
get.state <- function(runner1, runner2, runner3, outs){
  runners <- paste(runner1, runner2, runner3, sep = "")
  paste(runners, outs)
}

ANA2017$state <- get.state(runner1, runner2, runner3, ANA2017$outs)

# number of runners on base and number of outs after the play
n_runner1 <- with(ANA2017, as.numeric(runner_on_1st_dest == 1 | batter_dest== 1))
n_runner2 <- with(ANA2017, as.numeric(runner_on_1st_dest == 2 | runner_on_2nd_dest == 2 | batter_dest== 2))
n_runner3 <- with(ANA2017, as.numeric(runner_on_1st_dest == 3 | runner_on_2nd_dest == 3 | runner_on_3rd_dest == 3 | batter_dest== 3))
n_outs <- with(ANA2017, outs + outs_on_play)
ANA2017$new_state <- get.state(n_runner1, n_runner2, n_runner3, n_outs)

# filter for plays where the base-out state changed or runs scored on the play
ANA2017 <- subset(ANA2017, (state != new_state) | (runs_scored > 0))

library(plyr)

# filter for full innings only (not walk-offs etc) - introduces bias

data_outs <- ddply(ANA2017, .(half_inning), summarise, outs_inning = sum(outs_on_play))

ANA2017 <- merge(ANA2017, data_outs)

ANA2017c <- subset(ANA2017, outs_inning == 3)

ANA2017c <- subset(ANA2017c, batter_event_flag = TRUE)

library(car)

ANA2017c$new_state <- recode(ANA2017c$new_state, 
                             "c('000 3', '100 3', '010 3', '001 3',
                              '110 3', '101 3', '011 3', '111 3') = '3'")

# expected runs scored in the remainder of the inning (runs expectancy)
runs <- with(ANA2017c, aggregate(runs_roi, list(state), mean))

# display in a matrix, but first sort runs dataframe by number of outs 
runs$outs <- substr(runs$Group.1, 5, 5)

runs <- runs[order(runs$outs), ]

runs_out <- matrix(round(runs$x, 2), 8, 3)

# dimnames for run expectancy matrix
dimnames(runs_out)[[2]] <- c("0 outs", "1 out", "2 outs")

dimnames(runs_out)[[1]] <- c("000", "001", "010", "011", "100", "101", "110", "111")

# print run expectancy matrix
runs_out

# go back to dply (instead of plyr) to be safe

library(dplyr)

# compute transition probabilities
T.matrix <- with(ANA2017c, table(state, new_state))

P.matrix <- prop.table(T.matrix, 1)

P.matrix <- rbind(P.matrix, c(rep(0, 24), 1))

# (not required) examples of transition probabilities:
# the probability of moving from the "000 0" state to the 
# "000 0" state is .04. In other words, the probability
# of a HR was 4%.
P1 <- round(P.matrix["000 0", ], 3)
data.frame(Prob = P1[P1 > 0])
# probability of inning ending is 60.9% when you have runner 
# on second with 2 outs.
P2 <- round(P.matrix["010 2", ], 3)
data.frame(Prob = P2[P2 > 0])

# simulating the Markov chain. idea is that
# runs scored on play equals:
# (state B runners on base + state B outs + 1) - (state A runners on base + state A outs)
count_runners_outs <- function(s)
  sum(as.numeric(strsplit(s, "")[[1]]), na.rm = TRUE)

runners_outs <- sapply(dimnames(T.matrix)[[1]], count_runners_outs)[c(-25, -26, -27, -28, -29, -30)]

R <- outer(runners_outs + 1, runners_outs, FUN = "-")
dimnames(R)[[1]] <- dimnames(T.matrix)[[1]][-25]
dimnames(R)[[2]] <- dimnames(T.matrix)[[1]][-25]
R <- cbind(R, rep(0, 24))

simulate_half_inning <- function(P, R, start = 1){
  s <- start; path <- NULL; runs <- 0
  while(s < 25){
    s.new <- sample(1:25, 1, prob = P[s, ])
    path <- c(path, s.new)
    runs <- runs + R[s, s.new]
    s <- s.new
  }
  runs
}

set.seed(555)
# simulate from 000 0
runs_simulation <- replicate(500, simulate_half_inning(T.matrix, R, start = 1))

# 6.4% of innings are >= 5 runs
sum(runs_simulation[runs_simulation >= 5]) / 500

# simulate half-innings with different starting states
runs_j <- function(j){
  mean(replicate(500, simulate_half_inning(T.matrix, R, j)))
}

runs_expectancy <- sapply(1:24, runs_j)
runs_expectancy <- t(round(matrix(runs_expectancy, 3, 8), 2))
dimnames(runs_expectancy)[[2]] <- c("0 outs", "1 out", "2 outs")

dimnames(runs_expectancy)[[1]] <- c("000", "001", "010", "011", "100", "101", "110", "111")

# runs expectancy matrix after Markov simulation is similar
# to runs expectancy matrix built from historical data
runs_expectancy






