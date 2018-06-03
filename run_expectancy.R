

##CHAPTER 4 - THE RELATION BETWEEN RUNS AND WINS
setwd("C:/Users/ethompson/Desktop/baseball")
Teams <- read.csv("C:/Users/ethompson/Desktop/baseball/core/Teams.csv")

myteams <- subset(Teams, yearID > 2000)[ , 
                                         c("teamID", "yearID", "lgID", "G", "W", "L", "R", "RA")]

myteams$RD <- with(myteams, R-RA)
myteams$Wpct <- with(myteams, W/(W+L))

plot(myteams$RD, myteams$Wpct, xlab="run differential", ylab="winning percentage")

linfit <- lm(Wpct ~ RD, data=myteams)
linfit

abline(a=coef(linfit)[1], b=coef(linfit)[2], lwd=2)

myteams$linWpct <- predict(linfit)
myteams$linResiduals <- residuals(linfit)

plot(myteams$RD, myteams$linResiduals,
     xlab="run differential", ylab="residual")
abline(h=0, lty=3)
points(c(66,88), c(0.749, -0.0733), pch=19)
text(68, .0749, "LAA '08", pos=4, cex=.8)
text(88, -.0733, "CLE '06", pos=4, cex=.8)

mean(myteams$linResiduals)
linRMSE <- sqrt(mean(myteams$linResiduals ^2))
linRMSE

nrow(subset(myteams, abs(linResiduals) < linRMSE)) / nrow(myteams)
nrow(subset(myteams, abs(linResiduals) < 2*linRMSE)) / nrow(myteams)

myteams$pytWpct <- with(myteams, R^2/(R^2+RA^2))
myteams$pytResiduals <- myteams$Wpct - myteams$pytWpct
sqrt(mean(myteams$pytResiduals^2))

myteams$logWratio <- log(myteams$W / myteams$L)
myteams$logRratio <- log(myteams$R / myteams$RA)
pytFit <- lm(logWratio ~ 0 + logRratio, data=myteams)


#EXERCISES - question 3 - Managers who outperformed Pythag
myteamsSince51 <- subset(Teams, yearID & yearID > 1950)[ , c("teamID",
                                                             "yearID", "lgID", "G", "W", "L", "R", "RA")]
myteamsSince51$logWratio <- log(myteamsSince51$W / myteamsSince51$L)
myteamsSince51$logRratio <- log(myteamsSince51$R / myteamsSince51$RA)
pytFitSince51 <- lm(logWratio ~ 0 + logRratio, data=myteamsSince51)
pytFitSince51
#so, since 1951, the Pythag coefficient is 1.856 
myteamsSince51$pytWpct <- with(myteamsSince51, R^1.856 / (R^1.856 + RA^1.856))
myteamsSince51$Wpct <- with(myteamsSince51, W/(W+L))
myteamsSince51$pytResiduals <- with(myteamsSince51, Wpct - pytWpct)
#to finish this, need to load Manager data, probably merge it with myteamsSince51, 
#and then find which Managers exceeded and underperformed versus Pythag expctation
#What if we just analyzed it by teamID instred of manager?
#I will look at four AL West teams, since 1990

myteamsWestSince90 <- subset(Teams, 
                             yearID>=1990 &
                               (franchID=="SEA"|franchID=="ANA"|franchID=="TEX"|franchID=="OAK")
)[ , c("teamID", "franchID", "yearID", "lgID", "G", "W", "L", "R", "RA")]
myteamsWestSince90$logWratio <- log(myteamsWestSince90$W / myteamsWestSince90$L)
myteamsWestSince90$logRratio <- log(myteamsWestSince90$R / myteamsWestSince90$RA)
pytFitWestSince90 <- lm(logWratio ~ 0 + logRratio, data=myteamsWestSince90)
pytFitWestSince90
#so the Pythag coeff is 1.934 for current AL West teams, since 1990
myteamsWestSince90$pytWpct <- with(myteamsWestSince90, R^1.934 / (R^1.934 + RA^1.934))
myteamsWestSince90$Wpct <- with(myteamsWestSince90, W/(W+L))
myteamsWestSince90$pytResiduals <- with(myteamsWestSince90, Wpct - pytWpct)

windows(width=8, height=11)
plot(myteamsWestSince90$pytWpct, myteamsWestSince90$Wpct, 
     xlab="Pythagorean Win %",
     ylab="Actual Win %")
curve(0 + x, add=TRUE)
curve(.040 + x, add=TRUE)
curve(-.040 + x, add=TRUE)
text(.63, .71, "Overachievers")
text(.65, .65, "Met Expectation")
text(.65, .58, "Underachievers")
text(.462, .525, "SEA 2009", pos=2, cex=.8)
text(.489, .543, "SEA 2007", pos=2, cex=.8)
text(.528, .574, "OAK 2006", pos=2, cex=.8)
text(.550, .593, "OAK 1992", pos=2, cex=.8)
text(.545, .617, "ANA 2008", pos=2, cex=.8)


identify(myteamsWestSince90$pytWpct, myteamsWestSince90$Wpct, 
         labels=myteamsWestSince90$franchID, n=5)
identify(myteamsWestSince90$pytWpct, myteamsWestSince90$Wpct, 
         labels=myteamsWestSince90$yearID, n=5)























# ************************************************************************* #



#Good and Bad Predictions by the Pythagorean Formula

setwd("C:/Users/ethompson/Desktop/Lahman")
gl2011 <- read.table("gl2011.txt", sep=",")
glheaders <- read.csv("game_log_header.csv")
names(gl2011) <- names(glheaders)

BOS2011 <- subset(gl2011, HomeTeam=="BOS" | VisitingTeam=="BOS")[
  , c("VisitingTeam", "HomeTeam", "VisitorRunsScored", "HomeRunsScore")]
head(BOS2011)
BOS2011$ScoreDiff <- with(BOS2011, 
                          ifelse(HomeTeam=="BOS", HomeRunsScore - VisitorRunsScored,
                                 VisitorRunsScored - HomeRunsScore))
BOS2011$W <- BOS2011$ScoreDiff > 0
aggregate(abs(BOS2011$ScoreDiff), list(W=BOS2011$W), summary)

#so, 2011 Boston won its games by 4.30 runs on avg, but lost by 3.458 on avg. 
#so, it greatly underachieved versus its Pythag win expectancy


#analyzing games with close scores:
#first we create a column saying who which team won the game
results <- gl2011[,c("VisitingTeam", "HomeTeam",
                     "VisitorRunsScored", "HomeRunsScore")]
results$winner <- ifelse(results$HomeRunsScore >
                           results$VisitorRunsScored, as.character(results$HomeTeam),
                         as.character(results$VisitingTeam))
#now we look at only 1-run games
onerungames <- subset(results, abs(HomeRunsScore - VisitorRunsScored)==1)
onerunwins <- as.data.frame(table(onerungames$winner))
names(onerunwins) <- c("teamID", "onerunW")
#we look at relation between Pythag residuals and one-run wins
#need to change abbrev on the Angels bc it's different between Lahman and Retrsheet
teams2011 <- subset(myteams, yearID == 2011)
teams2011[teams2011$teamID == "LAA", "teamID"] <- "ANA"
teams2011 <- merge(teams2011, onerunwins)
plot(teams2011$onerunW, teams2011$pytResiduals,
     xlab="one run wins", ylab="Pythag residuals")
identify(teams2011$onerunW, teams2011$pytResiduals, labels=teams2011$teamID)

#do teams with dominant closers win a disproportionate number of 1-run games?
setwd("C:/Users/etsafe11/Analytics/R/R Studio Working Directory/Lahman Database")
pit <- read.csv("pitching.csv")
top_closers <- subset(pit, GF>50 & ERA < 2.50)[ ,c("playerID", "yearID", "teamID")]
teams_top_closers <- merge(myteams, top_closers)
summary(teams_top_closers$pytResiduals)

#10 runs is for a win (the rigorous way), see pg 100 to understand this
D(expression(G*R^2/(R^2+RA^2)), "R")
#then simplify    G * (2 * R)/(R^2 + RA^2) - G * R^2 * (2 * R)/(R^2 + RA^2)^2    to
# IR/W = ((R^2+RA^2)^2 / (2*G*R*RA^2)
#then, if R and RA are express as PER GAME, then you can remove G from the above
IR <- function(RS=5, RA=5){
  round((RS^2+RA^2)^2 / (2*RS*RA^2), 1)
}
#the above function is used to make a table of various RS/RA combinations
IRtable <- expand.grid(RS=seq(3, 6, .5), RA=seq(3, 6, .5))
rbind(head(IRtable), tail(IRtable))
IRtable$IRW <- IR(IRtable$RS, IRtable$RA)
#now we have a data frame saying the Run value of a win under diff RS/RA scenarios
#next, we show this data frame in tabular form 9easier to read)
xtabs(IRW ~ RS + RA, data=IRtable)



#EXERCISES - question 1
myteams60s <- subset(teams, 1960 < yearID & yearID <= 1969)[ , c("teamID",
                                                                 "yearID", "lgID", "G", "W", "L", "R", "RA")]
myteams70s <- subset(teams, 1970 < yearID & yearID <= 1979)[ , c("teamID",
                                                                 "yearID", "lgID", "G", "W", "L", "R", "RA")]
myteams80s <- subset(teams, 1980 < yearID & yearID <= 1989)[ , c("teamID",
                                                                 "yearID", "lgID", "G", "W", "L", "R", "RA")]
myteams90s <- subset(teams, 1990 < yearID & yearID <= 1999)[ , c("teamID",
                                                                 "yearID", "lgID", "G", "W", "L", "R", "RA")]
myteams60s$RD <- with(myteams60s, R-RA)
myteams60s$Wpct <- with(myteams60s, W/(W+L))
myteams70s$RD <- with(myteams70s, R-RA)
myteams70s$Wpct <- with(myteams70s, W/(W+L))
myteams80s$RD <- with(myteams80s, R-RA)
myteams80s$Wpct <- with(myteams80s, W/(W+L))
myteams90s$RD <- with(myteams90s, R-RA)
myteams90s$Wpct <- with(myteams90s, W/(W+L))
linfit60s <- lm(Wpct ~ RD, data=myteams60s)
linfit70s <- lm(Wpct ~ RD, data=myteams70s)
linfit80s <- lm(Wpct ~ RD, data=myteams80s)
linfit90s <- lm(Wpct ~ RD, data=myteams90s)
linfit60s
linfit70s
linfit80s
linfit90s
coef(linfit60s)[2] * 10 * 162
coef(linfit70s)[2] * 10 * 162
coef(linfit80s)[2] * 10 * 162
coef(linfit90s)[2] * 10 * 162


#EXERCISES - question 2
myteams1800s <- subset(teams, yearID & yearID <= 1899 & W > 0)[ , c("teamID",
                                                                    "yearID", "lgID", "G", "W", "L", "R", "RA")]
myteams1800s$logWratio <- log(myteams1800s$W / myteams1800s$L)
myteams1800s$logRratio <- log(myteams1800s$R / myteams1800s$RA)
pytFit1800s <- lm(logWratio ~ 0 + logRratio, data=myteams1800s)
pytFit1800s
#this shows us that the Pythag coefficient for the 1800s was 1.915
myteams1800s$pytWpct <- with(myteams1800s, R^1.915 / (R^1.915 + RA^1.915))
myteams1800s$Wpct <- with(myteams1800s, W/(W+L))
myteams1800s$pytResiduals <- with(myteams1800s, Wpct - pytWpct)
plot(myteams1800s$Wpct, myteams1800s$pytResiduals)
abline(h=0, lty=3)
#so, the really good teams in the 1800s beat their Pythag
#and the really bad teams in the 1800s were worse than their Pythag




