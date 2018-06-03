teams <- read.csv("core/Teams.csv")

myteams <- subset(teams, yearID > 2000)[ , c("teamID", "yearID",
                                            "lgID", "G", "W", "L", "R", "RA")]

tail(myteams)

myteams$RD <- with(myteams, R - RA)
myteams$Wpct <- with(myteams, W / (W + L))

plot(myteams$RD, myteams$Wpct,
     xlab="run differential",
     ylab="winning percentage")

# Section 4.3 Linear Regression

linfit <- lm(Wpct ~ RD, data=myteams)

abline(a=coef(linfit)[1], b=coef(linfit)[2], lwd=2)

myteams$linWpct <- predict(linfit)
myteams$linResiduals <- residuals(linfit)

plot(myteams$RD, myteams$linResiduals,
     xlab="run differential",
     ylab="residual")
abline(h=0, lty=3)
points(c(68, 88), c(.0749, -.0733), pch=19)
text(68, .0749, "LAA '08", pos=4, cex=.8)
text(88, -.0733, "CLE '06", pos=4, cex=.8)

mean(myteams$linResiduals)

linRMSE <- sqrt(mean(myteams$linResiduals ^ 2))
linRMSE

nrow(subset(myteams, abs(linResiduals) < linRMSE)) /
  nrow(myteams)

nrow(subset(myteams, abs(linResiduals) < 2 * linRMSE)) /
  nrow(myteams)

# Section 4.4 The Pythagorean Formula for Winning Percentage

myteams$pytWpct <- with(myteams, R ^ 2 / (R ^ 2 + RA ^ 2))

myteams$pytResiduals <- myteams$Wpct - myteams$pytWpct
sqrt(mean(myteams$pytResiduals ^ 2))

# Section 4.5  The Exponent in the Pythagorean Formula

myteams$logWratio <- log(myteams$W / myteams$L)
myteams$logRratio <- log(myteams$R / myteams$RA)
pytFit <- lm(logWratio ~ 0 + logRratio, data=myteams)
pytFit
















































myteams <-
  teams %>%
  mutate(year = 2018) %>%
  select(Tm, year, Lg, G, W, L, R, RA) %>%
  filter(Tm != "Avg")



tail(myteams)

myteams$RD <- with(myteams, R - RA)
myteams$Wpct <- with(myteams, W / (W + L))

plot(myteams$RD, myteams$Wpct,
     xlab="run differential",
     ylab="winning percentage")

# Section 4.3 Linear Regression

linfit <- lm(Wpct ~ RD, data=myteams)

abline(a=coef(linfit)[1], b=coef(linfit)[2], lwd=2)

myteams$linWpct <- predict(linfit)
myteams$linResiduals <- residuals(linfit)

plot(myteams$RD, myteams$linResiduals,
     xlab="run differential",
     ylab="residual")
abline(h=0, lty=3)
points(c(68, 88), c(.0749, -.0733), pch=19)
text(68, .0749, "LAA '08", pos=4, cex=.8)
text(88, -.0733, "CLE '06", pos=4, cex=.8)

mean(myteams$linResiduals)

linRMSE <- sqrt(mean(myteams$linResiduals ^ 2))
linRMSE

nrow(subset(myteams, abs(linResiduals) < linRMSE)) /
  nrow(myteams)

nrow(subset(myteams, abs(linResiduals) < 2 * linRMSE)) /
  nrow(myteams)

# Section 4.4 The Pythagorean Formula for Winning Percentage

myteams$pytWpct <- with(myteams, R ^ 2 / (R ^ 2 + RA ^ 2))

myteams$pytResiduals <- myteams$Wpct - myteams$pytWpct
sqrt(mean(myteams$pytResiduals ^ 2))

# Section 4.5  The Exponent in the Pythagorean Formula

myteams$logWratio <- log(myteams$W / myteams$L)
myteams$logRratio <- log(myteams$R / myteams$RA)
pytFit <- lm(logWratio ~ 0 + logRratio, data=myteams)
pytFit





#EXERCISES - question 2
myteams1800s <- subset(myteams, year & year <= 1899 & W > 0)

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








