setwd("C:/Users/etsafe11/Analytics/R/R Studio Working Directory/download.folder")
source("parse.retrosheet.pbp.R")
setwd("C:/Users/etsafe11/Analytics/R/R Studio Working Directory")
parse.retrosheet.pbp(1961)

setwd("C:/Users/etsafe11/Analytics/R/R Studio Working Directory/download.folder/unzipped")

data <- read.csv("all1961.csv", header=FALSE)
roster <- read.csv("roster1961.csv")
fields <- read.csv("fields.csv")
names(data) <- fields[, "Header"]

data1961 <- read.csv("all1961.csv", header=FALSE)
fields <- read.csv("fields.csv")
names(data1961) <- fields[, "Header"]

retro.ids <- read.csv("retrosheetIDs.csv")

maris.id <- as.character(subset(retro.ids,
                               FIRST=="Roger" & LAST=="Maris")$ID)
mantle.id <- as.character(subset(retro.ids,
                              FIRST=="Mickey" & LAST=="Mantle")$ID)

maris.data <- subset(data1961, BAT_ID == maris.id)
mantle.data <- subset(data1961, BAT_ID == mantle.id)

createdata <- function(d){
  d$Date <- as.Date(substr(d$GAME_ID, 4, 11),
                    format="%Y%m%d")
  d <- d[order(d$Date), ]
  d$HR <- ifelse(d$EVENT_CD == 23, 1, 0)
  d$cumHR <- cumsum(d$HR)
  d[, c("Date", "cumHR")]
}

maris.hr <- createdata(maris.data)
mantle.hr <- createdata(mantle.data)

plot(maris.hr, type="l", lwd=2, ylab="Home Runs", main="The 1961 Home Run Race")
lines(mantle.hr, lwd=2, col="grey")
abline(h=60, lty=3)
text(-3110, 57, "60 (Babe Ruth 1927)")
legend(-3080, 18, legend=c("Maris (61)", "Mantle (54)"),
       lwd=2, col=c("black", "grey"))

#locator(n=1)
#mtext("Maris: black line, Mantle: grey line", side=3)



