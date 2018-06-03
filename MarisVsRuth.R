#parsing 1961 data
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


#parsing 1927 data
setwd("C:/Users/etsafe11/Analytics/R/R Studio Working Directory/download.folder")
source("parse.retrosheet.pbp.R")
setwd("C:/Users/etsafe11/Analytics/R/R Studio Working Directory")
parse.retrosheet.pbp(1927)

setwd("C:/Users/etsafe11/Analytics/R/R Studio Working Directory/download.folder/unzipped")

data <- read.csv("all1927.csv", header=FALSE)
roster <- read.csv("roster1927.csv")
fields <- read.csv("fields.csv")
names(data) <- fields[, "Header"]

data1927 <- read.csv("all1927.csv", header=FALSE)
fields <- read.csv("fields.csv")
names(data1927) <- fields[, "Header"]

retro.ids <- read.csv("retrosheetIDs.csv")
maris.id <- as.character(subset(retro.ids,
                                FIRST=="Roger" & LAST=="Maris")$ID)
mantle.id <- as.character(subset(retro.ids,
                                 FIRST=="Mickey" & LAST=="Mantle")$ID)
ruth.id <- as.character(subset(retro.ids,
                                FIRST=="Babe" & LAST=="Ruth")$ID)

maris.data <- subset(data1961, BAT_ID == maris.id)
mantle.data <- subset(data1961, BAT_ID == mantle.id)
ruth.data <- subset(data1927, BAT_ID == ruth.id)

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
ruth.hr <- createdata(ruth.data)

#converting 1927 to 1961 by adding 12419, so we can plot Ruth on the same chart
ruth.hr$Date <- ruth.hr$Date+12419

plot(maris.hr, type="l", lwd=2, ylab="Home Runs", main="'61 Maris vs. '27 Ruth, Day-by-Day")
#lines(mantle.hr, lwd=2, col="blue")
lines(ruth.hr, lwd=2, col="grey")
legend(-3100, 25, legend=c("61 Maris (61 HR)", "'27 Ruth (60 HR)"),
       lwd=2, col=c("black", "grey"))



