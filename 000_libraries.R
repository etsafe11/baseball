# set wd to your local repo for this project
setwd("C:/Users/ethompson/Desktop/baseball")

# install "googlesheets" R package
# devtools::install_github("jennybc/googlesheets")

# if you need to authenticate, try this
# gs_auth(new_user = TRUE)

# for BigQuery reads/writes
#library(bigrquery)
#library(jsonlite)

#library(googlesheets)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(truncnorm)
library(RCurl)
library(data.table)
library(cowplot)

# usage in command prompt: bevent -y 2017 2017ANA.eva >data.txt





