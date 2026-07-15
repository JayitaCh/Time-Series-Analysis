require(tidyverse)
require(readr)
require(xts)

# Read Data
election_data <- read_csv("../elections-stories-over-time-20210111144254.csv")

# Data Wrangling
# Get a data overview
head(election_data)
glimpse(election_data)

# Check the data types
str(election_data)

# Time-series objects
# ts --> Month-wise ts data

date <- seq.Date(from = as.Date("2015-01-01"), 
                 to = as.Date("2020-12-31"), by="day")

election_news <- as.vector(election_data)

election_xts <- xts(
  election_news$count,
  order.by = election_news$date
)

head(election_xts)

# par(mar = c(4, 4, 2, 1))

plot(
  election_xts,
  col = "steelblue",
  lwd = 2,
  major.ticks = "years",
  xlab = "Year",
  ylab = "Number of stories",
  main = "Election News Over Time"
)

plot.xts(election_xts,
         main = "Election News Over Time",
         ylab = "Number of Stories", 
         col = "steelblue2", 
         lwd=2)

# create a "toy" time series with the same length
Election_2 <- xts(
  election_news$count*2,
  order.by = election_news$date
)
Election_3 <- xts(
  election_news$count*4,
  order.by = election_news$date
)

ElectionNews_multi <- merge(election_xts, Election_2, Election_3)

ElectionNews_multi_xts <- as.xts(ElectionNews_multi)

plot.xts(ElectionNews_multi_xts,
         main = "Election News Over Time",
         ylab = "Number of Stories", 
         lwd=2, lty=1,
         col = c("blue", "orange", "black"),
         multi.panel = T)
