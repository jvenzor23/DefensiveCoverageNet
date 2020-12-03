# This script pulls the route each receiver is running on each play.
# ***NOTE: Routes are not included for sack plays, and plays with an 
#          accepted penalty!

# Clean workspace
rm(list=ls())

# Setting Working Directory
setwd("~/Desktop/CoverageNet/inputs/")

# Calling Necessary Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(reticulate)
library(gganimate)
library(magick)
library(fitdistrplus)
library(skewt)
library(sn)
library(broom)
library(dplyr)

setwd("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/")
files = dir()[startsWith(dir(), "week")]

routes = data.frame()

for(file in files){
  
  pt_data = read.csv(paste0("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/",
                            file))
  
  pt_data2 = pt_data %>%
    filter(route != "") %>%
    distinct(gameId, playId, nflId, route)
  
  routes = rbind(routes, pt_data2)
  
}

routes2 = routes %>%
  arrange(gameId, playId, route)

check = routes2 %>%
  group_by(route) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

write.csv(routes2,
          "~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/routes.csv",
          row.names = FALSE)

