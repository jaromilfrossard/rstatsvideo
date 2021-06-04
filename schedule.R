rm(list=ls())
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tuber)
library(readr)
library(lubridate)
library(glue)
library(rtweet)
library(gert)
library(taskscheduleR)



taskscheduler_create(taskname = "twitterbot", rscript = "dailytask.R", 
                     schedule = "DAILY", starttime = "20:00")
