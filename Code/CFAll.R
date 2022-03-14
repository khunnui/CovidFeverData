#------------------------------------------------------------------------------#
# Description: Driver program for Covid-19 Fever data processing	  	         #
# Author:      hpy1                                                            #
# Created:     January 27, 2022                                                #
# Modified:    March 9, 2022                                                   #
#------------------------------------------------------------------------------#
library(tidyverse)
library(lubridate)
library(DBI)
library(odbc)

## Set folders -----------------------------------------------------------------
code_folder <- paste0(getwd(), "/Code")
data_folder <- paste0(getwd(), "/Data")

## Get data from SQL Server ----------------------------------------------------
dbConnector <- function(server, database, uid, pwd) {
  DBI::dbConnect(odbc::odbc(),
    Driver   = "SQL Server",
    Server   = server,
    Database = database,
    Uid      = uid,
    Pwd      = pwd)
}

server = "1.20.151.54,11433"
database = "CovidFever"
uid = rstudioapi::askForPassword("Database user")
pwd = rstudioapi::askForPassword("Database password")
dbConn <- dbConnector(server, database, uid, pwd)
tblSection1 <- dbGetQuery(dbConn,'select * from "tblSection1"')
tblSection2 <- dbGetQuery(dbConn,'select * from "tblSection2"')
tblSection3 <- dbGetQuery(dbConn,'select * from "tblSection3"')
tblSection4 <- dbGetQuery(dbConn,'select * from "tblSection4"')
#tblSection4_1 <- dbGetQuery(dbConn,'select * from "tblSection4_1"')
#tblSection5 <- dbGetQuery(dbConn,'select * from "tblSection5"')
#tblSection5_1 <- dbGetQuery(dbConn,'select * from "tblSection5_1"')
#tblSection6 <- dbGetQuery(dbConn,'select * from "tblSection6"')
#tblSection7 <- dbGetQuery(dbConn,'select * from "tblSection7"')
#tblSection8 <- dbGetQuery(dbConn,'select * from "tblSection8"')
LabPCRResult <- dbGetQuery(dbConn,'select * from "LabPCRResult"')
#LabSero <- dbGetQuery(dbConn,'select * from "LabSero"') #Nhong has added data

## Data wrangling for each data frame ------------------------------------------
source(paste0(code_folder, "/tblSection1.R"))
source(paste0(code_folder, "/tblSection2.R"))
source(paste0(code_folder, "/tblSection3.R"))
source(paste0(code_folder, "/tblSection4.R"))
# source(paste0(code_folder, "/tblSection4_1.R"))
# source(paste0(code_folder, "/tblSection5.R"))
# source(paste0(code_folder, "/tblSection5_1.R"))
# source(paste0(code_folder, "/tblSection6.R"))
# source(paste0(code_folder, "/tblSection7.R"))
# source(paste0(code_folder, "/tblSection8.R"))
source(paste0(code_folder, "/LabPCRResult.R"))
#source(paste0(code_folder, "/LabSero.R")) #Nhong has added data

# Screening page
df_scr <- tblSection1 %>%
  mutate(scrdate = floor_date(S1ScreenDate, "month")) %>%
  group_by(scrdate) %>% 
  tally()
df_scrage0 <- tblSection1 %>%
  summarize(
    n = n(),
    min = min(S1Age_Year, na.rm = TRUE),
    q1 = quantile(S1Age_Year, 0.25, na.rm = TRUE),
    median = median(S1Age_Year, na.rm = TRUE),
    mean = round(mean(S1Age_Year, na.rm = TRUE), 1),
    q3 = quantile(S1Age_Year, 0.75, na.rm = TRUE),
    max = max(S1Age_Year, na.rm = TRUE)
  )
df_scrage1 <- tblSection1 %>%
  group_by(Province) %>% 
  summarize(
    n = n(),
    min = min(S1Age_Year, na.rm = TRUE),
    q1 = quantile(S1Age_Year, 0.25, na.rm = TRUE),
    median = median(S1Age_Year, na.rm = TRUE),
    mean = round(mean(S1Age_Year, na.rm = TRUE), 1),
    q3 = quantile(S1Age_Year, 0.75, na.rm = TRUE),
    max = max(S1Age_Year, na.rm = TRUE)
  )
df_scrage2 <- tblSection1 %>%
  group_by(S1HospitalID) %>% 
  summarize(
    n = n(),
    min = min(S1Age_Year, na.rm = TRUE),
    q1 = quantile(S1Age_Year, 0.25, na.rm = TRUE),
    median = median(S1Age_Year, na.rm = TRUE),
    mean = round(mean(S1Age_Year, na.rm = TRUE), 1),
    q3 = quantile(S1Age_Year, 0.75, na.rm = TRUE),
    max = max(S1Age_Year, na.rm = TRUE)
  )
df_scrgender <- tblSection1 %>%
  group_by(S1Gender) %>% 
  tally()
df_screnrol <- tblSection1 %>%
  group_by(OLDCF, CF_Enrol) %>% 
  tally()

## Save all data frames in one data file (CFMast.RData) for later use ----------
save(
  list = c(
    "df_scr",
    "df_scrage0",
    "df_scrage1",
    "df_scrage2",
    "df_scrgender",
    "df_screnrol"
  ),
  file = paste0(data_folder, "/CFDashboard.Rdata")
)
