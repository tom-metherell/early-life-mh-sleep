library(data.table)
library(tidyverse)
library(magrittr)
library(chron)
library(lubridate)
library(fasttime)

# Listing all "summary" data files
filelist <- list.files(path = ".\\Summary")

#######
# Function: PAL
# Input: fileno, an integer denoting the position of the desired data file in filelist
# Method: the function reads the requested data file and returns only the needed columns
# Output: a dataframe containing a reduced version of the input data file
#######

PAL <- function(fileno){
  dataset <- fread(paste0(".\\Summary\\", filelist[fileno]))
  dataset %<>% select(ParticipantId, BoutId, ActivityType, Date, StartTime, EndTime, Intervals)
  return(dataset)
}

# Applying the PAL function to all data files in filelist
for(i in 1:length(filelist)){
  assign(
    paste("data", i, sep = "_"),
    PAL(i)
  )
}
rm(i)

# Removing the unnecessary filelist
rm(filelist)

# Binding all dataframes together
df <- bind_rows(mget(ls(pattern = "data_")))

# Removing individual dataframes
rm(list = ls(pattern = "data_"))

df$StartTime %<>% times()
df$EndTime %<>% times()

df %<>% 
  distinct() %>%
  group_by(ParticipantId) %>%
  filter(!duplicated(BoutId) & !duplicated(BoutId, fromLast = TRUE)) %>%
  ungroup() %>%
  mutate(refday = case_when(
    StartTime >= times("12:00:00") ~ as_date(Date, format = "dmy"),
    StartTime <= times("12:00:00") ~ as_date(Date, format = "dmy") - 1
  ), .after = Date)

cutoffs <- c(25200, 21600, 18000, 14400)

df_bedtime <- df %>%
  filter(ActivityType == "Sitting" & (StartTime >= times("19:00:00") | StartTime <= times("12:00:00")) & (EndTime >= times("19:00:00") | EndTime <= times("12:00:00"))) %>%
  group_by(ParticipantId, refday) %>%
  arrange(Date, StartTime, .by_group = TRUE) %>%
  mutate(lead1Intervals = lead(Intervals),
         lead2Intervals = lead(Intervals, 2),
         lead3Intervals = lead(Intervals, 3),
         lead4Intervals = lead(Intervals, 4),
         duration12 = Intervals + lead1Intervals,
         duration13 = duration12 + lead2Intervals,
         duration14 = duration13 + lead3Intervals,
         duration15 = duration14 + lead4Intervals,
         consec_marker = if_else(
           (strptime(paste(Date, lead(EndTime)), format = "%d-%m-%Y %H:%M:%S") - strptime(paste(Date, StartTime), format = "%d-%m-%Y %H:%M:%S")) +
             (strptime(paste(Date, lead(EndTime, 2)), format = "%d-%m-%Y %H:%M:%S") - strptime(paste(Date, lead(StartTime, 1)), format = "%d-%m-%Y %H:%M:%S")) +
             (strptime(paste(Date, lead(EndTime, 3)), format = "%d-%m-%Y %H:%M:%S") - strptime(paste(Date, lead(StartTime, 2)), format = "%d-%m-%Y %H:%M:%S")) +
             (strptime(paste(Date, lead(EndTime, 4)), format = "%d-%m-%Y %H:%M:%S") - strptime(paste(Date, lead(StartTime, 3)), format = "%d-%m-%Y %H:%M:%S"))
           <= 360,
           BoutId,
           NA_integer_
         )
  ) %>%
  mutate(timecat = case_when(
    StartTime >= times("19:00:00") & StartTime < times("21:00:00") ~ 1,
    StartTime >= times("21:00:00") & StartTime < times("22:00:00") ~ 2,
    StartTime >= times("22:00:00") & StartTime < times("23:00:00") ~ 3,
    (StartTime >= times("23:00:00") | StartTime < times("12:00:00")) ~ 4
  ), .after = EndTime) %>%
  mutate(bedtime_eligible = case_when(
    !is.na(consec_marker) & Intervals >= cutoffs[timecat] ~ 1,
    !is.na(lag(consec_marker)) & duration12 >= cutoffs[timecat] & Intervals >= 3600 ~ 1,
    !is.na(lag(consec_marker)) & lag(duration12) >= cutoffs[timecat] & lag(Intervals) <= 3600 ~ 1,
    !is.na(consec_marker) & duration13 >= cutoffs[timecat] & Intervals >= 3600 & lead1Intervals >= 3600 & lead2Intervals >= 3600 ~ 1,
    !is.na(consec_marker) & duration14 >= cutoffs[timecat] & Intervals >= 3600 & lead1Intervals >= 3600 & lead2Intervals >= 3600 & lead3Intervals >= 3600 ~ 1, 
    !is.na(consec_marker) & duration15 >= cutoffs[timecat] & Intervals >= 3600 & lead1Intervals >= 3600 & lead2Intervals >= 3600 & lead3Intervals >= 3600 & lead4Intervals >= 3600 ~ 1,
    consec_marker == last(na.omit(consec_marker)) ~ 1,
    all(is.na(consec_marker)) & row_number() == 1 & sum(Intervals) >= 14400 ~ 1
  )) %>%
  mutate(bedtime = bedtime_eligible == 1 & !duplicated(bedtime_eligible == 1)) %>%
  select(-timecat, -(lead1Intervals:lead4Intervals), -(duration12:duration15), -bedtime_eligible, -consec_marker) %>%
  ungroup()

rm(cutoffs)

df$bedtime <- NA

df %<>% rows_update(df_bedtime, by = c("ParticipantId", "BoutId"))
rm(df_bedtime)

df %<>%
  mutate(bedtime = case_when(bedtime ~ StartTime)) %>%
  group_by(ParticipantId, refday) %>%
  fill(bedtime, .direction = "downup") %>%
  ungroup()

df_waketime <- df %>%
  group_by(ParticipantId, refday) %>%
  mutate(ActivityType = if_else(
    ActivityType != "Sitting" & StartTime >= times("03:00:00") & EndTime <= times("06:00:00") & any(Intervals[ActivityType == "Sitting" & strptime(paste(Date, StartTime), format = "%d-%m-%Y %H:%M:%S") > strptime(paste(Date[row_number()], EndTime[row_number()]), format = "%d-%m-%Y %H:%M:%S")]) >= 9000,
    "Rejected sitting",
    ActivityType
  )) %>%
  filter(ActivityType != "Sitting")