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

df %<>% distinct()

df$StartTime %<>% times()
df$EndTime %<>% times()

df %<>%
  group_by(ParticipantId) %>%
  filter(!duplicated(BoutId) & !duplicated(BoutId, fromLast = TRUE)) %>%
  ungroup() %>%
  mutate(refday = case_when(
    StartTime >= times("12:00:00") ~ as_date(Date, format = "dmy"),
    StartTime < times("12:00:00") ~ as_date(Date, format = "dmy") - 1
  ), .after = Date) %>%
  mutate(cumtimestart = cumsum(lag(Intervals)),
         cumtimeend = cumsum(Intervals)
  )

cutoffs <- c(25200, 21600, 18000, 14400)

df_bedtime <- df %>%
  filter(ActivityType == "Sitting" & (StartTime >= times("19:00:00") | StartTime <= times("12:00:00")) & (EndTime >= times("19:00:00") | EndTime <= times("12:00:00"))) %>%
  group_by(ParticipantId, refday) %>%
  arrange(Date, StartTime, .by_group = TRUE) %>%
  mutate(reject = cumtimestart - lag(cumtimeend) >= 900 & (StartTime >= times("19:00:00") | StartTime < times("01:00:00") & lag(Intervals) > 9000)) %>%
  fill(reject, .direction = "up") %>%
  filter(!reject) %>%
  select(-reject) %>%
  mutate(lead1Intervals = lead(Intervals),
         lead2Intervals = lead(Intervals, 2),
         lead3Intervals = lead(Intervals, 3),
         lead4Intervals = lead(Intervals, 4),
         duration12 = Intervals + lead1Intervals,
         duration13 = duration12 + lead2Intervals,
         duration14 = duration13 + lead3Intervals,
         duration15 = duration14 + lead4Intervals,
         consec_marker2 = if_else(
           lead(cumtimestart) - cumtimeend <= 360,
           BoutId,
           NA_integer_
         ),
         consec_marker3 = if_else(
           (lead(cumtimestart) - cumtimeend) + (lead(cumtimestart, 2) - lead(cumtimeend)) <= 360,
           BoutId,
           NA_integer_
         ),
         consec_marker4 = if_else(
           (lead(cumtimestart) - cumtimeend) + (lead(cumtimestart, 2) - lead(cumtimeend)) + (lead(cumtimestart, 3) - lead(cumtimeend, 2)) <= 360,
           BoutId,
           NA_integer_
         ),
         consec_marker5 = if_else(
           (lead(cumtimestart) - cumtimeend) + (lead(cumtimestart, 2) - lead(cumtimeend)) + (lead(cumtimestart, 3) - lead(cumtimeend, 2)) + (lead(cumtimestart, 4) - lead(cumtimeend, 3)) <= 360,
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
    Intervals >= cutoffs[timecat] ~ 1,
    !is.na(consec_marker2) & duration12 >= cutoffs[timecat] & Intervals >= 3600 ~ 1,
    !is.na(lag(consec_marker2)) & lag(duration12) >= cutoffs[timecat] & lag(Intervals) <= 3600 ~ 1,
    !is.na(consec_marker3) & duration13 >= cutoffs[timecat] & Intervals >= 3600 & lead1Intervals >= 3600 & lead2Intervals >= 3600 ~ 1,
    !is.na(consec_marker4) & duration14 >= cutoffs[timecat] & Intervals >= 3600 & lead1Intervals >= 3600 & lead2Intervals >= 3600 & lead3Intervals >= 3600 ~ 1, 
    !is.na(consec_marker5) & duration15 >= cutoffs[timecat] & Intervals >= 3600 & lead1Intervals >= 3600 & lead2Intervals >= 3600 & lead3Intervals >= 3600 & lead4Intervals >= 3600 ~ 1,
    consec_marker5 == last(na.omit(consec_marker5)) ~ 1,
  )) %>%
  mutate(bedtime_eligible = if_else(
    row_number() == 1 & all(is.na(bedtime_eligible)) & sum(Intervals) >= 14400,
    1,
    bedtime_eligible
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

df %<>%
  group_by(ParticipantId, refday) %>%
  mutate(ActivityType = if_else(
    ActivityType != "Sitting" & StartTime >= times("03:00:00") & EndTime <= times("06:00:00") & any(ActivityType == "Sitting" & StartTime >= EndTime[row_number()] & StartTime < times("12:00:00") & Intervals >= 9000),
    "Rejected sitting",
    ActivityType
  )) %>%
  ungroup()

cutoffs1 <- c(600, 300, 312, 324, 336, 348, 360, 372, 384, 396)
cutoffs2 <- c(0.4, 0.2, 0.1, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08)

df_waketime <- df %>%
  filter(ActivityType != "Sitting" & ActivityType != "Rejected sitting") %>%
  group_by(ParticipantId, refday) %>%
  arrange(Date, StartTime, .by_group = TRUE) %>%
  mutate(
    search_start = if_else(
      cumtimestart - lag(cumtimeend) >= 14400,
      1,
      NA_real_
    ),
    eligible = search_start
  ) %>%
  fill(eligible, .direction = "down") %>%
  filter(eligible == 1) %>%
  select(-eligible) %>%
  mutate(non_consec = if_else(
    row_number() == 1 | lag(BoutId) != BoutId - 1,
    1,
    0
  )) %>%
  mutate(BoutGroup = as.integer(cumsum(non_consec)), .after = BoutId) %>%
  fill(BoutGroup, .direction = "down") %>%
  select(-non_consec) %>%
  mutate(
    act_cumtimeend = cumsum(Intervals),
    searchstarttime = cumtimestart[search_start]
  ) %>%
  select(-search_start) %>%
  fill(searchstarttime, .direction = "down") %>%
  ungroup() %>%
  mutate(waketime_eligible = if_else(
    BoutGroup <= 10 & act_cumtimeend > cutoffs1[BoutGroup] & act_cumtimeend/(cumtimeend - searchstarttime) > cutoffs2[BoutGroup],
    1,
    NA_real_
  )) %>%
  group_by(ParticipantId, refday, BoutGroup) %>%
  fill(waketime_eligible, .direction = "downup") %>%
  ungroup() %>% group_by(ParticipantId, refday) %>%
  mutate(waketime = waketime_eligible == 1 & !duplicated(waketime_eligible == 1)) %>%
  select(-BoutGroup, -(cumtimeend:waketime_eligible)) %>%
  ungroup()

rm(list = c("cutoffs1", "cutoffs2"))

df$waketime <- NA

df %<>% rows_update(df_waketime, by = c("ParticipantId", "BoutId"))
rm(df_waketime)

df %<>%
  mutate(waketime = case_when(waketime ~ StartTime)) %>%
  group_by(ParticipantId, refday) %>%
  fill(waketime, .direction = "downup") %>% # tempdf.csv snapshot here
  mutate(
    sleepdur = case_when(
      bedtime > waketime & bedtime < times("12:00:00") ~ NA_real_,
      waketime > times("12:00:00") ~ NA_real_,
      bedtime > waketime ~ as.double(waketime - bedtime + 1) * 86400,
      waketime > bedtime ~ as.double(waketime - bedtime) * 86400
    ),
    reject_Intervals = if_else(
      ActivityType == "Rejected sitting" & StartTime < waketime,
      Intervals,
      NA_real_
    ),
    rejected_sleep = sum(reject_Intervals, na.rm = TRUE),
    sleepdur = sleepdur - rejected_sleep,
    sleepdur = if_else(
      sleepdur < 0,
      NA_real_,
      sleepdur
    )
  ) %>%
  select(ParticipantId, refday, bedtime, waketime, sleepdur) %>%
  distinct() %>%
  ungroup() %>% group_by(ParticipantId) %>%
  mutate(md_vdb_sleepdur = if_else(
    sum(!is.na(sleepdur)) >= 3,
    median(sleepdur, na.rm = TRUE),
    NA_real_
  )) %>%
  select(ParticipantId, md_vdb_sleepdur) %>%
  distinct() %>%
  ungroup()