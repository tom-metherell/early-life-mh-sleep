library(haven)
library(tidyverse)
library(magrittr)
library(data.table)
library(dplyr)
library(lubridate)

diary <- read_sav("BCS46_sleep_diary.sav")
diary %<>% zap_labels(.)

newnames <- names(diary %>% select(ends_with("b"))) %>%
  gsub("^(.*?).{0}(.{2})$", "\\1b\\2", .) %>%
  gsub("^(.*?).{1}(.{0})$", "\\1\\2", .)
setnames(diary, old = names(diary %>% select(ends_with("b"))), new = newnames)
rm(newnames)

diary %<>% pivot_longer(
  cols = c(-AgencySerial, -Removed, -dateremoved, -timeremoved, -TimeRemovedbampm), 
  names_to = c(".value", "day"), 
  names_pattern = "(.*)(.)$"
)

### Date correction

is.na(diary[, "sleepdate"]) <- diary[, "sleepdate"] < "2016-01-01"

diary %<>% mutate(dv_sleepdate = case_when(
  day == 1 & !is.na(sleepdate) ~ as_date(sleepdate),
  day %in% 2:7 & !is.na(sleepdate)~ as_date(sleepdate),
  day %in% 2:7 & is.na(sleepdate) & lag(sleepdate) == as_date(sleepdate) - 1 & lead(sleepdate) == as_date(sleepdate) + 1 ~ as_date(lag(sleepdate)) + 1,
  day == 8 & !is.na(sleepdate) ~ as_date(sleepdate),
  day == 8 & is.na(sleepdate) & lag(sleepdate) == as_date(sleepdate) - 1 & lag(sleepdate, 2) == as_date(sleepdate) - 2 ~ as_date(lag(sleepdate)) + 1
  ), .after = sleepdate
)

# Next step: if at least 5 days follow correct pattern, fill in the rest

diary %<>% group_by(AgencySerial) %>% mutate(dv_refdate1 = as_date(dv_sleepdate[day == 1]) + as.integer(day) - 1,
                                             dv_refdate2 = as_date(dv_sleepdate[day == 2]) + as.integer(day) - 2,
                                             dv_refdate3 = as_date(dv_sleepdate[day == 3]) + as.integer(day) - 3,
                                             dv_refdate4 = as_date(dv_sleepdate[day == 4]) + as.integer(day) - 4,
                                             dv_refdate5 = as_date(dv_sleepdate[day == 5]) + as.integer(day) - 5,
                                             dv_refdate6 = as_date(dv_sleepdate[day == 6]) + as.integer(day) - 6,
                                             dv_refdate7 = as_date(dv_sleepdate[day == 7]) + as.integer(day) - 7,
                                             dv_refdate8 = as_date(dv_sleepdate[day == 8]) + as.integer(day) - 8) %>%
  ungroup()

diary %<>% rowwise() %>% mutate(dv_refdate1_acc = case_when(
                                !is.na(dv_refdate1) ~ sum(sapply(2:8, function(x) dv_refdate1 == get(paste0("dv_refdate", x)) | is.na(get(paste0("dv_refdate", x))))),
                                is.na(dv_refdate1) ~ 0L
                                ),
                                dv_refdate2_acc = case_when(
                                  !is.na(dv_refdate2) ~ sum(sapply(c(1, 3:8), function(x) dv_refdate2 == get(paste0("dv_refdate", x)) | is.na(get(paste0("dv_refdate", x))))),
                                  is.na(dv_refdate2) ~ 0L
                                ),
                                dv_refdate3_acc = case_when(
                                  !is.na(dv_refdate3) ~ sum(sapply(c(1:2, 4:8), function(x) dv_refdate3 == get(paste0("dv_refdate", x)) | is.na(get(paste0("dv_refdate", x))))),
                                  is.na(dv_refdate3) ~ 0L
                                ),
                                dv_refdate4_acc = case_when(
                                  !is.na(dv_refdate4) ~ sum(sapply(c(1:3, 5:8), function(x) dv_refdate4 == get(paste0("dv_refdate", x)) | is.na(get(paste0("dv_refdate", x))))),
                                  is.na(dv_refdate4) ~ 0L
                                ),
                                dv_refdate5_acc = case_when(
                                  !is.na(dv_refdate5) ~ sum(sapply(c(1:4, 6:8), function(x) dv_refdate5 == get(paste0("dv_refdate", x)) | is.na(get(paste0("dv_refdate", x))))),
                                  is.na(dv_refdate5) ~ 0L
                                ),
                                dv_refdate6_acc = case_when(
                                  !is.na(dv_refdate6) ~ sum(sapply(c(1:5, 7:8), function(x) dv_refdate6 == get(paste0("dv_refdate", x)) | is.na(get(paste0("dv_refdate", x))))),
                                  is.na(dv_refdate6) ~ 0L
                                ),
                                dv_refdate7_acc = case_when(
                                  !is.na(dv_refdate7) ~ sum(sapply(c(1:6, 8), function(x) dv_refdate7 == get(paste0("dv_refdate", x)) | is.na(get(paste0("dv_refdate", x))))),
                                  is.na(dv_refdate7) ~ 0L
                                ),
                                dv_refdate8_acc = case_when(
                                  !is.na(dv_refdate8) ~ sum(sapply(1:7, function(x) dv_refdate8 == get(paste0("dv_refdate", x)) | is.na(get(paste0("dv_refdate", x))))),
                                  is.na(dv_refdate8) ~ 0L
                                )
)

diary %<>% mutate(max_acc = max.col(across(dv_refdate1_acc:dv_refdate8_acc), ties.method = "first"))
diary[!is.na(diary$max_acc),] %<>% mutate(dv_sleepdate = get(paste0("dv_refdate", max_acc)))

diary %<>% select(-(dv_refdate1:max_acc))

diary[is.na(diary$dv_sleepdate),]$dv_sleepdate <- as_date("1582-10-13") + as.integer(diary[is.na(diary$dv_sleepdate),]$day)

### Time correction
suppressWarnings({
  diary$sleepbed %<>% hms()
  diary$sleepsleep %<>% hms()
  diary$sleepwake %<>% hms()
  diary$sleepup %<>% hms()
})

diary %<>% mutate(dv_sleepbed = case_when(
  !is.na(sleepbed) & sleepbed < hms("12:00:00") & (SleepBedb %in% c("AM", "")) ~ sleepbed,
  !is.na(sleepbed) & sleepbed < hms("12:00:00") & SleepBedb == "PM" ~ sleepbed + hms("12:00:00"),
  !is.na(sleepbed) & sleepbed >= hms("12:00:00") & SleepBedb == "AM" ~ sleepbed - hms("12:00:00"),
  !is.na(sleepbed) & sleepbed >= hms("12:00:00") & (SleepBedb %in% c("PM", "")) ~ sleepbed
), .after = sleepbed)

diary %<>% mutate(dv_sleepsleep = case_when(
  !is.na(sleepsleep) & sleepsleep < hms("12:00:00") & (SleepSleepb %in% c("AM", "")) ~ sleepsleep,
  !is.na(sleepsleep) & sleepsleep < hms("12:00:00") & SleepSleepb == "PM" ~ sleepsleep + hms("12:00:00"),
  !is.na(sleepsleep) & sleepsleep >= hms("12:00:00") & SleepSleepb == "AM" ~ sleepsleep - hms("12:00:00"),
  !is.na(sleepsleep) & sleepsleep >= hms("12:00:00") & (SleepSleepb %in% c("PM", "")) ~ sleepsleep
), .after = sleepsleep)

diary %<>% mutate(dv_sleepwake = case_when(
  !is.na(sleepwake) & sleepwake < hms("12:00:00") & (SleepWakeb %in% c("AM", "")) ~ sleepwake,
  !is.na(sleepwake) & sleepwake < hms("12:00:00") & SleepWakeb == "PM" ~ sleepwake + hms("12:00:00"),
  !is.na(sleepwake) & sleepwake >= hms("12:00:00") & SleepWakeb == "AM" ~ sleepwake - hms("12:00:00"),
  !is.na(sleepwake) & sleepwake >= hms("12:00:00") & (SleepWakeb %in% c("PM", "")) ~ sleepwake
), .after = sleepwake)

diary %<>% mutate(dv_sleepup = case_when(
  !is.na(sleepup) & sleepup < hms("12:00:00") & (SleepUpb %in% c("AM", "")) ~ sleepup,
  !is.na(sleepup) & sleepup < hms("12:00:00") & SleepUpb == "PM" ~ sleepup + hms("12:00:00"),
  !is.na(sleepup) & sleepup >= hms("12:00:00") & SleepUpb == "AM" ~ sleepup - hms("12:00:00"),
  !is.na(sleepup) & sleepup >= hms("12:00:00") & (SleepUpb %in% c("PM", "")) ~ sleepup
), .after = sleepup)

diary %<>% mutate(dv_sleepsleep = case_when(
  dv_sleepsleep < hms("06:00:00") ~ dv_sleepsleep,
  dv_sleepsleep >= hms("06:00:00") & dv_sleepsleep < hms("12:00:00") & dv_sleepwake < hms("12:00:00") ~ dv_sleepsleep + hms("12:00:00"),
  dv_sleepsleep >= hms("06:00:00") & dv_sleepsleep < hms("12:00:00") & dv_sleepwake >= hms("12:00:00") ~ dv_sleepsleep,
  dv_sleepsleep >= hms("12:00:00") & dv_sleepsleep < hms("16:00:00") & dv_sleepwake < hms("12:00:00") ~ dv_sleepsleep - hms("12:00:00"),
  dv_sleepsleep >= hms("12:00:00") & dv_sleepsleep < hms("16:00:00") & dv_sleepwake >= hms("12:00:00") ~ dv_sleepsleep,
  dv_sleepsleep > hms("16:00:00") ~ dv_sleepsleep
))

diary %<>% mutate(dv_sleepsleep = if_else(
  dv_sleepbed >= hms("13:00:00") & dv_sleepsleep >= hms("12:00:00") & dv_sleepsleep < hms("13:00:00"),
  dv_sleepsleep - hms("12:00:00"),
  dv_sleepsleep
))

diary %<>% mutate(dv_sleepwake = case_when(
  dv_sleepwake < hms("14:00:00") ~ dv_sleepwake,
  dv_sleepwake >= hms("14:00:00") & dv_sleepsleep < hms("12:00:00") ~ dv_sleepwake,
  dv_sleepwake >= hms("14:00:00") & dv_sleepsleep >= hms("12:00:00") ~ dv_sleepwake - hms("12:00:00")
))

diary %<>% mutate(dv_sleepbed = case_when(
  dv_sleepbed < dv_sleepup ~ ymd_hms(paste(as_date(dv_sleepdate), dv_sleepbed)),
  dv_sleepbed > dv_sleepup ~ ymd_hms(paste(as_date(dv_sleepdate) - 1, dv_sleepbed))
))

diary %<>% mutate(dv_sleepsleep = case_when(
  dv_sleepsleep < dv_sleepwake ~ ymd_hms(paste(as_date(dv_sleepdate), dv_sleepsleep)),
  dv_sleepsleep > dv_sleepwake ~ ymd_hms(paste(as_date(dv_sleepdate) - 1, dv_sleepsleep))
))