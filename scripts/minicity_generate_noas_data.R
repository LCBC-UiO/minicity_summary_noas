# Loops over Event logs created by running LCBC's MiniCity-task and generates summary statistics formatted for use
# in the NOAS database at LCBC.

library(tidyverse)
library(here)
source(here("scripts","add_check_digit.R"))

# Find all event logs in data
logs <- list.files(path = here("data"), recursive = TRUE, pattern = "*Events.csv")

# Import NOAS IDs
noas_ids <- readr::read_tsv(here("data","ids.tsv"))
last_id_wo_ctrl <- 62

# Initialize dataframe to be used for results
minidata <- tribble(
  ~subject_id, ~project_id, ~wave_code, ~target_hits, ~dfp_mean, ~dfp_median, ~dfp_std
)

# Loop through all event logs
for (log in 1: length(logs))
{
  current_log <- readr::read_csv(
    here("data",logs[log]),
    col_names = FALSE,
    col_types = list(.default = readr::col_guess())
  )
  
  # Add header to current log
  colnames(current_log) <- c("Id","Time","Event","Accuracy","Target","OptimalDistance","CurrentDistance")
  
  current_log_targets<-sum(current_log$Accuracy,na.rm=TRUE) # log number of targets reached
  
  # Calculate average overshoot past optimal distance for targets
  # TODO: Lots of duplicate code here. Consider refactor.
  current_overshoot_mean <- current_log %>% filter(Accuracy == 1) %>%
    mutate(Overshoot = CurrentDistance / OptimalDistance) %>%
    summarise(mean_distance = mean(Overshoot, na.rm = TRUE))
  
  current_overshoot_median <- current_log %>% filter(Accuracy == 1) %>%
    mutate(Overshoot = CurrentDistance / OptimalDistance) %>%
    summarise(mean_distance = median(Overshoot, na.rm = TRUE))
  
  current_overshoot_sd <- current_log %>% filter(Accuracy == 1) %>%
    mutate(Overshoot = CurrentDistance / OptimalDistance) %>%
    summarise(mean_distance = sd(Overshoot, na.rm = TRUE))
  
  # Find wave by checking the week folder the log exists in.
  # A bit hacky, but the info is not stored in the log itself
  path_split<-strsplit(logs[log],split="/")[[1]]
  
  if (!is.na(match("Wave_01",path_split))){ # if path contains a "Wave_01" folder, set wave to 1
    wave <- "1"
  } else if (!is.na(match("Wave_02",path_split))){
    wave <- "2"
  } else if (!is.na(match("Wave_03",path_split))){
    wave <- "3"
  } else if (!is.na(match("Wave_04",path_split))){
    wave <- "4"
  } else {
    print(paste("Warning: Did not find Wave folder in log ",logs[log],". Logging wave as NA. Check folder structure (see Readme)", sep = ""))
    wave <- "NA"
  }
  
  # Find ID in log and convert to project ID format
  log_id <- current_log[[1,1]]
  if(nchar(log_id) == 4){
    long_id <- 1700000 + round(log_id / 1000) + (log_id %% 1000) * 10
  } else if(nchar(log_id) == 7) {
    long_id <- log_id
  } else {
    long_id <- log_id
    print(paste("WARNING: Incorrect ID format ",log_id," for log ", logs[log]))
  }
  
  subject_id <- add_check_digit(long_id)
  has_ctrl <- FALSE
  no_ctrl <- FALSE
  # Correct for subjects without control digits.
  # TODO: Pretty inelegant. Consider refactor
  if (dim(noas_ids %>% filter(noas_ids == long_id))[1] == 1) {no_ctrl <- TRUE}
  if (dim(noas_ids %>% filter(noas_ids == subject_id))[1] == 1) {has_ctrl <- TRUE}
  if (no_ctrl && has_ctrl && (long_id != subject_id)){print(paste("WARNING: ID duplicate: ",long_id))}
  if (!no_ctrl && !has_ctrl){print(paste("WARNING: Missing ID in list: ",long_id))}
  if (no_ctrl && !has_ctrl){subject_id <- long_id}
  
  # Add current log's data as a row to the output dataframe
  minidata <- minidata %>% add_row(subject_id = subject_id, project_id = "S2C",wave_code = wave,
                                   target_hits = current_log_targets, dfp_mean = current_overshoot_mean[[1]],
                                   dfp_median = current_overshoot_median[[1]],dfp_std = current_overshoot_sd[[1]])
}

# Write data to results
minidata <- minidata %>% mutate(across(where(is.numeric), ~ round(., 5))) # set float precision for output
write_tsv(minidata,here("results",paste(Sys.Date(),"_minicity_s2c",".tsv",sep = "")))

