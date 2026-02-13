## This script imports the metadata for pooled pore water collection during
## the June 2022 TEMPEST event and organizes it in a format that
## can be used for further data manipulation.
## Raw Data are read in from GoogleDrive
## 
## Created: 2023-01 by Julia McElhinny for TEMPEST, with some code edited from
## processing scripts written by Allison Myers-Pigg.
## Updated for systeme level analysis on 10/5/23

# 1. Load Packages -------------------------------------------------------------

require(pacman)
pacman::p_load(tidyverse,
               broom,
               janitor,
               googlesheets4,
               googledrive)


# 2. Load Data -----------------------------------------------------------------

# check working directory - should be the root of the repository
getwd()

# link directory to google drive location
directory <- "https://drive.google.com/drive/folders/1l6ChfOs6wU82ujsoAUw9dGNfoOVL4E8R"

# check list of files found at the link
drive_ls(directory)

# filter to necessary files
# June 2022 event
files_pulse <- drive_ls(directory) %>%
  filter(grepl("TMP Data", name))

# download files to local drive
lapply(c(files_pulse$name), drive_download, 
       overwrite = TRUE)

lapply(split(files_pulse, seq_len(nrow(files_pulse))), 
       drive_download, overwrite = TRUE)


# some files were not xlsx format in file names but downloaded as such - updating  
# names to reflect this
for (n in 1:nrow(files_pulse)) {
  
  # if the name doesn't have the excel extension, add it
   if (!grepl(".xlsx", files_pulse$name[n], fixed = TRUE)) {
     files_pulse$name[n] <- paste0(files_pulse$name[n], ".xlsx")
   }
}

# read in data by named tabs - creates lists of tibbles
control_pulse <- lapply(files_pulse$name, function(x) readxl::read_excel(path = x,
                                                                         sheet = "CONTROL", 
                                                                          range = cell_rows(7:17)))

freshwater_pulse <- lapply(files_pulse$name, function(x) readxl::read_excel(path = x,
                                                                            sheet = "FRESHWATER", 
                                                                            range = cell_rows(7:17)))

seawater_pulse <- lapply(files_pulse$name, function(x) readxl::read_excel(path = x,
                                                                           sheet = "SEAWATER", 
                                                                           range = cell_rows(7:17)))


# 3. Pulse Data Edits ----------------------------------------------------------

## edit each data frame in the lists to have the collection time period
# pull time periods out of spreadsheet names
pulse_times <- as.data.frame(substr(files_pulse$name, 1,2))

# make sure the number date frame of each list matches that same number time
# in the pulse_times list
b = 1

for (a in 1:nrow(pulse_times)) {
  
  control_pulse[[b]] <- mutate(control_pulse[[b]], timepoint = pulse_times[a,1])
  freshwater_pulse[[b]] <- mutate(freshwater_pulse[[b]], timepoint = pulse_times[a,1])
  seawater_pulse[[b]] <- mutate(seawater_pulse[[b]], timepoint = pulse_times[a,1])
  
  # set index value for next round
  b = b + 1 
}

# make list of new column names for better readability
pulse_column_names <- c("Plot", "Grid", "DOC", "Ions", "Water_Isotopes", "HR-MS", 
                        "Saccharides", "Amino_Acids", "Pooled", "CDOM_Subsample",
                        "Time_Notes", "Notes", "Total_Volume", "Evacuated_cb", "Timepoint")

for (c in 1:length(control_pulse)) {

    # change column names  
    colnames(control_pulse[[c]]) <- pulse_column_names
      # make all columns character for future merging
      control_pulse[[c]] <- mutate(control_pulse[[c]], across(everything(), as.character))
  
    colnames(freshwater_pulse[[c]]) <- pulse_column_names
      freshwater_pulse[[c]] <- mutate(freshwater_pulse[[c]], across(everything(), as.character))
      
    colnames(seawater_pulse[[c]]) <- pulse_column_names
      seawater_pulse[[c]] <- mutate(seawater_pulse[[c]], across(everything(), as.character))
}

# merge all 15 tibbles together into one data frame
list <- c(control_pulse, freshwater_pulse, seawater_pulse)

pulse_all_plots <- bind_rows(list)
pulse_all_plots <- as.data.frame(pulse_all_plots)

# edit different forms of NA values
pulse_all_plots[pulse_all_plots == "—"] <- NA
pulse_all_plots[pulse_all_plots == "NA"] <- NA
pulse_all_plots[pulse_all_plots == "--"] <- NA

# total volume column contains some notes - separate those out
total_volume_notes <- pulse_all_plots %>%
  filter(grepl("discarded|keep|threw", Total_Volume)) # can put these notes into the Notes column

# total volume cells with notes replaced with NA (to indicate collection but no use for data purposes)
pulse_all_plots$Notes[grepl("discarded|keep|threw", pulse_all_plots$Total_Volume)] <- total_volume_notes$Total_Volume
pulse_all_plots$Total_Volume[grepl("discarded|keep|threw", pulse_all_plots$Total_Volume)] <- NA
pulse_all_plots$Pooled[grepl("discarded|keep|threw", pulse_all_plots$Notes)] <- NA


# 4. Manipulate Lysimeter Collection Time/Date Information ---------------------
# this code is unnecessary if read_excel reads in the times the right way in the column
# key is that if one tab has some times entered, all rows need to have one, even if 00:00 needs to be entered

# read in the time information from the sheets for individual lysimeters
#SW_T4 <- readxl::read_excel(path = files_pulse$name[5],
                   #sheet = "SEAWATER", 
                   #skip = 6) %>%
              #mutate(Time_Notes = format(Time_Notes, "%I:%M:%S %p")) %>%
              #mutate(Timepoint = "T4")

#SW_T2 <- readxl::read_excel(path = files_pulse$name[4],
                            #sheet = "SEAWATER", 
                            #skip = 6) %>%
  #mutate(Time_Notes = format(Time_Notes, "%I:%M:%S %p")) %>%
  #mutate(Timepoint = "T2")

#FW_T1 <- readxl::read_excel(path = files_pulse$name[1],
                            #sheet = "FRESHWATER", 
                            #skip = 6) %>%
  #mutate(Time_Notes = format(Time_Notes, "%I:%M:%S %p")) %>%
  #mutate(Timepoint = "T1")

# join the three data frames together where this data was collected
#colnames(SW_T4) <- pulse_column_names
  #SW_T4<- mutate(SW_T4, across(everything(), as.character))

#colnames(SW_T2) <- pulse_column_names
  #SW_T2<- mutate(SW_T2, across(everything(), as.character))
  
#colnames(FW_T1) <- pulse_column_names
  #FW_T1<- mutate(FW_T1, across(everything(), as.character))
  #FW_T1$Time_Notes[6] <- NA #entered as 00:00 in the parent google sheet to avoid read-in errors
  #FW_T1 <- FW_T1[-11,]
  
#collection_times <- rbind(SW_T4, SW_T2)
#collection_times <- rbind(collection_times, FW_T1)


# 5. Finalize Compiled Data Frame of Collection Volumes ------------------------

# replace incorrectly loaded time information
#pulse_all_plots$Time_Notes[!is.na(pulse_all_plots$Time_Notes)] <- collection_times$Time_Notes

# edit time formats read in automatically by read_excel
pulse_all_plots$Time_Notes[pulse_all_plots$Plot == "FRESHWATER" & pulse_all_plots$Timepoint == "T1" & pulse_all_plots$Grid == "B4"] <- NA

# fix variable types
pulse_all_plots[,c(3:10,13,14)] <- as.numeric(unlist(pulse_all_plots[,c(3:10,13,14)]))
pulse_all_plots[,"Plot"] <- factor(pulse_all_plots[,"Plot"], levels = c("CONTROL", "FRESHWATER", "SEAWATER"))
pulse_all_plots$Grid <- as.factor(pulse_all_plots$Grid)
pulse_all_plots$Timepoint <- factor(pulse_all_plots$Timepoint, levels = c("T0", 
                                                                          "T1", "T2",
                                                                          "T3", "T4"))

# make R recognize datetime and extract just time information as character values
pulse_all_plots$Time_Notes <- as.POSIXct(pulse_all_plots$Time_Notes, format = "%Y-%m-%d %H:%M:%S")
pulse_all_plots$Time_Notes <- format(pulse_all_plots$Time_Notes, format = "%H:%M:%S")


# 6. Read In Date/Time Info for Each Timepoint ---------------------------------

# make list of cells to extract
sheet_cells <- c("B3", "D3", "F3")

# make list of tab names
tab_names <- c("CONTROL", "FRESHWATER", "SEAWATER")

# pull individual cells out of excel spreadsheets
# control date
control_dates <- lapply(files_pulse$name, function(x) readxl::read_excel(path = x,
                                                                         sheet = tab_names[1], 
                                                                         range = sheet_cells[1]))
# control start time
control_start <- lapply(files_pulse$name, function(x) readxl::read_excel(path = x,
                                                                         sheet = tab_names[1], 
                                                                         range = sheet_cells[2]))
# control end time
control_end <- lapply(files_pulse$name, function(x) readxl::read_excel(path = x,
                                                                         sheet = tab_names[1], 
                                                                         range = sheet_cells[3]))
# fw date
fw_dates <- lapply(files_pulse$name, function(x) readxl::read_excel(path = x,
                                                                         sheet = tab_names[2], 
                                                                         range = sheet_cells[1]))
# fw start time
fw_start <- lapply(files_pulse$name, function(x) readxl::read_excel(path = x,
                                                                         sheet = tab_names[2], 
                                                                         range = sheet_cells[2]))
# fw end time
fw_end <- lapply(files_pulse$name, function(x) readxl::read_excel(path = x,
                                                                         sheet = tab_names[2], 
                                                                         range = sheet_cells[3]))

sw_dates <- lapply(files_pulse$name, function(x) readxl::read_excel(path = x,
                                                                    sheet = tab_names[3], 
                                                                    range = sheet_cells[1]))

sw_start <- lapply(files_pulse$name, function(x) readxl::read_excel(path = x,
                                                                    sheet = tab_names[3], 
                                                                    range = sheet_cells[2]))

sw_end <- lapply(files_pulse$name, function(x) readxl::read_excel(path = x,
                                                                  sheet = tab_names[3], 
                                                                  range = sheet_cells[3]))

dates_list <- list(control_dates, fw_dates, sw_dates)
start_list <- list(control_start, fw_start, sw_start)
end_list <- list(control_end, fw_end, sw_end)

## extract values from the lists and add timepoint/plot information
# make data frame to drop those values in
dates_times <- data.frame(matrix(NA,
                                     nrow=15,
                                     ncol =5))
colnames(dates_times) <- c("Plot", "Timepoint", "Date", "Start_time", "End_time")

# set index value for the row number where data is going to be inserted
d = 1

# cycle through plots
for (f in 1:length(tab_names)) {
  
  # for each plot cycle through the 5 timepoints
  for (e in 1:nrow(pulse_times)) {
  
    # check if a value is present
    # if present value, pull it and put it in the "date" column
    if (!is.na(colnames(dates_list[[f]][[e]])[1])) {
    dates_times[d,3] <- colnames(dates_list[[f]][[e]])[1]
      }

    # start time column
    if (!is.na(colnames(start_list[[f]][[e]])[1])) {
    dates_times[d,4] <- colnames(start_list[[f]][[e]])[1]
      }

    # end time column  
    if (!is.na(colnames(end_list[[f]][[e]])[1])) {
    dates_times[d,5] <- colnames(end_list[[f]][[e]])[1]
    }
    
    # add time point information
    dates_times[d,2] <- pulse_times[e,1]
    
    # add grid information
    dates_times[d,1] <- tab_names[f]
  
    # increase row number so no overwriting    
    d = d + 1
  }
}

## further data frame edits
# edit NA values
dates_times[dates_times == "—"] <- NA

# edit variable types
dates_times[,1:2] <- lapply(dates_times[,1:2], function(x) as.factor(x))
dates_times[,3:5] <- lapply(dates_times[,3:5], function(x) as.numeric(x))

# make excel serial dates into MDY
dates_times$Date <- excel_numeric_to_date(dates_times$Date)

# make decimal times into HMS
dates_times[,4:5] <- lapply(dates_times[,4:5], function(x) convert_to_datetime(x))
dates_times[,4:5] <- lapply(dates_times[,4:5], function(x) format(x, format = "%H:%M:%S"))

# replace NA dates with dates listed for that time point on other plots
dates_tofill <- dates_times %>%
  filter(!is.na(Date)) %>%
  select(Timepoint, Date)
dates_tofill <- unique(dates_tofill)

for (g in 1:nrow(dates_times)) {
  if (is.na(dates_times$Date[g])) {
    
    # get the date for replacing based on match between timepoint
    missing_date <- dates_tofill$Date[dates_tofill$Timepoint == dates_times$Timepoint[g]]
    
    # do the replacement
    dates_times$Date[g] <- missing_date
  
  }
}


# 7. Combine Data Frames for Final Export --------------------------------------

TMP_Event_June2022_Metadata <- left_join(pulse_all_plots, dates_times)

#reorder columns for easier reading
TMP_Event_June2022_Metadata <- TMP_Event_June2022_Metadata[,c(1,15:18,2:10,13,14,11,12)]

# edit plot names to acronyms
TMP_Event_June2022_Metadata <- TMP_Event_June2022_Metadata %>%
  mutate(Plot = case_when(
    Plot == "CONTROL" ~ "C",
    Plot == "FRESHWATER" ~ "FW",
    Plot == "SEAWATER" ~ "SW"
  ))

# write out as CSV with all volumes collected by grid by plot by time for future records
write.csv(TMP_Event_June2022_Metadata, "TMP_Event_June2022_gridlevel_volumes.csv",
        row.names = FALSE)


# 8. Create CSV File for Further Pooled Sample Calculations --------------------

# trim down columns
tmp_event_pooled_volumes <- TMP_Event_June2022_Metadata[,-c(7:12,14:17)]

# replace NA with 0 for calculations
tmp_event_pooled_volumes$Pooled[is.na(tmp_event_pooled_volumes$Pooled)] <- 0

# calculate total pooled volume for each of the plots at each time period
tmp_event_pooled_sum <- tmp_event_pooled_volumes %>%
  group_by(Plot, Timepoint) %>%
  summarize(total_pooled_volume = sum(Pooled))

# merge the total volume back with original data frame
tmp_event_pooled_volumes_tidy <- left_join(tmp_event_pooled_volumes, tmp_event_pooled_sum)

# write out RDS file
write_rds(tmp_event_pooled_volumes_tidy, file = "tmp_2022_event_pooled_pw_volumes_tidy.rds")

# alter format for easier viewing 
# tmp_event_pooled_volumes <- tmp_event_pooled_volumes %>%
  # spread(Grid, Pooled)
# would need more edits to make sure this function works to only have one row per plot/time combo


# 9. Edit Source Water Metadata ------------------------------------------------

# load source water info
files_source_water <- drive_ls(directory) %>%
  filter(grepl("Source", name))

# download files to local drive
lapply(c(files_source_water$name), drive_download, 
       overwrite = TRUE)

# add ".xlsx" to file name
files_source_water$name <- paste0(files_source_water$name, ".xlsx")

# make data frame 
source_water_meta <- readxl::read_excel(files_source_water$name)
source_water_meta <- source_water_meta %>%
  clean_names()

# save complete source water data as csv file
#write.csv(source_water_meta, "./data/raw/DOC meta/June2022_Event_SourceWater_Metadata.csv")

# edit water type column
source_water_meta_edit <- source_water_meta %>%
  rename(Plot = source_water_type,
         Date = date,
         Start_time = time) %>%
  mutate(Plot = case_when(Plot == "Freshwater" ~ "FW",
                          Plot == "Seawater" ~ "SW"),
         Grid = "SOURCE",
         Timepoint = gsub(".*SOURCE_", "", label_name))

# edit the time format
source_water_meta_edit$Start_time <- as.POSIXct(source_water_meta_edit$Start_time, format = "%Y-%m-%d %H:%M:%S")
source_water_meta_edit$Start_time <- format(source_water_meta_edit$Start_time, format = "%H:%M:%S")

# cut columns to match the porewater metadata
source_water_datetime <- source_water_meta_edit %>%
  select(Plot, Grid, Timepoint, Date, Start_time) %>%
  mutate(End_time = NA)


# 10. Make Final Date/Time Metadata File for June 2022 Event -------------------

# separate out the unique values of start time and timepoint
pw_timepoint_times <- TMP_Event_June2022_Metadata %>%
  ungroup() %>%
  group_by(Timepoint) %>%
  select(Date, Start_time) %>%
  unique()

# make a copy of the full metadata data frame
event_pw_datetime <- TMP_Event_June2022_Metadata

# fill in start time gaps - pulling the earliest start time reported based on other plots at same timepoint
# T1 control plot set to 12:45
event_pw_datetime$Start_time[is.na(event_pw_datetime$Start_time) & event_pw_datetime$Timepoint == "T1"] <- "12:45:00"

# T2 freshwater set to 16:14
event_pw_datetime$Start_time[is.na(event_pw_datetime$Start_time) & event_pw_datetime$Timepoint == "T2"] <- "16:14:00"

# T3 freshwater set to 9:38
event_pw_datetime$Start_time[is.na(event_pw_datetime$Start_time) & event_pw_datetime$Timepoint == "T3"] <- "9:38:00"

# reduce number of columns to merge
event_pw_datetime <- event_pw_datetime %>%
  select(Plot, Timepoint, Date, Start_time, End_time) %>%
  mutate(Grid = NA) %>%
  unique()

# merge source data and porewater data together
June2022_event_source_pw_datetime <- rbind(event_pw_datetime, source_water_datetime)


# 11. Final Clean-Up -----------------------------------------------------------

# write out csv of event dates and times
write_rds(June2022_event_source_pw_datetime, file = "./data/for processing/TMP_Event_June2022_META_PW_SOURCE_DateTime.rds")
write.csv(June2022_event_source_pw_datetime, "./data/for processing/TMP_Event_June2022_META_PW_SOURCE_DateTime.csv",
          row.names = FALSE)

# delete google sheet files
file.remove(c(files_pulse$name, files_source_water$name))

