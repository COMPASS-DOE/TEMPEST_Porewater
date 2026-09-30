# Function to reorganize digitized field porewater data sheet
# Created by Stephanie Wilson

process_event_pooled_inventory <- function(dat){
  
  inventory_new <- dat %>%
    mutate(
      Depth_cm = as.integer(Depth_cm)
    ) %>%
    mutate(
      SPE = as.character(SPE),
      SPE = str_trim(SPE),
      SPE = if_else(
        SPE %in% c("-", "–", "—", "", "NA", "n/a", "N/A"),
        NA_character_,
        SPE
      ),
      SPE = as.numeric(SPE)
    ) %>%
    pivot_wider(
      id_cols = c(Plot, Depth_cm, Collection_Date, Timepoint), 
      names_from = Grid_Square, 
      values_from = SPE, 
      names_prefix = "Grid_", 
    ) %>% 
    mutate(Total_Volume_mL = rowSums(across(c(Grid_B4, 
                                              Grid_C3, 
                                              Grid_C6, 
                                              Grid_D5, 
                                              Grid_E3, 
                                              Grid_F4, 
                                              Grid_F6, 
                                              Grid_H3, 
                                              Grid_H6, 
                                              Grid_I5)), na.rm = TRUE)) %>% 
    filter(Total_Volume_mL > 0) %>%
    mutate(
      Project = "COMPASS: TEMPEST", 
      Collection_Date = as.Date(Collection_Date, format = "%Y%m%d"),
      Collection_Date_YYYYMMDD = format(Collection_Date, "%Y%m%d"),
      Evacuation_date_YYYMMDD = Collection_Date_YYYYMMDD, ##This is not always true during the event, could be the day before 
      Sample_location = "SEQUIM",
      depth_code = paste0(Depth_cm, "cm"),
      Timepoint = gsub("-", "_", Timepoint)
    ) %>%
    mutate(
      Sample_ID = paste(
        "TMP",
        case_when(
          Plot == "Control" ~ "C",
          Plot == "Saltwater" ~ "SW",
          Plot == "Freshwater" ~ "FW",
          TRUE ~ NA_character_
        ),
        "PW",
        depth_code,
        "SPE",
        Collection_Date_YYYYMMDD,
        Timepoint,
        sep = "_"
      ),
      Vial_ID = paste(
        case_when(
          Plot == "Control" ~ "C",
          Plot == "Saltwater" ~ "SW",
          Plot == "Freshwater" ~ "FW",
          TRUE ~ NA_character_
        ),
        Depth_cm,
        Collection_Date_YYYYMMDD,
        Timepoint,
        sep = "_"
      )
    ) %>%
    mutate(
      Project = "COMPASS: TEMPEST"
      ) %>%
    select(
      Project,
      Plot,
      Sample_ID,
      Sample_location,
      Total_Volume_mL,
      Vial_ID,
      Grid_B4, 
      Grid_C3, 
      Grid_C6, 
      Grid_D5, 
      Grid_E3, 
      Grid_F4, 
      Grid_F6, 
      Grid_H3, 
      Grid_H6, 
      Grid_I5, 
      # Analyte,
      Evacuation_date_YYYMMDD, 
      Collection_Date_YYYYMMDD,
      Timepoint,
      Depth_cm, 
      everything()
    ) %>%
    mutate(
      Evacuation_date_YYYMMDD = as.numeric(Evacuation_date_YYYMMDD),
      Collection_Date_YYYYMMDD = as.numeric(Collection_Date_YYYYMMDD)
      # Collection_Start_Time_24hrs = as.numeric(Collection_Start_Time_24hrs),
      # Collection_End_Time_24hrs = as.numeric(Collection_End_Time_24hrs)
    ) 
  
  return(inventory_new)
}