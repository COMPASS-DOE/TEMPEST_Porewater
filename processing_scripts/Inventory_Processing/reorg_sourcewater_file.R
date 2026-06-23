# Created by Stephanie Wilson

process_sce_inventory <- function(dat){
  
  inventory_new <- dat %>%
    pivot_longer(
      cols = c(
        SO4_Cl_H2S,
        Fe,
        DOC,
        NUTR,
        CDOM,
        Isotopes,
        SPE, 
        GHG
      ),
      names_to = "Analyte",
      values_to = "Volume_mL"
    ) %>%
    #mutate(
     # Depth_cm = as.integer(Depth_cm)
    #) %>%
    mutate(
      Volume_mL = as.character(Volume_mL),
      Volume_mL = str_trim(Volume_mL),
      Volume_mL = if_else(
        Volume_mL %in% c("-", "–", "—", "", "NA", "n/a", "N/A"),
        NA_character_,
        Volume_mL
      ),
      Volume_mL = as.numeric(Volume_mL)
    ) %>%
    filter(!is.na(Volume_mL)) %>%
    mutate(
      Analyte = recode(
        Analyte,
        SO4_Cl_H2S = "SO4/Cl/H2S"
      ),
      Project = paste("COMPASS:", Project),
      Collection_Date = as.Date(as.character(Collection_Date), format = "%Y%m%d"),
      Collection_Date_YYYYMMDD = format(Collection_Date, "%Y%m%d"),
      #Evacuation_date_YYYMMDD = Collection_Date_YYYYMMDD,
      sample_location = if_else(Analyte == "CDOM", "SEQUIM", "SERC"),
      analyte_code = recode(
        Analyte,
        "SO4/Cl/H2S" = "SO4",
        Fe = "FE",
        NUTR = "NUTR",
        DOC = "DOC",
        CDOM = "CDOM",
        Isotopes = "ISO",
        SPE = "SPE"
      )#,
      #depth_code = paste0(Depth_cm, "cm")
    ) %>%
    mutate(
      Sample_ID = paste(
        "TMP",
        case_when(
          Plot == "Estuary" ~ "EST",
          Plot == "Saltwater" ~ "SW",
          Plot == "Freshwater" ~ "FW",
          TRUE ~ NA_character_
        ),
        "SCE",
        #Grid_Square,
        #depth_code,
        analyte_code,
        Collection_Date_YYYYMMDD,
        Timepoint,
        sep = "_"
      ),
      Vial_ID = paste(
        case_when(
          Plot == "Estuary" ~ "EST",
          Plot == "Saltwater" ~ "SW",
          Plot == "Freshwater" ~ "FW",
          TRUE ~ NA_character_
        ),
        #Grid_Square,
        #Depth_cm,
        analyte_code,
        Collection_Date_YYYYMMDD,
        Timepoint,
        sep = "_"
      )
    ) %>%
    mutate(
      Sample_ID = str_replace(
        Sample_ID,
        fixed("_SO4/Cl/H2S_"),
        "_SO4_"
      ),
      Vial_ID = str_replace(
        Vial_ID,
        fixed("_SO4/Cl/H2S_"),
        "_SO4_"
      )
    ) %>%
    mutate(
      Project = "COMPASS: TEMPEST",
      sample_location = "SERC"
    ) %>%
    select(
      Project,
      Plot,
      Sample_ID,
      Volume_mL,
      Vial_ID,
      sample_location,
      Analyte,
      Collection_Date_YYYYMMDD,
      Timepoint,
      everything()
    ) %>%
    mutate(
      #Evacuation_date_YYYMMDD = as.numeric(Evacuation_date_YYYMMDD),
      Collection_Date_YYYYMMDD = as.numeric(Collection_Date_YYYYMMDD),
      Collection_Start_Time_24hrs = as.numeric(Collection_Start_Time_24hrs),
      Collection_End_Time_24hrs = as.numeric(Collection_End_Time_24hrs)
    )
  
  return(inventory_new)
}