#Porewater pH

process_monthly_pw_pH <- function(dat){
  
  inventory_new <- dat %>%
    mutate(
      Depth_cm = as.integer(Depth_cm)
    ) %>%
    mutate(
      pH = as.character(pH),
      pH = str_trim(pH),
      pH = if_else(
        pH %in% c("-", "–", "—", "", "NA", "n/a", "N/A"),
        NA,
        pH
      ),
      pH = as.numeric(pH)
    ) %>%
    mutate(
      Temperature_C = as.character(Temperature_C),
      Temperature_C = str_trim(Temperature_C),
      Temperature_C = if_else(
        Temperature_C %in% c("-", "–", "—", "", "NA", "n/a", "N/A"),
        NA,
        Temperature_C
      ),
      Temperature_C = as.numeric(Temperature_C)
    ) %>% 
    mutate(Cond_units = case_when(
      Plot == "Control|Freshwater" ~ "uS/cm", 
      str_detect(Conductivity, "uS/cm") ~ "uS/cm",
      str_detect(Conductivity, "mS/cm") ~ "mS/cm",
      str_detect(Notes, "uS/cm") ~ "uS/cm",
      str_detect(Notes, "mS/cm") ~ "mS/cm",
      str_detect(Notes, "micro") ~ "uS/cm",
      TRUE ~ "mS/cm")) %>% 
    mutate(
      Conductivity = as.character(Conductivity),
      Conductivity = gsub("uS/cm", "", Conductivity), 
      Conductivity = gsub("mS/cm", "", Conductivity), 
      Conductivity = str_trim(Conductivity),
      Conductivity = if_else(
        Conductivity %in% c("-", "–", "—", "", "NA", "n/a", "N/A"),
        NA,
        Conductivity
      ),
      Conductivity = as.numeric(Conductivity)
    ) %>%
    filter(!if_all(c(pH, Temperature_C, Conductivity), is.na)) %>% 
    mutate(
      Site = "TEMPEST", 
      Collection_Date = as.Date(Collection_Date, format = "%d-%b-%Y"),
      Date_YYYYMMDD = format(Collection_Date, "%Y%m%d")) %>% 
    rename(
      Time_24hr = Collection_End_Time_24hrs,
      Location = Grid_Square, 
      "EDT/EST" = EST_EDT, 
      Cond = Conductivity, 
      Temp_C = Temperature_C, 
      "Colored_Y/N" = Colored
    )  %>% 
    mutate(Instrument = case_when(
      str_detect(Notes, "ProDSS") ~ "ProDSS",
      TRUE ~ "Oakton")) %>% 
    select(
      Site,
      Plot,
      Location,
      Depth_cm,
      Date_YYYYMMDD,
      'EDT/EST',
      Time_24hr,
      pH,
      Cond,
      Cond_units, # Need to add a column for conductivity units to the datasheets
      Temp_C, 
      'Colored_Y/N', 
      Instrument, # Need to add to the datasheet entry instrument used
      Notes, 
      everything()
    ) %>%
    mutate(
      Date_YYYYMMDD = as.numeric(Date_YYYYMMDD),
      Time_24hr = as.numeric(Time_24hr), 
      Depth_cm = as.numeric(Depth_cm), 
      Cond = as.numeric(Cond),
      Temp_C = as.numeric(Temp_C)
    ) 
  
  return(inventory_new)
}
