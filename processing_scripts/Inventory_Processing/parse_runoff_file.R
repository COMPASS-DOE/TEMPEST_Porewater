
parse_runoff_file <- function(path) {
  
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  
  sheets <- excel_sheets(path)
  
  #-------------------------
  # metadata (sheet 1 only)
  #-------------------------
  meta <- read_excel(path, sheet = 1, col_names = FALSE)
  
  project <- sub("^.*?:\\s*", "", meta[1,1])
  event <- sub("^.*?:\\s*", "", meta[2,1])
  measurement <- "Runoff Water"
  
  collection_date <- meta[4, 2] |> pull()
  collection_date <- as.numeric(collection_date)
  
  timepoint <- case_when(
    collection_date == 20260608 ~ "D2",
    collection_date == 20260609 ~ "D3",
    collection_date == 20260610 ~ "D4",
    collection_date == 20260611 ~ "D5",
    TRUE ~ NA_character_
  )
  
  personnel_header <- meta[5, 2] |> pull()
  
  # EST / EDT
  EST_EDT <- meta[1:5, ] %>%
    mutate(across(everything(), as.character)) %>%
    unlist() %>%
    str_trim() %>%
    str_subset("^EST$|^EDT$") %>%
    unique()
  
  EST_EDT <- if(length(EST_EDT) == 0) NA_character_ else EST_EDT[1]
  
  Collection_End_Time_24hrs <- NA_character_
  
  #-------------------------
  # helper: process ONE sheet
  #-------------------------
  process_sheet <- function(file, sheet_name) {
    
    data_start <- which(file[[1]] %in% c("Saltwater", "Freshwater"))[1]
    if (is.na(data_start)) return(NULL)
    
    header1 <- file[data_start - 3, ]
    header2 <- file[data_start - 2, ]
    header3 <- file[data_start - 1, ]
    
    headers <- paste(
      as.character(header1),
      as.character(header2),
      as.character(header3),
      sep = "_"
    ) %>%
      str_replace_all("NA", "") %>%
      str_replace_all("_+", "_") %>%
      str_replace_all("^_|_$", "")
    
    rename_map <- c(
      "Location" = "Plot",
      "SO4" = "SO4_Cl_H2S",
      "Cation" = "Fe",
      "DOC" = "DOC",
      "Nutr" = "NUTR",
      "CDOM" = "CDOM",
      "pH" = "pH",
      "Cond" = "Conductivity",
      "Temp" = "Temperature_C",
      "ISO" = "Isotopes",
      "SPE" = "SPE",
      "Colored" = "Colored",
      "Time of Sample" = "Collection_Start_Time_24hrs",
      "Notes" = "Notes",
      "Personnel" = "Personnel", 
      "T_id" = "T_id"
    )
    
    for (pattern in names(rename_map)) {
      headers[str_detect(headers, pattern)] <- rename_map[[pattern]]
    }
    
    dat <- file[data_start:nrow(file), ]
    names(dat) <- headers
    
    dat <- dat %>%
      filter(!if_all(everything(), is.na)) %>%
      mutate(
        Project = project,
        Event = event,
        Measurement = measurement,
        Timepoint = paste0(timepoint, "_", T_id),
        Collection_Date = collection_date,
        Collection_Personnel = personnel_header,
        Collection_End_Time_24hrs = Collection_End_Time_24hrs,
        EST_EDT = EST_EDT,
        Sheet = sheet_name
      )
    
    desired_order <- c(
      "Project","Event","Measurement","Timepoint",
      "Collection_Date","Collection_Start_Time_24hrs",
      "Collection_End_Time_24hrs","EST_EDT",
      "Collection_Personnel",
      "Plot","Grid_Square","Depth_cm",
      "SO4_Cl_H2S","Fe","DOC","NUTR","CDOM","GHG",
      "pH","Conductivity","Temperature_C","Isotopes","SPE",
      "Colored","Notes","Personnel"
    )
    
    dat %>% select(any_of(desired_order))
  }
  
  #-------------------------
  # loop ALL sheets
  #-------------------------
  all_dat <- lapply(sheets, function(sh) {
    file <- read_excel(path, sheet = sh, col_names = FALSE)
    process_sheet(file, sh)
  })
  
  bind_rows(all_dat)
}