# Function to parse digitized field porewater data sheet
# Created by Stephanie Wilson

parse_event_porewater_file <- function(file) {

    project <- sub("^.*?:\\s*", "", file[1,1])
    event <- sub("^.*?:\\s*", "", file[2,1])
    measurement <- sub("^.*?:\\s*", "", file[3,1])
    timepoint <- file[4, 2] |> pull()
    collection_date <- file[5, 2] |> pull()
    
    collection_date <- ifelse(
      is.na(collection_date),
      NA_character_,
      format(as.numeric(collection_date), scientific = FALSE, trim = TRUE)
    )
    
    personnel_header <- file[6, 2] |> pull()

    #look for EST/EDT and report it out
    EST_EDT <- file[1:5, ] %>%
      mutate(across(everything(), as.character)) %>%
      unlist() %>%
      str_trim() %>%
      str_subset("^EST$|^EDT$") %>%
      unique()
    
    #If there is no EST/EDT it will be filled with EST_EDT 
    EST_EDT <- if(length(EST_EDT) == 0) NA_character_ else EST_EDT[1]

    Collection_End_Time_24hrs <- "NA"


    # Find data start
    #-----------------------------
    data_start <- which(file[[1]] %in% c("Control", "Saltwater", "Freshwater"))[1]
    
    #this is here as a safe gaurd in case there isn't one of the start words in the file
    if(is.na(data_start)) {
      stop("Could not find data start row")
    }

    # Header rows immediately above data
    header1 <- file[data_start - 3, ]
    header2 <- file[data_start - 2, ]
    header3 <- file[data_start - 1, ]

    # Combine multi-row headers
    headers <- paste(
        as.character(header1),
        as.character(header2),
        as.character(header3),
        sep = "_"
    )

    # Clean up NAs and extra underscores
    headers <- headers %>%
        str_replace_all("NA", "") %>%
        str_replace_all("_+", "_") %>%
        str_replace_all("^_|_$", "")

    #create a look up table
    rename_map <- c(
        "Plot" = "Plot",
        "Grid" = "Grid_Square",
        "Depth" = "Depth_cm",
        "SO4" = "SO4_Cl_H2S", #needs to be changed later to make it SO4/Cl/H2S
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
        "Personnel" = "Personnel"
    )

    for (pattern in names(rename_map)) {
        headers[str_detect(headers, pattern)] <- rename_map[[pattern]]
    }


    # Extract data from each in table
    #-----------------------------
    dat <- file[data_start:nrow(file), ]

    names(dat) <- headers

    # Remove completely empty rows
    dat <- dat %>%
        filter(!if_all(everything(), is.na))

    # Fill missing Plot/Grid values
    # (your sheet leaves them blank
    # for deeper depths)
    #-----------------------------
    dat <- dat %>%
        fill(
            starts_with("Plot"),
            starts_with("Grid"),
            .direction = "down"
        )

    #-----------------------------
    # Add metadata columns
    #-----------------------------
    dat <- dat %>%
        mutate(
            Project = project,
            Event = event,
            Measurement = measurement,
            Timepoint = timepoint,
            Collection_Date = collection_date,
            Collection_Personnel = personnel_header,
            Collection_End_Time_24hrs = Collection_End_Time_24hrs,
            EST_EDT = EST_EDT
        )


    #set a column order
    desired_order <- c(
        "Project",
        "Event",
        "Measurement",
        "Timepoint",
        "Collection_Date",
        "Collection_Start_Time_24hrs",
        "Collection_End_Time_24hrs",
        "EST_EDT",
        "Collection_Personnel",
        "Plot",
        "Grid_Square",
        "Depth_cm",
        "SO4_Cl_H2S",
        "Fe",
        "DOC",
        "NUTR",
        "CDOM",
        "pH",
        "Conductivity",
        "Temperature_C",
        "Isotopes",
        "SPE",
        "Extra Metadata_Time of Sample Collection (24 hrs)",
        "Additional Unneeded Extracted Volume (mL)",
        "Total Volume Extracted (mL)",
        "Colored",
        "Notes",
        "Personnel"
    )

    dat %>% select(any_of(desired_order))

}
