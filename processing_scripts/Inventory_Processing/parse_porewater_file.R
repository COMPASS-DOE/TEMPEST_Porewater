# Function to parse digitized field porewater data sheet
# Created by Stephanie Wilson

parse_porewater_file <- function(file) {

    project <- sub("^.*?:\\s*", "", raw[1,1])
    event <- sub("^.*?:\\s*", "", raw[2,1])
    measurement <- sub("^.*?:\\s*", "", raw[3,1])
    timepoint <- raw[4, 2] |> pull()
    collection_date <- as.character(raw[5, 2] |> pull())
    personnel_header <- raw[6, 2] |> pull()

    #look for EST/EDT and report it out
    EST_EDT <- raw[1:5, ] %>%
        mutate(across(everything(), as.character)) %>%
        unlist() %>%
        str_trim() %>%
        str_subset("^EST$|^EDT$") %>%
        unique()

    Collection_End_Time_24hrs <- "NA"


    # Find data start
    #-----------------------------
    data_start <- which(raw[[1]] %in% c("Control", "Saltwater", "Freshwater"))[1]

    # Header rows immediately above data
    header1 <- raw[data_start - 3, ]
    header2 <- raw[data_start - 2, ]
    header3 <- raw[data_start - 1, ]

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
    dat <- raw[data_start:nrow(raw), ]

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
            EST_EDT = EST_EDT,

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

```

# Now let's pivot the dataframe so that it can be appended to the inventory
```{r, create sample and vial IDs from entered data to match inventory}

#Grab the columns that you need and pivot them longer
inventory_new <- dat %>%
    pivot_longer(
        cols = c(
            SO4_Cl_H2S,
            Fe,
            DOC,
            NUTR,
            CDOM,
            Isotopes,
            SPE
        ),
        names_to = "Analyte",
        values_to = "Volume_mL"
    )

#We are going to remove rows that were not collected (i.e., zero volume)
inventory_new <- inventory_new %>%
    mutate(
        Volume_mL = na_if(Volume_mL, "-"),
    ) %>%
    mutate(
        Volume_mL = as.numeric(Volume_mL)
    ) %>%
    filter(!is.na(Volume_mL))

#We need to recode the SO4 column to match the inventory
inventory_new <- inventory_new %>%
    mutate(
        Analyte = recode(
            Analyte,
            SO4_Cl_H2S = "SO4/Cl/H2S"
        )
    )

#Change the project to match the inventory
inventory_new <- inventory_new %>%
    mutate(
        Project = paste("COMPASS:", Project)
    )

#Change the date into an actual date
inventory_new <- inventory_new %>%
    mutate(
        Collection_Date = as.Date(Collection_Date, format = "%Y%m%d"),
        Collection_Date_YYYYMMDD = format(Collection_Date, "%Y%m%d")
    ) %>%
    mutate(Evacuation_date_YYYMMDD = Collection_Date_YYYYMMDD)

#Add a sample location
inventory_new <- inventory_new %>%
    mutate(
        sample_location = if_else(Analyte == "CDOM", "SEQUIM", "SERC")
    )

#Analyte code to make a sample ID
inventory_new <- inventory_new %>%
    mutate(
        analyte_code = recode(
            Analyte,
            SO4_Cl_H2S = "SO4",
            Fe = "FE",
            NUTR = "NUTR",
            DOC = "DOC",
            CDOM = "CDOM",
            Isotopes = "ISO",
            SPE = "SPE"
        )
    )

#Create a depth code
inventory_new <- inventory_new %>%
    mutate(
        depth_code = paste0(Depth_cm, "cm")
    )

#Now we will create the Sample_ID & Vial ID
inventory_new <- inventory_new %>%
    #Create the Sample_ID
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
            Grid_Square,
            depth_code,
            analyte_code,
            Collection_Date_YYYYMMDD,
            sep = "_"
        )
    ) %>%
    #Create the Vial_ID
    mutate(
        Vial_ID = paste(
            case_when(
                Plot == "Control" ~ "C",
                Plot == "Saltwater" ~ "SW",
                Plot == "Freshwater" ~ "FW",
                TRUE ~ NA_character_  # or Plot if you want to keep other values
            ),
            Grid_Square,
            Depth_cm,
            analyte_code,
            Collection_Date_YYYYMMDD,
            sep = "_"
        )
    ) %>%
    mutate(Sample_ID = str_replace(
        Sample_ID,
        fixed("_SO4/Cl/H2S_"),
        "_SO4_"
    )) %>%
    mutate(Vial_ID = str_replace(
        Vial_ID,
        fixed("_SO4/Cl/H2S_"),
        "_SO4_"
    ))

#add in the fixed columns and set the order
inventory_new <- inventory_new %>%
    mutate(
        Project = "COMPASS: TEMPEST",
        sample_location = "SERC",
        Volume_mL = as.numeric(Volume_mL)
    ) %>%
    filter(!is.na(Volume_mL)) %>%
    select(
        Project,
        Plot,
        Sample_ID,
        Volume_mL,
        Vial_ID,
        sample_location,
        Analyte,
        Collection_Date_YYYYMMDD,
        everything()
    )

#change the Evacuation date to a number
inventory_new <- inventory_new %>%
    mutate(
        Evacuation_date_YYYMMDD = as.numeric(Evacuation_date_YYYMMDD),
        Collection_Date_YYYYMMDD = as.numeric(Collection_Date_YYYYMMDD),
        Collection_Start_Time_24hrs = as.numeric(Collection_Start_Time_24hrs),
        Collection_End_Time_24hrs = as.numeric(Collection_End_Time_24hrs)
    )
