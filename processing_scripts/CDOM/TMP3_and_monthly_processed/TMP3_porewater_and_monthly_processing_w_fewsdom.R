# Processing EEMs data from Horiba Aqualog

## Things to be done before running the code
# 1. Gather your raw data: export your data from Aqualog (3 files for each sample)
#    1) Abs Spectra Graphs (axis in log)
#    2) Waterfall Plot Blank
#    3) Waterfall Plot Sample
# 2. Complete 'sample metadata sheet.xlsx'
# 3. Create a separate folder with the metadata file and all raw data files (.dat)
# 4. Make sure you have the right working directory - check with getwd()

# Load libraries
library(stringr)
library(readxl) # Read in excel files
library(writexl)
library(fewsdom) # Pre-process eems - Katie's package
library(eemR) # Pre-process eems
library(staRdom) # PARAFAC
library(dplyr)
library(tidyverse)

# If you don't have the fewsdom package installed, use devtools:
#library(devtools)
#install_github("katiewampler/fewsdom")


## 1. Set up a project path
getwd()
wd <- "/Users/kimj704/Github/CDOM_EEMs/raw_data" # replace this with your working directory
setwd(wd)

project <- "013_TEMPEST3/Porewater_monthly copy"
ifelse(!dir.exists(project), dir.create(project), "Project folder exists already") # create a project folder if not exist already
prjpath <- paste0(wd, "/", project) # where the raw data files are


## 2. Read in metadata
metadata <- read_xlsx(file.path(prjpath, "sample_metadata.xlsx"))
metadata


## 3. Process EEM's with master function
run_eems(prjpath = prjpath, meta_name = "sample_metadata.xlsx",
         get_doc = F, rayleigh_mask = c(20, 45, 15, 15)) # manually set up masking range.




