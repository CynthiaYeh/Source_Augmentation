# DESCRIPTION -------------------------------------------------------------
# Purpose        : Source Augmentation Script
# Script Version : 17 February 2021
# Database       :
# Created by     : Cynthia Yeh
#-------------------------------------------------------------------------
# Last run by  : (name) - Certara Confidential
# Date run     : 07 April 2020
# -------------------------------------------------------------------------
# Software : Version 1.1.383 - 2009-2017 RStudio, Inc.
#            R version 4.0.4
# Platform : Lenovo ThinkPad T490
# Environment : Windows 10 Enterprise, Intel(R) Core(TM) i7-10510U CPU @ 1.80 GHz 64-bit OS



# DATASET NAMING -------------------------------------------------------------
# OutcomesDB      <- augmented working DB
# SourceDB        <- source DB
# datSource       <- transform from SourceDB and will be the final augmented dataset
# datUniRef       <- dataset that contains unique references
# datUniCtrl      <-dataset that contains unique control drugs for studies
# datUniTrt       <- dataset that contains unique interventional drugs for studies
# datUniRef.join  <- Merged dataset from datUniRef/datUniCtrl/datUniTrt by source.number


# DEFAULTS ----------------------------------------------------------

rm(list = ls())

# Dependencies
#install.packages("dplyr")
#install.packages("purrr")
#install.packages("tidyverse")
#install.packages("lubridate")
library(dplyr)
library(purrr)
library(tidyverse)
library(lubridate)

# Directories
dirHome <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirHome)

# Check that working directory is correct
getwd()
dirSource <- '../csv'
dirResults <- '../csv'

# Files
sourcedb <- 'source_data.csv'
augmentdb<-'data_augmented_xl.csv'


# DATA IMPORT ----------------------------------------------------------------

OutcomesDB <- read.csv(file = paste(dirSource, augmentdb, sep = "/"), header = TRUE, fill = TRUE,
                       as.is = TRUE, sep = ",", na.strings = c(".","N/A"))
SourceDB <- read.csv(file = paste(dirSource, sourcedb, sep = "/"), header = TRUE, fill = TRUE,
                     as.is = TRUE, sep = ",", na.strings = c(".","N/A"), strip.white = TRUE)
# exclude empty rows
OutcomesDB <- filter(OutcomesDB, !source.number == "")
SourceDB <- filter(SourceDB, !source.number == "")

source('source.functions.R')

# Check original dimensions of database
dim(OutcomesDB)
dim(SourceDB)

# DATA PREP ---------------------------------------------------------------------

datSource <- SourceDB
dim(datSource) 

### Date transformation (datSource)
# Transform date format to yyyy-mm-dd
datSource$date.transform <- mdy(datSource$search.date)

# Convert date columns to desired format
dates <- c("search.date", "date.transform")
for (k in dates) {
  datSource[,k] <- paste('="', as.character(datSource[,k]), '"', sep = '') 
}

datSource$search.date <- ifelse(datSource$date.transform == "=\"NA\"", 
                                as.character(datSource$search.date), 
                                datSource$date.transform)

### Data Cleaning (datSource)
# Remove internal use columns (not in specs), please ignore date.transform column
# The 2 default columns are: entry, qc
names(datSource)
internalCols <- c("entry", "qc") # adjust internal colnames as needed

datSource <- datSource %>%
  mutate(across(where(is.character), trimws),
         across(c(search, include, reason, e.copy, database), tolower)) %>%
  select(!all_of(internalCols) & !date.transform)

names(datSource) # database should be the last column

### Subset needed comlumns (OutcomesDB)
OutcomesDB <- OutcomesDB %>%
  select(url:publication.type,
         study,
         registry.study.id,
         n.study,
         arm,
         control.arm,
         randomized.drug)


# DATA FORMAT CHECKS ----------------------------------------------------------------

# Check required columns 
checkReqSourceCols(datSource)
checkNumSourceCols(datSource)

# Use following for unallowed non-numeric entries. Please uncomment following 2 lines and change the colnames in x.
#x <- c("source.number", "publication.year")
#datSource[x] <- sapply(datSource[x], as.numeric)


# NAMED RANGES ------------------------------------------------------------------

# Run line-by-line
unique(datSource$search[!datSource$search %in% searchType])
unique(datSource$include[!datSource$include %in% yesNoUnclear])
unique(datSource$reason[!datSource$reason %in% reasonType])
unique(datSource$e.copy[!datSource$e.copy %in% yesNo])
unique(datSource$database[!datSource$database %in% yesNo])


# URL HARMONIZATION ---------------------------------------------------------------------- (need improvements)

# Generate PubMed/clintrials URLs
datSource$url[datSource$search == "pubmed"] <- paste0("https://www.ncbi.nlm.nih.gov/pubmed/",
                                                      datSource$source.number[datSource$search == "pubmed"])
datSource$url[datSource$search == "clinicaltrials.gov"] <- paste0("https://clinicaltrials.gov/ct2/show/",
                                                                  datSource$registry.study.id[datSource$search == "clinicaltrials.gov"])
# Remove everything that comes after a comma in the URL
datSource$url <- sapply(strsplit(datSource$url, ","), "[", 1)


# REFERENCES CHECKS -----------------------------------------------------------------

wDB.sNumber <- unique(OutcomesDB$source.number)
sDB.database.yes <- unique(datSource$source.number[datSource$database == "yes"])
sDB.database.no <- unique(datSource$source.number[datSource$database != "yes"])

# Check database = yes in source DB
wDB.no <- sDB.database.yes[!sDB.database.yes %in% wDB.sNumber]
checkSourceYes(wDB.no)

# Check database == no in source DB
wDB.yes <- sDB.database.no[sDB.database.no %in% wDB.sNumber]
datSource$database[datSource$source.number %in% wDB.yes] <- "yes"

# Check the rest of database == no (& include == yes) in Source DB
sDB.include.yes <- datSource %>%
  filter(include == "yes", database != "yes") %>%
  select(source.number)
checkSourceNo(sDB.include.yes)


# CREATE DATASETS ----------------------------------------------------------------

### Create datasets for unique references, unique control drugs, and unique randomized drugs for studies ###

### Dataset - unique referenes that contains maximum arm for each study
datUniRef <- TrimData(OutcomesDB, source.number, study, arm)
datUniRef$arm <- gsub(".*\\+|[^0-9]", "", datUniRef$arm)
datUniRef <- datUniRef %>%
  group_by(source.number, study) %>%
  slice_max(arm)

# Identify duplicated source.number for more than 1 study, if any
checkMultiStudy(datUniRef)
refs.multiple.study <- unique(datUniRef$source.number[duplicated(datUniRef$source.number)])

# Combine multiple studies information
datUniRef <- datUniRef %>%
  group_by(source.number) %>%
  mutate(across(c(study, registry.study.id, n.study, arm),
                function(x) paste(unique(x[!is.na(x) & x != ""]), collapse = " and "))) %>%
  filter(!duplicated(source.number))

### Dataset - unique control drugs dataset
datUniCtrl <- filter(OutcomesDB, !grepl("[A-z]", arm),            # exclude subarms
                     control.arm == "yes")                   # include control arms only
datUniCtrl <- TrimData(datUniCtrl, source.number, study, randomized.drug)

datUniCtrl <- datUniCtrl %>%
  group_by(source.number) %>%
  filter(!duplicated(randomized.drug)) %>%
  mutate(control.join = paste(randomized.drug, collapse = ", ")) %>%
  filter(!duplicated(source.number)) %>%
  select(source.number, control.join)


### Dataset - unique interventional drugs dataset
datUniTrt <- filter(OutcomesDB, !grepl("[A-z]", arm),             # exclude subarms
                    control.arm == "no")                     # include experimental arms only
datUniTrt <- TrimData(datUniTrt, source.number, study, randomized.drug)

datUniTrt <- datUniTrt %>%
  group_by(source.number) %>%
  filter(!duplicated(randomized.drug)) %>%
  mutate(treatment.join = paste(randomized.drug, collapse = ", ")) %>%
  filter(!duplicated(source.number)) %>%
  select(source.number, treatment.join)

# datUniRef and datUniTrt datasets should have the same x dimension \\\datUniCtrl could be missing due to single arm studies
dim(datUniRef)
dim(datUniCtrl)
dim(datUniTrt)


# FILL IN MISSING INFORMATION ---------------------------------------------------------------------------

# Merge datUniRef/datUniCtrl/datUniTrt
cols.join <- c("url",
               "authors",
               "publication.year",
               "title",
               "journal",
               "volume",
               "pages",
               "study",
               "registry.study.id",
               "n.study",
               "arm")

datUniRef.join <- datUniRef %>%
  select(source.number, all_of(cols.join)) %>%
  rename(n.arm = arm) %>%
  rename_at(vars(-source.number), paste0, ".join") %>%
  list(datUniCtrl, datUniTrt) %>%
  reduce(left_join, by = "source.number")

# For oncology database single arm study (can also run it in general database)
datUniRef.join$control.join[is.na(datUniRef.join$control.join)] <- "no control"


### Check if it's in the augmented database, but not in the source database ###
miss.in.source <- wDB.sNumber[!wDB.sNumber %in% datSource$source.number]

# Add missing refs' source numbers to datUniRef.join for filling information later  
for (miss.info in miss.in.source) {
  datSource <- datSource %>%
    add_row(source.number = miss.info, database = "yes")
}


### All augmentated DB source.number should be in the source database now, proceed to fill in information ###
cols.join <- gsub(".join", "", names(datUniRef.join[, names(datUniRef.join) != "source.number"]))
cols.join

datSource <- datSource %>%
  left_join(datUniRef.join, by = "source.number")

# Fill in info from augmented outcome DB
for(names in cols.join) {
  datSource[[names]] <- ReplaceInfo(names)
}

# CHECK INFORMATION FOR REFERENCES MORE THAN 1 STUDY -------------------------------------------------
check.multi.study <- datSource %>%
  filter(source.number %in% refs.multiple.study) %>%
  select(source.number,
         study, 
         registry.study.id,
         treatment,
         control,
         n.study,
         n.arm)
view(check.multi.study)

# DATA FINAL CLEANING --------------------------------------------------------------------------------

#Unify study name format (remove quotation marks)
datSource$study <- gsub('=|"', "", datSource$study)

# Final cleaning and save to file
datSource <- select(datSource, !ends_with("join"))
write.csv(datSource, paste(dirResults,'source_augmented.csv',sep = '/'), na = "", row.names = FALSE)


# MESSAGES ---------------------------------------------------------------

# Find the position for any new information needed manually
#miss.in.source <- wDB.sNumber[!wDB.sNumber %in% datSource$source.number]
#wDB.sNumber[!wDB.sNumber %in% datSource$source.number]
ManualFill(miss.in.source)

# Find the position for missing URLs, if there is any
MissingURLs(datSource)


# END OF CODE -----------------------------------------------------------------------------

unique(datSource$database[!datSource$database %in% yesNo])
