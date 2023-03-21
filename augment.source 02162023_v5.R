# DESCRIPTION -------------------------------------------------------------
# Purpose        : Source Augmentation Script
# Script Version : 2 December 2022
# Database       :
# Created by     : Cynthia Yeh
# -------------------------------------------------------------------------
# Last run by  : (name) - Certara Confidential
# Date run     : 07 April 2020
# -------------------------------------------------------------------------
# Software : Version 1.1.383 - 2009-2017 RStudio, Inc.
#            R version 4.0.4
# Platform : Lenovo ThinkPad T490
# Environment : Windows 10 Enterprise, Intel(R) Core(TM) i7-10510U CPU @ 1.80 GHz 64-bit OS

# DATASET NAMING AND NOTES -------------------------------------------------------------
# OutcomesDB      <- augmented working DB
# SourceDB        <- source DB
# datSource       <- transform from SourceDB and will be the final augmented dataset
# datUniRef       <- dataset that contains unique references
# datUniCtrl      <- dataset that contains unique control drugs for studies
# datUniTrt       <- dataset that contains unique interventional drugs for studies
# datUniRef.join  <- Merged dataset from datUniRef/datUniCtrl/datUniTrt by source.number


rm(list = ls())

##### Dependencies #####
#install.packages("dplyr")
#install.packages("purrr")
#install.packages("tidyverse")
#install.packages("lubridate")
library(dplyr)
library(purrr)
library(tidyverse)
library(lubridate)


##### Directories #####
dirHome <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirHome)

# Check that working directory is correct
getwd()
dirSource <- '../csv'
dirResults <- '../csv'

# Import Files
sourcedb <- 'source_data.csv'
augmentdb<-'data_augmented_xl.csv'

OutcomesDB <- read.csv(file = paste(dirSource, augmentdb, sep = "/"), header = TRUE, fill = TRUE,
                       as.is = TRUE, sep = ",", na.strings = c(".","N/A"))
SourceDB <- read.csv(file = paste(dirSource, sourcedb, sep = "/"), header = TRUE, fill = TRUE,
                     as.is = TRUE, sep = ",", na.strings = c(".","N/A"), strip.white = TRUE, encoding = "UTF-8")
source('source.functions.R')

# Check missing value in source.number
checkMissingSourceN(OutcomesDB)
checkMissingSourceN(SourceDB)

# Exclude empty rows
OutcomesDB <- filter(OutcomesDB, !(is.na(source.number)|source.number == ""))
SourceDB <- filter(SourceDB, !(is.na(source.number)|source.number == ""))

# Check original dimensions of database
dim(OutcomesDB)
dim(SourceDB)


##### Data Transformation & Cleaning (datSource) #####
datSource <- SourceDB
dim(datSource) 

# Date transformation
datSource$search.date <- tolower(datSource$search.date)
unique(datSource$search.date[grepl("[a-zA-Z]", datSource$search.date)])

rm_words <- c("augment", "board", "pd1 hemo") # Please enter the words shown above that are not related to dates
datSource$search.date <- gsub(paste(rm_words, collapse = "|"), "", datSource$search.date)
datSource$search.date <- trimws(datSource$search.date)
unique(datSource$search.date)

# Dates harmonization (yyyy-mm-dd)
date <- as.character(mdy(datSource$search.date))
date_tmp <- datSource$search.date
date <- ifelse(is.na(date), date_tmp, date)
res_list <- c()

for (i in date) {
  # print(i)
  
  if (grepl("^\\d{5}", i)){
    i <- as.character(as.Date(as.numeric(i), origin = "1899-12-30"))
  } else if (grepl("^20\\d{2}$", i)){
    i <- paste0(i, "-01-01")
  } else if (grepl("^20\\d{2}\\s", i)){
    i <- as.character(ym(i))
  }
  
  # print(i)
  res_list <- append(res_list, i) # append the results back to res_list
}

datSource$date.transform <- res_list

# Check if date transformation is complete
unique(datSource$date.transform)

# Convert date columns to desired format
dates <- c("search.date", "date.transform")
for (k in dates) {
  datSource[,k] <- paste('="', as.character(datSource[,k]), '"', sep = '') 
}

datSource$search.date <- ifelse(datSource$date.transform == "=\"NA\"", 
                                as.character(datSource$search.date), 
                                datSource$date.transform)

# Remove date.transform column after use
datSource <- datSource %>% select(!date.transform)

# Data Cleaning
# Remove internal use columns that are not in the specs (The 2 default columns are: entry, qc)
names(datSource)[1] <- "search"
names(datSource)
internalCols <- c() # adjust internal colnames as needed

datSource <- datSource %>%
  mutate(across(where(is.character), trimws),
         across(c(search, include, reason, e.copy, database), tolower)) %>%
  dplyr::select(!all_of(internalCols))

names(datSource) # database should be the last column

# Subset needed columns (OutcomesDB)
OutcomesDB <- OutcomesDB %>%
  dplyr::select(url:publication.type,
                study,
                registry.study.id,
                n.study,
                arm,
                control.arm,
                treatment)


##### Data Format Checks #####
# Check required columns 
checkReqSourceCols(datSource)
checkNumSourceCols(datSource)

# Use following for unallowed non-numeric entries. Please uncomment following 2 lines and change the colnames in x.
#x <- c("source.number", "publication.year")
#datSource[x] <- sapply(datSource[x], as.numeric)


##### Named Ranges Checks #####
# Run line-by-line and change as needed in SDB
unique(datSource$search[!datSource$search %in% searchType])
unique(datSource$include[!datSource$include %in% yesNoUnclear])
unique(datSource$reason[!datSource$reason %in% reasonType])
unique(datSource$e.copy[!datSource$e.copy %in% yesNo])
unique(datSource$database[!datSource$database %in% yesNo])


##### References Checks #####
wDB.sNumber <- unique(OutcomesDB$source.number)
sDB.database.yes <- unique(datSource$source.number[datSource$database == "yes"])
sDB.database.no <- unique(datSource$source.number[datSource$database != "yes"])

# Check database = yes in SDB but not found in ODB
wDB.no <- sDB.database.yes[!sDB.database.yes %in% wDB.sNumber]
checkSourceYes(wDB.no)

# Check database = no in source DB but is curated/augmented in ODB, automatically corrected in code ???also check if include is marked as "yes"???
wDB.yes <- sDB.database.no[sDB.database.no %in% wDB.sNumber]
datSource$database[datSource$source.number %in% wDB.yes] <- "yes"

#--- From this step forward, all refs augmented in ODB are marked as "database = yes" in SDB ---#

# Check database = pending or blanks

# When include is yes
# Marked include as no if (database != "yes") & (reason is filled)
sDB.include.yes <- datSource %>%
  filter(include == "yes", database != "yes") %>%
  dplyr::select(source.number)
num.include.yes <- sDB.include.yes$source.number

datSource$include[datSource$source.number %in% num.include.yes] <- 
  ifelse((datSource$database[datSource$source.number %in% num.include.yes] != "yes")&
           (datSource$reason[datSource$source.number %in% num.include.yes] != ""),
         "no", datSource$include[datSource$source.number %in% num.include.yes])

# Marked database as pending if (include = "yes") & (reason is not filled)
datSource$database[datSource$source.number %in% num.include.yes] <- 
  ifelse((datSource$include[datSource$source.number %in% num.include.yes] == "yes") &
           (datSource$reason[datSource$source.number %in% num.include.yes] == ""), 
         "pending", datSource$database[datSource$source.number %in% num.include.yes])

# When include is no
# Marked database as no if (include = "no") & (reason is filled)
sDB.pending <- datSource %>% 
  filter(database %in% c("pending", "")) %>%
  select(source.number)
num.pending <- sDB.pending$source.number

datSource$database[datSource$source.number %in% num.pending] <- 
  ifelse((datSource$include[datSource$source.number %in% num.pending] == "no")&
           (datSource$reason[datSource$source.number %in% num.pending] != ""),
         "no", datSource$database[datSource$source.number %in% num.pending])

#--- From this step forward, all refs in database column are marked as yes, no, or pending ---#

# Check the source.number of pending references
pending.refs <- datSource %>%
  filter(database == "pending") %>%
  select(source.number)
checkSourceNo(pending.refs)

# Message for missing exclusion reasons
MissingReasons(datSource)


##### Create Datasets #####
#--- Create datasets for unique references, unique control drugs, and unique randomized drugs for studies ---#

# datUniRef Dataset - unique referenes that contains maximum arm for each study
datUniRef <- TrimData(OutcomesDB, source.number, study, arm)
datUniRef$arm <- gsub(".*\\+|[^0-9]", "", datUniRef$arm)
datUniRef <- datUniRef %>%
  group_by(source.number, study) %>%
  slice_max(order_by = arm, with_ties = FALSE, n = 1)

# Identify duplicated source.number for more than 1 study, if any
checkMultiStudy(datUniRef)
refs.multiple.study <- unique(datUniRef$source.number[duplicated(datUniRef$source.number)])

# Combine multiple studies information
datUniRef <- datUniRef %>%
  group_by(source.number) %>%
  mutate(across(c(study, registry.study.id, n.study, arm),
                function(x) paste(unique(x[!is.na(x) & x != ""]), collapse = " and "))) %>%
  filter(!duplicated(source.number))

# datUniCtrl Dataset - unique control drugs dataset
datUniCtrl <- filter(OutcomesDB, !grepl("[A-z]", arm),            # exclude subarms
                     control.arm == "yes")                   # include control arms only
datUniCtrl <- TrimData(datUniCtrl, source.number, study, treatment)

datUniCtrl <- datUniCtrl %>%
  group_by(source.number) %>%
  filter(!duplicated(treatment)) %>%
  mutate(control.join = paste(treatment, collapse = ", ")) %>%
  filter(!duplicated(source.number)) %>%
  dplyr::select(source.number, control.join)


# datUniTrt Dataset - unique interventional drugs dataset
datUniTrt <- filter(OutcomesDB, !grepl("[A-z]", arm),             # exclude subarms
                    control.arm == "no")                     # include experimental arms only
datUniTrt <- TrimData(datUniTrt, source.number, study, treatment)

datUniTrt <- datUniTrt %>%
  group_by(source.number) %>%
  filter(!duplicated(treatment)) %>%
  mutate(treatment.join = paste(treatment, collapse = ", ")) %>%
  filter(!duplicated(source.number)) %>%
  dplyr::select(source.number, treatment.join)

# datUniRef and datUniTrt datasets should have the same x dimension
dim(datUniRef)
dim(datUniCtrl) #datUniCtrl could have diff x dimension due to single arm studies
dim(datUniTrt)
# Find out the source.number and check in augmented ODB in case of missing datUniTrt
datUniRef$source.number[!datUniRef$source.number %in% datUniTrt$source.number]

# Join treatment info if ODB contains subarms, strata, or control arm only references
tmp <- datUniRef$source.number[datUniRef$source.number %in% datUniTrt$source.number == FALSE]
tmp.trt <- OutcomesDB %>%
  filter(source.number %in% tmp) %>%
  filter(!duplicated(treatment)) %>%
  group_by(source.number) %>%
  mutate(treatment.join = paste(treatment, collapse = ", ")) %>%
  filter(!duplicated(source.number)) %>%
  select(source.number, treatment.join)

datUniTrt <- rbind(datUniTrt, tmp.trt)


##### Fill in Missing Information #####
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
  dplyr::select(source.number, all_of(cols.join)) %>%
  dplyr::rename(n.arm = arm) %>%
  rename_at(vars(-source.number), paste0, ".join") %>%
  list(datUniCtrl, datUniTrt) %>%
  reduce(left_join, by = "source.number")

# For oncology database single arm study (can also run it in general database)
datUniRef.join$control.join[is.na(datUniRef.join$control.join)] <- "no control"

# Check if augmented references miss filling in SDB
miss.in.source <- wDB.sNumber[!wDB.sNumber %in% datSource$source.number]

# Add the missing refs' source numbers to datUniRef.join for filling information later  
for (miss.info in miss.in.source) {
  datSource <- datSource %>%
    add_row(source.number = miss.info, database = "yes")
}

# All augmented refs' source.number should be in SDB now, proceed to fill in information
cols.join <- gsub(".join", "", names(datUniRef.join[, names(datUniRef.join) != c("source.number")]))
cols.join

datSource <- datSource %>%
  left_join(datUniRef.join, by = "source.number")

# Fill in info from augmented outcome DB
for(names in cols.join) {
  datSource[[names]] <- ReplaceInfo(names)
}


##### URL Harmonization ##### (need improvements)
# Generate PubMed/clintrials URLs
datSource$url[datSource$search == "pubmed"] <- paste0("https://www.ncbi.nlm.nih.gov/pubmed/",
                                                      datSource$source.number[datSource$search == "pubmed"])
datSource$url[datSource$search == "clinicaltrials.gov"] <- paste0("https://clinicaltrials.gov/ct2/show/",
                                                                  datSource$registry.study.id[datSource$search == "clinicaltrials.gov"])
# Remove everything that comes after first comma in URLs
datSource$url <- sapply(datSource$url, FirstWord)


##### Check Information for 1 Reference to Many Studies #####
check.multi.study <- datSource %>%
  filter(source.number %in% refs.multiple.study) %>%
  dplyr::select(source.number,
                study, 
                registry.study.id,
                treatment,
                control,
                n.study,
                n.arm)
view(check.multi.study)


##### Data Final Cleaning #####
# Unify study name format (remove quotation marks)
datSource$study <- gsub('=|"', "", datSource$study)

# Final cleaning and save to file
datSource <- dplyr::select(datSource, !ends_with("join"))
# datSource$include <- ifelse(datSource$database == "yes", "yes", datSource$include)
# datSource$include <- ifelse(datSource$database == "no", "no", datSource$include)

# Save as UTF-8 encoding file to avoid special characters in authors
write.csv(datSource, paste(dirResults,'Source database.csv',sep = '/'), na = "", row.names = FALSE, fileEncoding = "UTF-8")


##### Messages #####
# Find the position for any new information needed manually
# miss.in.source <- wDB.sNumber[!wDB.sNumber %in% datSource$source.number]
# wDB.sNumber[!wDB.sNumber %in% datSource$source.number]
ManualFill(miss.in.source)

# Find the position for missing URLs, if there is any
MissingURLs(datSource)

# Find if database column has entries other than yes, no, or pending
unique(datSource$database[!datSource$database %in% yesNo])