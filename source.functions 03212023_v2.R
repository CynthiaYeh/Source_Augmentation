# DESCRIPTION -------------------------------------------------------------
# Purpose        : Source Augmentation Functions Script
# Script Version : 17 February 2021
# Database       :
# Created by     : Cynthia Yeh
# -------------------------------------------------------------------------
# Last run by  : (name) - Certara Confidential
# Date run     : 
# -------------------------------------------------------------------------
# Software : Version 1.1.383 - 2009-2017 RStudio, Inc.
#            R version 4.0.4
# Platform : Lenovo ThinkPad T490
# Environment : Windows 10 Enterprise, Intel(R) Core(TM) i7-10510U CPU @ 1.80 GHz 64-bit OS

# Missing source.number
checkMissingSourceN = function(data) {
  ind <- which(is.na(data$source.number)|data$source.number == "")
  
  if (length(ind) >0){
    message("There might be missing entries for the database, please check the following row number:")
    ind+1
    }
  else {
    message("There are no missing entries for source numbers, please proceed the augmentation!")
    }
  }

# Generic Checks

checkReqSourceCols = function(data,show.details=T){
  req.cols = c('search',
               'search.date',
               'url',
               'source.number',
               'authors',
               'publication.year',
               'title',
               'journal',
               'volume',
               'pages',
               'abstract',
               'include',
               'reason',
               'reason.descr',
               'study',
               'registry.study.id',
               'treatment',
               'control',
               'n.study',
               'n.arm',
               'e.copy',
               'database'
  )
  miss.cols = NULL
  miss.cols = req.cols[!(req.cols %in% colnames(data))]
  k = length(miss.cols)
  if (k == 0) {
    message("All required columns are present.")
  } else {
    if (show.details == T) {
      message(paste("The following",k,"columns are missing and required:"))
      miss.cols
    } else {
      message(paste("There are",k,"columns missing."))
    }
  }
}

nonnum.unique = function(x){
  nonnum = is.na(suppressWarnings(as.numeric(as.character(x))))
  n = which(nonnum & !is.na(x))
  n.lab = paste("(",n,")",sep = '')
  if (length(n) == 0) {
    "all numeric"
  } else {
    paste(as.character(x[n]),n.lab,sep = '')
  }
}

checkNumSourceCols = function(data,show.details=T){
  num.cols = c('source.number', # added by cy
               'publication.year')
  num.cols2 = num.cols[num.cols %in% colnames(data)]
  df = data[,num.cols2]
  t2 = sapply(df,nonnum.unique) 
  t3 = t2[t2 != "all numeric"]
  k = length(t3)
  if (k == 0) {
    message("There are no columns with unallowed non-numerics.")
  } else {
    if (show.details == T) {
      message(paste("Warning: The following",k,"variables have unallowed non-numeric entries. Please correct them:"))
      names(t3)
    } else {
      message(paste("There are",k,"variables that have unallowed missings."))
    } 
  }
}

# Named Ranges Checks

searchType <- c("pubmed","clinicaltrials.gov","fda","ema","conference","clinical study report",
                "company website","cross-reference","web search")
yesNoUnclear<-c("yes","no","unclear")
yesNo<-c("yes","no")
reasonType<-c("","indication","design","treatment","comparison","endpoints","population",
              "sample size","availability","reliability","secondary reference","other", "study duration", "duplicate")

# Check Database

checkSourceYes = function(data){
  if (length(data > 0)) {
    message("The following references are marked as database = yes in source DB, but not found in outcomes DB. 
            Please double check and correct them:")
    sort(data)
  } else {
    message("Perfect! Let's go.")
  }
}

checkSourceNo = function(data){
  if (length(data) > 0) {
    message("These references are pending or not curated yet")
    sort(data)
  } else {
    message("Great!!! All included references are curated.")
  }
}

# Identify source.number for missing reasons of excluded references
MissingReasons <- function(data){
  missing.refs <- unique(data$source.number[data$reason == "" & data$include == "no"])
  if (length(missing.refs) > 0){
    message("The excluded references below have missing entries for reasons, please fill them.")
    sort(missing.refs)
  } else {
    message("No missing entries of reasons, please proceed.")
  }
}

# Dataset Trimming

TrimData <- function(dataset, ...){
  col_vars <- quos(...)
  datUni <- dataset  %>%
    group_by(!!!col_vars) %>%
    mutate(unique.id = cur_group_id()) %>%
    filter(!duplicated(unique.id))
}

# Identify Multiple Studies Sharing Single Source Number

checkMultiStudy = function(data){
  if (sum(duplicated(data$source.number)) > 0) {
    message("The following source numbers have more than 1 study")
    sort(unique(data$source.number[duplicated(data$source.number)]))
    } else {
      message("you are good to go! :D")
    }
}

# Replace Information From Augmented Database

ReplaceInfo = function (x) {
  datSource[[x]] <- ifelse(!is.na(datSource[[paste0(x, ".join")]]), datSource[[paste0(x, ".join")]], datSource[[x]])
  return(datSource[[x]])
}

# Keep first inctance of URL
FirstWord <- function(string){
  unlist(strsplit(string, ",|;"))[1]
}

# Warning Messages

ManualFill = function(data){
  if(length(data) > 0) {
    message("Please manually fill back info for following ", paste0(length(miss.in.source)),
            " references: ",
            paste(data, collapse = ", "))
  }
}

# Find the position for missing URLs, if there is any

MissingURLs = function(data){
  if (sum(is.na(data$url[data$database == "yes"]) > 0)) {
    message("Identify some missing URLs, please check them in the source DB")
    which(is.na(data$url[data$database == "yes"]))
    } else {
      message("No missing URLs. You are good to go!")
    }
}

# Check unallowed missings

MissingInfo = function(data){
  dbCols = c("search", "search.date", "url", "include", "e.copy")
  df = data[,dbCols]
  t2 = sapply(df, function(x) sum(is.na(x))) 
  t3 = sum(t2)
  if (t3 == 0) {
    message("No unallowed missings. You are good to go!")
    } else {
    message(paste("Warning: The following variables have unallowed missings. Please correct them:"))
    t2[t2 > 0]
    }
  }
