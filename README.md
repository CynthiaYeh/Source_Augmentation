# Purpose
Improve efficiency and accuracy for data between clinical outcomes databases and source databases.

# Getting Started
**Files needed:**
1. source_data.csv (Source database sheet save as *UTF-8* encoding csv file)
2. data_augmented_xl.csv (one of the output files from outcomes database main augmentation)

**Scripts needed:**
1. augment.source 02162023_v5.R
2. source.functions.R

**Directories:**<br>
Save the files in ***csv*** folder of the database you are currently working on, and the scripts in ***r*** folder.<br>
<img width="600" alt="screenshot" src="https://user-images.githubusercontent.com/49822948/223860085-6c833ba4-e071-45b0-a2c1-09055c93e10b.PNG">

### Usage
Make sure source_data.csv is in *UTF-8* encoding in order to avoid special characters. A few things listed below worth noticing while augmenting the source database.

Lines 74-81 are removing any non-date related words from search.date column. Please enter them in the variable **rm_words**. For example, if you see **2018 augment**, **2019 oct board**, or **2020 nov pd1 hemo**, simply enter augment, board, and pd1 hemo (all in lower cases) in the variable and it will work for you.
```r
# Date transformation
datSource$search.date <- tolower(datSource$search.date)
unique(datSource$search.date[grepl("[a-zA-Z]", datSource$search.date)])

rm_words <- c("augment", "board", "pd1 hemo") # Please enter the words shown above that are not related to dates
datSource$search.date <- gsub(paste(rm_words, collapse = "|"), "", datSource$search.date)
datSource$search.date <- trimws(datSource$search.date)
unique(datSource$search.date)
```

Lines 123-126 are coded to remove internal columns. Remove anything after **database** column.
```r
# Remove internal use columns (not in specs), please ignore date.transform column
# The 2 default columns are: entry, qc
names(datSource)
internalCols <- c("entry","QC") # adjust internal colnames as needed
```

Lines 157-162 are checking named ranges on search, include, reason, e.copy, and database columns. Please harmonize and change it as it see fits.
```r
# Run line-by-line
unique(datSource$search[!datSource$search %in% searchType])
unique(datSource$include[!datSource$include %in% yesNoUnclear])
unique(datSource$reason[!datSource$reason %in% reasonType])
unique(datSource$e.copy[!datSource$e.copy %in% yesNo])
unique(datSource$database[!datSource$database %in% yesNo])
```

Lines 270-287 are making sure the treatment info are not missing. Usually, the x dimension of datUniRef and datUniTrt should be the same. But when a reference/study has only subarms, strata, or control arms, the x dimension could vary. Please check the outcomes database to verify first. If it is a subarms, strata, or control arm only reference, the codes will add the treatment information back.
```r
# datUniRef and datUniTrt datasets should have the same x dimension
dim(datUniRef)
dim(datUniCtrl) #datUniCtrl could have diff x dimension due to single arm studies
dim(datUniTrt)
# Find out the source.number and check in augmented ODB in case of missing datUniTrt
datUniRef$source.number[!datUniRef$source.number %in% datUniTrt$source.number]
 
# Join treatment info if ODB contains subarms, strata, and control arm only references
tmp <- datUniRef$source.number[datUniRef$source.number %in% datUniTrt$source.number == FALSE]
tmp.trt <- OutcomesDB %>%
  filter(source.number %in% tmp) %>%
  filter(!duplicated(treatment)) %>%
  group_by(source.number) %>%
  mutate(treatment.join = paste(treatment, collapse = ", ")) %>%
  filter(!duplicated(source.number)) %>%
  select(source.number, treatment.join)

datUniTrt <- rbind(datUniTrt, tmp.trt)
```

Line 356 is to check if information for references more than 1 study has been properly concatenated.
```r
view(check.multi.study)
```

Please enjoy!
