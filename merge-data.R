######################
###### IMPORTANT ##### 
# Set this depending on which computer you are 
# running the script on (sets working directory)
setwd('./')
path <- '../json/'
# List of previously read JSON files to avoid reading in
old.json.files <- read.csv('json-files-2019-10-25.txt',  
                           stringsAsFactors = F,
                           header = F)$V1
old.unmatched.admission.df <- read.csv('2019-10-25-NeoTree-admission-ID-unmatched.csv',
                                       header = T,
                                       stringsAsFactors = F)
old.final.database <- read.csv('test-final-DB.csv',
                               header = T,
                               stringsAsFactors = F)

# Library loading
library(tidyr)
library(hms)
library(data.table)
#library(Stack)
library(ggplot2)
library(rjson)
library(plyr)
source('conversion-functions.R') # defined bespoke functions 

# File locations
admission.files <- list.files(path = path, 
                              pattern = '*NeoTree___Zimbabwe*.json')
admission.files <- admission.files[!admission.files %in% old.json.files]
admission.filenames <- paste0(path, 
                              '/', 
                              admission.files)
discharge.files <- list.files(path = path, 
                              pattern = '*NeoDischarge___Zimbabwe*.json')
discharge.files <- discharge.files[!discharge.files %in% old.json.files]

discharge.filenames <- paste0(path, 
                               '/',
                               discharge.files)

# Read in files and convert from json
admission.df <- jsonToDataFrame(admission.filenames, scriptType = "Admission")
discharge.df <- jsonToDataFrame(discharge.filenames, scriptType = "Discharge")

# Deduplicate
admission.df <- deduplicateAdmission(admission.df)
discharge.df <- deduplicateDischarge(discharge.df)

# Check for admission matches with the 
# discharges in the previous version
# of the database
#admission.df$NeoTreeID %in% old.final.database$Discharge.NeoTreeID)

# Find matches 
merged.df <- findMatchesWithinNewAdmissionDischarge(admission.df, discharge.df)

if (!is.null(merged.df)){ # There may be no matches in the new data
  # Add in unmatched discharges
  final.database.df <- addUnmatchedDischarges(merged.df, discharge.df)
}
if (is.null(merged.df)){
  final.database.df <- addUnmatchedDischarges(old.final.database, 
                             discharge.df)
}

final.database.df.v2 <- merge(final.database.df, 
                           old.final.database)
# Write this final database to csv
database.file <- paste0(Sys.Date(),'-NeoTree-database.csv')
# Check if file exists
# If file exists, add a random string to avoid overwriting
if (file.exists(database.file)){
  database.file <-  paste0(Sys.Date(), 
                           randomString(), 
                           '-NeoTree-database.csv')
}
write.csv(file=database.file, 
          final.database.df, 
          row.names = F,
          quote=T)

# Save the unmatched admissions as well   
unmatched.admission.df <- admission.df[which(!admission.df$Admission.UID %in% 
final.database.df$Admission.UID),]
write.csv(file=paste0(Sys.Date(),'-NeoTree-unmatched-admissions.csv'), 
          unmatched.admission.df, 
          row.names = F,
          quote=T)

# Save the files that have been processed in this run.
# Next time the script is run, change the old.json.files 
# to this filename
cat(c(admission.files, discharge.files), 
    sep =  '\n',
    file = paste0('json-files-', Sys.Date(), '.txt')) 

# Summary statistics by HCWID
source('summary-statistics.R')

# Felicity's plotting 
source('plot-generation.R')