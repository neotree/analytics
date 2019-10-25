######################
###### IMPORTANT ##### 
# Set this depending on which computer you are 
# running the script on (sets working directory)
setwd('./')
path <- '../json/'
# List of previously read JSON files to avoid reading in
old.json.files <- read.csv('MQXRG2970O-2019-10-25-json-files.txt',  
                           stringsAsFactors = F,
                           header = F)$V1
old.unmatched.admission.df <- read.csv('MQXRG2970O-2019-10-25-NeoTree-unmatched-admissions.csv',
                                       header = T,
                                       stringsAsFactors = F)
old.final.database <- read.csv('MQXRG2970O-2019-10-25-NeoTree-database.csv',
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
if (length(admission.files)==0){
  print("There are apparently no new admission files to analyse. The script 
        will stop now.")
  stop()
}
admission.filenames <- paste0(path, 
                              '/', 
                              admission.files)
discharge.files <- list.files(path = path, 
                              pattern = '*NeoDischarge___Zimbabwe*.json')
discharge.files <- discharge.files[!discharge.files %in% old.json.files]
discharge.filenames <- paste0(path, 
                               '/',
                               discharge.files)

# Random string to precede filenames (for uniqueness)
run.string <- randomString()

# Read in files and convert from json
admission.df <- jsonToDataFrame(admission.filenames, scriptType = "Admission")
discharge.df <- jsonToDataFrame(discharge.filenames, scriptType = "Discharge")

# Deduplicate
admission.df <- deduplicateAdmission(admission.df)
discharge.df <- deduplicateDischarge(discharge.df)

# Comparing with previous database
# 1. Remove any unmatched discharges from previous database
#    that are also present in the new discharge data.
previous.database <- old.final.database[which(!old.final.database$Discharge.NeoTreeID %in% 
                                                discharge.df$Discharge.NeoTreeID),]
# (We don't check for admission matches with previous database discharges because
#  I'm assuming the temporality means we don't have to. Ideally we would
#  for completeness but I haven't implemented this.)
# 2. Check for matches in the new data
# Find matches
new.merged.df <- findMatchesWithinNewAdmissionDischarge(admission.df, 
                                                      discharge.df)
unmatched.discharge.df <- discharge.df[which(!discharge.df$NeoTreeID %in% 
                                               new.merged.df$Discharge.NeoTreeID),]
# Remove those already in the previous version of the database
new.merged.df <- new.merged.df[which(!new.merged.df$Admission.NeoTreeID %in% 
                                       old.final.database$Admission.NeoTreeID),]


if (!is.null(new.merged.df)){ # If there are new matches to add
  # Add in unmatched discharges
  final.database.df <- addUnmatchedDischarges(new.merged.df, 
                                              unmatched.discharge.df)
  # Add in old final database
  final.database.df <- rbind(final.database.df, 
                             old.final.database)
} # There may be no matches in the new data
if (is.null(new.merged.df)){
  final.database.df <- addUnmatchedDischarges(old.final.database, 
                             discharge.df)
}

# Write this final database to csv
write.csv(file=paste0(run.string, '-', Sys.Date(),'-NeoTree-database.csv'), 
          final.database.df, 
          row.names = F,
          quote=T)

# Save the unmatched admissions as well 
unmatched.admission.df <- admission.df[which(!admission.df$Admission.UID %in% 
final.database.df$Admission.UID),]
# Check if these unmatched admissions were already seen
unmatched.admission.df <- unmatched.admission.df[which(!unmatched.admission.df$NeoTreeID %in%
                                                         old.unmatched.admission.df$NeoTreeID),]
total.unmatched.admissions <- rbind(unmatched.admission.df, old.unmatched.admission.df)
# Write this final database to csv
write.csv(file=paste0(run.string, '-', Sys.Date(),'-NeoTree-unmatched-admissions.csv'), 
          unmatched.admission.df, 
          row.names = F,
          quote=T)

# Save the files that have been processed in this run.
# Next time the script is run, change the old.json.files 
# to this filename
cat(c(admission.files, discharge.files, old.json.files), 
    sep =  '\n',
    file = paste0(run.string, '-', Sys.Date(), '-json-files.txt')) 

# Summary statistics by HCWID
source('summary-statistics.R')

# Felicity's plotting 
source('plot-generation.R')
