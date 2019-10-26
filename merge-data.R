# NeoTree processing scripts.
# This processes json files from tablets to find matched admissions/discharges.
# Author: Liam Shaw, liam.philip.shaw@gmail.com

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
# Old admission and discharge files (rds)
# These should be changed to the output from the last time the 
# script was run.
old.admission.df <- readRDS('YORFN6180G-2019-10-26-admissions.rds')
old.discharge.df <- readRDS('YORFN6180G-2019-10-26-discharges.rds')

# Library loading
library(tidyr)
library(hms)
library(data.table)
#library(Stack) # Not sure what this was for?
library(ggplot2)
library(rjson)
library(plyr)
source('conversion-functions.R') # defined bespoke functions 

# File locations
admission.files <- list.files(path = path, 
                              pattern = '*NeoTree___Zimbabwe*.json')
admission.files <- admission.files[!admission.files %in% old.json.files]
if (length(admission.files)==0){
  print('There are apparently no new admission files to analyse. The script 
        will stop now.')
  stop()
}
admission.filenames <- paste0(path, '/', admission.files)

discharge.files <- list.files(path = path, 
                              pattern = '*NeoDischarge___Zimbabwe*.json')
discharge.files <- discharge.files[!discharge.files %in% old.json.files]
discharge.filenames <- paste0(path, '/', discharge.files)

# Random string to precede filenames (for uniqueness)
# Create a directory using this random string for output files
run.string <- randomString()
output.directory <- paste0(Sys.Date(), '-', run.string)
dir.create(file.path(output.directory), 
           showWarnings = FALSE)

# Read in files and convert from json
admission.df <- jsonToDataFrame(admission.filenames, 
                                scriptType = 'Admission')
discharge.df <- jsonToDataFrame(discharge.filenames, 
                                scriptType = 'Discharge')

# Deduplicate
admission.df <- deduplicateAdmission(admission.df)
discharge.df <- deduplicateDischarge(discharge.df)

# Remove those previously seen in old versions
admission.df <- admission.df[which(!admission.df$NeoTreeID %in% 
                                     old.admission.df$NeoTreeID),]
discharge.df <- discharge.df[which(!discharge.df$NeoTreeID %in% 
                                     old.discharge.df$NeoTreeID),]

# Integrate these together
new.admission.df <- rbind(old.admission.df, admission.df)
new.discharge.df <- rbind(old.discharge.df, discharge.df)

# Save these versions
saveRDS(new.admission.df, file = paste0(output.directory,
                                        '/', 
                                        run.string,
                                        '-', 
                                        Sys.Date(),
                                        '-admissions.rds'))
saveRDS(new.discharge.df, file = paste0(output.directory,
                                        '/',
                                        run.string,
                                        '-', 
                                        Sys.Date(),
                                        '-discharges.rds'))

# Check for matched admission/discharge pairs 
new.merged.df <- findMatchesWithinNewAdmissionDischarge(new.admission.df, 
                                                      new.discharge.df)
unmatched.discharge.df <- new.discharge.df[which(!new.discharge.df$NeoTreeID %in% 
                                                   new.merged.df$Discharge.NeoTreeID),]

# Combine the merged pairs with the unmatched discharges
combined.df <- merge(new.merged.df, 
                     unmatched.discharge.df, 
                     all = TRUE)

# Save and write to csv
write.csv(file=paste0(output.directory, '/', run.string, '-', Sys.Date(),'-NeoTree-database.csv'), 
          combined.df, 
          row.names = F,
          quote=T)
saveRDS(combined.df, file=paste0(output.directory, '/', run.string, '-', Sys.Date(), '-NeoTree-database.rds'))

# Unmatched admissions
unmatched.admission.df <- new.admission.df[which(!new.admission.df$Admission.UID %in%
                                                   new.merged.df$Admission.UID),]
# Write this final database to csv
write.csv(file=paste0(output.directory, '/', run.string, '-', Sys.Date(),'-NeoTree-unmatched-admissions.csv'), 
          unmatched.admission.df, 
          row.names = F,
          quote=T)
# N.B. Currently these numbers don't completely match so something is slightly
# wrong. However, let's assume it mostly works for now. 

# Save all the cumulative json files that have been processed in this run.
# Next time the script is run, change the old.json.files 
# to this filename
cat(c(admission.files, discharge.files, old.json.files), 
    sep =  '\n',
    file = paste0(run.string, '-', Sys.Date(), '-json-files.txt')) 

# Summary statistics by HCWID
# Not sure if this produces the intended output files or not. 
source('summary-statistics.R')

# Felicity's plotting 
# Currently this throws an error. 
# Not sure if this produces the intended output files or not. 
source('plot-generation.R')
