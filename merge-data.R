# NeoTree data merging scripts.
# This processes json files from tablets to perform merges and create all outputs.
# Author: Liam Shaw, liam.philip.shaw@gmail.com
# Current version update: Sam Neal, samuel.neal.19@ucl.ac.uk

############################
### SETUP BEFORE RUNNING ###

### OPTIONAL ###
# Set this depending on which computer you are 
# running the script on (sets working directory)
# Currently this is set to run on the UCL remote desktop
# and shouldn't need to be changed for regular use. 
setwd('S://ICH_PPP_CHAMPP_NeoTree/Zimbabwe-merged-data/Scripts/')
path <- '../NeoTreeZimJson/'
path2<- '../Outputs/Databases/'

### MANDATORY ###
# 1.
# Set this to the date and random string associated with
# the last time the script was run (most recent)
# Format: "YYYY-MM-DD-XXXXXXXXXX"
date_string<- "2020-03-18-LVBCB6851L"

# 2.
# Set this to whichever month of data you want to analyse
# Format: "YYYY-MM"
analysis_month<- "2020-03"

####### END OF SETUP #######
############################

# Library loading
library(tidyr)
library(hms)
library(data.table)
library(lubridate)
library(tibble)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(rjson)
library(plyr)
library(rmarkdown)
source('custom-functions.R') # defined bespoke functions 

# Load previous files
## List of previously read JSON files
old.json.files <- read.csv(paste0(path2, date_string, '/', date_string, '-NeoTreeZim-json-files.txt'),  
                           stringsAsFactors = F,
                           header = F)$V1
## Old admission file (rds)
old.admission.df <- readRDS(paste0(path2, date_string, '/', date_string, '-NeoTreeZim-admissions.rds'))
## Old discharge file (rds)
old.discharge.df <- readRDS(paste0(path2, date_string, '/', date_string, '-NeoTreeZim-discharges.rds'))

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

# Random string to precede filenames (for uniqueness, to prevent accidental overwriting)
# Create output directories using this random string for different types of output files
### SRN 16.12.19 - Created a separate directory for Outputs and subdirectories
# for each type of output from data merges.
run.string <- randomString()
output.directory <- paste0(Sys.Date(), '-', run.string)

dir.create(file.path('../Outputs/Databases', output.directory), 
           showWarnings = FALSE)
dir.create(file.path('../Outputs/Statistics', output.directory), 
           showWarnings = FALSE)
dir.create(file.path('../Outputs/Plots', output.directory), 
           showWarnings = FALSE)

# Read in files and convert from json (function from custom-functions*.R)
admission.df <- jsonToDataFrame(admission.filenames, 
                                scriptType = 'Admission')
discharge.df <- jsonToDataFrame(discharge.filenames, 
                                scriptType = 'Discharge')

# Deduplicate (functions from custom-functions*.R)
admission.df <- deduplicateAdmission(admission.df)
discharge.df <- deduplicateDischarge(discharge.df)

# Remove those previously seen in old versions
admission.df <- admission.df[which(!admission.df$NeoTreeID %in% 
                                     old.admission.df$NeoTreeID),]
discharge.df <- discharge.df[which(!discharge.df$NeoTreeID %in% 
                                     old.discharge.df$NeoTreeID),]

# Integrate these together
#FF 121219 first allow inclusion of new variable names
#(which may have been added since last data merge)

old.admission.df[setdiff(names(admission.df), names(old.admission.df))] <- NA
admission.df[setdiff(names(old.admission.df), names(admission.df))] <- NA
old.discharge.df[setdiff(names(discharge.df), names(old.discharge.df))] <- NA
discharge.df[setdiff(names(old.discharge.df), names(discharge.df))] <- NA
### Sam & Mario notes:
### setdiff (a,b) returns the elements which are in a but not in b
### note that setdiff(a,b) != setdiff(b,a), and that setdiff(a,b)=integer(0)  if 
### all elements of a are contained in b
### the previous lines assign NA to elements of df which are in names(admission.df) but 
### aren't in names(old.admission.df)

### Sam & Mario notes:
### N.B. new.admission.df and new.discharge.df refer to 'new' R objects (in the 
### sense that they were created now, i.e. not in the sense that they refer to new
### babies admitted/discharged since last data merge).
### It is the admision/discharge.df objects that ARE new babies since last data merge.

new.admission.df <- rbind(old.admission.df, admission.df)
new.discharge.df <- rbind(old.discharge.df, discharge.df)
### pastes vertically old.*.df and *.df (i.e. appends 'new' babies to all previous babies)


# Save these versions
### SRN 16.12.19 - changed file paths to match new output directories
### SRN 16.12.19 - format of file name rearranged for consistency
saveRDS(new.admission.df, file = paste0('../Outputs/Databases/', output.directory,
                                        '/', Sys.Date(),'-', run.string, '-NeoTreeZim-',
                                        'admissions.rds'))
saveRDS(new.discharge.df, file = paste0('../Outputs/Databases/', output.directory,
                                        '/', Sys.Date(),'-', run.string, '-NeoTreeZim-',
                                        'discharges.rds'))

# Check for matched admission/discharge pairs
# (findMatches* function from custom-functions*.R)
new.merged.df <- findMatchesWithinNewAdmissionDischarge(new.admission.df, 
                                                        new.discharge.df)

# Save new.merged.df as RDS and CSV files

saveRDS(new.merged.df, file = paste0('../Outputs/Databases/', output.directory,
                                     '/', Sys.Date(),'-', run.string, '-NeoTreeZim-',
                                     'matched.rds'))
write.csv(file = paste0('../Outputs/Databases/', output.directory,
                        '/', Sys.Date(),'-', run.string, '-NeoTreeZim-',
                        'matched.csv'),
          new.merged.df,
          row.names = FALSE,
          quote = TRUE)

### Sam & Mario notes:
### this function creates a new (space!) merged.df binding admissions and discharge
### which is returned from the function then assigned to new.merged.df in the
### global environment.
### also, it cleans some variables, e.g. by eliminating unnecessary spaces

unmatched.discharge.df <- new.discharge.df[which(!new.discharge.df$NeoTreeID %in% 
                                                   new.merged.df$Discharge.NeoTreeID),]

# Save unmatched.discharge.df as RDS and CSV files

saveRDS(unmatched.discharge.df, file = paste0('../Outputs/Databases/', output.directory,
                                              '/', Sys.Date(),'-', run.string, '-NeoTreeZim-',
                                              'unmatched-discharges.rds'))
write.csv(file = paste0('../Outputs/Databases/', output.directory,
                        '/', Sys.Date(),'-', run.string, '-NeoTreeZim-',
                        'unmatched-discharges.csv'),
          unmatched.discharge.df,
          row.names = FALSE,
          quote = TRUE)

# Unmatched admissions - SRN 14.01.20 moved up a few lines in the code
unmatched.admission.df <- new.admission.df[which(!new.admission.df$Admission.UID %in%                                                    new.merged.df$Admission.UID),]

# Write this unmatched admissions database to rds and csv
saveRDS(unmatched.admission.df,
        file = paste0('../Outputs/Databases/', output.directory,
                      '/', Sys.Date(),'-', run.string, '-NeoTreeZim-',
                      'unmatched-admissions.rds'))
write.csv(file=paste0('../Outputs/Databases/', output.directory,
                      '/', Sys.Date(), '-', run.string, '-NeoTreeZim-',
                      'unmatched-admissions.csv'), 
          unmatched.admission.df, 
          row.names = F,
          quote=T)

# Combine the merged pairs with the unmatched discharges
#FF 30.11 name of dataframe changed from combined.df to complete.df for consistency
#with previous iterations
complete.df <- merge(new.merged.df, unmatched.discharge.df, all = TRUE)

# Save and write complete.df to csv
saveRDS(complete.df, file=paste0('../Outputs/Databases/', output.directory,
                                 '/', Sys.Date(), '-', run.string, '-NeoTreeZim-',
                                 'database.rds'))

write.csv(file=paste0('../Outputs/Databases/', output.directory,
                      '/', Sys.Date(), '-', run.string, '-NeoTreeZim-',
                      'database.csv'), 
          complete.df, 
          row.names = F,
          quote=T)


# N.B. Currently these numbers don't completely match so something is slightly
# wrong. However, let's assume it mostly works for now. 

### IMPORTANT ###
# Save all the cumulative json files that have been processed in this run.
# Next time the script is run, change the old.json.files 
# to this filename
cat(c(admission.files, discharge.files, old.json.files), 
    sep =  '\n',
    file = paste0('../Outputs/Databases/', output.directory,
                  '/', Sys.Date(), '-', run.string, '-NeoTreeZim-',
                  'json-files.txt'))

### GENERATE STATISTICS ###
source("statistics.R")
