######################
###### IMPORTANT ##### 
# Set this depending on which computer you are 
# running the script on (sets working directory)
setwd('./')
path <- '../json/'
# List of previously read JSON files to avoid reading in
old.json.files <- read.csv('json-files-',  
                           stringsAsFactors = F,
                           header = F)$V1
previous.unmatched.admission.df <- ''


# Library loading
library(tidyr)
library(hms)
library(data.table)
library(Stack)
library(ggplot2)
library(rjson)
library(plyr)
source('conversion-functions.R') # defined bespoke functions 

# File locations
admission.files <- list.files(path = path, 
                              pattern = '*NeoTree___Zimbabwe*.json')
admission.filenames <- paste0(path, 
                              '/', 
                              admission.files)
discharge.files <- list.files(path = path, 
                              pattern = '*NeoDischarge___Zimbabwe*.json')
discharge.filenames <- paste0(path, 
                               '/',
                               discharge.files)

# Read in files and convert from json
admission.df <- jsonToDataFrame(admission.filenames, scriptType = "Admission")
discharge.df <- jsonToDataFrame(discharge.filenames, scriptType = "Discharge")

# Deduplicate
admission.df <- deduplicateAdmission(admission.df)
discharge.df <- deduplicateDischarge(discharge.df)

# Find matches 
merged.df <- findMatchesWithinNewAdmissionDischarge(admission.df, discharge.df)

# Add in unmatched discharges
final.database.df <- addUnmatchedDischarges(merged.df, discharge.df)

# Write this final database to csv
write.csv(file=paste0(Sys.Date(),'-NeoTree-database.csv'), 
          final.database.df, 
          row.names = F,
          quote=T)

# Save the unmatched admissions as well   
unmatched.admissions <- admission.df$Admission.UID[which(!admission.df$Admission.UID %in% 
merged.df$Admission.UID)]
# Unmatched names
admission.need.match <- admission.df$Admission.UID[which(!admission.df$Admission.UID %in% admission.df.matched$Admission.UID)]
discharge.need.match <- discharge.df$NeoTreeID[which(!discharge.df$NeoTreeID %in% merged.df$Discharge.NeoTreeID)]
admission.need.match.df <- cbind(admission.need.match, 
                                 sapply(admission.need.match, 
                                        function(x) as.character(admission.df$Admission.DateAdmission[which(admission.df$Admission.UID==x)])),
                                 sapply(admission.need.match, 
                                        function(x) as.character(admission.df$Admission.TimeAdmission[which(admission.df$Admission.UID==x)])))
colnames(admission.need.match.df) <- c("Admission.UID", "Admission.DateAdmission", "AdmissionTimeAdmission")
admission.need.match.df <- admission.df[which(admission.df$Admission.UID %in% admission.need.match),]
#add date thing
admission.need.match.df$Admission.DateAdmission <- as.Date(as.numeric(admission.need.match.df$Admission.DateAdmission), origin="1970-01-01")


write.csv(admission.need.match.df, 
          file=paste0(Sys.Date(),'-NeoTree-admission-ID-unmatched.csv'),
          row.names = F)



# Save the files that have been processed in this run.
# Next time the script is run, change the old.json.files 
# to this filename
cat(c(admission.files, discharge.files), 
    sep =  '\n',
    file = paste0('json-files-', Sys.Date(), '.txt')) 



# Summary statistics by HCWID
# Table of admissions per week
print("Weekly admissions by worker, including unmatched")
weekly.admissions <- table(admission.df$Admission.MonthWeekAdmission, admission.df$Admission.HCWID)
write.csv(weekly.admissions, file=paste0(Sys.Date(), ' NeoTree Weekly Admissions.csv'),
          row.names = T)

print("Weekly discharges by worker")
weekly.discharges <- table(discharge.df$Discharge.MonthWeekDischarge, discharge.df$Discharge.HCWIDDis)
write.csv(weekly.discharges, file=paste0(Sys.Date(), ' NeoTree Weekly Discharges.csv'),
          row.names = T)
#discharge/death by admission month
#Need to change Month number as needed eg 07 for July
Month<-subset(complete.df, complete.df$Admission.MonthAdmission=="08")
Tab<-table(Month$Discharge.HCWIDDis,Month$Discharge.NeoTreeOutcome)
write.csv(Tab, file=paste0('DischargeMatchedByHCWID.csv'), row.names = T)
#number of unmatched by HCWID
#Need to change Month number e.g. 07 for July as above
MonthUnmatched<-subset(discharge.need.match.df, discharge.need.match.df$Discharge.MonthDischarge=="08")
tabU<-table(MonthUnmatched$Discharge.HCWIDDis,MonthUnmatched$Discharge.NeoTreeOutcome)
write.csv(TabU, file=paste0('DischargeUnmatchedByHCWID.csv'), row.names = T)
#attempting to merge unmatched discharges with matched file

complete.df<-rbindlist(list(merged.df, discharge.need.match.df), fill = TRUE)
write.csv(complete.df, file=paste0('Complete_WIthUnmatchedDis.csv'), row.names = T)
tab<-table(complete.df$Discharge.NeoTreeOutcome)
tab
#creating final outcome variable
complete.df$Discharge.DateTimeFinalOutcome <- ifelse(!is.na(complete.df$Discharge.DateTimeDeath), complete.df$Discharge.DateTimeDeath, ifelse(!is.na(complete.df$Discharge.DateTimeDischarge), complete.df$Discharge.DateTimeDischarge, NA))
write.csv(complete.df, file=paste0(Sys.Date(), ' Final.csv'),
          row.names = T)
#duration of admission
complete.df$DateFinal <- as.Date(gsub(pattern = "T.*", replacement = "", complete.df$Discharge.DateTimeFinalOutcome), format="%Y-%m-%d")
complete.df$MonthFinal <- format(complete.df$DateFinal, "%m") # Month as number from 1-12
complete.df$MonthFinal<- ordered(complete.df$MonthFinal, levels=c("11", "12", "01", "02","03","04","05","06","07","08","09","10"))
complete.df$DaysAdmission<- (complete.df$DateFinal)-(complete.df$Admission.DateAdmission)
summary(complete.df$DaysAdmission)
tapply(complete.df$DaysAdmission,complete.df$Discharge.NeoTreeOutcome,summary)
complete.df$DaysAdmission<-as.numeric(complete.df$DaysAdmission)
table(complete.df$DaysAdmission)