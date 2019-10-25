######################
###### IMPORTANT ##### 
# Set this depending on which computer you are 
# running the script on (sets working directory)
setwd("S:/ICH_PPP_CHAMPP_NeoTree/Zimbabwe-merged-data/NeoTreeZimJson")
path <- '.'

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
admission.filenames <- paste0(path, '/', list.files(path = path, pattern ="*NeoTree___Zimbabwe*.json"))
discharge.filenames <-  paste0(path, '/',list.files(path = path, pattern ="*NeoDischarge___Zimbabwe*.json"))
lab.filenames <-  paste0(path, '/', list.files(path = path, pattern = "*NeoLab*"))

# Read in files and convert from json
admission.df <- jsonToDataFrame(admission.filenames, scriptType = "Admission")
discharge.df <- jsonToDataFrame(discharge.filenames, scriptType = "Discharge")

# Deduplicate
admission.df <- deduplicateAdmission(admission.df)
discharge.df <- deduplicateDischarge(discharge.df)

# Check the 'other' options to make consistent

# Admission.AdmReasonOth = macrosomia or Macrosomia could you change Admission.AdmReason = Macro (rather than O for other)
admission.df$Admission.AdmReason <- ifelse(admission.df$Admission.AdmReasonOth %in% c("macrosomia", "Macrosomia"), "Macro", admission.df$Admission.AdmReason)
# Same for Admission.Diagnoses = Macro (rather than OTH for other) if Admission.DiagnosesOth = macrosomia or Macrosomia
admission.df$Admission.Diagnoses <- ifelse(admission.df$Admission.Diagnoses %in% c("macrosomia", "Macrosomia"), 
                                           "Macro", admission.df$Admission.AdmReason)

# Similarly if Admission.AdmReasonOth = safekeeping or safe keeping or Safe keeping could you change Admission.AdmReason = Safe (rather than O for other)
admission.df$Admission.AdmReason <- ifelse(admission.df$Admission.AdmReasonOth %in% c("safekeeping", "safe keeping", "Safe keeping"), 
                                           "Safe", admission.df$Admission.AdmReason)
admission.df$Admission.Diagnoses <- ifelse(admission.df$Admission.DiagnosesOth %in% c("safekeeping", "safe keeping", "Safe keeping"), 
                                           "Safe", admission.df$Admission.Diagnoses)
#Discharge.DIAGDIS1OTH = macrosomia or Macrosomia could you change Discharge.DIAGDIS1 = Mac(rather than O for other)
discharge.df$Discharge.DIAGDIS1 <- ifelse(discharge.df$Discharge.DIAGDIS1OTH %in% c("macrosomia", "Macrosomia"), "Mac", discharge.df$Discharge.DIAGDIS1)
# Same for Admission.Diagnoses = Macro (rather than OTH for other) if Admission.DiagnosesOth = macrosomia or Macrosomia

# Similarly if Discharge.DIAGDIS1OTH = safekeeping or safe keeping or Safe keeping could you change Discharge.DIAGDIS1 = Safe (rather than O for other)
discharge.df$Discharge.DIAGDIS1 <- ifelse(discharge.df$Discharge.DIAGDIS1OTH %in% c("safekeeping", "safe keeping", "Safe keeping", "Safe Keeping", "Safekeeping", "SafeKeeping","safe keeping:mother had 3rd degree tear"), 
                                          "Safe", discharge.df$Discharge.DIAGDIS1)
#Jaundice in Discharge.DIAGDIS1OTH
discharge.df$Discharge.DIAGDIS1 <- ifelse(discharge.df$Discharge.DIAGDIS1OTH %in% c("Neonatal jaundice", "Neonatal Jaundice", "Jaundice", "NNJ","NNJ on day 1 of life"), 
                                          "JAUN", discharge.df$Discharge.DIAGDIS1)
#BBA in Discharge.DIAGDIS1OTH
discharge.df$Discharge.DIAGDIS1 <- ifelse(discharge.df$Discharge.DIAGDIS1OTH %in% c("BBA", "born before arrival","Born before arrival","Born before Arrival","Born Before Arrival","BORN BEFORE ARRIVAL","born before arrival. ophthalmia neonatorum"), 
                                          "BBA", discharge.df$Discharge.DIAGDIS1)
#Congenital syphilis in Discharge.DIAGDIS1OTH
discharge.df$Discharge.DIAGDIS1 <- ifelse(discharge.df$Discharge.DIAGDIS1OTH %in% c("syphillis", "congenital syphilis","congenital syphillis","Congenital Syphillis"), 
                                          "SYPH", discharge.df$Discharge.DIAGDIS1)
#syphilis exposure in Discharge.DIAGDIS1OTH
discharge.df$Discharge.DIAGDIS1 <- ifelse(discharge.df$Discharge.DIAGDIS1OTH %in% c("Syphilis exposure", "syphillis exposure","Syphillis Exposure mum untreated","syphyllis exposure RPR positive untreated mother","RPR exposed"), 
                                          "SYPHEx", discharge.df$Discharge.DIAGDIS1)
table(discharge.df$Discharge.DIAGDIS1)
#Other causes of death
table(discharge.df$Discharge.CauseDeathOther)
discharge.df$Discharge.CauseDeath <- ifelse(discharge.df$Discharge.CauseDeathOther %in% c("gastroschisis", "Gastroschisis", "Gastrochisis"), 
                                            "Gastroschisis", discharge.df$Discharge.CauseDeath)
discharge.df$Discharge.CauseDeath <- ifelse(discharge.df$Discharge.CauseDeathOther %in% c("Aspiration"), 
                                            "ASP", discharge.df$Discharge.CauseDeath)
discharge.df$Discharge.CauseDeath <- ifelse(discharge.df$Discharge.CauseDeathOther %in% c("RDS and Risk of Sepsis", "Respiratory distress", "Macrosomia with respiratory distress","RDS","Respiratory Distress","Respiratory distress syndrome"), 
                                            "RDS", discharge.df$Discharge.CauseDeath)
discharge.df$Discharge.CauseDeath <- ifelse(discharge.df$Discharge.CauseDeath %in% c("Gastro", "Gastroschisis", "Gastrochisis"), 
                                            "Gastroschisis", discharge.df$Discharge.CauseDeath)
table(discharge.df$Discharge.CauseDeath)


# Those discharges which have a match
discharge.have.match <-  discharge.df$NeoTreeID[discharge.df$NeoTreeID %in% admission.df$NeoTreeID]
have.match.df <- data.frame(admissionID=discharge.have.match,
                            dischargeID=discharge.have.match,
                            matchType="perfect")
# Those discharges which don't have a match
discharge.need.match <- discharge.df$NeoTreeID[!discharge.df$NeoTreeID %in% admission.df$NeoTreeID]
admission.need.match <- admission.df$NeoTreeID[!admission.df$NeoTreeID %in% discharge.df$NeoTreeID]

# Find possible matches using mismatch function
discharge.possible.matches <- unlist(as.vector(sapply(discharge.need.match, function(x) 
  admission.need.match[which(admission.need.match %in% makeAllMismatches(x))])))
# Check for duplicates
duplicates <- discharge.possible.matches[duplicated(discharge.possible.matches)]
discharge.duplicated <- names(discharge.possible.matches[which(discharge.possible.matches %in% duplicates)])


discharge.possible.matches <- discharge.possible.matches[!discharge.possible.matches %in% duplicates]
matches.df <- data.frame(admissionID=discharge.possible.matches, stringsAsFactors = F)
matches.df$dischargeID <- names(discharge.possible.matches)
matches.df$matchType <- "approximate"
matches.df <- rbind(matches.df, have.match.df)

# Find those without matches still
discharge.need.match <- discharge.df$NeoTreeID[which(!discharge.df$NeoTreeID %in% matches.df$dischargeID)]
admission.need.match <- admission.df$NeoTreeID[which(!admission.df$NeoTreeID %in% matches.df$admissionID)]
# Use the longer ID for these ones
admission.UID.need.match <- sapply(admission.need.match,
                                   function(x) admission.df$Admission.UID[which(admission.df$NeoTreeID==x)])
# Find matches for 8 digit ones
makeEightCharacterMismatches <- function(UID){
  dash <- paste0(substr(UID, 1, 4), "-", substr(UID, nchar(UID)-3, nchar(UID)))
  space <- paste0(substr(UID, 1, 4), " ", substr(UID, nchar(UID)-3, nchar(UID)))
  continuous <- paste0(substr(UID, 1, 4), substr(UID, nchar(UID)-3, nchar(UID)))
  return(c(UID, dash, space, continuous))
}
discharge.possible.matches <- unlist(as.vector(sapply(discharge.need.match, function(x) 
  as.character(admission.UID.need.match[which(admission.UID.need.match %in% makeEightCharacterMismatches(x))]))))
# There are some duplicates...just exclude them
duplicate.discharge <- discharge.possible.matches[duplicated(discharge.possible.matches)]
discharge.possible.matches <- discharge.possible.matches[!duplicated(discharge.possible.matches)]


# names are the discharge NeoTreeIDs, values are the admission UIDs
# add them to the matched data frame
have.eight.match.df <- data.frame(admissionID=sapply(discharge.possible.matches, function(x) admission.df$NeoTreeID[admission.df$Admission.UID==x]),
                                  dischargeID=names(discharge.possible.matches),
                                  matchType="approximate")
matches.df <- rbind(matches.df, have.eight.match.df)

# Create a merged data frame of all the matched pairs
admission.df.matched <- admission.df[which(admission.df$NeoTreeID %in% matches.df$admissionID),]
admission.df.matched.2 <- admission.df[which(admission.df$Admission.UID %in% matches.df$admissionID),]
admission.df.matched <- rbind(admission.df.matched, admission.df.matched.2)
rownames(admission.df.matched) <- admission.df.matched$NeoTreeID
admission.df.matched$Admission.NeoTreeID <- admission.df.matched$Admission.UID # use UID
admission.df.matched$NeoTreeID <- NULL

# Only take the subset that are not duplicated 
#matches.df <- matches.df[which(matches.df$admissionID %in% c(admission.df.matched$NeoTreeID, admission.df.matched$Admission.UID)),]
# Discharges
discharge.df.matched <- discharge.df[which(discharge.df$NeoTreeID %in% matches.df$dischargeID),]
rownames(discharge.df.matched) <- discharge.df.matched$NeoTreeID
discharge.df.matched$Discharge.NeoTreeID <- discharge.df.matched$NeoTreeID
discharge.df.matched$NeoTreeID <- NULL

merged.df <- cbind(admission.df.matched[matches.df$admissionID,],
                   discharge.df.matched[matches.df$dischargeID,])
merged.df$matchType <- ifelse(merged.df$Admission.NeoTreeID %in% matches.df$admissionID[which(matches.df$matchType=="perfect")],
                              "perfect", "approximate")
other.columns <- colnames(merged.df)# order column names
other.columns <- other.columns[which(!other.columns %in% c("Admission.NeoTreeID", "Discharge.NeoTreeID","matchType" ))]
merged.df <- merged.df[,c("Admission.NeoTreeID", "Discharge.NeoTreeID", "matchType", other.columns)]
merged.df$matchType <- ifelse(merged.df$Admission.NeoTreeID==merged.df$Discharge.NeoTreeID, "perfect", "approximate")
#add date thing
merged.df$Admission.DateAdmission <- as.Date(as.numeric(merged.df$Admission.DateAdmission), origin="1970-01-01")
merged.df$Discharge.DateDischarge <- as.Date(as.numeric(merged.df$Discharge.DateDischarge), origin="1970-01-01")

# Write to csv
write.csv(file=paste0(Sys.Date(),'NeoTree-matched-admission-discharge-pairs.csv'), 
          merged.df, 
          row.names = F,
          quote=T)
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
admission.need.match.df$Admission.DateAdmission <- as.Date(as.numeric(merged.df$Admission.DateAdmission), origin="1970-01-01")


write.csv(admission.need.match.df, 
          file=paste0(Sys.Date(),'NeoTree-admission-ID-unmatched.csv'),
          row.names = F)
discharge.need.match.df <- cbind(discharge.need.match, 
                                 sapply(discharge.need.match, 
                                        function(x) as.character(discharge.df$Discharge.DateDischarge[which(discharge.df$NeoTreeID==x)])),
                                 sapply(discharge.need.match, 
                                        function(x) as.character(discharge.df$Discharge.TimeDischarge[which(discharge.df$NeoTreeID==x)])))
colnames(discharge.need.match.df) <- c("Discharge.ID", "Discharge.DateDischarge", "Discharge.TimeDischarge")
discharge.need.match.df <- discharge.df[which(discharge.df$NeoTreeID %in% discharge.need.match),]
discharge.need.match.df$Discharge.DateDischarge<-as.Date(as.numeric(discharge.need.match.df$Discharge.DateDischarge), origin="1970-01-01")
write.csv(discharge.need.match.df, 
          file=paste0(Sys.Date(),'NeoTree-discharge-ID-unmatched.csv'),
          row.names = F)



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