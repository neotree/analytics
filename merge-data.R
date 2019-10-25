library(rjson)
library(plyr)
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


# Function to convert dates
# Convert the date/time field to the correct format etc.
convertDates <- function(df.plot, scriptType = "Admission"){
  
  if (scriptType=="Admission"){
    df.plot$DateAdmission <- as.Date(gsub(pattern = "T.*", replacement = "", df.plot$DateTimeAdmission), format="%Y-%m-%d")
    df.plot$WeekdayAdmission <- ordered(weekdays(df.plot$DateAdmission),
                                        levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    # Actual day (monday, tuesday etc.)
    df.plot$MonthAdmission <- format(df.plot$DateAdmission, "%m") # Month as number from 1-12
    df.plot$WeekAdmission <- format(df.plot$DateAdmission, "%W") # Week as number from 0-51
    df.plot$MonthWeekAdmission <- paste0("Month: ", df.plot$MonthAdmission, ", Week: ", df.plot$WeekAdmission)
    
    df.plot$TimeAdmission <- as.hms(gsub(pattern="\\+.*", replacement="",
                                         gsub(pattern = ".*T", replacement = "", df.plot$DateTimeAdmission)))
    return(df.plot)
  }
  
  if (scriptType=="Discharge"){
    df.plot$DateDischarge <- as.Date(gsub(pattern = "T.*", replacement = "", df.plot$DateTimeDischarge), format="%Y-%m-%d")
    df.plot$WeekdayDischarge <- ordered(weekdays(df.plot$DateDischarge),
                                        levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    # Actual day (monday, tuesday etc.)
    df.plot$MonthDischarge <- format(df.plot$DateDischarge, "%m") # Month as number from 1-12
    df.plot$WeekDischarge <- format(df.plot$DateDischarge, "%W") # Week as number from 0-51
    df.plot$MonthWeekDischarge <- paste0("Month: ", df.plot$MonthDischarge, ", Week: ", df.plot$WeekDischarge)
    
    df.plot$TimeDischarge <- as.hms(gsub(pattern="\\+.*", replacement="",
                                         gsub(pattern = ".*T", replacement = "", df.plot$DateTimeDischarge)))
    return(df.plot)
    
  }
  
  
}

# Read in all files
admission.filenames <- paste0(path, '/', list.files(path = path, pattern ="*NeoTree___Zimbabwe*.json"))
discharge.filenames <-  paste0(path, '/',list.files(path = path, pattern ="*NeoDischarge___Zimbabwe*.json"))
lab.filenames <-  paste0(path, '/', list.files(path = path, pattern = "*NeoLab*"))


jsonToDataFrame <- function(json.filenames, scriptType = "Admission"){
  json.files = vector("list", length(json.filenames))
  
  for (i in 1:length(json.filenames)){
    f = json.filenames[i]
    # Check files first to ignore the bad files
    a <- readChar(f, file.info(f)$size)
    if(substr(a, nchar(a), nchar(a))=="}"){
           json.files[[i]] = do.call("rbind", fromJSON(file = f) )
    }
    else{
      print(paste0("Error in file: ", f, " -- skipping!"))
    }
  }
  df <- json.files[[1]]
  if (length(json.filenames)>1){
    for (i in 2:length(json.filenames)){
      df <- cbind(df, json.files[[i]])
    }
  }
  
  session.list <- apply(df, FUN= function(x) x$sessions$entries, MARGIN=2)
  
  
  # 1. Just to keep track of the session names (assuming each tablet has independent sessions on?)
  session.names <- paste("session", seq(1,length(session.list)))
  # 2. Get all the variable keys by running function over list
  keys <- lapply(session.list, function(x) lapply(x, function(y) y$key))
  # 3. Get all the variable values by running function over list
  values <- lapply(session.list, function(x) lapply(x, function(y) paste(unlist(lapply(y$values, function(z) unlist(z$value))), collapse=',')))
  
  
  
  # Combine these (1, 2, 3) into a single object with 3 columns: session, variable name, variable value
  session.data.list <- sapply(seq(1,length(session.list)), function(x) cbind(session.names[x], keys[[x]], values[[x]]))
  
  # Convert to a single data frame
  df <- ldply(session.data.list, data.frame)
  
  # Unlist function (unlists a list, keeping NULL entries as NA)
  #unlistKeepNull <- function(a.list){
  #  do.call(c, lapply(a.list, (function(x){
  #    if (is.null(x) | length(x) == 0) {NA}
  #    else { x }
  #  }
  #  )))
  #}
  # Create version of the data frame for analysis by unlisting each column
  #df.analysis <- as.data.frame(apply(df, MARGIN=2, FUN=unlistKeepNull), stringsAsFactors = FALSE)
  colnames(df) <- c("session", "key", "value")
  
  # Problem of duplicate identifiers? The 7 duplicate row pairs in this dataset
  # are all genuine duplicates, so OK to remove (I *think*)
  if (length(table(duplicated(df)))>1){
    df <- df[-which(duplicated(df)),]
  }
  
  # First, get rid of duplicated key values e.g. two 'DischWeight' entries, NA and real, for a single session
  if (length(table(duplicated(paste(df$session, df$key))))>0){
    df <- df[-which(duplicated(paste(df$session, df$key))),]
  }
  
  # Spread the data into wide format
  df.analysis <- spread(na.omit(df), key = key, value = value)
  
  # Remove those which have no HCWID, because these are not real entries
  df.analysis <- df.analysis[which(!is.na(df.analysis$HCWID)),]
  
  # Convert date format fields
  df.analysis <- convertDates(df.analysis, scriptType = scriptType)
  
  # Add script type to column names
  colnames(df.analysis) <- as.character(sapply(colnames(df.analysis), function(x) paste(scriptType, x, sep=".")))
  
  # Set 'NULL' values to NA
  #df.analysis <- apply(df.analysis, FUN=unlist, MARGIN=2)
  mat <- apply(df.analysis, unlist, MARGIN=1:2)
  mat[mat=="NULL"] <- NA
  df.analysis <- data.frame(mat)
  for (col in colnames(df.analysis)){
    df.analysis[,col] <- unlist(df.analysis[,col])
  }
  df.analysis[which(is.null(df.analysis)),] <- NA
  
  # Convert HCWID to upper case
  if (scriptType=="Admission"){
    df.analysis[,"Admission.HCWID"] <- toupper(gsub(" ", "", df.analysis[,"Admission.HCWID"]))
  }
  if (scriptType=="Discharge"){
    df.analysis[,"Discharge.HCWIDDis"] <- toupper(gsub(" ", "", df.analysis[,"Discharge.HCWIDDis"]))
  }
  
  return(df.analysis)
}

admission.df <- jsonToDataFrame(admission.filenames, scriptType = "Admission")
discharge.df <- jsonToDataFrame(discharge.filenames, scriptType = "Discharge")
# Keep 8 digit ID if after 30.1.19

# Get rid of duplicate rows (first column is session, but some are duplicated)
admission.df <- admission.df[!duplicated(admission.df[,seq(2, ncol(admission.df))]),]
discharge.df <- discharge.df[!duplicated(discharge.df[,seq(2, ncol(discharge.df))]),]
# Convert ID to lower case
admission.df$Admission.UID <- tolower(admission.df$Admission.UID)
discharge.df$NeoTreeID <- tolower(discharge.df$Discharge.NeoTreeID)

# First 3 and last 3 characters of admission.df$UID should match discharge.df$NeoTreeID
admission.df$NeoTreeID <- sapply(admission.df$Admission.UID, function(x) paste0(substr(x, start=1, stop=3),
                                                                                substr(x, start=(nchar(x)-2), stop=(nchar(x)))))
#admission.df$NeoTreeID <- sapply(seq(1, nrow(admission.df), 1), 
#       function(x) ifelse(admission.df$Admission.DateAdmission[x]>"2018-01-29",
#                          admission.df$Admission.UID[x], admission.df$NeoTreeID[x]))
#admission.df$NeoTreeID <- tolower(admission.df$NeoTreeID)

# Remove duplicates if they exist
if (length(which(duplicated(admission.df$NeoTreeID)))>0){
  admission.df <- admission.df[-which(duplicated(admission.df$NeoTreeID)),]
}
if (length(which(duplicated(discharge.df$NeoTreeID)))>0){
  discharge.df <- discharge.df[-which(duplicated(discharge.df$NeoTreeID)),]
  
}
table(discharge.df$NeoTreeID %in% admission.df$NeoTreeID)

discharge.df$Discharge.NeoTreeID <- NULL
table(discharge.df$NeoTreeID %in% admission.df$NeoTreeID)

# Check the 'other' options to make consistent

#Admission.AdmReasonOth = macrosomia or Macrosomia could you change Admission.AdmReason = Macro (rather than O for other)
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

# Function to make all possible mismatches from admission UIDs
makeAllMismatches <- function(UID){
  mismatches <- c(UID) # To store possible mismatches
  # oe05-0006 
  # oe05 0006 
  # Replace dashes by spaces and vice versa
  mismatches <- c(mismatches, gsub(" ", "-", UID)) # space to dash
  mismatches <- c(mismatches, gsub("-", " ", UID)) # dash to space
  mismatches <- c(mismatches, gsub("-", "", UID)) # remove dash
  mismatches <- c(mismatches, gsub(" ", "", UID)) # remove space
  
  
  # all possible lengths of repeat zeroes
  mismatches <- c(mismatches, gsub("(0)\\1+", "0", mismatches))
  mismatches <- c(mismatches, gsub("(0)\\1+", "00", mismatches))
  mismatches <- c(mismatches, gsub("(0)\\1+", "000", mismatches))
  mismatches <- c(mismatches, gsub("(0)\\1+", "0000", mismatches))
  # Expand zeroes
  mismatches <- c(mismatches, gsub("(0.*?)0", "\\100", mismatches))
  mismatches <- c(mismatches, gsub("(0.*?)0", "\\1000", mismatches))
  mismatches <- c(mismatches, gsub("(0.*?)0", "\\10000", mismatches))
  # Insert dash before zeroes
  mismatches <- c(mismatches, gsub("0(.*)", "-0\\1", mismatches))
  
  
  # Replace second O with 0: OE050006 -> OEO50006
  mismatches <- c(mismatches, gsub("(o.*?)o", "\\10", UID))
  # More O/0 replacing
  mismatches <- c(mismatches, gsub("o", "0", mismatches))
  
  mismatches <- c(mismatches, gsub("([1-9])0", "\\1 ", UID))# Add space between first and second part (between first non-zero number and zero)
  mismatches <- c(mismatches, gsub("([1-9])0", "\\1-0", UID)) # Add dash between first and second part (between first non-zero number and zero)
  
  # EF780022 becomes EF7022
  if (nchar(UID)>=6){
    mismatches <- c(mismatches, paste0(substr(UID, 1, 3), substr(UID, nchar(UID)-2, nchar(UID))))
    mismatches <- c(mismatches, gsub("o", "0", mismatches))
  }
  # Apply to mismatches as well
  
  # Only keep unique ones
  mismatches <- unique(mismatches)
  return(mismatches)
}
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
#histogram of admission
p5<-ggplot(complete.df, aes(x=DaysAdmission, fill=Discharge.NeoTreeOutcome)) +
  geom_histogram(binwidth=2, position="dodge")+
  ylab("number")+
  xlab("Duration admission")+
  ggtitle("Outcome by Duration Admission")+
  scale_fill_discrete(name="Outcome",
                      breaks=c("DC","NND"),
                      labels=c("Discharge","Died"))
p5

#hie
complete.df$HIE<-ifelse(grepl("HIE",complete.df$Discharge.DIAGDIS1),"1",
                        ifelse(grepl("HIE",complete.df$Discharge.CauseDeath),"1",
                               ifelse(grepl("",complete.df$Discharge.CauseDeath),"0",
                                      ifelse(grepl("",complete.df$Discharge.DIAGDIS1),"0","NA"))))
#hypothermia 
complete.df$Admission.Temperature<-as.numeric(complete.df$Admission.Temperature)
complete.df$hypothermia<-cut(complete.df$Admission.Temperature, c(0,32,35.9,36.4,37.5,42),labels = c("Severe Hypothermia","Moderate Hypothermia","Mild Hypothermia","Normothermic","Fever"))
summary(complete.df$hypothermia)
p1<-ggplot(data=subset(complete.df, !is.na(hypothermia)), aes(x=hypothermia, fill=hypothermia)) +
  geom_bar(stat="count")+
  ylab("number")+
  xlab("Temperature category")+
  ggtitle("Temperature at admission\nHarare Central Hospital")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_discrete(name="Temperature in degrees",
                      breaks=c("Severe Hypothermia","Moderate Hypothermia","Mild Hypothermia","Normothermic","Fever"),
                      labels=c("0-32","32.1-35.9","36-36.4","36.5-37.5","37.6-42"))
p1
p2<-ggplot(data=subset(complete.df, !is.na(hypothermia)), aes(x=hypothermia, fill=Admission.InOrOut)) +
  geom_bar(stat="count", position="dodge")+
  ylab("number")+
  xlab("Temperature category")+
  ggtitle("Temperature at admission\nBy In or Outborn")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_discrete(name="In or Outborn",
                      breaks=c("TRUE","FALSE"),
                      labels=c("Inborn","Outborn"))
p2
complete.df$Admission.BW<-as.numeric(complete.df$Admission.BW)
p3<-ggplot(data=subset(complete.df, !is.na(hypothermia)), aes(x=Admission.BW, fill=hypothermia)) +
  geom_histogram(binwidth=500, position="dodge")+
  ylab("number")+
  xlab("Admission birth weight")+
  ggtitle("Temperature at admission\nBy birthweight")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_discrete(name="Temperature in degrees",
                      breaks=c("Severe Hypothermia","Moderate Hypothermia","Mild Hypothermia","Normothermic","Fever"),
                      labels=c("0-32","32.1-35.9","36-36.4","36.5-37.5","37.6-42"))
p3
complete.df$Admission.Gestation<-as.numeric(complete.df$Admission.Gestation)
p4<-ggplot(data=subset(complete.df, !is.na(hypothermia)), aes(x=Admission.Gestation, fill=hypothermia)) +
  geom_histogram(binwidth=2, position="dodge")+
  ylab("number")+
  xlab("Gestation")+
  ggtitle("Temperature at admission\nBy Gestation")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_discrete(name="Temperature in degrees",
                      breaks=c("Severe Hypothermia","Moderate Hypothermia","Mild Hypothermia","Normothermic","Fever"),
                      labels=c("0-32","32.1-35.9","36-36.4","36.5-37.5","37.6-42"))
p4

tab<-table(complete.df$Admission.InOrOut,complete.df$hypothermia)
write.csv(tab, file=paste0('HypothermiaInbornOutborn.csv'), row.names = T)

#Twins
complete.df$Multiple<-ifelse(grepl("S",complete.df$Admission.TypeBirth),"Singleton",
                             ifelse(grepl("Tr1",complete.df$Admission.TypeBirth),"Triplet",
                                    ifelse(grepl("Tr2",complete.df$Admission.TypeBirth),"Triplet",
                                           ifelse(grepl("Tr3",complete.df$Admission.TypeBirth),"Triplet",
                                                  ifelse(grepl("Tw1",complete.df$Admission.TypeBirth),"Twin",
                                                         ifelse(grepl("Tw2",complete.df$Admission.TypeBirth),"Twin",
                                                                ifelse(grepl("",complete.df$Admission.TypeBirth),"0","NA")))))))
multiple<-subset(complete.df, subset = Multiple %in% c("Twin","Triplet","Singleton"))
p14<-ggplot(data=subset(multiple, !is.na(Multiple)), aes(x=Admission.MonthAdmission, fill=Multiple)) +
  geom_bar(stat="count", position="dodge")+
  ylab("number")+
  xlab("Multiples by Month")+
  ggtitle("Multiples by month")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p14

#early versus late deaths
Dea<-subset(complete.df,complete.df$Discharge.NeoTreeOutcome=="NND")
Dea$DaysAdmission<-as.numeric(Dea$DaysAdmission)
table(Dea$DaysAdmission)
Dea$Early<-cut(Dea$DaysAdmission, c(-24,6,50),labels = c("Early","Late"))
summary(Dea$Early)
#quarterly data for M&M meeting 
Quarter1<-subset(complete.df, subset = Admission.MonthAdmission %in% c("01","02","03"))
Quarter2<-subset(complete.df, subset = Admission.MonthAdmission %in% c("04","05","06"))

table(Quarter1$Discharge.NeoTreeOutcome)
table(Quarter2$Discharge.NeoTreeOutcome)

Dea1<-subset(Quarter1, Quarter1$Discharge.NeoTreeOutcome=="NND")
Dea1$Early<-cut(Dea1$DaysAdmission, c(-24,6,50),labels = c("Early","Late"))
table(Dea1$Early)
table(Dea1$DaysAdmission)
p20<-ggplot(data=Dea1, aes(x=Discharge.CauseDeath, fill=Discharge.CauseDeath)) +
  geom_bar(stat="count")+
  ylab("number")+
  xlab("Cause of Death")+
  ggtitle("Cause of Death\nFirst Quarter 2019")+
  scale_x_discrete(breaks=c("ASP","CA","EONS","Gastroschisis","HIE","LONS","OTH","PN","PR","PRRDS","RDS"),
                   labels=c("Aspiration","Congenital abnormality","EONS","Gastroschisis","HIE","Late Onset Sepsis","Other","Pneumonia","Prematurity","Prematurity with RDS","RDS"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_discrete(name="Cause of Death",
                      breaks=c("ASP","CA","EONS","Gastroschisis","HIE","LONS","OTH","PN","PR","PRRDS","RDS"),
                      labels=c("Aspiration","Congenital abnormality","EONS","Gastroschisis","HIE","Late Onset Sepsis","Other","Pneumonia","Prematurity","Prematurity with RDS","RDS"))
p20
Dea2<-subset(Quarter2, Quarter2$Discharge.NeoTreeOutcome=="NND")
Dea2$Early<-cut(Dea2$DaysAdmission, c(-24,6,50),labels = c("Early","Late"))
table(Dea2$Early)
table(Dea1$DaysAdmission)

p21<-ggplot(data=Dea2, aes(x=Discharge.CauseDeath, fill=Discharge.CauseDeath)) +
  geom_bar(stat="count")+
  ylab("number")+
  xlab("Cause of Death")+
  ggtitle("Cause of Death\nSecond Quarter 2019")+
  scale_x_discrete(breaks=c("ASP","CA","EONS","Gastroschisis","HIE","LONS","OTH","PN","PR","PRRDS","RDS"),
                   labels=c("Aspiration","Congenital abnormality","EONS","Gastroschisis","HIE","Late Onset Sepsis","Other","Pneumonia","Prematurity","Prematurity with RDS","RDS"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_discrete(name="Cause of Death",
                      breaks=c("ASP","CA","EONS","Gastroschisis","HIE","LONS","OTH","PN","PR","PRRDS","RDS"),
                      labels=c("Aspiration","Congenital abnormality","EONS","Gastroschisis","HIE","Late Onset Sepsis","Other","Pneumonia","Prematurity","Prematurity with RDS","RDS"))
p21
#other causes of death- ignore RDS and gastroschisis as these have been added to Cause of death
table(Quarter1$Discharge.CauseDeathOther)
table(Quarter2$Discharge.CauseDeathOther)
#alternative diagnosis at admission and discharge
p22<-ggplot(data=subset(Quarter1, !is.na(Admission.Diagnoses)), aes(x=Admission.Diagnoses, fill=Admission.Diagnoses)) +
  geom_bar(stat="count")+
  ylab("number")+
  xlab("Admission Diagnosis")+
  ggtitle("Diagnoses at Admission\nFirst Quarter 2019")+
  scale_x_discrete(breaks=c("AD","Apg", "BA", "BBA","BI","CHD","Cong","Conv","DIB","DU","EONS","FD","Fev","G","HIVX","Hydro","J","LBW","Mac","Mec","NTD","O","OM","Prem","PremRDS","RDS","Risk","Safe","Twin","Vomit"),
                   labels=c("Abdo distension","Low Apgar", "HIE", "BBA","Birth injury","Congenital heart","Congenital","Convulsions","DIB","Dumped","EONS","Feeding Problem","Fever","Gastroschisis","HIV Exp","Hydroceph","Jaundice","LBW","Macrosomia","Meconium Exposure","NTD","Other","Omphalocele","Prematurity","Prem RDS","RDS","Sepsis Risk","SafeKeeping","Accompanying twin","Vomit"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_discrete(breaks=c("AD","Apg", "BA", "BBA","BI","CHD","CA","Conv","DIB","DU","EONS","FD","Fev","G","HIVX","Hydro","J","LBW","Mac","Mec","NTD","O","OM","Prem","PremRDS","RDS","Risk","Safe","Twin","Vomit"),
                      labels=c("Abdo distension","Low Apgar", "HIE", "BBA","Birth Injury","Congenital heart","Congenital","Convulsions","DIB","Dumped","EONS","Feeding Problem","Fever","Gastroschisis","HIV Exp","Hydroceph","Jaundice","LBW","Macrosomia","Meconium Exposure","NTD","Other","Omphalocele","Prematurity","Prem RDS","RDS","Sepsis Risk","SafeKeeping","Accompanying twin","Vomit"))
p22
table(Quarter2$Admission.Diagnoses)
p23<-ggplot(data=subset(Quarter2, !is.na(Admission.Diagnoses)), aes(x=Admission.Diagnoses, fill=Admission.Diagnoses)) +
  geom_bar(stat="count")+
  ylab("number")+
  xlab("Admission Diagnosis")+
  ggtitle("Diagnoses at Admission\nSecond Quarter 2019")+
  scale_x_discrete(breaks=c("AD","Apg", "BA", "BBA","BI","CHD","Cong","Conv","DIB","DU","EONS","FD","Fev","G","HIVX","Hydro","J","LBW","Mac","Mec","NTD","O","OM","Prem","PremRDS","RDS","Risk","Safe","SPn","Twin","Vomit"),
                   labels=c("Abdo distension","Low Apgar", "HIE", "BBA","Birth injury","Congenital heart","Congenital","Convulsions","DIB","Dumped","EONS","Feeding Problem","Fever","Gastroschisis","HIV Exp","Hydroceph","Jaundice","LBW","Macrosomia","Meconium Exposure","NTD","Other","Omphalocele","Prematurity","Prem RDS","RDS","Sepsis Risk","SafeKeeping","Severe pneumonia","Accompanying twin","Vomit"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_discrete(breaks=c("AD","Apg", "BA", "BBA","BI","CHD","CA","Conv","DIB","DU","EONS","FD","Fev","G","HIVX","Hydro","J","LBW","Mac","Mec","NTD","O","OM","Prem","PremRDS","RDS","Risk","Safe","SPn","Twin","Vomit"),
                      labels=c("Abdo distension","Low Apgar", "HIE", "BBA","Birth Injury","Congenital heart","Congenital","Convulsions","DIB","Dumped","EONS","Feeding Problem","Fever","Gastroschisis","HIV Exp","Hydroceph","Jaundice","LBW","Macrosomia","Meconium Exposure","NTD","Other","Omphalocele","Prematurity","Prem RDS","RDS","Sepsis Risk","SafeKeeping","Severe pneumonia","Accompanying twin","Vomit"))
p23

hie1<-subset(Quarter1, Quarter1$HIE=="1")
p24<-ggplot(data=subset(hie1, !is.na(MonthFinal)), aes(x=MonthFinal,fill=Discharge.NeoTreeOutcome)) +
  geom_bar(stat="count")+
  ylab("number")+
  xlab("Month")+
  ggtitle("Outcome for Babies\nwith HIE by month Quarter 1")+
  scale_fill_discrete(name="Outcome",
                      breaks=c("DC","NND"),
                      labels=c("Discharge","Died"))
p24
hie2<-subset(Quarter2, Quarter2$HIE=="1")
p25<-ggplot(data=subset(hie2, !is.na(MonthFinal)), aes(x=MonthFinal,fill=Discharge.NeoTreeOutcome)) +
  geom_bar(stat="count")+
  ylab("number")+
  xlab("Month")+
  ggtitle("Outcome for Babies\nwith HIE by month Quarter 2")+
  scale_fill_discrete(name="Outcome",
                      breaks=c("DC","NND"),
                      labels=c("Discharge","Died"))
p25
table(Dea1$Discharge.CauseDeath)
table(Dea1$Discharge.CauseDeathOther)
Dea1$Discharge.CauseDeath <- ifelse(Dea1$Discharge.CauseDeathOther %in% c("gastroschisis", "Gastroschisis", "Gastrochisis"), 
                                    "Gastroschisis", Dea1$Discharge.CauseDeath)
Dea1$Discharge.CauseDeath <- ifelse(Dea1$Discharge.CauseDeathOther %in% c("Aspiration"), 
                                    "ASP", Dea1$Discharge.CauseDeath)
Dea1$Discharge.CauseDeath <- ifelse(Dea1$Discharge.CauseDeathOther %in% c("RDS and Risk of Sepsis", "Respiratory distress", "Macrosomia with respiratory distress"), 
                                    "RDS", Dea1$Discharge.CauseDeath)
table(Dea2$Discharge.CauseDeathOther)
Dea2$Discharge.CauseDeath <- ifelse(Dea2$Discharge.CauseDeathOther %in% c("Respiratory distress syndrome ", "Respiratory distress", "Macrosomia with respiratory distress"), 
                                    "RDS", Dea2$Discharge.CauseDeath)
#Antenatal steroids by birthweight
complete.df$Admission.BW<-as.numeric(complete.df$Admission.BW)
class(complete.df$Admission.BW)
LBW<-subset(complete.df, complete.df$Admission.BW < 1501 & complete.df$Admission.BW > 250)
p15<-ggplot(data=subset(LBW, !is.na(Admission.ANSteroids)), aes(x=Admission.BW, fill=Admission.ANSteroids)) +
  geom_histogram(binwidth=250, position="dodge")+
  ylab("number")+
  xlab("Birthweight (g)")+
  ggtitle("Antenatal steroids by Birthweight to end August")+
  scale_fill_discrete(name="Steroids Received",
                      breaks=c("Y","N","U"),
                      labels=c("Yes","No","Unknown"))
ggsave(p15, width=10, height=8, file='steroids.pdf')
table(LBW$Admission.ANSteroids)
table(LBW$Admission.BW)
tab<-table(LBW$MonthFinal,LBW$Admission.ANSteroids)
tab2 <- addmargins(tab, FUN = list(Total = sum), quiet = TRUE)
tab2
write.csv(tab2, file=paste0('MonthlyAntenatalSteroidsLBW.csv'),
          row.names = T)

#gastroschisis
table(complete.df$Discharge.DIAGDIS1)
table(complete.df$Admission.DiagnosesOth)
table(complete.df$Admission.AdmReasonOth)
table(complete.df$Discharge.CauseDeath)
table(complete.df$Discharge.DIAGDIS1OTH)
#making subset for gastroschisis at discharge OR death
complete.df$gastro<-ifelse(grepl("G",complete.df$Discharge.DIAGDIS1),"1",
                           ifelse(grepl("Gastroschisis",complete.df$Discharge.CauseDeath),"1",
                                  ifelse(grepl("",complete.df$Discharge.CauseDeath),"0",
                                         ifelse(grepl("",complete.df$Discharge.DIAGDIS1),"0","NA"))))
table(complete.df$gastro)                          
gastro<-subset(complete.df,complete.df$gastro=="1")
p16<-ggplot(data=subset(gastro, !is.na(Discharge.NeoTreeOutcome)), aes(x=Discharge.BWTDis, fill=Discharge.NeoTreeOutcome)) +
  geom_histogram(binwidth=250, position="dodge")+
  ylab("number")+
  xlab("Birthweight (g)")+
  ggtitle("Outcome by Birthweight\ngastroschisis")+
  scale_fill_discrete(name="Outcome",
                      breaks=c("DC","NND"),
                      labels=c("Discharge","Died"))
ggsave(p16, width=10, height=8, file='gastro.pdf')
write.csv(gastro, file=paste0('gastro.csv'),
          row.names = T)