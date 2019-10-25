# Conversion functions used in merge-data.R
# Author: Liam Shaw liam.philip.shaw@gmail.com

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

jsonToDataFrame <- function(json.filenames, scriptType = "Admission"){
  json.files = vector("list", length(json.filenames))
  
  for (i in 1:length(json.filenames)){
    f = json.filenames[i]
    # Check files first to ignore the bad files
    a <- readChar(f, file.info(f)$size)
    if(substr(a, nchar(a), nchar(a))=="}"){
      json.files[[i]] = do.call("rbind", fromJSON(file = f) )
      print(paste0("Processed: ", i, " of ", length(json.filenames), " files."))
    }
    else{
      print(paste0("Error in file: ", f, " -- skipping!"))
    }
  }
  print("Combining files. This may take a while...")
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
  session.data.list <- sapply(seq(1,length(session.list)), function(x) cbind(session.names[x], unlist(keys[[x]]), unlist(values[[x]])))
  
  # Convert to a single data frame
  df <- ldply(session.data.list, data.frame)
  
  colnames(df) <- c("session", "key", "value")
  
  # Problem of duplicate identifiers? The 7 duplicate row pairs in this dataset
  # are all genuine duplicates, so OK to remove (I *think*)
  if (length(table(duplicated(df)))>0){
    df <- df[-which(duplicated(df)),]
  }
  
  # First, get rid of duplicated key values e.g. two 'DischWeight' entries, NA and real, for a single session
  if (length(table(duplicated(paste(df$session, df$key))))>1){
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


deduplicateAdmission <- function(admission.data.frame){
  # Get rid of duplicate rows (first column is session, but some are duplicated)
  admission.data.frame <- admission.data.frame[!duplicated(admission.data.frame[,seq(2, ncol(admission.data.frame))]),]
  # Convert ID to lower case
  admission.data.frame$Admission.UID <- tolower(admission.data.frame$Admission.UID)
  # First 3 and last 3 characters of admission.df$UID should match discharge.df$NeoTreeID ?
  admission.data.frame$NeoTreeID <- sapply(admission.data.frame$Admission.UID, function(x) paste0(substr(x, start=1, stop=3),
                                                                                  substr(x, start=(nchar(x)-2), stop=(nchar(x)))))
  
  # Remove duplicates if they exist
  if (length(which(duplicated(admission.data.frame$NeoTreeID)))>0){
    admission.data.frame <- admission.data.frame[-which(duplicated(admission.data.frame$NeoTreeID)),]
  }
  return(admission.data.frame)
}

deduplicateDischarge <- function(discharge.data.frame){
  discharge.data.frame <- discharge.data.frame[!duplicated(discharge.data.frame[,seq(2, ncol(discharge.data.frame))]),]
  discharge.data.frame$NeoTreeID <- tolower(discharge.data.frame$Discharge.NeoTreeID)
  
  if (length(which(duplicated(discharge.data.frame$NeoTreeID)))>0){
    discharge.data.frame <- discharge.data.frame[-which(duplicated(discharge.data.frame$NeoTreeID)),]
    
  }

  #discharge.data.frame$Discharge.NeoTreeID <- NULL
  return(discharge.data.frame)
}

# Functions to check data and make it consistent (manual entry etc.)
correctAdmissionData <- function(admission.df){
  # Return: "if Admission.AdmReasonOth = macrosomia or Macrosomia 
  # could you change Admission.AdmReason = Macro (rather than O for other)"
  admission.df$Admission.AdmReason <- ifelse(admission.df$Admission.AdmReasonOth %in% 
                                               c("macrosomia", "Macrosomia"), 
                                             "Macro", 
                                             admission.df$Admission.AdmReason)
  
  # Request: "Same for Admission.Diagnoses = Macro (rather than OTH for other) 
  # if Admission.DiagnosesOth = macrosomia or Macrosomia"
  admission.df$Admission.Diagnoses <- ifelse(admission.df$Admission.Diagnoses %in% 
                                               c("macrosomia", "Macrosomia"), 
                                             "Macro", 
                                             admission.df$Admission.AdmReason)
  
  # Request: "Similarly 
  # if Admission.AdmReasonOth = safekeeping or safe keeping or Safe keeping 
  # could you change 
  # Admission.AdmReason = Safe (rather than O for other)"
  admission.df$Admission.AdmReason <- ifelse(admission.df$Admission.AdmReasonOth %in% 
                                               c("safekeeping", "safe keeping", "Safe keeping"), 
                                             "Safe", 
                                             admission.df$Admission.AdmReason)
  admission.df$Admission.Diagnoses <- ifelse(admission.df$Admission.DiagnosesOth %in% 
                                               c("safekeeping", "safe keeping", "Safe keeping"), 
                                             "Safe", 
                                             admission.df$Admission.Diagnoses)
  
  return(admission.df)
}

correctDischargeData <- function(discharge.df){
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
  #Other causes of death
  discharge.df$Discharge.CauseDeath <- ifelse(discharge.df$Discharge.CauseDeathOther %in% c("gastroschisis", "Gastroschisis", "Gastrochisis"), 
                                              "Gastroschisis", discharge.df$Discharge.CauseDeath)
  discharge.df$Discharge.CauseDeath <- ifelse(discharge.df$Discharge.CauseDeathOther %in% c("Aspiration"), 
                                              "ASP", discharge.df$Discharge.CauseDeath)
  discharge.df$Discharge.CauseDeath <- ifelse(discharge.df$Discharge.CauseDeathOther %in% c("RDS and Risk of Sepsis", "Respiratory distress", "Macrosomia with respiratory distress","RDS","Respiratory Distress","Respiratory distress syndrome"), 
                                              "RDS", discharge.df$Discharge.CauseDeath)
  discharge.df$Discharge.CauseDeath <- ifelse(discharge.df$Discharge.CauseDeath %in% c("Gastro", "Gastroschisis", "Gastrochisis"), 
                                              "Gastroschisis", discharge.df$Discharge.CauseDeath)
  
  return(discharge.df)
}

findMatchesWithinNewAdmissionDischarge <- function(admission.df, discharge.df){
  # Those discharges which have a match
  # N.B. UID / NeoTreeID is confusing me, but I think this is the right way to do it
  discharge.have.match <-  discharge.df$NeoTreeID[discharge.df$NeoTreeID %in% admission.df$NeoTreeID | discharge.df$NeoTreeID %in% admission.df$Admission.UID]
  if (length(discharge.have.match)==0){ # this is perfect match only
    return()
  }
  else{
      have.match.df <- data.frame(admissionID=discharge.have.match,
                                  dischargeID=discharge.have.match,
                                  matchType="perfect")
      # Those discharges which don't have a match
      discharge.need.match <- discharge.df$NeoTreeID[!discharge.df$NeoTreeID %in% discharge.have.match]
      admission.need.match <- admission.df$NeoTreeID[!(admission.df$NeoTreeID %in% discharge.have.match | admission.df$Admission.UID %in% discharge.have.match)]
      
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
      have.eight.match.df <- data.frame(admissionID=sapply(as.character(discharge.possible.matches), 
                                                           function(x) 
                                                             as.character(na.omit(admission.df$NeoTreeID[admission.df$Admission.UID==x]))),
                                        dischargeID=names(discharge.possible.matches),
                                        matchType="approximate")
      matches.df <- rbind(matches.df, have.eight.match.df)
      
      # Create a merged data frame of all the matched pairs
      admission.df.matched <- admission.df[which(admission.df$NeoTreeID %in% matches.df$admissionID),]
      admission.df.matched.2 <- admission.df[which(admission.df$Admission.UID %in% matches.df$admissionID),]
      admission.df.matched <- rbind(admission.df.matched, admission.df.matched.2)
      # drop duplicates
      admission.df.matched <- admission.df.matched[!duplicated(admission.df.matched$NeoTreeID),]
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
      return(merged.df)
    }
}


addUnmatchedDischarges <- function(merged.df, discharge.df){
  # Those that remain unmatched
  unmatched.discharges <- discharge.df$NeoTreeID[which(!discharge.df$NeoTreeID %in% merged.df$Discharge.NeoTreeID)]
  unmatched.discharge.df <- discharge.df[which(discharge.df$NeoTreeID %in% 
                                                 unmatched.discharges),]
  unmatched.discharge.df$Discharge.NeoTreeID <- unmatched.discharge.df$NeoTreeID
  unmatched.discharge.df$NeoTreeID <- NULL
  unmatched.discharge.df$Discharge.DateDischarge <- as.Date(as.numeric(unmatched.discharge.df$Discharge.DateDischarge), origin="1970-01-01")
  
  combined.merged.df <- merge(merged.df, 
                              unmatched.discharge.df,
                              all = TRUE)
  return(combined.merged.df)
}

randomString <- function(n = 1) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

uniqueFileName <- function(some.string){
  if (file.exists(some.string)){
    some.string <-  paste0(randomString(), 
                             '-',
                             some.string)
  }
  return(some.string)
}
