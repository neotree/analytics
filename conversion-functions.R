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