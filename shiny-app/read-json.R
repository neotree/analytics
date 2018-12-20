library(rjson)
library(plyr)

# Read in json file
json1 <- fromJSON(file = "test-data.json")

# Parse the json 
json1.df <- do.call("rbind", json1)

# Parse the json - bit hacky but it'll do for now
# Get list, each item is a session
session.list <- apply(json1.df, FUN= function(x) x$sessions$entries, MARGIN=2)

# 1. Just to keep track of the session names (assuming each tablet has independent sessions on?)
session.names <- paste("session", seq(1,length(session.list)))
# 2. Get all the variable keys by running function over list
keys <- lapply(session.list, function(x) lapply(x, function(y) y$key))
# 3. Get all the variable values by running function over list 
values <- lapply(session.list, function(x) lapply(x, function(y) as.character(unlist(y$value)["value"])))


# Combine these (1, 2, 3) into a single object with 3 columns: session, variable name, variable value
session.data.list <- sapply(seq(1,length(session.list)), function(x) cbind(session.names[x], keys[[x]], values[[x]]))

# Convert to a single data frame
df <- ldply(session.data.list, data.frame)

# Unlist function (unlists a list, keeping NULL entries as NA)
unlistKeepNull <- function(a.list){
  do.call(c, lapply(a.list, (function(x){
    if (is.null(x) | length(x) == 0) {NA} 
    else { x }
  }
  )))
}
# Create version of the data frame for plotting by unlisting each column
df.plot <- as.data.frame(apply(df, MARGIN=2, FUN=unlistKeepNull), stringsAsFactors = FALSE)
colnames(df.plot) <- c("session", "key", "value")

# Problem of duplicate identifiers? The 7 duplicate row pairs in this dataset 
# are all genuine duplicates, so OK to remove (I *think*)
df.plot <- df.plot[-which(duplicated(df.plot)),]

# Spread the data into wide format
require(tidyr)
df.plot <- spread(df.plot, key, value)

# Convert the date/time field to the correct format etc.
df.plot$DateAdmission <- as.Date(gsub(pattern = "T.*", replacement = "", df.plot$DateTimeAdmission), format="%Y-%m-%d")
df.plot$WeekdayAdmission <- ordered(weekdays(df.plot$DateAdmission),
                                    levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
                                    # Actual day (monday, tuesday etc.)
df.plot$MonthAdmission <- format(df.plot$DateAdmission, "%m") # Month as number from 1-12
df.plot$WeekAdmission <- format(df.plot$DateAdmission, "%w") # Week as number from 0-51

df.plot$TimeAdmission <- as.hms(gsub(pattern="\\+.*", replacement="", 
                     gsub(pattern = ".*T", replacement = "", df.plot$DateTimeAdmission)))

# Example plot
# require(ggplot2)
# ggplot(df.plot, aes(TimeAdmission))+geom_histogram(fill='black')+theme_bw()+facet_wrap(~WeekdayAdmission)
