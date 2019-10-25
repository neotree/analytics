# Creating things used below by Felicity's script
final.database.df <- new.merged.df[which(!is.na(new.merged.df$Admission.NeoTreeID)),]
discharge.need.match.df <- unmatched.discharge.df

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
Month<-subset(final.database.df, final.database.df$Admission.MonthAdmission=="08")
Tab<-table(Month$Discharge.HCWIDDis,Month$Discharge.NeoTreeOutcome)
write.csv(Tab, file=paste0('DischargeMatchedByHCWID.csv'), row.names = T)
#number of unmatched by HCWID
#Need to change Month number e.g. 07 for July as above
MonthUnmatched<-subset(discharge.need.match.df, discharge.need.match.df$Discharge.MonthDischarge=="08")
tabU<-table(MonthUnmatched$Discharge.HCWIDDis,MonthUnmatched$Discharge.NeoTreeOutcome)
write.csv(tabU, file=paste0('DischargeUnmatchedByHCWID.csv'), row.names = T)
#attempting to merge unmatched discharges with matched file


#creating final outcome variable
final.database.df$Discharge.DateTimeFinalOutcome <- ifelse(!is.na(final.database.df$Discharge.DateTimeDeath), final.database.df$Discharge.DateTimeDeath, ifelse(!is.na(final.database.df$Discharge.DateTimeDischarge), final.database.df$Discharge.DateTimeDischarge, NA))
write.csv(final.database.df, file=paste0(Sys.Date(), ' Final.csv'),
          row.names = T)
#duration of admission
final.database.df$DateFinal <- as.Date(gsub(pattern = "T.*", replacement = "", final.database.df$Discharge.DateTimeFinalOutcome), format="%Y-%m-%d")
final.database.df$MonthFinal <- format(final.database.df$DateFinal, "%m") # Month as number from 1-12
final.database.df$MonthFinal<- ordered(final.database.df$MonthFinal, levels=c("11", "12", "01", "02","03","04","05","06","07","08","09","10"))
final.database.df$DaysAdmission<- (final.database.df$DateFinal)-(final.database.df$Admission.DateAdmission)
summary(final.database.df$DaysAdmission)
tapply(final.database.df$DaysAdmission,final.database.df$Discharge.NeoTreeOutcome,summary)
final.database.df$DaysAdmission<-as.numeric(final.database.df$DaysAdmission)
table(final.database.df$DaysAdmission)
