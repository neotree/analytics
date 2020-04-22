# NeoTree Zimbabwe statistics generating script
# This processes the outputs from merge-data.R and creates the required statistics
# The statistics are then saved within Outputs/Statistics on the S: drive
# Author: Sam Neal
# Date: 21.01.2020
# Updated 16.03.2020: added 'na.rm = T' to xlims of plot1 and plot2 (although need to fix why there are NA values)


###### SETUP ######

# Create output directory shorthand variables
path.stats<- paste0('../Outputs/Statistics/', output.directory,
                         '/', 'NeoTreeZim-', analysis_month, '-')
path.plots<- paste0('../Outputs/Plots/', output.directory,
                    '/', 'NeoTreeZim-', analysis_month, '-')

# Create disDied (binary) outcome variable
# some cases have both DateTimeDeath & DateTimeDischarge != NA, for now I have counted them as died

new.discharge.df$disDied<- factor(ifelse(!is.na(new.discharge.df$Discharge.DateTimeDeath), 1,
                                         ifelse(!is.na(new.discharge.df$Discharge.DateTimeDischarge), 0, NA)))
levels(new.discharge.df$disDied)<- c('discharged', 'died')

new.merged.df$disDied<- factor(ifelse(!is.na(new.merged.df$Discharge.DateTimeDeath), 1,
                                      ifelse(!is.na(new.merged.df$Discharge.DateTimeDischarge), 0, NA)))
levels(new.merged.df$disDied)<- c('discharged', 'died')

complete.df$disDied<- factor(ifelse(!is.na(complete.df$Discharge.DateTimeDeath), 1,
                                    ifelse(!is.na(complete.df$Discharge.DateTimeDischarge), 0, NA)))
levels(complete.df$disDied)<- c('discharged', 'died')

unmatched.discharge.df$disDied<- factor(ifelse(!is.na(unmatched.discharge.df$Discharge.DateTimeDeath), 1,
                                        ifelse(!is.na(unmatched.discharge.df$Discharge.DateTimeDischarge), 0, NA)))
levels(unmatched.discharge.df$disDied)<- c('discharged', 'died')

### Create disDied date and time variable

new.discharge.df$disDiedDateTime<- ifelse(!is.na(new.discharge.df$Discharge.DateTimeDeath),
                                                    as.character(new.discharge.df$Discharge.DateTimeDeath),
                                                    ifelse(
                                                      !is.na(new.discharge.df$Discharge.DateTimeDischarge),
                                                      as.character(new.discharge.df$Discharge.DateTimeDischarge),
                                                      NA))

new.merged.df$disDiedDateTime<- ifelse(!is.na(new.merged.df$Discharge.DateTimeDeath),
                                                 as.character(new.merged.df$Discharge.DateTimeDeath),
                                                 ifelse(
                                                   !is.na(new.merged.df$Discharge.DateTimeDischarge),
                                                   as.character(new.merged.df$Discharge.DateTimeDischarge),
                                                   NA))

complete.df$disDiedDateTime<- ifelse(!is.na(complete.df$Discharge.DateTimeDeath),
                                               as.character(complete.df$Discharge.DateTimeDeath),
                                               ifelse(
                                                 !is.na(complete.df$Discharge.DateTimeDischarge),
                                                 as.character(complete.df$Discharge.DateTimeDischarge),
                                                 NA))

unmatched.discharge.df$disDiedDateTime<- ifelse(!is.na(unmatched.discharge.df$Discharge.DateTimeDeath),
                                     as.character(unmatched.discharge.df$Discharge.DateTimeDeath),
                                     ifelse(
                                       !is.na(unmatched.discharge.df$Discharge.DateTimeDischarge),
                                       as.character(unmatched.discharge.df$Discharge.DateTimeDischarge),
                                       NA))

### convert dates to POSIXct with CAT time zone using lubridate
new.admission.df$Admission.DateTimeAdmission<- ymd_hms(new.admission.df$Admission.DateTimeAdmission,
                                                       tz = "Africa/Harare")
new.discharge.df$Discharge.DateTimeDischarge<- ymd_hms(new.discharge.df$Discharge.DateTimeDischarge,
                                                       tz = "Africa/Harare")
new.discharge.df$Discharge.DateTimeDeath<- ymd_hms(new.discharge.df$Discharge.DateTimeDeath,
                                                   tz = "Africa/Harare")
new.discharge.df$disDiedDateTime<- ymd_hms(new.discharge.df$disDiedDateTime,
                                                     tz = "Africa/Harare")
new.merged.df$Admission.DateTimeAdmission<- ymd_hms(new.merged.df$Admission.DateTimeAdmission,
                                                    tz = "Africa/Harare")
new.merged.df$Discharge.DateTimeDischarge<- ymd_hms(new.merged.df$Discharge.DateTimeDischarge,
                                                    tz = "Africa/Harare")
new.merged.df$Discharge.DateTimeDeath<- ymd_hms(new.merged.df$Discharge.DateTimeDeath,
                                                tz = "Africa/Harare")
new.merged.df$disDiedDateTime<- ymd_hms(new.merged.df$disDiedDateTime,
                                                  tz = "Africa/Harare")
complete.df$Admission.DateTimeAdmission<- ymd_hms(complete.df$Admission.DateTimeAdmission,
                                                  tz = "Africa/Harare")
complete.df$Discharge.DateTimeDischarge<- ymd_hms(complete.df$Discharge.DateTimeDischarge,
                                                  tz = "Africa/Harare")
complete.df$Discharge.DateTimeDeath<- ymd_hms(complete.df$Discharge.DateTimeDeath,
                                              tz = "Africa/Harare")
complete.df$disDiedDateTime<- ymd_hms(complete.df$disDiedDateTime,
                                                tz = "Africa/Harare")

###### PROGRESS TO DATE ######

all.events<- nrow(new.admission.df) + nrow(new.discharge.df)
matched.events<- nrow(new.merged.df)
unmatched.adm<- nrow(unmatched.admission.df)
unmatched.dis<- nrow(unmatched.discharge.df)


df1<- data.frame(EventType=c("All events (admissions + discharges)",
                             "Matched admission-discharge/death pairs",
                             "Unmatched admissions",
                             "Unmatched discharges/deaths"),
                             Number=c(all.events, matched.events,
                                      unmatched.adm, unmatched.dis))

write.csv(df1, file=paste0(path.stats, "progress-to-date.csv"), row.names = FALSE)

###### MONTHLY STATS ######
month.adm<- nrow(new.admission.df[which(grepl(analysis_month,
                                              new.admission.df$Admission.DateTimeAdmission)==TRUE), ])

month.disDied<- nrow(new.discharge.df[which(grepl(analysis_month,
                                                  new.discharge.df$disDiedDateTime)==TRUE), ])

month.dis<- nrow(new.discharge.df[which(grepl(analysis_month,
                                              new.discharge.df$Discharge.DateTimeDischarge)==TRUE), ])

month.died<- nrow(new.discharge.df[which(grepl(analysis_month,
                                               new.discharge.df$Discharge.DateTimeDeath)==TRUE), ])

month.cfr<- (month.died/month.adm)*100
#very simply admissions this month divided by deaths this month as %

df2<- data.frame(EventType=c("This month's admissions (including unmatched)",
                             "This month's discharges/deaths (including unmatched)",
                             "This month's discharges (including unmatched)",
                             "This month's deaths (including unmatched)",
                             "This month's CFR in %"),
                 Number=c(month.adm, month.disDied,
                          month.dis, month.died, month.cfr))

write.csv(df2, file=paste0(path.stats, "monthly-stats.csv"), row.names = FALSE)

###### MONTHLY STATS FOR SISTER IN CHARGE ######
### Need to work out how to do early/late NNDs, as currently can only do this on matched data
### (which will not capture all the deaths)
#died.df<- subset(new.discharge.df, new.discharge.df$disDied == 'died')
#dis.df<- subset(new.discharge.df, new.discharge.df$disDied == 'discharged')

new.discharge.df$PMTCT<-ifelse(grepl("NVP", new.discharge.df$Discharge.MedsGiven),"1",
                    ifelse(grepl("",new.discharge.df$Discharge.MedsGiven),"0", NA))
new.discharge.df$PMTCT<- as.numeric(as.character(new.discharge.df$PMTCT))
month.PMTCT<- sum(new.discharge.df$PMTCT[which(grepl(analysis_month,
                                                     new.discharge.df$disDiedDateTime))],
                  na.rm = TRUE)

df3<- data.frame(EventType=c("Admissions",
                             "Discharges",
                             "Deaths (all)",
                             "PMTCT"),
                 Number=c(month.adm, month.dis,
                          month.died, month.PMTCT))

write.csv(df3, file=paste0(path.stats, "for-sister-in-charge-counts.csv"), row.names = FALSE)

# Table of discharge diagnoses
dis_diags<- levels(new.discharge.df$Discharge.DIAGDIS1)

df4<- data.frame(DischargeDiagnosis=c("Anaemia", "BBA", "Birth injury", "Bowel obstruction", "Congenital HD",
                            "Dehydration", "Early onset sepsis", "Feeding difficulty", "Gastroschisis",
                            "HIE", "HIV exposed", "HIV high risk", "HIV low risk", "Jaundice", "LBW", "Late onset sepsis",
                            "Meconium aspiration", "Macrosomia", "MSK abnormalities", "Normal baby",
                            "Other congenital abnormality", "Omphalocele", "Other", "Pneumonia",
                            "Premature", "Prem with RDS", "Sepsis risk", "Safekeeping", "TTN", "Accompanying twin"),
                 Number=rep(NA, times=30))

for(i in 1:30){
  df4[i,2]<-
    sum(new.discharge.df$Discharge.DIAGDIS1[
      which(grepl(analysis_month, new.discharge.df$disDiedDateTime))] == dis_diags[i],
      na.rm=TRUE)
}

# Now extract top 5 diagnoses
## Currently, this does not account for ties and simply takes the top 5 rows.

df4a<- as_tibble(df4)
df4a<- arrange(df4a, desc(Number))
df4a<- df4a[1:5,]

write.csv(df4a, file=paste0(path.stats, "for-sister-in-charge-top5-diagnoses.csv"), row.names = FALSE)

# Table of causes of death and counts
death_causes<- levels(new.discharge.df$Discharge.CauseDeath)

df5<- data.frame(CauseOfDeath=c("Aspiration",
                                "Congenital Abnormality",
                                "Early Onset Sepsis",
                                "Gastroschisis",
                                "HIE",
                                "Late Onset Sepsis",
                                "Meconium Aspiration Syndrome",
                                "Necrotising Enterocolitis",
                                "Other",
                                "Pneumonia",
                                "Prematurity",
                                "Prematurity with RDS"),
                 Number=rep(NA, times=12))

for(i in 1:12){
  df5[i,2]<-
      sum(new.discharge.df$Discharge.CauseDeath[
        which(grepl(analysis_month, new.discharge.df$disDiedDateTime))] == death_causes[i],
        na.rm=TRUE)
}

write.csv(df5, file=paste0(path.stats, "for-sister-in-charge-cause-of-deaths.csv"), row.names = FALSE)




###### PLOTS #######

### Longitudinal plots ###

# Number of admissions for each month of project (including unmatched admissions)
new.admission.df$adm.month.floor<- floor_date(new.admission.df$Admission.DateTimeAdmission, unit='month')
### lubridate function that floors date to YYYY-mm-01

plot1<- ggplot(new.admission.df, aes(x = adm.month.floor)) +
  geom_bar(fill = "steelblue") +
  scale_x_datetime(date_breaks="1 months", date_labels="%b %y") +
  coord_cartesian(xlim=c(min(new.admission.df$adm.month.floor, na.rm = T), max(new.admission.df$adm.month.floor, na.rm = T))) +
  labs(x="Month", y="Number of babies admitted", 
       title="Number of babies admitted per month\n(all events, including unmatched)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  NULL

ggsave(plot1, width=10, height=8, file=paste0(path.plots, "admissions-per-month.pdf"))

# Number of discharge/deaths for each month of project (including unmatched discharges)
new.discharge.df$disDied.month.floor<- floor_date(new.discharge.df$disDiedDateTime, unit='month')
### lubridate function that floors date to YYYY-mm-01

plot2<- ggplot(new.discharge.df, aes(x=disDied.month.floor)) +
  geom_bar(fill = "steelblue") +
  scale_x_datetime(date_breaks="1 months",
                   date_labels="%b %y") +
  coord_cartesian(xlim=c(min(new.discharge.df$disDied.month.floor, na.rm = T), max(new.discharge.df$disDied.month.floor, na.rm = T))) +
  labs(x="Month", y="Number of babies dicharged or who died", 
       title="Number of babies discharge or who died per month\n(all events, including unmatched)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  NULL

ggsave(plot2, width=10, height=8, file=paste0(path.plots, "disDied-per-month.pdf"))

# Number of deaths only for each month of project (including unmatched discharges)
new.discharge.df$died.month.floor<- new.discharge.df$Discharge.DateTimeDeath
new.discharge.df$died.month.floor<- floor_date(new.discharge.df$Discharge.DateTimeDeath, unit='month')
### lubridate function that floors date to YYYY-mm-01

plot3<- ggplot(subset(new.discharge.df, !is.na(Discharge.DateTimeDeath)), aes(x=died.month.floor)) +
  geom_bar(fill = "steelblue") +
  scale_x_datetime(date_breaks="1 months",
                   date_labels="%b %y") +
  coord_cartesian(xlim=c(min(new.discharge.df$died.month.floor[!is.na(new.discharge.df$died.month.floor)]),
                         max(new.discharge.df$died.month.floor[!is.na(new.discharge.df$died.month.floor)]))) +
  labs(x="Month", y="Number of babies who died", 
       title="Number of babies who died per month\n(all events, including unmatched)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  NULL

ggsave(plot3, width=10, height=8, file=paste0(path.plots, "deaths-per-month.pdf"))

### This month's data plots ###

# Diagnosis at admission
monthAdmDiag.df<- extractAdmissionDiagnoses(dataFrame=new.admission.df, anMonth=analysis_month)

plot4<- ggplot(monthAdmDiag.df, aes(x=diagnosis, y=count, fill = diagnosis)) +
  geom_bar(stat="identity") +
  scale_x_discrete(labels=c("Anaemia", "BBA", "Birth injury", "Congenital abnormality", "Congenital HD",
                            "Dehydration", "Dumped", "Feeding difficulty", "HIV high risk",
                            "HIV low risk", "HIE", "Gastroschisis", "Jaundice", "LBW", "Macrosomia",
                            "Meconium aspiration", "Meconium exposed", "Sepsis", "Normal baby", "Pneumonia",
                            "Premature", "Prem with RDS", "Sepsis risk", "Safekeeping", "Accompanying twin", "TTN", "Other"),
                   limits=monthAdmDiag.df$diagnosis,
                   drop=FALSE) +
  labs(x="Diagnosis", y="Number of babies with this diagnosis", 
       title=paste0("Admission diagnoses ", analysis_month, "\n(all events, including unmatched)\n(N.B. each baby can have more than one diagnosis)")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + 
  NULL

ggsave(plot4, width=10, height=8, file=paste0(path.plots, "monthly-admission-diagnoses.pdf"))

# Diagnosis at discharge
## There doesn't seem to be the same problem of multiple discharge diagnoses, is this correct??***
## seems to be HIVX, HIVXH and HIVXL here - need to check on the app why there are 3 classes
plot5<- ggplot(subset(new.discharge.df, !is.na(Discharge.DIAGDIS1) &
                                        grepl(analysis_month, new.discharge.df$Discharge.DateTimeDischarge)==TRUE),
               aes(x=Discharge.DIAGDIS1, fill = Discharge.DIAGDIS1)) +
  geom_bar() +
  scale_x_discrete(labels=c("Anaemia", "BBA", "Birth injury", "Bowel obstruction", "Congenital HD",
                            "Dehydration", "EONS", "Feeding difficulty", "Gastroschisis",
                            "HIE", "HIV exposed", "HIV high risk", "HIV low risk", "Jaundice", "LBW", "LONS",
                            "Meconium aspiration", "Macrosomia", "MSK abnormalities", "Normal baby",
                            "Other congenital abnormality", "Omphalocele", "Other", "Pneumonia",
                            "Premature", "Prem with RDS", "Sepsis risk", "Safekeeping", "TTN", "Accompanying twin"),
                   drop=FALSE) +
  labs(x="Diagnosis", y="Number of babies with this diagnosis", 
       title=paste0("Primary discharge diagnosis ", analysis_month, "\n(all events, including unmatched)\n(N.B. each baby only has one discharge diagnosis")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + 
  NULL

ggsave(plot5, width=10, height=8, file=paste0(path.plots, "monthly-discharge-diagnoses.pdf"))

# Cause of death
plot6<- ggplot(subset(new.discharge.df, !is.na(Discharge.CauseDeath) &
                                        grepl(analysis_month, new.discharge.df$Discharge.DateTimeDeath)==TRUE),
               aes(x=Discharge.CauseDeath, fill = Discharge.CauseDeath)) +
  geom_bar() +
  scale_x_discrete(labels=c("Aspiration", "Congenital abnormality", "EONS", "Gastroschisis", "HIE", "LONS",
                             "Meconium aspiration", "NEC", "Other", "Pneumonia", "Prematurity",
                             "Prem with RDS"),
                   drop=FALSE) +
  labs(x="Cause of death", y="Number of babies with this cause of death", 
       title=paste0("Cause of death ", analysis_month, "\n(all events, including unmatched)")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + 
  NULL

ggsave(plot6, width=10, height=8, file=paste0(path.plots, "monthly-cause-of-death.pdf"))

# Temperature at admission (% hypothermic on admission)
## First, convert temperature from factor to numeric and split into bins
new.admission.df$Admission.Temperature<- as.numeric(as.character(new.admission.df$Admission.Temperature))
new.admission.df$Admission.TemperatureGroup<- cut(x = new.admission.df$Admission.Temperature,
           breaks = c(0.0, 32.0, 35.9, 36.4, 37.5, 42.0),
           labels = c("severe", "moderate", "mild", "normal", "fever"),
           ordered_result = TRUE
)


plot7<- ggplot(subset(new.admission.df, !is.na(Admission.TemperatureGroup) &
                                        grepl(analysis_month, new.admission.df$Admission.DateTimeAdmission)==TRUE),
               aes(x=Admission.TemperatureGroup, fill = Admission.TemperatureGroup)) +
  geom_bar() +
  scale_x_discrete(breaks=c("severe","moderate","mild","normal","fever"),
                   labels=c("Severe Hypothermia\n(0.0-32.0)","Moderate Hypothermia\n(32.1-35.9)",
                           "Mild Hypothermia\n(36.0-36.4)","Normothermia\n(36.5-37.5)","Fever\n(37.6-42.0)"),
                   drop=FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x="Temperature category", y="Number of babies", 
       title=paste0("Temperature at admission ", analysis_month, "\n(all events, including unmatched)")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + 
  NULL

ggsave(plot7, width=10, height=8, file=paste0(path.plots, "monthly-admission-temperature.pdf"))

# Number of temperatures recorded vs. number of admissions
## First create variable if temp == NA
new.admission.df$Admission.TemperatureMeasured<- as.factor(ifelse(is.na(new.admission.df$Admission.Temperature),
                                                                  "no", "yes"))

plot8<- ggplot(subset(new.admission.df, grepl(analysis_month, new.admission.df$Admission.DateTimeAdmission)==TRUE),
               aes(x=Admission.TemperatureMeasured, fill = Admission.TemperatureMeasured)) +
  geom_bar() +
  scale_x_discrete(breaks=c("no", "yes"),
                   labels=c("Not measured", "Measured"),
                   drop = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x="Temperature measured?", y="Number of babies", 
       title=paste0("Was temperature measured at admission? ", analysis_month, "\n(all events, including unmatched)")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "none") + 
  NULL

ggsave(plot8, width=10, height=8, file=paste0(path.plots, "monthly-temperatures-recorded.pdf"))

# Temperature at admission by inborn vs. outborn
plot9<- ggplot(subset(new.admission.df, !is.na(Admission.TemperatureGroup) &
                                        grepl(analysis_month, new.admission.df$Admission.DateTimeAdmission)==TRUE),
               aes(x=Admission.TemperatureGroup, fill=Admission.InOrOut)) +
  geom_bar() +
  scale_x_discrete(breaks=c("severe","moderate","mild","normal","fever"),
                   labels=c("Severe Hypothermia\n(0.0-32.0)","Moderate Hypothermia\n(32.1-35.9)",
                            "Mild Hypothermia\n(36.0-36.4)","Normothermia\n(36.5-37.5)","Fever\n(37.6-42.0)"),
                   drop=FALSE) +
  scale_fill_brewer(palette = "Dark2",
                    name="Inborn or Outborn",
                    breaks=c("TRUE","FALSE"),
                    labels=c("Inborn","Outborn")) +
  labs(x="Temperature category", y="Number of babies", 
       title=paste0("Temperature at admission ", analysis_month,
                    "\n(all events, including unmatched)")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  NULL

ggsave(plot9, width=10, height=8, file=paste0(path.plots, "monthly-temperatures-inborn-outborn.pdf"))

# Temperature at admission by birthweight
# First convert to numeric and create birthweight groups
new.admission.df$Admission.BW<- as.numeric(as.character(new.admission.df$Admission.BW))
new.admission.df$Admission.BWGroup<- cut(x = new.admission.df$Admission.BW,
                                         breaks = seq(0, 5000, 500),
                                         ordered_result = TRUE
)

plot10<- ggplot(subset(new.admission.df, !is.na(Admission.TemperatureGroup) & !is.na(Admission.BWGroup) &
                         grepl(analysis_month, new.admission.df$Admission.DateTimeAdmission)==TRUE),
                aes(x=Admission.BWGroup, fill=Admission.TemperatureGroup)) +
  geom_bar(position=position_dodge2(preserve = "single")) +
  scale_x_discrete(labels=c("<=500", "500-1000", "1001-1500", "1501-2000", "2001-2500", "2501-3000",
                            "3001-3500", "3501-4000", "4001-4500", "4501-5000"),
                   drop = FALSE) +
  scale_fill_brewer(palette = "Dark2",
                    name = "Temperature category",
                    breaks=c("severe","moderate","mild","normal","fever"),
                    labels=c("Severe Hypothermia\n(0.0-32.0)","Moderate Hypothermia\n(32.1-35.9)",
                            "Mild Hypothermia\n(36.0-36.4)","Normothermia\n(36.5-37.5)","Fever\n(37.6-42.0)"),
                    drop = FALSE) +
  labs(x="Birthweight (g)", y="Number of babies", 
       title=paste0("Temperature at admission by birthweight ", analysis_month,
                    "\n(all events, including unmatched)")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  NULL

ggsave(plot10, width=10, height=8, file=paste0(path.plots, "monthly-temperatures-birthweight.pdf"))

# Temperature at admission by gestation
# First convert to numeric and create birthweight groups
new.admission.df$Admission.Gestation<- as.numeric(as.character(new.admission.df$Admission.Gestation))
new.admission.df$Admission.GestationGroup<- cut(x = new.admission.df$Admission.Gestation,
                                         breaks = seq(20, 50, 5),
                                         ordered_result = TRUE
)

plot11<- ggplot(subset(new.admission.df, !is.na(Admission.TemperatureGroup) & !is.na(Admission.GestationGroup) &
                         grepl(analysis_month, new.admission.df$Admission.DateTimeAdmission)==TRUE),
                aes(x=Admission.GestationGroup, fill=Admission.TemperatureGroup)) +
  geom_bar(position=position_dodge2(preserve = "single")) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_brewer(palette = "Dark2",
                    name = "Temperature category",
                    breaks=c("severe","moderate","mild","normal","fever"),
                    labels=c("Severe Hypothermia\n(0.0-32.0)","Moderate Hypothermia\n(32.1-35.9)",
                             "Mild Hypothermia\n(36.0-36.4)","Normothermia\n(36.5-37.5)","Fever\n(37.6-42.0)"),
                    drop = FALSE) +
  labs(x="Gestational age (weeks)", y="Number of babies", 
       title=paste0("Temperature at admission by gestation ", analysis_month,
                    "\n(all events, including unmatched)")) +
  NULL

ggsave(plot11, width=10, height=8, file=paste0(path.plots, "monthly-temperatures-gestation.pdf"))

# Amoxicillin at discharge
# First create binary variable
new.discharge.df$Discharge.AmoxHome<- ifelse(grepl("AMOX", new.discharge.df$Discharge.MEDSDIS),"1",
                                             ifelse(grepl("",new.discharge.df$Discharge.MEDSDIS),"0", NA))
new.discharge.df$Discharge.AmoxHome<- as.factor(new.discharge.df$Discharge.AmoxHome)
levels(new.discharge.df$Discharge.AmoxHome)<- c("no", "yes")

plot12<- ggplot(subset(new.discharge.df, !is.na(Discharge.AmoxHome) &
                         grepl(analysis_month, new.discharge.df$disDiedDateTime)),
                aes(x=Discharge.AmoxHome, fill = Discharge.AmoxHome)) +
  geom_bar(position=position_dodge2(preserve = "single")) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_brewer(palette = "Dark2",
                    drop = FALSE) +
  labs(x="Discharged home with amoxicillin?", y="Number of babies", 
       title=paste0("Amoxicillin at discharge ", analysis_month,
                    "\n(all events, including unmatched)")) +
  theme(legend.position = "none") +
  NULL

ggsave(plot12, width=10, height=8, file=paste0(path.plots, "monthly-amoxicillin-at-discharge.pdf"))

# Render Rmd reports --------------------------------------------------

# Report for the Sister in Charge
rmarkdown::render(input = "sister_in_charge_report.Rmd",
                  output_file = paste0("sister_in_charge_report_", analysis_month),
                  output_dir = paste0('../Outputs/Statistics/', output.directory, '/'))

# Monthly report
rmarkdown::render(input = "monthly_report.Rmd",
                  output_file = paste0("monthly_report_", analysis_month),
                  output_dir = paste0('../Outputs/Statistics/', output.directory, '/'))
