# Things written by Felicity for subsequent plotting
complete.df <- final.database.df[which(!is.na(final.database.df$Admission.NeoTreeId)),]

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