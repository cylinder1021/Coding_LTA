#clear environment
rm(list=ls())

#load datasets
load("21600-0001-Data.rda")
load("21600-0005-Data.rda")
load("21600-0008-Data.rda")

W1 <- da21600.0001
W2 <- da21600.0005
W3 <- da21600.0008

###################################################################################
###################################Data Cleaning###################################
###################################################################################
vars_keep_W1 <- c("AID", "H1GI1Y", "IYEAR",
                  "H1PF1", "H1PF23", #parental warmth
                  "BIO_SEX",
                  "H1DS14", "H1DS6",#physical violence
                  "H1WP17D","H1WP18D","H1WP17F","H1WP18F","H1WP17H", "H1WP18H", "H1WP17J", "H1WP18J", #extent of communication
                  "H1WP17A", "H1WP17B", "H1WP17C", "H1WP17E", "H1WP17I", #time together with mother
                  "H1WP11", "H1WP15", "H1WP12", "H1WP16", #Academic Expectations
                  "H1PF5", "H1PF25", "H1PF4", "H1PF24", #Relationship/Communication Satisfaction
                  "H1PF3", #inductive discipline
                  "H1GH1", #Physical health
                  "H1FS3", "H1FS6", "H1FS16" #depression
)
W1 <- W1[, vars_keep_W1]


rename_W1 <- c("ID", "BirthYear_W1", "IYear_W1",
               "Warmth_M", "Warmth_F", #M means Mother-reported, F means Father-reported, C means Child-reported
               "BioSex_C",
               "FreqGroup_w1", "HurtOthers_w1",
               "TalkDate_M","TalkDate_F","TalkPro_M","TalkPro_F","TalkSchool_M", "TalkSchool_F", "TalkActi_M", "TalkActi_F",
               "TimeShop_M", "TimeSport_M", "TimeReligious_M", "TimeMovie_M", "TimeWork_M",
               "AcaExpec_M", "AcaExpec_F", "AcaExpec_H_M", "AcaExpec_H_F", #H means high school
               "SatisRela_M", "SatisRela_F", "SatisComm_M", "SatisComm_F",
               "InducDis_M",
               "PhyHealth",
               "H1FS3", "H1FS6", "H1FS16" #depression
)

colnames(W1) <- rename_W1

names(W1)

# Re-scale, W1[c(1:N)], N means how many variables
numeric_columns <- sapply(W1[c(1:34)], function(column) {
  as.numeric(gsub("^\\D*(\\d+).*", "\\1", column))
})

W1[c(1:34)] <- numeric_columns

str(W1)
##370 missing values of no mom, 6 refused, and 5 don't know = 381
sum(is.na(W1$TalkDate_M))
##1952 missing values of no dad, 6 refused, 3 don't know, 1 not applicable = 1962
sum(is.na(W1$TalkDate_F))

#Age# AT W1
W1$Age_C_W1 <- W1$IYear_W1 - W1$BirthYear_W1 
range(W1$Age_C_W1, na.rm=T)
table(W1$Age_C_W1)

#Sex#
table(W1$BioSex_C)
W1$BioSex_C <- ifelse(W1$BioSex_C==1,0,1) #female = 1, male = 0
sum(is.na(W1$BioSex_C)) #NA=1
table(W1$BioSex_C)

#Parental Warmth 
table(W1$Warmth_M)
sum(is.na(W1$Warmth_M))
table(W1$Warmth_F)
W1$Warmth_M <- 6-W1$Warmth_M #5 means strongly agree
table(W1$Warmth_M)
W1$Warmth_F <- 6-W1$Warmth_F #5 means strongly agree
table(W1$Warmth_F)

#Extent of Communication
sum(is.na(W1$TalkDate_M))
W1$Comm_M <- rowSums(W1[,c("TalkDate_M", "TalkPro_M", "TalkSchool_M", "TalkActi_M")])
sum(is.na(W1$Comm_M))
table(W1$Comm_M)

W1$Comm_F <- rowSums(W1[,c("TalkDate_F", "TalkPro_F", "TalkSchool_F", "TalkActi_F")])
sum(is.na(W1$Comm_F))
table(W1$Comm_F) #6+1952+3+1=1962, match codebook


#Time together with mother
sum(is.na(W1$TimeShop_M))
W1$Timetogether_M <- rowSums(W1[,c("TimeShop_M", "TimeSport_M", "TimeReligious_M", "TimeMovie_M", "TimeWork_M")])
sum(is.na(W1$Timetogether_M))
table(W1$Timetogether_M)

#delete participants with no dad or mom
W1 <- W1[complete.cases(W1$Comm_M),]
sum(is.na(W1$Comm_M))
W1 <- W1[complete.cases(W1$Comm_F),]
sum(is.na(W1$Comm_F))

#Academic Expectations
table(W1$AcaExpec_M)
sum(is.na(W1$AcaExpec_M))
#other variables within Academic Expectations remain its original code

#Satisfaction with Relationship and Communication
table(W1$SatisRela_M)
W1$SatisRela_M <- 6-W1$SatisRela_M #5 means strongly agree
table(W1$SatisRela_M)
sum(is.na(W1$SatisRela_M))

table(W1$SatisRela_F)
W1$SatisRela_F <- 6-W1$SatisRela_F
table(W1$SatisRela_F)

table(W1$SatisComm_M)
W1$SatisComm_M <- 6-W1$SatisComm_M #5 means strongly agree
table(W1$SatisComm_M)

table(W1$SatisComm_F)
W1$SatisComm_F <- 6-W1$SatisComm_F #5 means strongly agree
table(W1$SatisComm_F)

#inductive discipline (mother only)
table(W1$InducDis_M)
W1$InducDis_M <- 6-W1$InducDis_M #5 means strongly agree
table(W1$InducDis_M)

#Physical health
table(W1$PhyHealth) #5 means poor

#Depression#
table(W1$H1FS3)
table(W1$H1FS6)
table(W1$H1FS16)

#cronbach's alpha at W1
library(dplyr)
cbind(W1$H1FS3, W1$H1FS6, W1$H1FS16) %>% psych::alpha() #.80

#Mental Health at W1
W1$MH_W1 <- rowSums(W1[,c("H1FS3","H1FS6","H1FS16")])
table(W1$MH_W1)
sum(is.na(W1$MH_W1))

#Physical Violence
table(W1$HurtOthers_w1)
sum(is.na(W1$HurtOthers_w1))

sum(is.na(W1$FreqGroup_w1))#NA=41
table(W1$FreqGroup_w1)


names(W1)

vars_keep_W1 <- c("ID", "BioSex_C", 
                  "Age_C_W1", 
                  "Warmth_M", "Warmth_F",
                  "AcaExpec_M", "AcaExpec_F", "SatisRela_M", "SatisRela_F",
                  "SatisComm_M", "SatisComm_F", "Comm_M", "Comm_F",
                  "Timetogether_M", "MH_W1", "H1FS3","H1FS6","H1FS16",
                  "HurtOthers_w1", "FreqGroup_w1")

W1 <- W1[, vars_keep_W1]

#########W2##########
vars_keep_W2 <- c("AID",
                  "H2DS13", "H2FV22", #physical violence
                  "H2FS3", "H2FS6", "H2FS16") #depression

rename_W2 <- c("ID", 
               "FreqGroup_w2", "HurtOthers_w2",
               "H2FS3", "H2FS6", "H2FS16") #depression

W2 <- W2[,vars_keep_W2]
colnames(W2) <- rename_W2

# Re-scale, W1[c(1:N)], N means how many variables
numeric_columns <- sapply(W2[c(1:6)], function(column) {
  as.numeric(gsub("^\\D*(\\d+).*", "\\1", column))
})

W2[c(1:6)] <- numeric_columns
#Mental health at W2
W2$MH_W2 <- rowSums(W2[,c("H2FS3","H2FS6","H2FS16")])
table(W2$MH_W2)
library(psych)
library(dplyr)
cbind(W2$H2FS3, W2$H2FS6, W2$H2FS16) %>% psych::alpha() #0.82

table(W2$HurtOthers_w2)
sum(is.na(W2$HurtOthers_w2))
W2$HurtOthers_w2 <- if_else(is.na(W2$HurtOthers_w2),0,W2$HurtOthers_w2)
table(W2$HurtOthers_w2)

sum(is.na(W2$FreqGroup_w2))#NA=31
table(W2$FreqGroup_w2)

#########W3##########
vars_keep_W3 <- c("AID", 
                  "H3DS7", "H3DS17", #physical violence
                  "H3SP6", "H3SP9", "H3SP12") #depression
rename_W3 <- c("ID", 
               "FreqGroup_w3", "HurtOthers_w3",
               "H3SP6", "H3SP9", "H3SP12") 
W3 <- W3[,vars_keep_W3]
colnames(W3) <- rename_W3

# Re-scale, W1[c(1:N)], N means how many variables
numeric_columns <- sapply(W3[c(1:6)], function(column) {
  as.numeric(gsub("^\\D*(\\d+).*", "\\1", column))
})

W3[c(1:6)] <- numeric_columns

#Mental health at W3
sum(is.na(W3$H3SP6))
W3$MH_W3 <- rowSums(W3[,c("H3SP6","H3SP9","H3SP12")])
table(W3$MH_W3)

#recode to the same scale as W1-W2
table(W3$HurtOthers_w3)
sum(is.na(W3$HurtOthers_w3))#NA=80
# back to the original coding of wave 2
W3$HurtOthers_w3 <- ifelse(W3$HurtOthers_w3==2,1,W3$HurtOthers_w3)
table(W3$HurtOthers_w3)
W3$HurtOthers_w3 <- ifelse(W3$HurtOthers_w3>2 & W3$HurtOthers_w3<5,2,W3$HurtOthers_w3)
table(W3$HurtOthers_w3)
W3$HurtOthers_w3 <- ifelse(W3$HurtOthers_w3>=5,3,W3$HurtOthers_w3)
table(W3$HurtOthers_w3)
sum(is.na(W3$HurtOthers_w3))

sum(is.na(W3$FreqGroup_w3))#NA=41
table(W3$FreqGroup_w3)

#########Combine#########
# Merge the datasets by ID
data <- merge(merge(W1, W2, by="ID", all = T), W3, by="ID", all =T)

data <- data[complete.cases(data$Comm_M),]
sum(is.na(data$Comm_M))
data <- data[complete.cases(data$Comm_F),]
sum(is.na(data$Comm_F))

####### Remove participants that missed all wave's outcome(s) separately: Note: we start with N = 4303 ##############
data <- filter(data,
               !is.na(data$MH_W1)|!is.na(data$MH_W2)|
                 !is.na(data$MH_W3))
data <- filter(data,
               !is.na(data$HurtOthers_w1)|!is.na(data$HurtOthers_w2)|
                 !is.na(data$HurtOthers_w3))

data <- filter(data,
               !is.na(data$FreqGroup_w1)|!is.na(data$FreqGroup_w2)|
                 !is.na(data$FreqGroup_w3))

save(data, file = "final_data.dat")
#data_outcome[is.na(data_outcome)] <- -999

#str(data_outcome)
#MplusAutomation::prepareMplusData(data_outcome, "data_outcome_0418.dat")