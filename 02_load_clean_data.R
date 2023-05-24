###########################################################################################
###
### Load data for survival models
###
###########################################################################################

# rm(list=ls())
#
# library(Hmisc)
# library(lubridate)
# library(xlsx)

###
### Load Data
###

filepath <- "~/Documents/Data/Harvest/"

df_harv_dmu <-  read.csv(paste0(filepath,"AgingHarvestStudyAreaDMUs.csv"))
names(df_harv_dmu) <- tolower(gsub("[[:punct:]]","",names(df_harv_dmu)))
head(df_harv_dmu)


# df_harv_dmu <-  read.csv(paste0(filepath,"AgingHarvestDMUs70A_73A_70C_70D_Thru2013_01-25-2018.csv"))
# names(df_harv_dmu) <- tolower(gsub("[[:punct:]]","",names(df_harv_dmu)))
# head(df_harv_dmu)

###################################################################################################################################
###
### Extracting overall total harvested number of deer
### Retaining antlered/antlerless, and specifying East/West Study Area
###
###################################################################################################################################

df_harv_total <- df_harv_dmu[,1:10]
head(df_harv_total)

###################################################################################################################################
### Loading and cleaning earliest Age Composition data (no cwd test), 
### includes data prior to start of study
###################################################################################################################################

df_harv_aah <- df_harv_dmu[,c(1:2,11:ncol(df_harv_dmu))]
head(df_harv_aah)


###################################################################################################################################
### Loading conversion factors for AAH data
###################################################################################################################################

filepath2 <- "~/Documents/Data/Habitat_Calibration/"

load(paste0(filepath2,"grant_correct.Rdata"))
load(paste0(filepath2,"iowa_w_correct.Rdata"))
load(paste0(filepath2,"iowa_e_correct.Rdata"))
load(paste0(filepath2,"dane_e_correct.Rdata"))
load(paste0(filepath2,"dmu86_73_w_correct.Rdata"))
load( paste0(filepath2,"dmu86_73c_w_correct.Rdata"))
load( paste0(filepath2,"dmu86_70d_e_correct.Rdata"))
load( paste0(filepath2,"dmu86_70c_e_correct.Rdata"))
load( paste0(filepath2,"dmu86_70a_e_correct.Rdata"))
load( paste0(filepath2,"dmu99_73e_w_correct.Rdata"))
load( paste0(filepath2,"dmu99_70d_e_correct.Rdata"))
load( paste0(filepath2,"dmu99_70c_e_correct.Rdata"))
load( paste0(filepath2,"dmu99_70a_e_correct.Rdata"))
load( paste0(filepath2,"dmu13_73e_w_correct.Rdata"))
load( paste0(filepath2,"dmu13_70d_e_correct.Rdata"))
load( paste0(filepath2,"dmu13_70c_e_correct.Rdata"))
load( paste0(filepath2,"dmu13_70a_e_correct.Rdata"))




###################################################################################################################################
### Loading overall population estimate from SAK model
###################################################################################################################################

filepath <- "~/Documents/Data/Harvest/"
df_pop_estimate <-  read.csv(paste0(filepath, "Total_pop_size_UNIT.csv"))
df_pop_estimate$total[3]

###################################################################################################################################
#Loading and cleaning early Age Composition data (no cwd test)
###################################################################################################################################

df_age_early_male <-  read.csv(paste0(filepath,"age_composition_1994_2001_male.csv"))
df_age_early_male$U <- NULL
df_age_early_male <- df_age_early_male %>% group_by(YR) %>% 
    summarise(across(everything(), sum, na.rm = TRUE),
                .groups = 'drop')  %>%
    as.data.frame()
df_age_early_female <-  read.csv(paste0(filepath,"age_composition_1994_2001_female.csv"))
df_age_early_female$U <- NULL
df_age_early_female <- df_age_early_female %>% group_by(YR) %>% 
    summarise(across(everything(), sum,na.rm = TRUE),
                .groups = 'drop')  %>%
    as.data.frame()
df_age_early_female <- df_age_early_female %>% pivot_longer(!YR, names_to = "age", values_to = "count")
df_age_early_male <- df_age_early_male %>% pivot_longer(!YR, names_to = "age", values_to = "count")
df_age_early_female$age <- df_age_early_female$age %>% recode("A0" = 1,"A2" = 2, "A3" = 3, "A4" = 4,"A6" = 6, "A9" = 9, "ADD" = 0)
df_age_early_male$age <- df_age_early_male$age %>% recode("ADB" = 0,"LS0" = 1, "RB0" = 1, "RB2" = 2,"RB3" = 3, "RB4" = 4, "RB6" = 6,"SB0" = 1,"SB3" = 3)
df_age_early_male  <- suppressWarnings(df_age_early_male %>% group_by(YR,age) %>% dplyr::summarize(count=sum(count)))
df_age_early_male$sex <- "Male"
df_age_early_female$sex <- "Female"
df_age_early <- rbind(df_age_early_female,df_age_early_male)

names(df_age_early) <- c("year", "age", "n", "sex")
df_age_early <- arrange(df_age_early,year,sex,age)

fixn9m <-length(which(!(1994:2001 %in% df_age_early$year[df_age_early$sex=="Male" & df_age_early$age=="9"])))
df_age_early <- rbind(df_age_early,data.frame(year=c(1994:2001),sex=rep("Male",fixn9m),age=rep("9",fixn9m),n=rep(0,fixn9m)))
df_age_early <- arrange(df_age_early,year,sex,age)

###################################################################################################################################
#loading CWD surveillance data for calculating ages for CWD tested deer 
###################################################################################################################################

cwd_df_1 <- suppressWarnings(read_excel("~/Documents/Data/WDNR_surveillance/1999-2021countydeer.xlsx",sheet=1))
cwd_df_2 <- suppressWarnings(read_excel("~/Documents/Data/WDNR_surveillance/1999-2021countydeer.xlsx",sheet=2))
cwd_df_3 <- suppressWarnings(read_excel("~/Documents/Data/WDNR_surveillance/1999-2021countydeer.xlsx",sheet=3))

cwd_df <- as.data.frame(rbind(cwd_df_1,cwd_df_2,cwd_df_3))
names(cwd_df) <- gsub(" ","_",names(cwd_df))
names(cwd_df) <- gsub("/","_",names(cwd_df))
names(cwd_df) <- tolower(names(cwd_df))

###################################################################################################################################
# Cleaning Data
###################################################################################################################################

###
### Restricting to counties we care about
###

counties <- c("Iowa","Dane","Grant")
cwd_df <- cwd_df[cwd_df$county %in% counties,]

###
### Cleaning up age classes
###
# "44291" = "4-5"
# "44450" = "9-11"
# "44355" = "6-8"
# "44656" = "4-5"
# "44720" =  "6-8"
# "44815" = "9-11"

cwd_df$age <- as.factor(cwd_df$age)
levels(cwd_df$age) <- c("1","12+","2","3","4-5","6-8","9-11","4-5","6-8","9-11","ADULT","0")
cwd_df <- cwd_df[cwd_df$age != "ADULT", ]
cwd_df$age <- as.factor(as.character(cwd_df$age))
cwd_df$kill_date <- as.Date(cwd_df$kill_date,origin="1899-12-30")

#removing surveillence data with no town, range, range direction, or section
cwd_df <- cwd_df[!is.na(cwd_df$dtrs),]
cwd_df <- cwd_df[!is.na(cwd_df$sect),]
cwd_df <- cwd_df[!is.na(cwd_df$town),]
cwd_df <- cwd_df[!is.na(cwd_df$range),]
cwd_df <- cwd_df[!is.na(cwd_df$range_dir),]

# cwd_df$trs <- paste0(cwd_df$town,"-",cwd_df$range,"-",cwd_df$sect)
# removing deer without a kill date
cwd_df <- cwd_df[order(cwd_df$kill_date),]
cwd_df <- cwd_df[!is.na(cwd_df$kill_date),]

#removing surveillence data with no town, range, range direction, or section
cwd_df <- cwd_df[!is.na(cwd_df$sect),]
cwd_df <- cwd_df[!is.na(cwd_df$town),]
cwd_df <- cwd_df[!is.na(cwd_df$range),]
cwd_df <- cwd_df[!is.na(cwd_df$range_dir),]
cwd_df$trs <- paste0(cwd_df$town,"-",cwd_df$range,"-",cwd_df$sect)
cwd_df <- cwd_df[order(cwd_df$kill_date),]
cwd_df <- cwd_df[!is.na(cwd_df$kill_date),]

source("cleanData.R")
cwd_df <- cleanData(cwd_df)
ageclass <- as.numeric(levels(as.factor(cwd_df$age.num)))

####################################
### Restricing to the study area 
####################################

study_df <- sf::st_read("~/Documents/Data/Study_Area/secrdtrs_selection.shp")
#creating sections that account for range_direction first
study_df$dsection <- paste0(study_df$dir,"-",study_df$sectionid)
cwd_df$dsection <- do.call(paste, c(cwd_df[c("range_dir","range", "town", "sect")], sep = "-"))
cwd_df <- cwd_df[cwd_df$dsection %in% study_df$dsection,]
cwd_df$year <- lubridate::year(cwd_df$kill_date)

#setting all deer killed in January 2022 to be part of study year 2021
cwd_df$year[cwd_df$year==2022] <- 2021

########no disease status included######################################
# df_age_cwd <- cwd_df %>% group_by(year,sex,age) %>% summarise(n=n())
# df_age_cwd$sex <- as.factor(df_age_cwd$sex)
# levels(df_age_cwd$sex) <- c("Male","Female")
########################################################################

df_age_cwdpos <- cwd_df %>%filter(teststatus==1)%>% group_by(year,sex,age) %>% summarise(n=n())
df_age_cwdneg <- cwd_df %>%filter(teststatus==0)%>% group_by(year,sex,age) %>% summarise(n=n())

df_age_cwdpos$sex <- as.factor(df_age_cwdpos$sex)
levels(df_age_cwdpos$sex) <- c("Male","Female")
df_age_cwdpos

df_age_cwdneg$sex <- as.factor(df_age_cwdneg$sex)
levels(df_age_cwdneg$sex) <- c("Male","Female")
df_age_cwdneg

###
### Aged data without CWD testing
###

df_age_nocwd <- df_age_notest %>% group_by(yr) %>% summarise(mfawn = sum(mfawn),
                                      m1 = sum(m1forked,
                                               m1sublegal,
                                               m1legalspike,
                                               m1unknown,
                                               na.rm=TRUE),
                                      m2 = sum(m2,na.rm=TRUE),
                                      m3 = sum(m3,na.rm=TRUE),
                                      m4 = sum(m45,na.rm=TRUE),
                                      m6 = sum(m68,na.rm=TRUE),
                                      m9 = sum(m911,na.rm=TRUE),
                                      ffawn = sum(ffawn),
                                      f1 = sum(f1,na.rm=TRUE),
                                      f2 = sum(f2,na.rm=TRUE),
                                      f3 = sum(f3,na.rm=TRUE),
                                      f4 = sum(f45,na.rm=TRUE),
                                      f6 = sum(f68,na.rm=TRUE),
                                      f9 = sum(f911,f12,na.rm=TRUE)) %>% pivot_longer(cols=-yr)

#####################################################
###
### Combine cwd aged and nocwd aged data frames
###
#####################################################

names(df_age_nocwd) <- c("year", "age", "n")
class(df_age_nocwd$age)
df_age_nocwd$sex <- rep(c(rep("Male",7), rep("Female", 7)), 8)
df_age_nocwd$age <- as.factor(df_age_nocwd$age)
levels(df_age_nocwd$age) <- c("1","2","3","4","6","9","0","1","2","3","4","6","9","0")
df_age_nocwd$age <- as.numeric(as.character(df_age_nocwd$age))
df_age_nocwd <- df_age_nocwd[,c(1,4,2,3)]

df_age_inf <- df_age_cwdpos
df_age_sus <- df_age_cwdneg

#correct for these sex/age classes without any observations, and set those to 0 within the data frame
# c(2002:2021)[which(!(2002:2021 %in% df_age_sus$year[df_age_sus$sex=="Male" & df_age_sus$age=="9"]))]
# c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="9"]))]
# c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="6"]))]
# c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="9"]))]
# c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="6"]))]

fixn9m <-length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$sex=="Male" & df_age_sus$age=="9"])))
df_age_sus <-rbind(df_age_sus,data.frame(year=c(2002:2021)[which(!(2002:2021 %in% df_age_sus$year[df_age_sus$sex=="Male" & df_age_sus$age=="9"]))],sex=rep("Male",fixn9m),age=rep("9",fixn9m),n=rep(0,fixn9m)))

fixp9m <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="9"])))
fixp6m <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="6"])))
# fixp4m <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="4"])))
# fixp3m <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="3"])))
# fixp2m <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="2"])))
# fixp1m <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="1"])))
fixp0m <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="0"])))

df_age_inf <-rbind(df_age_inf,data.frame(year=c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="9"]))],sex=rep("Male",fixp9m),age=rep("9",fixp9m),n=rep(0,fixp9m)))
df_age_inf <-rbind(df_age_inf,data.frame(year=c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="6"]))],sex=rep("Male",fixp6m),age=rep("6",fixp6m),n=rep(0,fixp6m)))
df_age_inf <-rbind(df_age_inf,data.frame(year=c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="0"]))],sex=rep("Male",fixp0m),age=rep("0",fixp0m),n=rep(0,fixp0m)))


fixp9f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="9"])))
fixp6f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="6"])))
# fixp4f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="4"])))
# fixp3f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="3"])))
# fixp2f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="2"])))
# fixp1f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="1"])))
fixp0f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="0"])))

df_age_inf <-rbind(df_age_inf,data.frame(year=c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="9"]))],sex=rep("Female",fixp9f),age=rep("9",fixp9f),n=rep(0,fixp9f)))
df_age_inf <-rbind(df_age_inf,data.frame(year=c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="6"]))],sex=rep("Female",fixp6f),age=rep("6",fixp6f),n=rep(0,fixp6f)))
df_age_inf <-rbind(df_age_inf,data.frame(year=c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="0"]))],sex=rep("Female",fixp0f),age=rep("0",fixp6f),n=rep(0,fixp0f)))


df_age_inf <- arrange(df_age_inf,year,sex,age)
df_age_sus <- arrange(df_age_sus,year,sex,age)


for(i in 1:nrow(df_age_sus)){
    for(j in 1:nrow(df_age_nocwd)){
        if(df_age_sus$year[i] == df_age_nocwd$year[j] & 
            df_age_sus$sex[i] == df_age_nocwd$sex[j] & 
            df_age_sus$age[i] == df_age_nocwd$age[j]) {
            df_age_sus$n[i] <- df_age_sus$n[i]+df_age_nocwd$n[j]
        }
    }
}

df_age_sus <- rbind(df_age_early,df_age_sus)

### Number of age classes and sex classes
Age <- 7 
Sex <- 2

### number of years in the study
n_year <- length(unique(df_age_sus$year))

#structuring classification data to fit into the model
Cage_sus <- array(NA,c(Sex,Age,n_year))
for(j in 1:n_year){
    Cage_sus[1,,j] <- df_age_sus$n[df_age_sus$year == (1993+j) &
                                    df_age_sus$sex == "Female"]
    Cage_sus[2,,j] <- df_age_sus$n[df_age_sus$year == (1993+j) &
                                    df_age_sus$sex == "Male"]
}
Cage_sus[2,,]

#structuring classification data to fit into the model
Cage_inf <- array(NA,c(Sex,Age,length(unique(df_age_inf$year))))
for(j in 1:20){
    Cage_inf[1,,j] <- df_age_inf$n[df_age_inf$year == (2001+j) &
                                    df_age_inf$sex == "Female"]
    Cage_inf[2,,j] <- df_age_inf$n[df_age_inf$year == (2001+j) &
                                    df_age_inf$sex == "Male"]
}
# Cage_inf[,1,]
# Cage_inf[,2,]

Cage <- Cage_sus
Cage[,,9:28] <- Cage[,,9:28] + Cage_inf

#Aggregating the oldest age class for males into the next oldest age
Cage[2, 6, ] <- Cage[2, 6, ] + Cage[2, 7,]
Cage[2, 7, ] <- 0

####################################################################################
###
### Total harvest data
###
#####################################################################################

### if not using data by gun/bow types:
# df_harv <- df_harv[df_harv$yr>2001,]
# df_harvest <- df_harv %>% group_by(yr)%>%mutate(antlered = sum(antlered),
#                                   antlerless = sum(antlerless),
#                                   unk = sum(unk),
#                                   total = sum(total))
# df_harvest <- df_harvest[1:20,]
# df_harvest$cty <- NULL
# df_harvest
# df_harvest$total_minus_unknown <- df_harvest$total - df_harvest$unk
# Ototal <- df_harvest[,2:3]


#### using data that is restricted to bow/gun types
year <-1992:2021
df_harvest <- data.frame(year)
df_harvest$antlered <- df_harvest$antlerless <- c()
df_harvest$antlered_gun <- df_harvest$antlerless_gun <- c()
df_harvest$antlered_bow <- df_harvest$antlerless_bow <- c()
df_harvest$gun_unk <- df_harvest$bow_unk <- c()

for(i in 1992:2021){
    df_harvest$antlered[i-1992+1] <- sum(df_harv$antleredgun[df_harv$yr == i],na.rm=TRUE) +
                                     sum(df_harv$antleredbow[df_harv$yr == i],na.rm=TRUE) +
                                     sum(df_harv$antleredcrossbow[df_harv$yr == i],na.rm=TRUE)
    df_harvest$antlerless[i-1992+1] <- sum(df_harv$antlerlessgun[df_harv$yr == i],na.rm=TRUE) +
                                     sum(df_harv$antlerlessbow[df_harv$yr == i],na.rm=TRUE) +
                                     sum(df_harv$antlerlesscrossbow[df_harv$yr == i],na.rm=TRUE)
    df_harvest$antlered_gun[i-1992+1] <- sum(df_harv$antleredgun[df_harv$yr == i],na.rm=TRUE)
    df_harvest$antlerless_gun[i-1992+1] <- sum(df_harv$antlerlessgun[df_harv$yr == i],na.rm=TRUE)
    df_harvest$antlered_bow[i-1992+1] <- sum(df_harv$antleredbow[df_harv$yr == i],na.rm=TRUE) +
                                     sum(df_harv$antleredcrossbow[df_harv$yr == i],na.rm=TRUE)
    df_harvest$antlerless_bow[i-1992+1] <- sum(df_harv$antlerlessbow[df_harv$yr == i],na.rm=TRUE) +
                                     sum(df_harv$antlerlesscrossbow[df_harv$yr == i],na.rm=TRUE)
    df_harvest$gun_unk[i-1992+1] <- sum(df_harv$unknowngun[df_harv$yr == i],na.rm=TRUE) 
    df_harvest$bow_unk[i-1992+1] <- sum(df_harv$unknownbow[df_harv$yr == i],na.rm=TRUE) +
                                     sum(df_harv$unknowncrossbow[df_harv$yr == i],na.rm=TRUE)    
}
df_harvest$unk_total <- df_harvest$gun_unk+df_harvest$bow_unk

# O <- df_harvest[df_harvest$year>2001,]
# O <- df_harvest
O <- df_harvest[df_harvest$year>1993,]
Y <- nrow(O)

Ototal <- O[,c(1,2,3,10)]
Ogun<- O[,c(1,4,5,8)]
Obow <- O[,c(1,6:7,9)]

############################################################
###
### Separating the overall total number of harvested deer
###
############################################################

#removing the total number of positive deer from the surveillance data 
#from the overall total, so that we can have a separate Ototal_inf and Ototal_sus
#an assumption here is that negative deer in the subscripts could techinically
#be positive, they just tested negative (given the uncertainty of the diagnostic tests)

Ototal_inf <- data.frame(year=2002:2021,
                         antlered = apply(Cage_inf[2,,],2,sum) + Cage_inf[1,1,],
                         antlerless = apply(Cage_inf[1,2:7,],2,sum)
                         )
Ototal_sus  <-  Ototal

nysus <- dim(Ototal_sus)[1]
nyinf <- dim(Ototal_inf)[1]

#removing infected deer from the susceptible harvest deer overall
Ototal_sus$antlered[(nysus-nyinf+1):nysus] <- Ototal_sus$antlered[(nysus-nyinf+1):nysus] - Ototal_inf$antlered
Ototal_sus$antlerless[(nysus-nyinf+1):nysus] <- Ototal_sus$antlerless[(nysus-nyinf+1):nysus]  - Ototal_inf$antlerless


####################################################################################
### Loading and cleaning harvest compliance rate data
#####################################################################################

report_df <- suppressWarnings(read_excel("~/Documents/Data/Harvest/ComplianceRate2020.xlsx",sheet=7))
report_hyp_sum <- apply(report_df[,2:3],2,mean)

beta.moments <- function(mu,sigma){
	alpha = (mu^2-mu^3-mu*sigma^2)/sigma^2
	beta = (mu-2*mu^2+mu^3-sigma^2+mu*sigma^2)/(sigma^2)
	return(list(alpha=alpha,beta=beta))
}

report_hyp_all <- unlist(beta.moments(report_hyp_sum[1],report_hyp_sum[2]))
report_hyp_y <- matrix(NA,nrow(report_df),2)

for(i in 1:nrow(report_df)){
    report_hyp_y[i,] <- unlist(beta.moments(report_df$compliance_rate[i],report_df$se[i]))
}
report_hyp_y <- data.frame(report_hyp_y)
names(report_hyp_y) <- c("alpha","beta")

####################################################################################
###
### Loading and cleaning fawn:doe ratio estimates data
###
#####################################################################################

#from raw data
# fawndoe_df <- read.csv("~/Documents/Data/fawn_doe_ratio/County_Fawn_Doe_Ratio_Data_1997_2017.csv", header=TRUE)
# county <- fawndoe_df[,1]
# type <- fawndoe_df[,2]
# fawndoe_df <- data.frame(t(fawndoe_df[,3:23]))
# names(fawndoe_df) <- paste0(county,"_",type)
# fawndoe_df$year <- 1997:2017
# fawndoe_df <- fawndoe_df[,c(10,1:9)]
# rownames(fawndoe_df) <- NULL
# write.csv(fawndoe_df,file="~/Documents/Data/fawn_doe_ratio/fawndoe_1997_2017.csv",row.names=FALSE)

fawndoe_df <- read.csv("~/Documents/Data/fawn_doe_ratio/fawndoe_1997_2017.csv",header=TRUE)

#calculating overall fawn:doe ratios across all three counties
fawndoe_df$overall_doe <- fawndoe_df$dane_num_doe + fawndoe_df$iowa_num_doe + fawndoe_df$grant_num_doe
fawndoe_df$overall_fawn <- fawndoe_df$dane_num_fawn + fawndoe_df$iowa_num_fawn + fawndoe_df$grant_num_fawn
fawndoe_df$overall_fd <- fawndoe_df$overall_fawn/fawndoe_df$overall_doe

#Restricting to the years of the study
# fawndoe_df <- fawndoe_df[fawndoe_df$year>2001 & fawndoe_df$year<2017,]
fawndoe_df <- fawndoe_df[fawndoe_df$year<2017,]

#2017-2021
df_camtrap_fd <- read.csv("~/Documents/Data/fawn_doe_ratio/Iowa_FDR_2017-2021_with_sd.csv")

#reading data from 1992-2015
fd_older_df <- read_excel("~/Documents/Data/fawn_doe_ratio/SW_FDR_1992-2015.xlsx",1)
fd_older_df  <- fd_older_df%>%filter(year>1991 & year < 1997)
fd_older_df

names(fd_older_df) <- c("spatial.unit","year","overall_fawn","overall_doe","overall_fd")
for(j in 1:5){
    fawndoe_df[nrow(fawndoe_df)+1,] <- NA
}
indx_add <- which(is.na(fawndoe_df$year))

fawndoe_df$year[indx_add] <- fd_older_df$year
fawndoe_df$overall_doe[indx_add] <- fd_older_df$overall_doe
fawndoe_df$overall_fawn[indx_add] <- fd_older_df$overall_fawn
fawndoe_df$overall_fd[indx_add] <- fd_older_df$overall_fd
fawndoe_df <- fawndoe_df[order(fawndoe_df$year),]

fawndoe_df <- fawndoe_df[fawndoe_df$year>1993,]

# ##########################################
# ### moment matching functions
# ##########################################

# lognormal_moments <- function(barx,s){
# 	mu <- log(barx / sqrt((s^2) / (barx^2) + 1))
# 	sigma <- sqrt(log((s^2) / (barx^2) + 1))
# 	return(list(mu=mu,sigma=sigma))
# }

# gamma_moments <- function(mu,sigma){
# 	alpha <- (mu^2)/(sigma^2)
# 	beta <- mu/(sigma^2)
# 	return(list(alpha=alpha,beta=beta))
# }
# ##########################################
# ### calculate moments 
# ##########################################

# ##########################
# ### Option 2: lognormal
# ##########################

# fdr_ct_moments_2017_2021 <- lognormal_moments(df_camtrap_fd$fdr_mean,df_camtrap_fd$fdr_sd)

# #how to set the sd for the years without uncertainty?
# #approximate using the mean of the estimates from Jen's method and doubling it?
# mean(df_camtrap_fd$fdr_sd)*2

# # fdr_ct_moments_2002_2016 <- lognormal_moments(fawndoe_df$overall_fd,mean(df_camtrap_fd$fdr_sd)*2)
# fdr_ct_moments_1997_2016 <- lognormal_moments(fawndoe_df$overall_fd,mean(df_camtrap_fd$fdr_sd)*2)


# # obs_ct_fd_mu  <- c(fdr_ct_moments_2002_2016$mu,fdr_ct_moments_2017_2021$mu)
# # obs_ct_fd_sd <- c(fdr_ct_moments_2002_2016$sigma,fdr_ct_moments_2017_2021$sigma)

# obs_ct_fd_mu  <- c(fdr_ct_moments_1997_2016$mu,fdr_ct_moments_2017_2021$mu)
# obs_ct_fd_sd <- c(fdr_ct_moments_1997_2016$sigma,fdr_ct_moments_2017_2021$sigma)


# ##########################
# ### Option 3: gamma
# ##########################

# # fdr_ct_gam_moments_2002_2016 <- gamma_moments(fawndoe_df$overall_fd,mean(df_camtrap_fd$fdr_sd)*2)
# fdr_ct_gam_moments_1997_2016 <- gamma_moments(fawndoe_df$overall_fd,mean(df_camtrap_fd$fdr_sd)*2)
# fdr_ct_gam_moments_2017_2021 <- gamma_moments(df_camtrap_fd$fdr_mean,df_camtrap_fd$fdr_sd)

# # obs_ct_fd_alpha  <- c(fdr_ct_gam_moments_2002_2016$alpha,fdr_ct_gam_moments_2017_2021$alpha)
# # obs_ct_fd_beta <- c(fdr_ct_gam_moments_2002_2016$beta,fdr_ct_gam_moments_2017_2021$beta)


# obs_ct_fd_alpha  <- c(fdr_ct_gam_moments_1997_2016$alpha,fdr_ct_gam_moments_2017_2021$alpha)
# obs_ct_fd_beta <- c(fdr_ct_gam_moments_1997_2016$beta,fdr_ct_gam_moments_2017_2021$beta)

# # fec_init <- c(fawndoe_df$overall_fd,df_camtrap_fd$fdr_mean)
# #setting the first 5 years 1992-1996 to be the same as 1997
# obs_ct_fd_alpha  <- c(rep(obs_ct_fd_alpha[1],5),obs_ct_fd_alpha)
# obs_ct_fd_beta <- c(rep(obs_ct_fd_beta[1],5),obs_ct_fd_beta)

# ###
# ### repeating fawn:doe ratios from 1997, to cover the fact that I currently don't have
# ### data on f:d for 1992-1996

# for(i in 1:5){
#     fawndoe_df <- rbind(fawndoe_df[1,],fawndoe_df)
# }
# fawndoe_df$year[1:5] <-1992:1996 
# fec_init <- c(rep(fawndoe_df$overall_fd[1],5),fawndoe_df$overall_fd,df_camtrap_fd$fdr_mean)

# ###################################################################
# ### Option 4: gamma for camera trap, poisson for earlier data
# ###################################################################

# # fdr_ct_gam_moments_2002_2016 <- gamma_moments(fawndoe_df$overall_fd,mean(df_camtrap_fd$fdr_sd)*2)
# # fdr_ct_gam_moments_2017_2021 <- gamma_moments(df_camtrap_fd$fdr_mean,df_camtrap_fd$fdr_sd)

# # fdr_ct_gam_moments_2002_2016 <- gamma_moments(fawndoe_df$overall_fd,mean(df_camtrap_fd$fdr_sd)*2)
# fdr_ct_gam_moments_1992_2016 <- gamma_moments(fawndoe_df$overall_fd,mean(df_camtrap_fd$fdr_sd)*2)
# fdr_ct_gam_moments_2017_2021 <- gamma_moments(df_camtrap_fd$fdr_mean,df_camtrap_fd$fdr_sd)

# # obs_ct_fd_alpha  <- c(fdr_ct_gam_moments_2002_2016$alpha,fdr_ct_gam_moments_2017_2021$alpha)
# # obs_ct_fd_beta <- c(fdr_ct_gam_moments_2002_2016$beta,fdr_ct_gam_moments_2017_2021$beta)

# obs_ct_fd_alpha  <- c(fdr_ct_gam_moments_1992_2016$alpha,fdr_ct_gam_moments_2017_2021$alpha)
# obs_ct_fd_beta <- c(fdr_ct_gam_moments_1992_2016$beta,fdr_ct_gam_moments_2017_2021$beta)

# fec_init <- c(fawndoe_df$overall_fd,df_camtrap_fd$fdr_mean)

####################################################################################
###
### Loading data on occurance of earn-a-buck
###
#####################################################################################

filepath <- "~/Documents/Data/Harvest/"
df_eab <- read.csv(paste0(filepath,"eab_present.csv"))

# df_eab$EAB
# save(df_eab,file="datafiles/df_eab.Rdata")

