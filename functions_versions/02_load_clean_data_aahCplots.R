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
### Load Data and fix column names
###

filepath <- "~/Documents/Data/Harvest/"

df_age_notest <-  read_excel(paste0(filepath,"AgingDaneIowaGrant_2014-2021.xlsx"),1)
# df_harvest_iowa <- read_excel(paste0(filepath,"IowaGrantDaneCountyHarvest.xlsx"),1)
# df_harvest_grant <- read_excel(paste0(filepath,"IowaGrantDaneCountyHarvest.xlsx"),2)
# df_harvest_dane <- read_excel(paste0(filepath,"IowaGrantDaneCountyHarvest.xlsx"),3)
df_harv <- read_excel(paste0(filepath,"HarvestDaneIowaGrant_1992-2021.xlsx"),1)

# df_dmu_harvest <- read_excel(paste0(filepath,"HarvestDMUs70A_73A_70C_70D_2002_2013.xlsx"))

#changing column names
names(df_age_notest) <- tolower(gsub("[[:punct:]]","",names(df_age_notest)))
# names(df_harvest_iowa) <- tolower(gsub("[[:punct:]]","",names(df_harvest_iowa)))
# names(df_harvest_dane) <- tolower(gsub("[[:punct:]]","",names(df_harvest_dane)))
# names(df_harvest_grant) <- tolower(gsub("[[:punct:]]","",names(df_harvest_grant)))
names(df_harv) <- tolower(gsub("[[:punct:]]","",names(df_harv)))
# names(df_dmu_harvest) <- tolower(gsub("[[:punct:]]","",names(df_dmu_harvest)))

# df_harv <- rbind(df_harvest_dane,df_harvest_iowa,df_harvest_grant)

### Double check loading correctly
head(df_age_notest)
head(df_harv)
# head(df_dmu_harvest)

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
df_age_nocwd <- df_age_nocwd[,c(1,4,2,3)]

df_age_inf <- df_age_cwdpos
df_age_sus <- df_age_cwdneg

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


df_age_inf <- arrange(df_age_inf, year, sex, age)
df_age_sus <- arrange(df_age_sus, year, sex, age)

# for(i in 1:nrow(df_age_sus)){
#     for(j in 1:nrow(df_age_nocwd)){
#         if(df_age_sus$year[i] == df_age_nocwd$year[j] & 
#             df_age_sus$sex[i] == df_age_nocwd$sex[j] & 
#             df_age_sus$age[i] == df_age_nocwd$age[j]) {
#             df_age_sus$n[i] <- df_age_sus$n[i]+df_age_nocwd$n[j]
#         }
#     }
# }

n_year <- length(unique(df_age_sus$year))

### Number of age classes and sex classes
Age <- 7 
Sex <- 2

#structuring classification data to fit into the model
Cage_sus <- array(NA,c(Sex,n_year,Age))
for(j in 1:n_year){
    Cage_sus[1,j,] <- df_age_sus$n[df_age_sus$year == (2001+j) & df_age_sus$sex == "Female"]
    Cage_sus[2,j,] <- df_age_sus$n[df_age_sus$year == (2001+j) & df_age_sus$sex == "Male"]
}
Cage_sus[2,,]

#structuring classification data to fit into the model
Cage_inf <- array(NA,c(Sex,length(unique(df_age_inf$year)),Age))
for(j in 1:20){
    Cage_inf[1,j,] <- df_age_inf$n[df_age_inf$year == (2001+j) & df_age_inf$sex == "Female"]
    Cage_inf[2,j,] <- df_age_inf$n[df_age_inf$year == (2001+j) & df_age_inf$sex == "Male"]
}
# Cage_inf[,1,]
# Cage_inf[,2,]


Cage_aah <- array(NA,c(Sex,length(unique(df_age_nocwd$year)),Age))
for(j in 1:length(unique(df_age_nocwd$year))){
    Cage_aah[1,j,] <- df_age_nocwd$n[df_age_nocwd$year == (2013+j) & df_age_nocwd$sex == "Female"]
    Cage_aah[2,j,] <- df_age_nocwd$n[df_age_nocwd$year == (2013+j) & df_age_nocwd$sex == "Male"]
}

#these are all from surveillance data
Cage <- Cage_sus + Cage_inf

# #Aggregating the oldest age class for males into the next oldest age
Cage[2,,6] <- Cage[2,,6] + Cage[2,,7]
Cage[2,,7] <- 0


#adding antlerless male fawns to the Cage_less data
Cage_less <- cbind(Cage[1,,],Cage[2,,1])
Cage_ant <- Cage[2,,,2:6]


Cage_antlerless  <- data.frame(Cage_less[,1:7])
Cage_antlerless <- Cage_antlerless/apply(Cage_antlerless,1,sum)
names(Cage_antlerless)=c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+")
Cage_antlerless$Year <- 1994:2021
Cage_less_long <- pivot_longer(Cage_antlerless,cols=1:7)
Cage_less_long$name <- factor(as.factor(Cage_less_long$name),levels=c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))
plotless_legend <- ggplot(Cage_less_long,aes(x=Year,y=value))+
    geom_col(aes(fill=name))+theme_bw()+
    scale_fill_manual(name="Age Class",values=rev(met.brewer("Veronese",7)))+
    ylab("Proportion")+ggtitle("Female")

ggsave("figures/plotless_surv_legend.png",plotless_legend,height=6,width=10)


Cage_antlerless_nocwd  <- data.frame(Cage_aah[,1,1:7])
Cage_antlerless_nocwd <- Cage_antlerless_nocwd/apply(Cage_antlerless_nocwd,1,sum)
names(Cage_antlerless_nocwd)=c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+")
Cage_antlerless_nocwd$Year <- 2014:2021
Cage_less_long <- pivot_longer(Cage_antlerless_nocwd,cols=1:7)
Cage_less_long$name <- factor(as.factor(Cage_less_long$name),levels=c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))
plotless_nocwd_legend <- ggplot(Cage_less_long,aes(x=Year,y=value))+
    geom_col(aes(fill=name))+theme_bw()+
    scale_fill_manual(name="Age Class",values=rev(met.brewer("Veronese",7)))+
    ylab("Proportion")+ggtitle("Female")
plotless_nocwd_legend
ggsave("figures/plotless_nocwd_legend.png",plotless_nocwd_legend,height=6,width=10)


Cage_antler <- cbind(Cage_less[,8],Cage_ant)
Cage_antler  <- data.frame(Cage_antler)
Cage_antler <- Cage_antler/apply(Cage_antler,1,sum)
names(Cage_antler)=c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5+")
Cage_antler$Year <- 2002:2021
Cage_ant_long <- pivot_longer(Cage_antler,cols=1:6)
Cage_ant_long$name <- factor(as.factor(Cage_ant_long$name), levels = rev(c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5+")))
plotant <- ggplot(Cage_ant_long,aes(x=Year,y=value))+
    geom_col(aes(fill=name))+theme_bw()+
    scale_fill_manual(name="Age Class",values=met.brewer("Veronese",7)[2:7])+
    ylab("Proportion")+ggtitle("Male")
plotant

ggsave("figures/plotant_surv.png",plotant,height=6,width=10)


Cage_antler_nocwd  <- data.frame(Cage_aah[,2,1:6])
Cage_antler_nocwd <- Cage_antler_nocwd/apply(Cage_antler_nocwd,1,sum)
names(Cage_antler_nocwd)=c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5+")
Cage_antler_nocwd$Year <- 2014:2021
Cage_ant_long_nocwd <- pivot_longer(Cage_antler_nocwd,cols=1:6)
Cage_ant_long_nocwd$name <- factor(as.factor(Cage_ant_long_nocwd$name),levels=rev(c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5+")))
plotant_nocwd <- ggplot(Cage_ant_long_nocwd,aes(x=Year,y=value))+
    geom_col(aes(fill=name))+theme_bw()+
    scale_fill_manual(name="Age Class",values=met.brewer("Veronese",7)[2:7])+
    ylab("Proportion")+ggtitle("Male")
plotant_nocwd
ggsave("figures/plotant_aah.png",plotant_nocwd,height=6,width=10)


Cage_antlerless <- Cage_antlerless[Cage_antlerless$Year > 2013,]
Cage_less_long <- pivot_longer(Cage_antlerless,cols=1:7)
Cage_less_long$name <- factor(as.factor(Cage_less_long$name),levels=c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))
plotless_2014 <- ggplot(Cage_less_long,aes(x=Year,y=value))+
    geom_col(aes(fill=name))+theme_bw()+
    scale_fill_manual(name="Age Class",values=rev(met.brewer("Veronese",7)))+
    ylab("Proportion")+ggtitle("Female")

plotless_2014

ggsave("figures/plotless_legend_2014_2021.png",plotless_2014,height=6,width=10)


Cage_antler <- Cage_antler[Cage_antler$Year>2013,]
Cage_ant_long <- pivot_longer(Cage_antler,cols=1:6)
Cage_ant_long$name <- factor(as.factor(Cage_ant_long$name),levels=rev(c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5+")))
plotant_2014 <- ggplot(Cage_ant_long,aes(x=Year,y=value))+
    geom_col(aes(fill=name))+theme_bw()+
    scale_fill_manual(name="Age Class",values=met.brewer("Veronese",7)[2:7])+
    ylab("Proportion")+ggtitle("Male")
# plotant
plotant_2014

ggsave("figures/plotant_surv_2014_2021.png",plotant_2014,height=6,width=10)


# #Aggregating the oldest age class for males into the next oldest age
# Cage[,2,6] <- Cage[,2,6] + Cage[,2,7]
# Cage[,2,7] <- 0

####################################################################################
###
### Age-at-harvest Data
###
#####################################################################################

dim(df_age_sus)
dim(df_age_inf)
dim(df_age_nocwd)
names(df_age_nocwd)

ggplot(df_age_nocwd,aes(x=age,y=n,color=sex)) +
    geom_point() +
    facet_wrap(.~year)
    


Cage_antlerless  <- data.frame(Cage_less[,1:7])
Cage_antlerless <- Cage_antlerless/apply(Cage_antlerless,1,sum)
names(Cage_antlerless)=c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+")
Cage_antlerless$Year <- 2002:2021

Cage_less_long <- pivot_longer(Cage_antlerless,cols=1:7)
Cage_less_long$name <- factor(as.factor(Cage_less_long$name),levels=c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))
plotless_legend <- ggplot(Cage_less_long,aes(x=Year,y=value))+
    geom_col(aes(fill=name))+theme_bw()+
    scale_fill_manual(name="Age Class",values=rev(met.brewer("Veronese",7)))+
    ylab("Proportion")+ggtitle("Female")
ggsave("figures/plotless_legend.png",plotless_legend,height=6,width=10)




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
O <- df_harvest
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
                         antlered = apply(Cage_inf[,2,],1,sum) + Cage_inf[,1,1],
                         antlerless = apply(Cage_inf[,1,2:7],1,sum)
                         )

Ototal_sus  <-  Ototal

nysus <- dim(Ototal_sus)[1]
nyinf <- dim(Ototal_inf)[1]
#removing infected deer from the susceptible harvest deer overall
Ototal_sus$antlered[(nysus-nyinf+1):nysus] <- Ototal_sus$antlered[(nysus-nyinf+1):nysus] - Ototal_inf$antlered
Ototal_sus$antlerless[(nysus-nyinf+1):nysus] <- Ototal_sus$antlerless[(nysus-nyinf+1):nysus]  - Ototal_inf$antlerless


####################################################################################
###
### Loading and cleaning harvest compliance rate data
###
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




####################################################################################
###
### Plotting DMU total harvest data vs County total harvest data
###
#####################################################################################

### Ototal == Ogun+Obow 
### checks okay

###
### Initial values
###

#initial popsize by sex and age class, just using the first year of aged data...
#but we should do this better
# logN <- log(Cage[1,,])
#the first year has 0 for males in the oldest age class, which is -Inf
#so setting it to 0, which is equivalent to popsize of 1, in that age class


################################################################################
###
### read in DMU shapefiles
###
###############################################################################

# #these are the DMUs that roughly align with our study area
# units <-c("70C-CWD","70D-CWD","70A-CWD","73E-CWD")

# dmu_2002 <- st_read("/home/aketz/Documents/Data/DMU_shapefiles_2002_2013/dmu_2002.shp")
 
# # st_crs(dmu_2013) <- 4326
# # st_transform(dmu_2013,"+proj=tmerc +lat_0=0 +lon_0=-90 +k=0.9996 +x_0=520000+y_0=-4480000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" )

# dmu_2002$study<-ifelse(dmu_2002$UNIT_ID %in% units,1,0)

# dmu_2013 <- st_read("/home/aketz/Documents/Data/DMU_shapefiles_2002_2013/dmu_2013.shp")
# dmu_2013$study<-ifelse(dmu_2013$UNIT_ID %in% units,1,0)

# dmu_2013 <- st_read("/home/aketz/Documents/Data/DMU_shapefiles_2002_2013/dmu_2013.shp")
# dmu_2013$study<-ifelse(dmu_2013$UNIT_ID %in% units,1,0)


# study_area <- st_read("/home/aketz/Documents/Data/Study_Area/secrdtrsWGS84selection.shp")
# study_bound <-st_union(study_area)
# plot(study_bound)
# head(study_bound)

# ggplot()+
#   geom_sf(data=dmu_2002,color="black",aes(fill=study))+
#   geom_sf(data=study_bound,alpha=0,color="white")+ggtitle("2002")

# dmu_vs_study <- ggplot()+
#   geom_sf(data=dmu_2013,color="black",aes(fill=study))+
#   geom_sf(data=study_bound,alpha=0,color="white")+ggtitle("2013")
# dmu_vs_study
# ggsave(dmu_vs_study,file="figures/dmu_vs_study_2013.png")


# ####################################
# ###
# ### county vs study 
# ###
# ####################################

# counties_study = c("Dane","Iowa","Grant")
# county <- st_read("/home/aketz/Documents/Data/counties/dmu_2018_2020.shp")
# county$study<-ifelse(county$CTY_NAME %in% counties_study,1,0)

# county_vs_study <- ggplot()+
#   geom_sf(data = county, color = "black",aes(fill = study)) +
#   geom_sf(data = study_bound, alpha = 0, color = "white") + ggtitle("2018-2020")

# county_vs_study

# ggsave(county_vs_study,file="figures/county_vs_study_2018_2020.png")


# ####################################
# ###
# ### county vs dmu 
# ###
# ####################################

# county_dmu_plot <- ggplot()+
#   geom_sf(data=dmu_2013,color="grey")+
#   geom_sf(data=county,color="darkgrey",aes(fill=study),alpha=.2)+
#   geom_sf(data=study_bound,alpha=0,color="white")+ggtitle("County lines and deer DMU 2013")

# county_dmu_plot
# ggsave(county_dmu_plot,file="figures/county_dmu_plot.png")



# ####################################
# ###
# ### dane county proportion harvest 
# ###
# ####################################

# county_dmu_plot <- ggplot()+
#   geom_sf(data=dmu_2013,color="grey")+
#   geom_sf(data=county,color="darkgrey",aes(fill=study),alpha=.2)+
#   geom_sf(data=study_bound,alpha=0,color="white")+ggtitle("County lines and deer DMU 2013")

# county_dmu_plot
# ggsave(county_dmu_plot,file="figures/county_dmu_plot.png")


# #extracting dane from county shapefile
# st_crs(county)
# st_transform(study_bound,st_crs(county))
# st_intersection(county[county$dmu=="Dane",],study_bound)

# ######################################################################################


# df_dmu_totharvest <-df_dmu_harvest %>% group_by(yr) %>% summarise(antlered_gun = sum(antleredgun),
#                                               antlerlessgun = sum(antlerlessgun),
#                                               unknowngun = sum(unknowngun),
#                                               totalgun = sum(totalgun),
#                                               antleredbow = sum(antleredbow),
#                                               antlered = sum(antleredgun)+sum(antleredbow),
#                                               antlerless = sum(antlerlessgun),
#                                               unknown = sum(unknowngun)
#                                                ) #%>% pivot_longer(cols=-yr)



# df_dmu_totharvest$antlered/df_harvest$antlered[df_harvest$year %in% (2002:2013)]


# plot(2002:2013,df_dmu_totharvest$antlered/df_harvest$antlered[df_harvest$year %in% (2002:2013)])

# png("figures/dmu_county_harvest_num.png")
# plot(df_dmu_totharvest$antlered,df_harvest$antlered[df_harvest$year %in% (2002:2013)])
# dev.off()


# png("figures/dmu_county_harvest_prop.png")
# plot(2002:2013,df_dmu_totharvest$antlered/df_harvest$antlered[df_harvest$year %in% (2002:2013)])
# dev.off()

###
### calculate proportion area of county that is in study area for each county. 
###

#https://gis.stackexchange.com/questions/287602/how-to-calculate-the-polygon-area-and-create-a-column-of-results-in-r



###########################
###
###
###
###########################


# Ototal_long <- pivot_longer(Ototal[,1:3],2:3)
# names(Ototal_long) <- c("Year","Harvest_Type","Harvest_Total")
# Ototal_long$Harvest_Type <- factor(Ototal_long$Harvest_Type,labels=c("Antlered","Antlerless"))
# ototal_plot <- ggplot(Ototal_long,aes(x=Year, y=Harvest_Total, color = Harvest_Type)) +
#     geom_line(size=1.1) + 
#     geom_point(size=1.4) +
#     theme_bw() + 
#     scale_color_manual(values=met.brewer("Troy",2),name="Harvest Type") +
#     ylab("Harvest Total")+
#     theme(axis.text=element_text(size=14),
#             axis.title=element_text(size=16),
#             legend.text=element_text(size=14),
#             legend.title=element_text(size=16))
# ggsave("figures/total_harvest_plot.png",ototal_plot,height=7,width=9)


# Cage_antlerless  <- data.frame(Cage_less[,1:7])
# Cage_antlerless <- Cage_antlerless/apply(Cage_antlerless,1,sum)
# names(Cage_antlerless)=c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+")
# Cage_antlerless$Year <- 2002:2021


# Cage_less_long <- pivot_longer(Cage_antlerless,cols=1:7)
# Cage_less_long$name <- factor(as.factor(Cage_less_long$name),levels=c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))
# plotless_legend <- ggplot(Cage_less_long,aes(x=Year,y=value))+
#     geom_col(aes(fill=name))+theme_bw()+
#     scale_fill_manual(name="Age Class",values=rev(met.brewer("Veronese",7)))+
#     ylab("Proportion")+ggtitle("Female")
# ggsave("figures/plotless_legend.png",plotless_legend,height=6,width=10)


# Cage_less_long$name <- factor(as.factor(Cage_less_long$name),levels=rev(c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+")))
# plotless <- ggplot(Cage_less_long,aes(x=Year,y=value))+
#     geom_col(aes(fill=name))+theme_bw()+
#     scale_fill_manual(name="Age Class",values=met.brewer("Veronese",7))+
#     ylab("Proportion")+ggtitle("Female")+theme(legend.position="bottom")+
#     guides(color = guide_legend(nrow = 1))


# Cage_antler <- cbind(Cage_less[,8],Cage_ant)
# Cage_antler  <- data.frame(Cage_antler)
# Cage_antler <- Cage_antler/apply(Cage_antler,1,sum)
# names(Cage_antler)=c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5+")
# Cage_antler$Year <- 2002:2021
# Cage_ant_long <- pivot_longer(Cage_antler,cols=1:6)
# Cage_ant_long$name <- factor(as.factor(Cage_ant_long$name),levels=rev(c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5+")))

# # plotant <- ggplot(Cage_ant_long,aes(x=Year,y=value))+
# # geom_point()+
# # facet_wrap(.~name,ncol=1)+
# # theme_bw()+
# # ylab("Number Deer")+ylim(0,1000)

# plotant <- ggplot(Cage_ant_long,aes(x=Year,y=value))+
#     geom_col(aes(fill=name))+theme_bw()+
#     scale_fill_manual(name="Age Class",values=met.brewer("Veronese",7)[2:7])+
#     ylab("Proportion")+ggtitle("Male")
# # plotant

# ggsave("figures/plotant.png",plotant,height=6,width=10)
# ggsave("figures/plotless.png",plotless,height=6,width=10)


# combo_ant_less <- grid.arrange(plotant + theme(legend.position="none"),
#                          plotless + theme(legend.position="none"),
#                          nrow=1)

# ggsave("figures/combo_ant_less.png",combo_ant_less,height=6,width=10)


# fd_df <- data.frame(year=2002:2021,fd_ratio=c(fawndoe_df$overall_fd[1:15],
# df_camtrap_fd$fdr_mean),fd_low=c(rep(NA,15),df_camtrap_fd$fdr_lower95),
# fd_up=c(rep(NA,15),df_camtrap_fd$fdr_upper95)
# )

# fec_plot <- ggplot(fd_df,aes(x=year,y=fd_ratio))+
#     geom_line(size=2)+
#     geom_point(size=2)+theme_bw()+ylab("Fawn:Doe Ratio")+
#     geom_errorbar(aes(ymin=fd_low, ymax=fd_up), width=.5,
#                  position=position_dodge(.9))+
#     theme(axis.text=element_text(size=14),
#             axis.title=element_text(size=16),
#             legend.text=element_text(size=14),
#             legend.title=element_text(size=16))+xlab("Year")


# ggsave("figures/fec_plot.png",fec_plot,width=8,height=6)



# report_plot <- ggplot(report_df,aes(x=year,y=compliance_rate)) +
#     geom_point() +
#     geom_line() +
#     geom_errorbar(aes(ymin=compliance_rate - 2*se,ymax = compliance_rate + 2*se),width=.2)+
#     geom_hline(yintercept =report_hyp_sum[1],linetype="dashed",color=met.brewer("Veronese",1))+
#     theme_bw()+
#     ggtitle("Compliance Rate of 1st Harvested Deer")+
#     ylab("Compliance Rate") +
#     xlab("Year")+
#     theme(axis.text=element_text(size=14),
#             axis.title=element_text(size=16),
#             legend.text=element_text(size=14),
#             legend.title=element_text(size=16),
#             title = element_text(size=16))

# ggsave("figures/report_plot.png",report_plot,height = 6, width = 8)



