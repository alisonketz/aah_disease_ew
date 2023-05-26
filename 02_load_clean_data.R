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
names(df_harv_dmu)[2] <- "dmu"
head(df_harv_dmu)

df_harv_total_county <- read_excel(paste0(filepath,"HarvestDaneIowaGrant_1992-2021.xlsx"),1)
names(df_harv_total_county) <- tolower(gsub("[[:punct:]]","",names(df_harv_total_county)))
df_harv_total_county <- df_harv_total_county[df_harv_total_county$yr>2013,]

#changing NAs to zeros 
df_harv_total_county[is.na(df_harv_total_county)] <- 0

#summing of landtype, which are just public/private land harvest
df_harv_total_county <- as.data.frame(df_harv_total_county %>% group_by(yr,cty) %>%
    summarise(antleredgun = sum(antleredgun),
    antlerlessgun = sum(antlerlessgun),
    antleredbow = sum(antleredbow),
    antlerlessbow = sum(antlerlessbow),
    antleredcrossbow = sum(antleredcrossbow),
    antlerlesscrossbow = sum(antlerlesscrossbow)))

df_harv_total_county$study_area <- c()
df_harv_total_county$study_area[df_harv_total_county$cty == "Dane"] <- "east"
df_harv_total_county$study_area[df_harv_total_county$cty == "Grant"] <- "west"
df_harv_total_county$study_area[df_harv_total_county$cty == "Iowa"] <- "east"


df_harv_total_county <- rbind(df_harv_total_county,
      cbind(df_harv_total_county[df_harv_total_county$cty == "Iowa",1:8],
      study_area=rep("west",sum(df_harv_total_county$cty == "Iowa")))
      )

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
# head(df_harv_total)

###################################################################################################################################
### Loading and cleaning earliest Age Composition data (no cwd test), 
### includes data prior to start of study
###################################################################################################################################

df_harv_aah <- df_harv_dmu[,c(1:2,11:ncol(df_harv_dmu))]
# head(df_harv_aah)


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


harvest_area <- c("grant_w",
"iowa_w",
"iowa_e",
"dane_e",
"dmu86_73",
"dmu86_73c",
"dmu86_70d",
"dmu86_70c",
"dmu86_70a",
"dmu99_73e",
"dmu99_70d",
"dmu99_70c",
"dmu99_70a",
"dmu13_73e",
"dmu13_70d",
"dmu13_70c",
"dmu13_70a")


correction_factors <- c(grant_correct,
iowa_w_correct,
iowa_e_correct,
dane_e_correct,
dmu86_73_w_correct,
dmu86_73c_w_correct,
dmu86_70d_e_correct,
dmu86_70c_e_correct,
dmu86_70a_e_correct,
dmu99_73e_w_correct,
dmu99_70d_e_correct,
dmu99_70c_e_correct,
dmu99_70a_e_correct,
dmu13_73e_w_correct,
dmu13_70d_e_correct,
dmu13_70c_e_correct,
dmu13_70a_e_correct)

# write.csv(data.frame(harvest_area,correction_factors),file="results/harvest_area_correction.csv")

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

study_df <- sf::st_read("~/Documents/Data/Study_Area/study_df.shp")

#creating sections that account for range_direction first
cwd_df$dsection <- do.call(paste, c(cwd_df[c("range_dir","range", "town", "sect")], sep = "-"))
cwd_df <- cwd_df[cwd_df$dsection %in% study_df$dsection,]
cwd_df$year <- lubridate::year(cwd_df$kill_date)

#setting all deer killed in January 2022 to be part of study year 2021
cwd_df$year[cwd_df$year==2022] <- 2021

#setting up east/west study areas
cwd_df$ew <- rep(NA,nrow(cwd_df))
for(i in 1:nrow(study_df)){
        j <- which(cwd_df$dsection %in% study_df$dsection[i])
            cwd_df$ew[j] <- study_df$ew[i]
        }
#table(cwd_df$ew)
# head(cwd_df$ew)
# cwd_df$ew <- as.numeric(as.factor(cwd_df$ew))#east ==1, west == 2

########no disease status included######################################
# df_age_cwd <- cwd_df %>% group_by(year,sex,age) %>% summarise(n=n())
# df_age_cwd$sex <- as.factor(df_age_cwd$sex)
# levels(df_age_cwd$sex) <- c("Male","Female")
########################################################################

df_age_cwdpos <- cwd_df %>%filter(teststatus == 1) %>% group_by(year,sex,age) %>% summarise(n=n())
df_age_cwdneg <- cwd_df %>%filter(teststatus == 0) %>% group_by(year,sex,age) %>% summarise(n=n())

df_age_cwdpos$sex <- as.factor(df_age_cwdpos$sex)
levels(df_age_cwdpos$sex) <- c("Male","Female")
df_age_cwdpos

df_age_cwdneg$sex <- as.factor(df_age_cwdneg$sex)
levels(df_age_cwdneg$sex) <- c("Male","Female")
df_age_cwdneg

#####################################################################
###
### calibrating overall harvest totals by DMU, 
### and county, depending on the year
###
### dmu86:1983-1998 (will calibrate for all data prior to 1998)
### dmu99:1999-2001
### dmu13:2002-2013
### county-level:2013-2021 
###
#####################################################################

#####################################################################
###
### dmu86:1983:1998 (will calibrate for all data prior to 1998)
###
### dmu86_73_w_correct
### dmu86_73c_w_correct
### dmu86_70d_e_correct
### dmu86_70c_e_correct
### dmu86_70a_e_correct
###
#####################################################################

df_harv_total_dmu86 <- df_harv_total[df_harv_total$yr < 1999,]
#replacing NAs w/0's for addition
df_harv_total_dmu86[is.na(df_harv_total_dmu86)] <- 0

df_harv_total_dmu86$antleredHarvest <- df_harv_total_dmu86$antleredgun +
                                        df_harv_total_dmu86$antleredbow

df_harv_total_dmu86$antlerlessHarvest <- df_harv_total_dmu86$antlerlessgun +
                                         df_harv_total_dmu86$antlerlessbow
#there's 0 harvest depending on dmu
# df_harv_total_dmu86[df_harv_total_dmu86$antleredHarvest==0,] 

# plot_harv_total_dmu86_antlered <- ggplot(data = df_harv_total_dmu86) +
#     geom_point(aes(x = yr, y = antleredHarvest, color = dmu), size = 3) +
#         geom_line(aes(x = yr, y = antleredHarvest,color = dmu))+
#     ggtitle("dmu antlered harvest before 1999")+
#     theme_bw()
# ggsave(plot_harv_total_dmu86_antlered,file="figures/plot_harv_total_dmu86_antlered.png")


# plot_harv_total_dmu86_antlerless <- ggplot(data=df_harv_total_dmu86)+
#     geom_point(aes(x=yr,y=antlerlessHarvest,color=dmu),size=3)+
#     geom_line(aes(x=yr,y=antlerlessHarvest,color=dmu))+
#     ggtitle("dmu antlerless harvest before 1999")+
#     theme_bw()
# plot_harv_total_dmu86_antlerless
# ggsave(plot_harv_total_dmu86_antlerless,file="figures/plot_harv_total_dmu86_antlerless.png")


#calibrate antlered harvest by dmu

df_harv_total_dmu86$antlered_harv_tot <- c()
df_harv_total_dmu86$antlerless_harv_tot <- c()
df_harv_total_dmu86$study_area <- c()

df_harv_total_dmu86$antlered_harv_tot[df_harv_total_dmu86$dmu == "73"]  <- 
    df_harv_total_dmu86$antleredHarvest[df_harv_total_dmu86$dmu == "73"] *
    dmu86_73_w_correct

df_harv_total_dmu86$antlered_harv_tot[df_harv_total_dmu86$dmu == "73C"]  <- 
    df_harv_total_dmu86$antleredHarvest[df_harv_total_dmu86$dmu == "73C"] *
    dmu86_73c_w_correct

df_harv_total_dmu86$antlered_harv_tot[df_harv_total_dmu86$dmu == "70D"]  <- 
    df_harv_total_dmu86$antleredHarvest[df_harv_total_dmu86$dmu == "70D"] *
    dmu86_70d_e_correct

df_harv_total_dmu86$antlered_harv_tot[df_harv_total_dmu86$dmu == "70C"]  <- 
    df_harv_total_dmu86$antleredHarvest[df_harv_total_dmu86$dmu == "70C"] *
    dmu86_70c_e_correct

df_harv_total_dmu86$antlered_harv_tot[df_harv_total_dmu86$dmu == "70A"]  <-    
    df_harv_total_dmu86$antleredHarvest[df_harv_total_dmu86$dmu == "70A"] *
    dmu86_70a_e_correct

### antlerless

df_harv_total_dmu86$antlerless_harv_tot[df_harv_total_dmu86$dmu == "73"]  <- 
    df_harv_total_dmu86$antlerlessHarvest[df_harv_total_dmu86$dmu == "73"] *
    dmu86_73_w_correct

df_harv_total_dmu86$antlerless_harv_tot[df_harv_total_dmu86$dmu == "73C"]  <- 
    df_harv_total_dmu86$antlerlessHarvest[df_harv_total_dmu86$dmu == "73C"] *
    dmu86_73c_w_correct

df_harv_total_dmu86$antlerless_harv_tot[df_harv_total_dmu86$dmu == "70D"]  <- 
    df_harv_total_dmu86$antlerlessHarvest[df_harv_total_dmu86$dmu == "70D"] *
    dmu86_70d_e_correct

df_harv_total_dmu86$antlerless_harv_tot[df_harv_total_dmu86$dmu == "70C"]  <- 
    df_harv_total_dmu86$antlerlessHarvest[df_harv_total_dmu86$dmu == "70C"] *
    dmu86_70c_e_correct

df_harv_total_dmu86$antlerless_harv_tot[df_harv_total_dmu86$dmu == "70A"]  <-    
    df_harv_total_dmu86$antlerlessHarvest[df_harv_total_dmu86$dmu == "70A"] *
    dmu86_70a_e_correct

#setting east/west study area
df_harv_total_dmu86$study_area[df_harv_total_dmu86$dmu == "73"]  <- "west"
df_harv_total_dmu86$study_area[df_harv_total_dmu86$dmu == "73C"]  <- "west"
df_harv_total_dmu86$study_area[df_harv_total_dmu86$dmu == "70D"]  <- "east"
df_harv_total_dmu86$study_area[df_harv_total_dmu86$dmu == "70C"]  <- "east"
df_harv_total_dmu86$study_area[df_harv_total_dmu86$dmu == "70A"]  <- "east"

head(df_harv_total_dmu86[c(1,2,13,14,15)])



#####################################################################
###
### dmu99:1999-2001
###
### dmu99_73e_w_correct
### dmu99_70d_e_correct
### dmu99_70c_e_correct
### dmu99_70a_e_correct
###
#####################################################################

df_harv_total_dmu99 <- df_harv_total[df_harv_total$yr>1998 & df_harv_total$yr<2002,]
#replacing NAs w/0's for addition
df_harv_total_dmu99[is.na(df_harv_total_dmu99)] <- 0

df_harv_total_dmu99$antleredHarvest <- df_harv_total_dmu99$antleredgun + 
                                        df_harv_total_dmu99$antleredbow

df_harv_total_dmu99$antlerlessHarvest <- df_harv_total_dmu99$antlerlessgun +
                                         df_harv_total_dmu99$antlerlessbow
#there's 0 harvest depending on dmu
# df_harv_total_dmu99[df_harv_total_dmu99$antleredHarvest==0,] 
# plot_harv_total_dmu99_antlered <- ggplot(data=df_harv_total_dmu99)+
#     geom_point(aes(x=yr,y=antleredHarvest,color=dmu),size=3)+
#         geom_line(aes(x=yr,y=antleredHarvest,color=dmu))+
#     ggtitle("dmu antlered harvest 1999-2001")+
#     theme_bw()
# ggsave(plot_harv_total_dmu99_antlered,file="figures/plot_harv_total_dmu99_antlered.png")


# plot_harv_total_dmu99_antlerless <- ggplot(data=df_harv_total_dmu99)+
#     geom_point(aes(x=yr,y=antlerlessHarvest,color=dmu),size=3)+
#     geom_line(aes(x=yr,y=antlerlessHarvest,color=dmu))+
#     ggtitle("dmu antlerless harvest 1999-2001")+
#     theme_bw()
# plot_harv_total_dmu99_antlerless
# ggsave(plot_harv_total_dmu99_antlerless,file="figures/plot_harv_total_dmu99_antlerless.png")

### calibrate antlered harvest by dmu

df_harv_total_dmu99$antlered_harv_tot <- c()
df_harv_total_dmu99$antlerless_harv_tot <- c()
df_harv_total_dmu99$study_area <- c()

df_harv_total_dmu99$antlered_harv_tot[df_harv_total_dmu99$dmu == "73E"]  <-
    df_harv_total_dmu99$antleredHarvest[df_harv_total_dmu99$dmu == "73E"] *
    dmu99_73e_w_correct

df_harv_total_dmu99$antlered_harv_tot[df_harv_total_dmu99$dmu == "70D"]  <-
    df_harv_total_dmu99$antleredHarvest[df_harv_total_dmu99$dmu == "70D"] *
    dmu99_70d_e_correct

df_harv_total_dmu99$antlered_harv_tot[df_harv_total_dmu99$dmu == "70C"]  <-
    df_harv_total_dmu99$antleredHarvest[df_harv_total_dmu99$dmu == "70C"] *
    dmu99_70c_e_correct

df_harv_total_dmu99$antlered_harv_tot[df_harv_total_dmu99$dmu == "70A"]  <-
    df_harv_total_dmu99$antleredHarvest[df_harv_total_dmu99$dmu == "70A"] *
    dmu99_70a_e_correct

### calibrate antlerless harvest by dmu

df_harv_total_dmu99$antlerless_harv_tot[df_harv_total_dmu99$dmu == "73E"]  <-
    df_harv_total_dmu99$antlerlessHarvest[df_harv_total_dmu99$dmu == "73E"] *
    dmu99_73e_w_correct

df_harv_total_dmu99$antlerless_harv_tot[df_harv_total_dmu99$dmu == "70D"]  <-
    df_harv_total_dmu99$antlerlessHarvest[df_harv_total_dmu99$dmu == "70D"] *
    dmu99_70d_e_correct

df_harv_total_dmu99$antlerless_harv_tot[df_harv_total_dmu99$dmu == "70C"]  <-
    df_harv_total_dmu99$antlerlessHarvest[df_harv_total_dmu99$dmu == "70C"] *
    dmu99_70c_e_correct

df_harv_total_dmu99$antlerless_harv_tot[df_harv_total_dmu99$dmu == "70A"]  <-
    df_harv_total_dmu99$antlerlessHarvest[df_harv_total_dmu99$dmu == "70A"] *
    dmu99_70a_e_correct

### setting east/west study area

df_harv_total_dmu99$study_area[df_harv_total_dmu99$dmu == "73E"]  <- "west"
df_harv_total_dmu99$study_area[df_harv_total_dmu99$dmu == "70D"]  <- "east"
df_harv_total_dmu99$study_area[df_harv_total_dmu99$dmu == "70C"]  <- "east"
df_harv_total_dmu99$study_area[df_harv_total_dmu99$dmu == "70A"]  <- "east"

head(df_harv_total_dmu99[c(1,2,13,14,15)])

#####################################################################
### dmu13:2002-2013
###
### dmu13_73e_w_correct
### dmu13_70d_e_correct
### dmu13_70c_e_correct
### dmu13_70a_e_correct
###
#####################################################################

df_harv_total_dmu13 <- df_harv_total[df_harv_total$yr > 2001, ]

#replacing NAs w/0's for addition
df_harv_total_dmu13[is.na(df_harv_total_dmu13)] <- 0

df_harv_total_dmu13$antleredHarvest <- df_harv_total_dmu13$antleredgun +
                                        df_harv_total_dmu13$antleredbow

df_harv_total_dmu13$antlerlessHarvest <- df_harv_total_dmu13$antlerlessgun +
                                         df_harv_total_dmu13$antlerlessbow
#there's 0 harvest depending on dmu
# df_harv_total_dmu13[df_harv_total_dmu13$antleredHarvest==0,] 
# plot_harv_total_dmu13_antlered <- ggplot(data=df_harv_total_dmu13)+
#     geom_point(aes(x=yr,y=antleredHarvest,color=dmu),size=3)+
#         geom_line(aes(x=yr,y=antleredHarvest,color=dmu))+
#     ggtitle("dmu antlered harvest 2002-2013")+
#     theme_bw()
# ggsave(plot_harv_total_dmu13_antlered,file="figures/plot_harv_total_dmu13_antlered.png")


# plot_harv_total_dmu13_antlerless <- ggplot(data = df_harv_total_dmu13)+
#     geom_point(aes(x = yr,y = antlerlessHarvest, color = dmu),size=3)+
#     geom_line(aes(x = yr,y = antlerlessHarvest, color = dmu))+
#     ggtitle("dmu antlerless harvest 2002-2013")+
#     theme_bw()
# plot_harv_total_dmu13_antlerless
# ggsave(plot_harv_total_dmu13_antlerless, file = "figures/plot_harv_total_dmu13_antlerless.png")


### calibrate antlered harvest by dmu
df_harv_total_dmu13$dmu
df_harv_total_dmu13$antlered_harv_tot <- c()
df_harv_total_dmu13$antlerless_harv_tot <- c()
df_harv_total_dmu13$study_area <- c()

df_harv_total_dmu13$antlered_harv_tot[df_harv_total_dmu13$dmu == "73ECWD"]  <-
    df_harv_total_dmu13$antleredHarvest[df_harv_total_dmu13$dmu == "73ECWD"] *
    dmu13_73e_w_correct

df_harv_total_dmu13$antlered_harv_tot[df_harv_total_dmu13$dmu == "70DCWD"]  <-
    df_harv_total_dmu13$antleredHarvest[df_harv_total_dmu13$dmu == "70DCWD"] *
    dmu13_70d_e_correct

df_harv_total_dmu13$antlered_harv_tot[df_harv_total_dmu13$dmu == "70CCWD"]  <-
    df_harv_total_dmu13$antleredHarvest[df_harv_total_dmu13$dmu == "70CCWD"] *
    dmu13_70c_e_correct

df_harv_total_dmu13$antlered_harv_tot[df_harv_total_dmu13$dmu == "70ACWD"]  <-
    df_harv_total_dmu13$antleredHarvest[df_harv_total_dmu13$dmu == "70ACWD"] *
    dmu13_70a_e_correct

### calibrate antlerless harvest by dmu

df_harv_total_dmu13$antlerless_harv_tot[df_harv_total_dmu13$dmu == "73ECWD"]  <-
    df_harv_total_dmu13$antlerlessHarvest[df_harv_total_dmu13$dmu == "73ECWD"] *
    dmu13_73e_w_correct

df_harv_total_dmu13$antlerless_harv_tot[df_harv_total_dmu13$dmu == "70DCWD"]  <-
    df_harv_total_dmu13$antlerlessHarvest[df_harv_total_dmu13$dmu == "70DCWD"] *
    dmu13_70d_e_correct

df_harv_total_dmu13$antlerless_harv_tot[df_harv_total_dmu13$dmu == "70CCWD"]  <-
    df_harv_total_dmu13$antlerlessHarvest[df_harv_total_dmu13$dmu == "70CCWD"] *
    dmu13_70c_e_correct

df_harv_total_dmu13$antlerless_harv_tot[df_harv_total_dmu13$dmu == "70ACWD"]  <-
    df_harv_total_dmu13$antlerlessHarvest[df_harv_total_dmu13$dmu == "70ACWD"] *
    dmu13_70a_e_correct

### setting east/west study area

df_harv_total_dmu13$study_area[df_harv_total_dmu13$dmu == "73ECWD"]  <- "west"
df_harv_total_dmu13$study_area[df_harv_total_dmu13$dmu == "70DCWD"]  <- "east"
df_harv_total_dmu13$study_area[df_harv_total_dmu13$dmu == "70CCWD"]  <- "east"
df_harv_total_dmu13$study_area[df_harv_total_dmu13$dmu == "70ACWD"]  <- "east"

head(df_harv_total_dmu99[c(1,2,13,14,15)])

#####################################################################
### county:2014-2021
###
### grant_correct
### iowa_w_correct
### iowa_e_correct
### dane_e_correct
###
#####################################################################

df_harv_total_county$antleredHarvest <- df_harv_total_county$antleredgun + 
                                        df_harv_total_county$antleredbow +
                                        df_harv_total_county$antleredcrossbow

df_harv_total_county$antlerlessHarvest <- df_harv_total_county$antlerlessgun + 
                                          df_harv_total_county$antlerlessbow +
                                          df_harv_total_county$antlerlesscrossbow

# plot_harv_total_county_antlered <- ggplot(data=df_harv_total_county) +
#     geom_point(aes(x = yr, y = antleredHarvest, color = cty), size = 3) +
#     geom_line(aes(x = yr, y = antleredHarvest, color = cty)) +
#     ggtitle("county antlered harvest 2014-2021") +
#     theme_bw()
# plot_harv_total_county_antlered
# ggsave(plot_harv_total_county_antlered,file="figures/plot_harv_total_county_antlered.png")

# plot_harv_total_county_antlerless <- ggplot(data=df_harv_total_county) +
#     geom_point(aes(x=yr,y=antlerlessHarvest,color=cty),size=3) +
#     geom_line(aes(x=yr,y=antlerlessHarvest,color=cty)) +
#     ggtitle("county antlerless harvest 1999-2001")+
#     theme_bw()
# plot_harv_total_county_antlerless
# ggsave(plot_harv_total_county_antlerless,file="figures/plot_harv_total_county_antlerless.png")

### calibrate antlered harvest
### add in extra row for doubling iowa, to setup east/west iowa split

df_harv_total_county$antlered_harv_tot <- c()
df_harv_total_county$antlerless_harv_tot <- c()

df_harv_total_county$antlered_harv_tot[df_harv_total_county$cty == "Grant"]  <-
    df_harv_total_county$antleredHarvest[df_harv_total_county$cty == "Grant"] *
    grant_correct

df_harv_total_county$antlered_harv_tot[df_harv_total_county$cty == "Dane"]  <-
    df_harv_total_county$antleredHarvest[df_harv_total_county$cty == "Dane"] *
    dane_e_correct

df_harv_total_county$antlered_harv_tot[df_harv_total_county$cty == "Iowa" &
    df_harv_total_county$study_area == "west"]  <-
    df_harv_total_county$antleredHarvest[df_harv_total_county$cty == "Iowa" &
    df_harv_total_county$study_area == "west"] *
    iowa_w_correct

df_harv_total_county$antlered_harv_tot[df_harv_total_county$cty == "Iowa" &
    df_harv_total_county$study_area == "east"]  <-
    df_harv_total_county$antleredHarvest[df_harv_total_county$cty == "Iowa" &
    df_harv_total_county$study_area == "east"] *
    iowa_e_correct

### calibrate antlerless

df_harv_total_county$antlerless_harv_tot[df_harv_total_county$cty == "Grant"]  <-
    df_harv_total_county$antlerlessHarvest[df_harv_total_county$cty == "Grant"] *
    grant_correct

df_harv_total_county$antlerless_harv_tot[df_harv_total_county$cty == "Dane"]  <-
    df_harv_total_county$antlerlessHarvest[df_harv_total_county$cty == "Dane"] *
    dane_e_correct

df_harv_total_county$antlerless_harv_tot[df_harv_total_county$cty == "Iowa" &
    df_harv_total_county$study_area == "west"]  <-
    df_harv_total_county$antlerlessHarvest[df_harv_total_county$cty == "Iowa" &
    df_harv_total_county$study_area == "west"] *
    iowa_w_correct

df_harv_total_county$antlerless_harv_tot[df_harv_total_county$cty == "Iowa" &
    df_harv_total_county$study_area == "east"]  <-
    df_harv_total_county$antlerlessHarvest[df_harv_total_county$cty == "Iowa" &
    df_harv_total_county$study_area == "east"] *
    iowa_e_correct




#####################################################################
###
### aggregating calibrated harvest totals for east/west study area
###
#####################################################################

df_harv_overall_total <- data.frame(rbind(
    ### dmu86
    df_harv_total_dmu86 %>%
        group_by(yr,study_area) %>%
        summarise(antlered_harv_tot = sum(antlered_harv_tot),
        antlerless_harv_tot = sum(antlerless_harv_tot)
        ),
    ### dmu99
    df_harv_total_dmu99 %>%
        group_by(yr,study_area) %>%
        summarise(antlered_harv_tot = sum(antlered_harv_tot),
        antlerless_harv_tot = sum(antlerless_harv_tot)
        ),
    ### dmu13
    df_harv_total_dmu13 %>%
        group_by(yr,study_area) %>%
        summarise(antlered_harv_tot = sum(antlered_harv_tot),
        antlerless_harv_tot = sum(antlerless_harv_tot)
        ), 
    ### county
    df_harv_total_county %>%
        group_by(yr,study_area) %>%
        summarise(antlered_harv_tot = sum(antlered_harv_tot),
        antlerless_harv_tot = sum(antlerless_harv_tot)
        ) 
    )
)

# plot_cal_antlered_harvest <- ggplot(data=df_harv_overall_total) + 
#     geom_point(aes(x = yr, y = antlered_harv_tot), size = 3) +
#     geom_line(aes(x = yr, y = antlered_harv_tot)) +
#     ggtitle("Calibrated Antlered Total Harvest") +
#     xlab("Year")+
#     ylab("Total Harvest (# deer)") +
#     theme_bw() + facet_wrap(.~study_area) 

# # ggsave(plot_cal_antlered_harvest,file="figures/plot_cal_antlered_harvest.png")


# plot_cal_antlerless_harvest <- ggplot(data=df_harv_overall_total) + 
#     geom_point(aes(x = yr, y = antlerless_harv_tot), size = 3) +
#     geom_line(aes(x = yr, y = antlerless_harv_tot)) +
#     ggtitle("Calibrated Antlerless Total Harvest") +
#     xlab("Year")+
#     ylab("Total Harvest (# deer)") +
#     theme_bw() + facet_wrap(.~study_area) 

# # ggsave(plot_cal_antlerless_harvest,file="figures/plot_cal_antlerless_harvest.png")


#####################################################################
###
### Calibrating age-at-harvest (AAH) composition data  
### by DMU, and county, depending on the year
###
### dmu86:1983-1998 (will calibrate for all data prior to 1998)
### dmu99:1999-2001
### dmu13:2002-2013
### county-level:2013-2021 
###
#####################################################################

#####################################################################
###
### dmu86:1983:1998 (will calibrate for all data prior to 1998)
###
### dmu86_73_w_correct
### dmu86_73c_w_correct
### dmu86_70d_e_correct
### dmu86_70c_e_correct
### dmu86_70a_e_correct
###
#####################################################################


# constraining to later data, because not using earlier data
df_harv_aah <- df_harv_aah[df_harv_aah$yr > 1982, ]

df_harv_aah[is.na(df_harv_aah)] <- 0

df_harv_aah_dmu86 <- df_harv_aah[df_harv_aah$yr < 1999, ]

#setting east/west study area
df_harv_aah_dmu86$study_area <- c()
df_harv_aah_dmu86$study_area[df_harv_aah_dmu86$dmu == "73"]  <- "west"
df_harv_aah_dmu86$study_area[df_harv_aah_dmu86$dmu == "73C"]  <- "west"
df_harv_aah_dmu86$study_area[df_harv_aah_dmu86$dmu == "70D"]  <- "east"
df_harv_aah_dmu86$study_area[df_harv_aah_dmu86$dmu == "70C"]  <- "east"
df_harv_aah_dmu86$study_area[df_harv_aah_dmu86$dmu == "70A"]  <- "east"

df_harv_aah_dmu86[df_harv_aah_dmu86$dmu=="73",3:(ncol(df_harv_aah_dmu86)-1)] <- 
    df_harv_aah_dmu86[df_harv_aah_dmu86$dmu=="73",3:(ncol(df_harv_aah_dmu86)-1)] *
    dmu86_73_w_correct

df_harv_aah_dmu86[df_harv_aah_dmu86$dmu=="73C",3:(ncol(df_harv_aah_dmu86)-1)] <- 
    df_harv_aah_dmu86[df_harv_aah_dmu86$dmu=="73C",3:(ncol(df_harv_aah_dmu86)-1)] *
    dmu86_73c_w_correct

df_harv_aah_dmu86[df_harv_aah_dmu86$dmu=="70D",3:(ncol(df_harv_aah_dmu86)-1)] <- 
    df_harv_aah_dmu86[df_harv_aah_dmu86$dmu=="70D",3:(ncol(df_harv_aah_dmu86)-1)] *
    dmu86_70d_e_correct

df_harv_aah_dmu86[df_harv_aah_dmu86$dmu=="70C",3:(ncol(df_harv_aah_dmu86)-1)] <- 
    df_harv_aah_dmu86[df_harv_aah_dmu86$dmu=="70C",3:(ncol(df_harv_aah_dmu86)-1)] *
    dmu86_70c_e_correct

df_harv_aah_dmu86[df_harv_aah_dmu86$dmu=="70A",3:(ncol(df_harv_aah_dmu86)-1)] <- 
    df_harv_aah_dmu86[df_harv_aah_dmu86$dmu=="70A",3:(ncol(df_harv_aah_dmu86)-1)] *
    dmu86_70a_e_correct

#####################################################################
###
### dmu99:1999-2001
###
### dmu99_73e_w_correct
### dmu99_70d_e_correct
### dmu99_70c_e_correct
### dmu99_70a_e_correct
###
#####################################################################

df_harv_aah_dmu99 <- df_harv_aah[df_harv_aah$yr > 1998 & df_harv_aah$yr < 2002,]

#setting east/west study area
df_harv_aah_dmu99$study_area <- c()
df_harv_aah_dmu99$study_area[df_harv_aah_dmu99$dmu == "73E"]  <- "west"
df_harv_aah_dmu99$study_area[df_harv_aah_dmu99$dmu == "70D"]  <- "east"
df_harv_aah_dmu99$study_area[df_harv_aah_dmu99$dmu == "70C"]  <- "east"
df_harv_aah_dmu99$study_area[df_harv_aah_dmu99$dmu == "70A"]  <- "east"

df_harv_aah_dmu99[df_harv_aah_dmu99$dmu=="73C",3:(ncol(df_harv_aah_dmu99)-1)] <- 
    df_harv_aah_dmu99[df_harv_aah_dmu99$dmu=="73C",3:(ncol(df_harv_aah_dmu99)-1)] *
    dmu99_73e_w_correct

df_harv_aah_dmu99[df_harv_aah_dmu99$dmu=="70D",3:(ncol(df_harv_aah_dmu99)-1)] <- 
    df_harv_aah_dmu99[df_harv_aah_dmu99$dmu=="70D",3:(ncol(df_harv_aah_dmu99)-1)] *
    dmu99_70d_e_correct

df_harv_aah_dmu99[df_harv_aah_dmu99$dmu=="70C",3:(ncol(df_harv_aah_dmu99)-1)] <- 
    df_harv_aah_dmu99[df_harv_aah_dmu99$dmu=="70C",3:(ncol(df_harv_aah_dmu99)-1)] *
    dmu99_70c_e_correct

df_harv_aah_dmu99[df_harv_aah_dmu99$dmu=="70A",3:(ncol(df_harv_aah_dmu99)-1)] <- 
    df_harv_aah_dmu99[df_harv_aah_dmu99$dmu=="70A",3:(ncol(df_harv_aah_dmu99)-1)] *
    dmu99_70a_e_correct


#####################################################################
###
### dmu13:2002-2013
###
### dmu13_73e_w_correct
### dmu13_70d_e_correct
### dmu13_70c_e_correct
### dmu13_70a_e_correct
###
#####################################################################

df_harv_aah_dmu13 <- df_harv_aah[df_harv_aah$yr > 2001, ]

#setting east/west study area
df_harv_aah_dmu13$study_area <- c()
df_harv_aah_dmu13$study_area[df_harv_aah_dmu13$dmu == "73ECWD"]  <- "west"
df_harv_aah_dmu13$study_area[df_harv_aah_dmu13$dmu == "70DCWD"]  <- "east"
df_harv_aah_dmu13$study_area[df_harv_aah_dmu13$dmu == "70CCWD"]  <- "east"
df_harv_aah_dmu13$study_area[df_harv_aah_dmu13$dmu == "70ACWD"]  <- "east"

df_harv_aah_dmu13[df_harv_aah_dmu13$dmu=="73CCWD",3:(ncol(df_harv_aah_dmu13)-1)] <- 
    df_harv_aah_dmu13[df_harv_aah_dmu13$dmu=="73CCWD",3:(ncol(df_harv_aah_dmu13)-1)] *
    dmu13_73e_w_correct

df_harv_aah_dmu13[df_harv_aah_dmu13$dmu=="70DCWD",3:(ncol(df_harv_aah_dmu13)-1)] <- 
    df_harv_aah_dmu13[df_harv_aah_dmu13$dmu=="70DCWD",3:(ncol(df_harv_aah_dmu13)-1)] *
    dmu13_70d_e_correct

df_harv_aah_dmu13[df_harv_aah_dmu13$dmu=="70CCWD",3:(ncol(df_harv_aah_dmu13)-1)] <- 
    df_harv_aah_dmu13[df_harv_aah_dmu13$dmu=="70CCWD",3:(ncol(df_harv_aah_dmu13)-1)] *
    dmu13_70c_e_correct

df_harv_aah_dmu13[df_harv_aah_dmu13$dmu=="70ACWD",3:(ncol(df_harv_aah_dmu13)-1)] <- 
    df_harv_aah_dmu13[df_harv_aah_dmu13$dmu=="70ACWD",3:(ncol(df_harv_aah_dmu13)-1)] *
    dmu13_70a_e_correct



#####################################################################
###
### Calibration of AAH non-cwd-tested county data:2014-2021
###
### grant_correct
### iowa_w_correct
### iowa_e_correct
### dane_e_correct
###
#####################################################################

# iowa_e_correct_aah <- iowa_e_correct/(iowa_w_correct +iowa_e_correct)
# iowa_w_correct_aah <- iowa_w_correct/(iowa_w_correct +iowa_e_correct)

df_aah_county <-  data.frame(read_excel(paste0(filepath,"AgingDaneIowaGrant_2014-2021.xlsx"),1))
names(df_aah_county) <- tolower(gsub("[[:punct:]]","",names(df_aah_county)))

#removing NAs for summation
df_aah_county[is.na(df_aah_county)] <- 0

### study area
df_aah_county$study_area <- c()
df_aah_county$study_area[df_aah_county$cty=="Dane"] <- "east"
df_aah_county$study_area[df_aah_county$cty=="Grant"] <- "west"
df_aah_county$study_area[df_aah_county$cty=="Iowa"] <- "east"
df_aah_county <- rbind(df_aah_county,
      cbind(df_aah_county[df_aah_county$cty == "Iowa",1:22],
      study_area=rep("west",sum(df_aah_county$cty == "Iowa")))
      )

df_aah_county[df_aah_county$cty == "Dane" &
    df_aah_county$study_area == "west",3:22]  <-
    df_aah_county[df_aah_county$cty == "Dane" &
    df_aah_county$study_area == "west",3:22] *
    dane_e_correct

df_aah_county[df_aah_county$cty == "Grant" &
    df_aah_county$study_area == "west",3:22]  <-
    df_aah_county[df_aah_county$cty == "Grant" &
    df_aah_county$study_area == "west",3:22] *
    grant_correct


df_aah_county[df_aah_county$cty == "Iowa" &
    df_aah_county$study_area == "west",3:22]  <-
    df_aah_county[df_aah_county$cty == "Iowa" &
    df_aah_county$study_area == "west",3:22] *
    iowa_w_correct

df_aah_county[df_aah_county$cty == "Iowa" &
    df_aah_county$study_area == "east",3:22]  <-
    df_aah_county[df_aah_county$cty == "Iowa" &
    df_aah_county$study_area == "east",3:22] *
    iowa_e_correct

###################################################################################
###
### Aggregate AAH data by study area
###
###################################################################################

df_aah_dmu <- data.frame(rbind(
    ### dmu86
    df_harv_aah_dmu86 %>%
        group_by(yr,study_area) %>%
        summarise_if(is.numeric,.funs=sum
        ),
    ### dmu99
    df_harv_aah_dmu99 %>%
        group_by(yr,study_area) %>%
        summarise_if(is.numeric,.funs=sum
        ),
    ### dmu13
    df_harv_aah_dmu13 %>%
        group_by(yr,study_area) %>%
        summarise_if(is.numeric,.funs=sum
        )
    )
)



###################################################################################
###
### Aggregate AAH data no cwd testing, into age classes
###
###################################################################################


df_aah_county_notest <- df_aah_county %>% group_by(study_area,yr) %>% summarise(mfawn = sum(mfawn),
                                      m1 = sum(m1forked,
                                               m1sublegal,
                                               m1legalspike,
                                               m1unknown,
                                               na.rm=TRUE),
                                      m2 = sum(m2,na.rm=TRUE),
                                      m3 = sum(m3,na.rm=TRUE),
                                      m4 = sum(m45,na.rm=TRUE),
                                      m6 = sum(m68,m911,na.rm=TRUE),
                                      ffawn = sum(ffawn),
                                      f1 = sum(f1,na.rm=TRUE),
                                      f2 = sum(f2,na.rm=TRUE),
                                      f3 = sum(f3,na.rm=TRUE),
                                      f4 = sum(f45,na.rm=TRUE),
                                      f6 = sum(f68,na.rm=TRUE),
                                      f9 = sum(f911,f12,na.rm=TRUE))

df_aah_dmu_notest <- df_aah_dmu %>% group_by(study_area,yr) %>% summarise(mfawn = sum(mfawn),
                                      m1 = sum(m1forked,
                                               m1sublegal,
                                               m1legalspike,
                                               na.rm=TRUE),
                                      m2 = sum(m2,na.rm=TRUE),
                                      m3 = sum(m3,na.rm=TRUE),
                                      m4 = sum(m45,na.rm=TRUE),
                                      m6 = sum(m68,m912,na.rm=TRUE),
                                      ffawn = sum(ffawn),
                                      f1 = sum(f1,na.rm=TRUE),
                                      f2 = sum(f2,na.rm=TRUE),
                                      f3 = sum(f3,na.rm=TRUE),
                                      f4 = sum(f45,na.rm=TRUE),
                                      f6 = sum(f68,na.rm=TRUE),
                                      f9 = sum(f912,f12,na.rm=TRUE))

df_aah_notest <- rbind(df_aah_dmu_notest, df_aah_county_notest)


###################################################################################
###
### AAH data including disease status (comes from the surveillance dataset)
###
###################################################################################


df_aah_notest <- df_aah_notest %>% pivot_longer(cols = -c(yr,study_area))
names(df_aah_notest) <- c("study_area","year","age","n")
df_aah_notest$sex <- as.factor(substr(df_aah_notest$age,1,1))
levels(df_aah_notest$sex) <- c("Female","Male")
df_aah_notest$sex <- as.character(df_aah_notest$sex)
df_aah_notest$age <- as.factor(df_aah_notest$age)
levels(df_aah_notest$age) <- c(1,2,3,4,6,9,0,1,2,3,4,6,0)
df_aah_notest$age <- as.numeric(as.character(df_aah_notest$age))

df_age_cwdpos <- cwd_df %>%filter(teststatus==1)%>% group_by(ew,year,sex,age) %>% summarise(n=n())
df_age_cwdneg <- cwd_df %>%filter(teststatus==0)%>% group_by(ew,year,sex,age) %>% summarise(n=n())

df_age_cwdpos$sex <- as.factor(df_age_cwdpos$sex)
levels(df_age_cwdpos$sex) <- c("Male","Female")
df_age_cwdpos

df_age_cwdneg$sex <- as.factor(df_age_cwdneg$sex)
levels(df_age_cwdneg$sex) <- c("Male","Female")
df_age_cwdneg

names(df_age_cwdneg)[1] <- "study_area"
names(df_age_cwdpos)[1] <- "study_area"

###################################################################
###
### incorporating the 0 count levels for the missing age/sex combos
### i.e. no counts of oldest males in some years, so including those
### levels and adding those rows to the end of the sus and inf data frames
###
###################################################################

df_age_inf <- df_age_cwdpos
df_age_sus <- df_age_cwdneg

df_age_sus$sex <- as.character(df_age_sus$sex)
df_age_inf$sex <- as.character(df_age_inf$sex)

df_age_sus$age <- as.numeric(as.character(df_age_sus$age))
df_age_inf$age <- as.numeric(as.character(df_age_inf$age))

##########################
### east study area
##########################


# fixn9m <- length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "east" & df_age_sus$sex=="Male" & df_age_sus$age==9])))
# fixn6m <- length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "east" & df_age_sus$sex=="Male" & df_age_sus$age==6])))
# fixn4m <- length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "east" & df_age_sus$sex=="Male" & df_age_sus$age==4])))
# fixn3m <- length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "east" & df_age_sus$sex=="Male" & df_age_sus$age==3])))
# fixn2m <- length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "east" & df_age_sus$sex=="Male" & df_age_sus$age==2])))
# fixn1m <- length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "east" & df_age_sus$sex=="Male" & df_age_sus$age==1])))
# fixn0m <- length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "east" & df_age_sus$sex=="Male" & df_age_sus$age==0])))

# fixn9m
# fixn6m
# fixn4m
# fixn3m
# fixn2m
# fixn1m
# fixn0m

fixn9m <-length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "east" &
                                                      df_age_sus$sex == "Male" &
                                                      df_age_sus$age == 9])))
df_age_sus <-rbind(df_age_sus,
                   data.frame(study_area = rep("east", fixn9m),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area=="east" &
                                                                                         df_age_sus$sex=="Male" &
                                                                                         df_age_sus$age==9]))],
                              sex = rep("Male", fixn9m),
                              age = rep(9, fixn9m),
                             n = rep(0, fixn9m)))

fixn6m <-length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "east" &
                                                      df_age_sus$sex == "Male" &
                                                      df_age_sus$age == 6])))

df_age_sus <-rbind(df_age_sus,
                   data.frame(study_area = rep("east", fixn6m),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "east" &
                                                                                         df_age_sus$sex == "Male" &
                                                                                         df_age_sus$age == 6]))],
                              sex = rep("Male", fixn6m),
                              age = rep(6, fixn6m),
                             n = rep(0, fixn6m)))

fixn9f <-length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "east" & df_age_sus$sex=="Female" & df_age_sus$age==9])))
# fixn6f <-length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "east" & df_age_sus$sex=="Female" & df_age_sus$age==6])))
# fixn4f <-length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "east" & df_age_sus$sex=="Female" & df_age_sus$age==4])))
# fixn3f <-length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "east" & df_age_sus$sex=="Female" & df_age_sus$age==3])))
# fixn2f <-length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "east" & df_age_sus$sex=="Female" & df_age_sus$age==2])))
# fixn1f <-length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "east" & df_age_sus$sex=="Female" & df_age_sus$age==1])))
# fixn0f <-length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "east" & df_age_sus$sex=="Female" & df_age_sus$age==0])))

# fixn9f
# fixn6f
# fixn4f
# fixn3f
# fixn2f
# fixn1f
# fixn0f

df_age_sus <-rbind(df_age_sus,
                   data.frame(study_area = rep("east", fixn9f),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area=="east" &
                                                                                         df_age_sus$sex=="Female" &
                                                                                         df_age_sus$age==9]))],
                              sex = rep("Female", fixn9f),
                              age = rep(9, fixn9f),
                             n = rep(0, fixn9f)))


# fixp9m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" & df_age_inf$sex=="Male" & df_age_inf$age==9])))
# fixp6m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" & df_age_inf$sex=="Male" & df_age_inf$age==6])))
# fixp4m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" & df_age_inf$sex=="Male" & df_age_inf$age==4])))
# fixp3m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" & df_age_inf$sex=="Male" & df_age_inf$age==3])))
# fixp2m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" & df_age_inf$sex=="Male" & df_age_inf$age==2])))
# fixp1m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" & df_age_inf$sex=="Male" & df_age_inf$age==1])))
# fixp0m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" & df_age_inf$sex=="Male" & df_age_inf$age==0])))

# fixp9m
# fixp6m
# fixp4m
# fixp3m
# fixp2m
# fixp1m
# fixp0m


fixp9m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" &
                                                        df_age_inf$sex == "Male" &
                                                        df_age_inf$age == 9])))
fixp6m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" &
                                                        df_age_inf$sex == "Male" &
                                                        df_age_inf$age == 6])))
fixp0m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" &
                                                        df_age_inf$sex == "Male" &
                                                        df_age_inf$age == 0])))

df_age_inf <-rbind(df_age_inf,data.frame(study_area = rep("east",fixp9m),
                                         year = c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" &
                                                                                                  df_age_inf$sex == "Male" &
                                                                                                  df_age_inf$age == 9]))],
                                         sex = rep("Male", fixp9m),
                                         age = rep(9, fixp9m),
                                         n = rep(0, fixp9m)))
df_age_inf <-rbind(df_age_inf,
                   data.frame(study_area = rep("east", fixp6m),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" &
                                                                                         df_age_inf$sex == "Male" &
                                                                                         df_age_inf$age == 6]))],
                              sex = rep("Male", fixp6m),
                              age = rep(6, fixp6m),
                              n = rep(0, fixp6m)))
df_age_inf <-rbind(df_age_inf,
                   data.frame(study_area = rep("east", fixp0m),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" &
                                                                                         df_age_inf$sex == "Male" &
                                                                                         df_age_inf$age == 0]))],
                              sex = rep("Male", fixp0m),
                              age = rep(0, fixp0m),
                              n = rep(0, fixp0m)))


# fixp9f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" & df_age_inf$sex=="Female" & df_age_inf$age==9])))
# fixp6f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" & df_age_inf$sex=="Female" & df_age_inf$age==6])))
# fixp4f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" & df_age_inf$sex=="Female" & df_age_inf$age==4])))
# fixp3f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" & df_age_inf$sex=="Female" & df_age_inf$age==3])))
# fixp2f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" & df_age_inf$sex=="Female" & df_age_inf$age==2])))
# fixp1f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" & df_age_inf$sex=="Female" & df_age_inf$age==1])))
# fixp0f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" & df_age_inf$sex=="Female" & df_age_inf$age==0])))

# fixp9f
# fixp6f
# fixp4f
# fixp3f
# fixp2f
# fixp1f
# fixp0f

fixp9f <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" &
                                                        df_age_inf$sex == "Female" &
                                                        df_age_inf$age == 9])))
fixp6f <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" &
                                                        df_age_inf$sex == "Female" &
                                                        df_age_inf$age == 6])))
fixp0f <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" &
                                                        df_age_inf$sex == "Female" &
                                                        df_age_inf$age == 0])))

df_age_inf <-rbind(df_age_inf,
                   data.frame(study_area = rep("east",fixp9f),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" & 
                                                                                         df_age_inf$sex =="Female" &
                                                                                         df_age_inf$age == 9]))],
                              sex = rep("Female", fixp9f),
                              age = rep(9, fixp9f),
                              n = rep(0, fixp9f)))
df_age_inf <-rbind(df_age_inf,
                   data.frame(study_area = rep("east",fixp6f),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" &
                                                                                         df_age_inf$sex == "Female" &
                                                                                         df_age_inf$age == 6]))],
                              sex = rep("Female", fixp6f),
                              age = rep(6, fixp6f),
                              n = rep(0, fixp6f)))
df_age_inf <-rbind(df_age_inf,
                   data.frame(study_area = rep("east", fixp0f),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "east" &
                                                                                         df_age_inf$sex == "Female" &
                                                                                         df_age_inf$age == 0]))],
                              sex = rep("Female", fixp0f),
                              age = rep(0, fixp6f),
                              n = rep(0, fixp0f)))

##########################
### west study area
##########################


# fixn9m <- length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "west" & df_age_sus$sex=="Male" & df_age_sus$age==9])))
# fixn6m <- length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "west" & df_age_sus$sex=="Male" & df_age_sus$age==6])))
# fixn4m <- length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "west" & df_age_sus$sex=="Male" & df_age_sus$age==4])))
# fixn3m <- length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "west" & df_age_sus$sex=="Male" & df_age_sus$age==3])))
# fixn2m <- length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "west" & df_age_sus$sex=="Male" & df_age_sus$age==2])))
# fixn1m <- length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "west" & df_age_sus$sex=="Male" & df_age_sus$age==1])))
# fixn0m <- length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "west" & df_age_sus$sex=="Male" & df_age_sus$age==0])))

# fixn9m
# fixn6m
# fixn4m
# fixn3m
# fixn2m
# fixn1m
# fixn0m

fixn9m <-length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "west" &
                                                      df_age_sus$sex == "Male" &
                                                      df_age_sus$age == 9])))
df_age_sus <-rbind(df_age_sus,
                   data.frame(study_area = rep("west", fixn9m),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area=="west" &
                                                                                         df_age_sus$sex=="Male" &
                                                                                         df_age_sus$age==9]))],
                              sex = rep("Male", fixn9m),
                              age = rep(9, fixn9m),
                             n = rep(0, fixn9m)))

fixn6m <-length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "west" &
                                                      df_age_sus$sex == "Male" &
                                                      df_age_sus$age == 6])))

df_age_sus <-rbind(df_age_sus,
                   data.frame(study_area = rep("west", fixn6m),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "west" &
                                                                                         df_age_sus$sex == "Male" &
                                                                                         df_age_sus$age == 6]))],
                              sex = rep("Male", fixn6m),
                              age = rep(6, fixn6m),
                             n = rep(0, fixn6m)))

fixn9f <-length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "west" & df_age_sus$sex=="Female" & df_age_sus$age==9])))
# fixn6f <-length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "west" & df_age_sus$sex=="Female" & df_age_sus$age==6])))
# fixn4f <-length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "west" & df_age_sus$sex=="Female" & df_age_sus$age==4])))
# fixn3f <-length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "west" & df_age_sus$sex=="Female" & df_age_sus$age==3])))
# fixn2f <-length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "west" & df_age_sus$sex=="Female" & df_age_sus$age==2])))
# fixn1f <-length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "west" & df_age_sus$sex=="Female" & df_age_sus$age==1])))
# fixn0f <-length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area == "west" & df_age_sus$sex=="Female" & df_age_sus$age==0])))

# fixn9f
# fixn6f
# fixn4f
# fixn3f
# fixn2f
# fixn1f
# fixn0f

df_age_sus <-rbind(df_age_sus,
                   data.frame(study_area = rep("west", fixn9f),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_sus$year[df_age_sus$study_area=="west" &
                                                                                         df_age_sus$sex=="Female" &
                                                                                         df_age_sus$age==9]))],
                              sex = rep("Female", fixn9f),
                              age = rep(9, fixn9f),
                             n = rep(0, fixn9f)))


# fixp9m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" & df_age_inf$sex=="Male" & df_age_inf$age==9])))
# fixp6m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" & df_age_inf$sex=="Male" & df_age_inf$age==6])))
# fixp4m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" & df_age_inf$sex=="Male" & df_age_inf$age==4])))
# fixp3m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" & df_age_inf$sex=="Male" & df_age_inf$age==3])))
# fixp2m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" & df_age_inf$sex=="Male" & df_age_inf$age==2])))
# fixp1m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" & df_age_inf$sex=="Male" & df_age_inf$age==1])))
# fixp0m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" & df_age_inf$sex=="Male" & df_age_inf$age==0])))

# fixp9m
# fixp6m
# fixp4m
# fixp3m
# fixp2m
# fixp1m
# fixp0m


fixp9m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                        df_age_inf$sex == "Male" &
                                                        df_age_inf$age == 9])))
fixp6m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                        df_age_inf$sex == "Male" &
                                                        df_age_inf$age == 6])))
fixp4m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                        df_age_inf$sex == "Male" &
                                                        df_age_inf$age == 4])))
fixp3m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                        df_age_inf$sex == "Male" &
                                                        df_age_inf$age == 3])))
fixp2m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                        df_age_inf$sex == "Male" &
                                                        df_age_inf$age == 2])))
fixp1m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                        df_age_inf$sex == "Male" &
                                                        df_age_inf$age == 1])))                                                                                                                       
fixp0m <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                        df_age_inf$sex == "Male" &
                                                        df_age_inf$age == 0])))

df_age_inf <-rbind(df_age_inf,data.frame(study_area = rep("west",fixp9m),
                                         year = c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                                                                  df_age_inf$sex == "Male" &
                                                                                                  df_age_inf$age == 9]))],
                                         sex = rep("Male", fixp9m),
                                         age = rep(9, fixp9m),
                                         n = rep(0, fixp9m)))
df_age_inf <-rbind(df_age_inf,
                   data.frame(study_area = rep("west", fixp6m),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                                                         df_age_inf$sex == "Male" &
                                                                                         df_age_inf$age == 6]))],
                              sex = rep("Male", fixp6m),
                              age = rep(6, fixp6m),
                              n = rep(0, fixp6m)))
df_age_inf <-rbind(df_age_inf,
                   data.frame(study_area = rep("west", fixp4m),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                                                         df_age_inf$sex == "Male" &
                                                                                         df_age_inf$age == 4]))],
                              sex = rep("Male", fixp4m),
                              age = rep(4, fixp4m),
                              n = rep(0, fixp4m)))
df_age_inf <-rbind(df_age_inf,
                   data.frame(study_area = rep("west", fixp3m),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                                                         df_age_inf$sex == "Male" &
                                                                                         df_age_inf$age == 3]))],
                              sex = rep("Male", fixp3m),
                              age = rep(3, fixp3m),
                              n = rep(0, fixp3m)))
df_age_inf <-rbind(df_age_inf,
                   data.frame(study_area = rep("west", fixp2m),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                                                         df_age_inf$sex == "Male" &
                                                                                         df_age_inf$age == 2]))],
                              sex = rep("Male", fixp2m),
                              age = rep(2, fixp2m),
                              n = rep(0, fixp2m)))
df_age_inf <-rbind(df_age_inf,
                   data.frame(study_area = rep("west", fixp1m),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                                                         df_age_inf$sex == "Male" &
                                                                                         df_age_inf$age == 1]))],
                              sex = rep("Male", fixp1m),
                              age = rep(1, fixp1m),
                              n = rep(0, fixp1m)))
df_age_inf <-rbind(df_age_inf,
                   data.frame(study_area = rep("west", fixp0m),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                                                         df_age_inf$sex == "Male" &
                                                                                         df_age_inf$age == 0]))],
                              sex = rep("Male", fixp0m),
                              age = rep(0, fixp0m),
                              n = rep(0, fixp0m)))


# fixp9f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" & df_age_inf$sex=="Female" & df_age_inf$age==9])))
# fixp6f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" & df_age_inf$sex=="Female" & df_age_inf$age==6])))
# fixp4f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" & df_age_inf$sex=="Female" & df_age_inf$age==4])))
# fixp3f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" & df_age_inf$sex=="Female" & df_age_inf$age==3])))
# fixp2f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" & df_age_inf$sex=="Female" & df_age_inf$age==2])))
# fixp1f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" & df_age_inf$sex=="Female" & df_age_inf$age==1])))
# fixp0f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" & df_age_inf$sex=="Female" & df_age_inf$age==0])))

# fixp9f
# fixp6f
# fixp4f
# fixp3f
# fixp2f
# fixp1f
# fixp0f

fixp9f <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                        df_age_inf$sex == "Female" &
                                                        df_age_inf$age == 9])))
fixp6f <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                        df_age_inf$sex == "Female" &
                                                        df_age_inf$age == 6])))
fixp4f <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                        df_age_inf$sex == "Female" &
                                                        df_age_inf$age == 4])))
fixp3f <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                        df_age_inf$sex == "Female" &
                                                        df_age_inf$age == 3])))
fixp2f <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                        df_age_inf$sex == "Female" &
                                                        df_age_inf$age == 2])))
fixp1f <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                        df_age_inf$sex == "Female" &
                                                        df_age_inf$age == 1])))
fixp0f <- length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                        df_age_inf$sex == "Female" &
                                                        df_age_inf$age == 0])))

df_age_inf <-rbind(df_age_inf,
                   data.frame(study_area = rep("west",fixp9f),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" & 
                                                                                         df_age_inf$sex =="Female" &
                                                                                         df_age_inf$age == 9]))],
                              sex = rep("Female", fixp9f),
                              age = rep(9, fixp9f),
                              n = rep(0, fixp9f)))
df_age_inf <-rbind(df_age_inf,
                   data.frame(study_area = rep("west",fixp6f),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                                                         df_age_inf$sex == "Female" &
                                                                                         df_age_inf$age == 6]))],
                              sex = rep("Female", fixp6f),
                              age = rep(6, fixp6f),
                              n = rep(0, fixp6f)))
df_age_inf <-rbind(df_age_inf,
                   data.frame(study_area = rep("west", fixp4f),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                                                         df_age_inf$sex == "Female" &
                                                                                         df_age_inf$age == 4]))],
                              sex = rep("Female", fixp4f),
                              age = rep(4, fixp4f),
                              n = rep(0, fixp4f)))
df_age_inf <-rbind(df_age_inf,
                   data.frame(study_area = rep("west", fixp3f),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                                                         df_age_inf$sex == "Female" &
                                                                                         df_age_inf$age == 3]))],
                              sex = rep("Female", fixp3f),
                              age = rep(3, fixp3f),
                              n = rep(0, fixp3f)))
df_age_inf <-rbind(df_age_inf,
                   data.frame(study_area = rep("west", fixp2f),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                                                         df_age_inf$sex == "Female" &
                                                                                         df_age_inf$age == 2]))],
                              sex = rep("Female", fixp2f),
                              age = rep(2, fixp2f),
                              n = rep(0, fixp2f)))
df_age_inf <-rbind(df_age_inf,
                   data.frame(study_area = rep("west", fixp1f),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                                                         df_age_inf$sex == "Female" &
                                                                                         df_age_inf$age == 1]))],
                              sex = rep("Female", fixp1f),
                              age = rep(1, fixp1f),
                              n = rep(0, fixp1f)))
df_age_inf <-rbind(df_age_inf,
                   data.frame(study_area = rep("west", fixp0f),
                              year = c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$study_area == "west" &
                                                                                         df_age_inf$sex == "Female" &
                                                                                         df_age_inf$age == 0]))],
                              sex = rep("Female", fixp0f),
                              age = rep(0, fixp0f),
                              n = rep(0, fixp0f)))



#combining based on known CWD status
df_age_inf <- arrange(df_age_inf,study_area,year,sex,age)
df_age_sus <- arrange(df_age_sus,study_area,year,sex,age)


##################################################################
###
### incorporating the 0 count levels for the missing age/sex combos
### i.e. no counts of oldest males in some years, so including those
### levels and adding those rows to the end of the aah df w/o cwdstatus
###
###################################################################

### to check corrections run all of the following... 
### only needed to correct fixp9m b/c that's the only one >0
fixp9m <- length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "east" & df_aah_notest$sex=="Male" & df_aah_notest$age==9])))
# fixp6m <- length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "east" & df_aah_notest$sex=="Male" & df_aah_notest$age==6])))
# fixp4m <- length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "east" & df_aah_notest$sex=="Male" & df_aah_notest$age==4])))
# fixp3m <- length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "east" & df_aah_notest$sex=="Male" & df_aah_notest$age==3])))
# fixp2m <- length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "east" & df_aah_notest$sex=="Male" & df_aah_notest$age==2])))
# fixp1m <- length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "east" & df_aah_notest$sex=="Male" & df_aah_notest$age==1])))
# fixp0m <- length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "east" & df_aah_notest$sex=="Male" & df_aah_notest$age==0])))

df_aah_notest <-rbind(df_aah_notest,
                   data.frame(study_area = rep("east", fixp9m),
                              year = c(1983:2021)[which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "east" &
                                                                                         df_aah_notest$sex == "Male" &
                                                                                         df_aah_notest$age == 9]))],
                              sex = rep("Male", fixp9m),
                              age = rep(9, fixp9m),
                             n = rep(0, fixp9m)))

# fixp9f <-length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "east" & df_aah_notest$sex=="Female" & df_aah_notest$age==9])))
# fixp6f <-length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "east" & df_aah_notest$sex=="Female" & df_aah_notest$age==6])))
# fixp4f <-length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "east" & df_aah_notest$sex=="Female" & df_aah_notest$age==4])))
# fixp3f <-length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "east" & df_aah_notest$sex=="Female" & df_aah_notest$age==3])))
# fixp2f <-length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "east" & df_aah_notest$sex=="Female" & df_aah_notest$age==2])))
# fixp1f <-length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "east" & df_aah_notest$sex=="Female" & df_aah_notest$age==1])))
# fixp0f <-length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "east" & df_aah_notest$sex=="Female" & df_aah_notest$age==0])))

# fixp9f
# fixp6f
# fixp4f
# fixp3f
# fixp2f
# fixp1f
# fixp0f

fixp9m <- length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "west" & df_aah_notest$sex=="Male" & df_aah_notest$age==9])))
fixp6m <- length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "west" & df_aah_notest$sex=="Male" & df_aah_notest$age==6])))
fixp4m <- length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "west" & df_aah_notest$sex=="Male" & df_aah_notest$age==4])))
fixp3m <- length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "west" & df_aah_notest$sex=="Male" & df_aah_notest$age==3])))
fixp2m <- length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "west" & df_aah_notest$sex=="Male" & df_aah_notest$age==2])))
fixp1m <- length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "west" & df_aah_notest$sex=="Male" & df_aah_notest$age==1])))
fixp0m <- length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "west" & df_aah_notest$sex=="Male" & df_aah_notest$age==0])))

df_aah_notest <-rbind(df_aah_notest,
                   data.frame(study_area = rep("west", fixp9m),
                              year = 1983:2021,
                              sex = rep("Male", fixp9m),
                              age = rep(9, fixp9m),
                             n = rep(0, fixp9m)))

#### these are all equal, so implementing expansion in a for loop
# fixp6m
# fixp4m
# fixp3m
# fixp2m
# fixp1m
# fixp0m

for(i in c(0,1,2,3,4,6)){
    df_aah_notest <-rbind(df_aah_notest,
                   data.frame(study_area = rep("west", fixp6m),
                              year = c(1983:2021)[which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "west" &
                                                                                         df_aah_notest$sex == "Male" &
                                                                                         df_aah_notest$age == i]))],
                              sex = rep("Male", fixp6m),
                              age = rep(i, fixp6m),
                             n = rep(0, fixp6m)))
}


fixp9f <-length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "west" & df_aah_notest$sex=="Female" & df_aah_notest$age==9])))
fixp6f <-length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "west" & df_aah_notest$sex=="Female" & df_aah_notest$age==6])))
fixp4f <-length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "west" & df_aah_notest$sex=="Female" & df_aah_notest$age==4])))
fixp3f <-length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "west" & df_aah_notest$sex=="Female" & df_aah_notest$age==3])))
fixp2f <-length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "west" & df_aah_notest$sex=="Female" & df_aah_notest$age==2])))
fixp1f <-length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "west" & df_aah_notest$sex=="Female" & df_aah_notest$age==1])))
fixp0f <-length(which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "west" & df_aah_notest$sex=="Female" & df_aah_notest$age==0])))

fixp9f
fixp6f
fixp4f
fixp3f
fixp2f
fixp1f
fixp0f

#### these area all the same, so using a for loop to make correction expansion of dataframe
for(i in c(0,1,2,3,4,6,9)){
    df_aah_notest <-rbind(df_aah_notest,
                   data.frame(study_area = rep("west", fixp6m),
                              year = c(1983:2021)[which(!(1983:2021 %in% df_aah_notest$year[df_aah_notest$study_area == "west" &
                                                                                         df_aah_notest$sex == "Female" &
                                                                                         df_aah_notest$age == i]))],
                              sex = rep("Female", fixp6m),
                              age = rep(i, fixp6m),
                             n = rep(0, fixp6m)))
}

df_aah_notest <- arrange(df_aah_notest,study_area,year,sex,age)

###################################################################
###
### Extracting the early aah data for the 
### prior on the initial population age structure
###
###################################################################

df_age_early <- df_aah_notest[df_aah_notest$year > 1993 & df_aah_notest$year < 2002,]
df_age_before <- df_aah_notest[df_aah_notest$year < 1994,]


###################################################################
###
### Aggregating the aah data with the surveillance aah data  
###
###################################################################

for(i in 1:nrow(df_age_sus)){
    for(j in 1:nrow(df_aah_notest)){
        if(df_age_sus$study_area[i] == df_aah_notest$study_area[j] & 
            df_age_sus$year[i] == df_aah_notest$year[j] & 
            df_age_sus$sex[i] == df_aah_notest$sex[j] & 
            df_age_sus$age[i] == df_aah_notest$age[j]) {
                df_age_sus$n[i] <- df_age_sus$n[i]+df_aah_notest$n[j]
            }
    }
}

df_age_sus <- rbind(df_age_early,df_age_sus)



