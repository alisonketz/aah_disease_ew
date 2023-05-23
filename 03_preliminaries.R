#########################################################################################################################3
###
### Formatting/combining of data
###
#########################################################################################################################3

# n_year <- length(2002:2021)
# n_year <- length(1992:2021)
n_year <- length(1994:2021)
n_ageclass <- 7
n_ageclassm <- 6
n_ageclassf <- 7
n_agem <- 7
n_agef <- 10
n_sex <- 2

####################################
###
### Calculating sex ratio prior to 
### study starting
###
####################################
df_age_before_antlerless <- df_age_before_female
df_age_before_antlerless$total[df_age_before_antlerless$age==0] <- 
              df_age_before_female$total[df_age_before_female$age==0] +
              df_age_before_male$total[df_age_before_male$age==0]

df_age_before_antlered <- df_age_before_male %>% filter(age != 0)
df_age_before_female_nofawn <- df_age_before_female %>% filter(age != 0)

doe_per <- df_age_before_female_nofawn$total[df_age_before_female_nofawn$age == 1]/sum(df_age_before_female_nofawn$total)
buck_per <- df_age_before_antlered$total[df_age_before_antlered$age == 1]/sum(df_age_before_antlered$total)

sex_ratio_early <- doe_per / buck_per
sex_ratio_1994 <- sum(df_age_early_female %>% filter(YR==1994) %>% filter(age!=0)%>% select(count))/sum(data.frame(df_age_early_male %>% filter(YR==1994) %>% filter(age!=0))$count)


df_age_before_antlerless$proportion <- df_age_before_antlerless$total/sum(df_age_before_antlerless$total)
df_age_before_antlered$proportion <- df_age_before_antlered$total/sum(df_age_before_antlered$total)
df_age_before_female$proportion <- df_age_before_female$total/sum(df_age_before_female$total)
df_age_before_male$proportion <- df_age_before_male$total/sum(df_age_before_male$total)

########################################
###
### Hyper prior for  sex-age
### structured initial population size
###
########################################

#year/age/sex specific prevalence
# prev <- array(0,c(2,10,20))
# for(i in 1:20){
#   for(a in 1:4){
#     prev[1,a,i] <- Cage_inf[1,a,i]/apply(Cage[1,,9:28],2,sum)[i]
#   }
#    prev[1,5,i] <- (Cage_inf[1,5,i]/apply(Cage[1,,9:28],2,sum)[i])/2
#    prev[1,6,i] <- (Cage_inf[1,5,i]/apply(Cage[1,,9:28],2,sum)[i])/2
#    prev[1,7,i] <- (Cage_inf[1,6,i]/apply(Cage[1,,9:28],2,sum)[i])/3
#    prev[1,8,i] <- (Cage_inf[1,6,i]/apply(Cage[1,,9:28],2,sum)[i])/3
#    prev[1,9,i] <- (Cage_inf[1,6,i]/apply(Cage[1,,9:28],2,sum)[i])/3
#    prev[1,10,i] <- (Cage_inf[1,7,i]/apply(Cage[1,,9:28],2,sum)[i])

#    for(a in 1:4){
#     prev[2,a,i] <- Cage_inf[2,a,i]/apply(Cage[2,,9:28],2,sum)[i]
#   }
#    prev[2,5,i] <- (Cage_inf[2,5,i]/apply(Cage[2,,9:28],2,sum)[i])/2
#    prev[2,6,i] <- (Cage_inf[2,5,i]/apply(Cage[2,,9:28],2,sum)[i])/2
#    prev[2,7,i] <- (Cage_inf[2,6,i]/apply(Cage[2,,9:28],2,sum)[i])
#    }

# Cage_inf[1,,]/apply(Cage[1,,9:28],2,sum)

#prevalence w/o age structure
prevalence_f <- (apply(Cage_inf[1,,],2,sum)/apply(Cage[1,,9:28],2,sum))
prevalence_m <- (apply(Cage_inf[2,,],2,sum)/apply(Cage[2,,9:28],2,sum))
df_prev_f <- data.frame(x=2002:2021,prevalence_f=log(prevalence_f))
lm_f <- lm(prevalence_f~x,data=df_prev_f)
summary(lm_f)
df_prev_m <- data.frame(x=2002:2021,prevalence_m=log(prevalence_m))
lm_m <- lm(prevalence_m~x,data=df_prev_m)
summary(lm_m)
pred_prev_f <- exp(predict(lm_f,newdata=data.frame(x=1993:2001)))
pred_prev_m <- exp(predict(lm_m,newdata=data.frame(x=1993:2001)))

pred_prev_f
pred_prev_m

########################################
###
### Hyper prior for  sex-age
### structured initial population size
###
########################################

# Ototal from 1993
initO <- df_harvest[df_harvest$year==1993,c(3,2)]
initN_sus <- array(0, dim = c(n_sex,n_agef))
initN_inf <- array(0, dim = c(n_sex,n_agef))


#susceptible initial population
###antlerless
initN_sus[1, 1] <- initO$antlerless * .5 * df_age_before_antlerless$proportion[1] * (1 - pred_prev_f[1])   #F,1,2,3,
for(a in 2:4){
      initN_sus[1, a] <- initO$antlerless * df_age_before_antlerless$proportion[a] * (1 - pred_prev_f[1])   #F,1,2,3,
}

initN_sus[1, 5] <- initO$antlerless * df_age_before_antlerless$proportion[5]/2 * (1 - pred_prev_f[[1]])  #4
initN_sus[1, 6] <- initO$antlerless * df_age_before_antlerless$proportion[5]/2 * (1 - pred_prev_f[[1]])  #5
initN_sus[1, 7] <- initO$antlerless * df_age_before_antlerless$proportion[6]/3 * (1 - pred_prev_f[[1]])  #6
initN_sus[1, 8] <- initO$antlerless * df_age_before_antlerless$proportion[6]/3 * (1 - pred_prev_f[[1]]) #7
initN_sus[1, 9] <- initO$antlerless * df_age_before_antlerless$proportion[6]/3 * (1 - pred_prev_f[[1]]) #8
initN_sus[1, 10] <- initO$antlerless * df_age_before_antlerless$proportion[6]/3 * (1 - pred_prev_f[[1]])  #9+,same as 6-8

###antlered
initN_sus[2, 1] <- initO$antlerless * .5 * df_age_before_antlerless$proportion[1] * (1 - pred_prev_m[1])  #1,2,3,
for(a in 2:4){
      initN_sus[2, a] <- initO$antlered * df_age_before_antlered$proportion[a-1] * (1 - pred_prev_m[1])  #1,2,3,
}
initN_sus[2, 5] <- 1  # 4
initN_sus[2, 6] <- 1  # 5
initN_sus[2, 7] <- 1  # 6+

#infected initial population
###antlerless
initN_inf[1, 1] <- initO$antlerless * .5 * df_age_before_antlerless$proportion[1] * (pred_prev_f[1])   #F,1,2,3,
for(a in 2:4){
      initN_inf[1, a] <- initO$antlerless * df_age_before_antlerless$proportion[a] * (pred_prev_f[1])   #F,1,2,3,
}

initN_inf[1, 5] <- initO$antlerless * df_age_before_antlerless$proportion[5]/2 * (pred_prev_f[[1]])  #4
initN_inf[1, 6] <- initO$antlerless * df_age_before_antlerless$proportion[5]/2 * (pred_prev_f[[1]])  #5
initN_inf[1, 7] <- initO$antlerless * df_age_before_antlerless$proportion[6]/3 * (pred_prev_f[[1]])  #6
initN_inf[1, 8] <- initO$antlerless * df_age_before_antlerless$proportion[6]/3 * (pred_prev_f[[1]]) #7
initN_inf[1, 9] <- initO$antlerless * df_age_before_antlerless$proportion[6]/3 * (pred_prev_f[[1]]) #8
initN_inf[1, 10] <- initO$antlerless * df_age_before_antlerless$proportion[6]/3 * (pred_prev_f[[1]])  # 9+, set to same as 6-8

###antlered
initN_inf[2, 1] <- initO$antlerless * .5 * df_age_before_antlerless$proportion[1] * (pred_prev_m[1])  #F
for(a in 2:4){
      initN_inf[2, a] <- initO$antlered * df_age_before_antlered$proportion[a-1] * (pred_prev_m[1])  #1,2,3,
}
initN_inf[2, 5] <- 1  # 4
initN_inf[2, 6] <- 1  # 5
initN_inf[2, 7] <- 1  # 6+


initN_sus[1,]
initN_sus[2,]
initN_inf[1,]
initN_inf[2,]

f_logpop_sus <- log(initN_sus[1,])
f_logpop_inf <- log(initN_inf[1,])
m_logpop_sus <- log(initN_sus[2,1:n_agem])
m_logpop_inf <- log(initN_inf[2,1:n_agem])

########################################
###
### Initializing values for sex-age
### structured initial population size
###
########################################

# #assumed starting population >>> by sex/age/infection class (needs to be logpop for model)
# assumN_sus <- array(0, dim = c(n_sex,n_agef,n_year))

# #year/sex specific prevalence
# prevalence_f <- (apply(Cage_inf[1,,],2,sum)/apply(Cage[1,,9:28],2,sum))
# prevalence_m <- (apply(Cage_inf[2,,],2,sum)/apply(Cage[2,,9:28],2,sum))

# for(y in 1:n_year){
#   for(a in 1:4){
#     assumN_sus[1, a, y] <- Ototal$antlerless[y]*Cage_sus[1, a, y]/sum(Cage_sus[1,,y]) #F,1,2,3,
#   }
#   assumN_sus[1, 5, y] <- (Ototal$antlerless[y]*Cage_sus[1, 5, y]/sum(Cage_sus[1, ,y]))/2 #F,1,2,3,4
#   assumN_sus[1, 6, y] <- (Ototal$antlerless[y]*Cage_sus[1, 5, y]/sum(Cage_sus[1, ,y]))/2 #5
#   assumN_sus[1, 7, y] <- (Ototal$antlerless[y]*Cage_sus[1, 6, y]/sum(Cage_sus[1, ,y]))/3 #6
#   assumN_sus[1, 8, y] <- (Ototal$antlerless[y]*Cage_sus[1, 6, y]/sum(Cage_sus[1, ,y]))/3 #7
#   assumN_sus[1, 9, y] <- (Ototal$antlerless[y]*Cage_sus[1, 6, y]/sum(Cage_sus[1, ,y]))/3 #8
#   assumN_sus[1, 10, y] <- Ototal$antlerless[y]*Cage_sus[1, 7, y]/sum(Cage_sus[1, ,y]) #9+
#   for(a in 1:4){
#     assumN_sus[2, a, y] <- Ototal$antlered[y]*Cage_sus[2, a, y]/sum(Cage_sus[2,,y]) #F,1,2,3,
#   }
#   assumN_sus[2, 5, y] <- (Ototal$antlered[y]*Cage_sus[2, 5, y]/sum(Cage_sus[2,,y]))/2 #4
#   assumN_sus[2, 6, y] <- (Ototal$antlered[y]*Cage_sus[2, 5, y]/sum(Cage_sus[2,,y]))/2 #5
#   assumN_sus[2, 7, y] <-  Ototal$antlered[y]*Cage_sus[2, 6, y]/sum(Cage_sus[2,,y])
# }
# pop_sus_init <- assumN_sus
# llpop_sus_init <- log(pop_sus_init)

# #assumed starting population >>> by sex/age class (needs to be logpop for model)
# assumN_inf <- array(0, dim = c(n_sex,n_agef,n_year))

# #or we could values from lit? estimate these?
# # assumN_inf.f.1 <- c(2229,2008,1115, 651,414,284,212,158,122,393) 
# # assumN_inf.m.1 <- c(2529,2098,1115, 651,414,284,212,158,122,3)
# for(y in 9:n_year){
#   for(a in 1:4){
#     assumN_inf[1, a, y] <- Ototal$antlerless[y] * prevalence_f[y - 8] *
#                            Cage_inf[1, a, y - 8] /
#                            sum(Cage_inf[1,,y - 8]) #F,1,2,3,
#   }
#   assumN_inf[1, 5, y] <- (Ototal$antlerless[y] *
#                           prevalence_f[y - 8] *
#                           Cage_inf[1, 5, y - 8]/sum(Cage_inf[1, , y - 8]))/2 #F,1,2,3,4
#   assumN_inf[1, 6, y] <- (Ototal$antlerless[y]*
#                           prevalence_f[y - 8] *
#                           Cage_inf[1, 5, y - 8]/sum(Cage_inf[1, , y - 8]))/2 #5
#   assumN_inf[1, 7, y] <- (Ototal$antlerless[y] *
#                           prevalence_f[y - 8] *
#                           Cage_inf[1, 6, y - 8]/sum(Cage_inf[1, , y - 8]))/3 #6
#   assumN_inf[1, 8, y] <- (Ototal$antlerless[y]*
#                           prevalence_f[y - 8] *
#                           Cage_inf[1, 6, y - 8]/sum(Cage_inf[1, , y - 8]))/3 #7
#   assumN_inf[1, 9, y] <- (Ototal$antlerless[y] *
#                           prevalence_f[y - 8] *
#                           Cage_inf[1, 6, y - 8]/sum(Cage_inf[1, , y - 8]))/3 #8
#   assumN_inf[1, 10, y] <- Ototal$antlerless[y]*
#                           prevalence_f[y - 8] *
#                           Cage_inf[1, 7, y - 8]/sum(Cage_inf[1, , y - 8]) #9+

#   for(a in 1:4){
#     assumN_inf[2, a, y] <- Ototal$antlered[y] *
#                               prevalence_m[y - 8] *
#                               Cage_inf[2, a, y - 8]/sum(Cage_inf[2, , y - 8]) #F,1,2,3,
#   }
#   assumN_inf[2, 5, y] <- (Ototal$antlered[y] *
#                             prevalence_m[y - 8] *
#   Cage_inf[1, 5, y - 8]/sum(Cage_inf[2,, y - 8])) / 2 #4
#   assumN_inf[2, 6, y] <- (Ototal$antlered[y - 8]*
#                             prevalence_m[y - 8] *
#   Cage_inf[1, 5, y - 8]/sum(Cage_inf[2,, y - 8])) / 2 #5
#   assumN_inf[2, 7, y] <-  Ototal$antlered[y]*
#                             prevalence_m[y - 8] *
#                             Cage_inf[1, 6, y - 8] / sum(Cage_inf[2, , y - 8])
# }
# #How initialize CWD+ population? in 1994?
# for(y in 1:8){
#   assumN_inf[1,,y] <- assumN_inf[1,,9]*.01*y
#   assumN_inf[2,,y] <- assumN_inf[2,,9]*.01*y
# }
# pop_inf_init <- assumN_inf
# llpop_inf_init <- log(pop_inf_init)

# f_logpop_inf <- log(assumN_inf[1,,1])
# f_logpop_sus <- log(assumN_sus[1,,1])
# m_logpop_inf <- log(assumN_inf[2,1:n_agem,1])
# m_logpop_sus <- log(assumN_sus[2,1:n_agem,1])

# f_logpop_sus <- ifelse(f_logpop_sus < 0, -5, f_logpop_sus)
# m_logpop_sus <- ifelse(m_logpop_sus < 0, -5, m_logpop_sus)
# f_logpop_inf <- ifelse(f_logpop_inf < 0, -5, f_logpop_inf)
# m_logpop_inf <- ifelse(m_logpop_inf < 0, -5, m_logpop_inf)

# llpop_sus_init <- ifelse(llpop_sus_init < 0, -5, llpop_sus_init)
# llpop_inf_init <- ifelse(llpop_inf_init < 0, -5, llpop_inf_init)

################################################
###
### Setting up the Cage data
###
################################################

#adding antlerless male fawns to the Cage_less data
Cage_less <- rbind(Cage[1,,], Cage[2,1,])
Cage_ant <- Cage[2,2:6,]

sizeCage_f <- apply(Cage_less,2,sum)
sizeCage_m <- apply(Cage_ant,2,sum)

#############################################
###
### initial values for reporting rates
###
#############################################

report_overall_init <- rbeta(1,report_hyp_all[1], report_hyp_all[2])

report_init <- rep(report_overall_init,n_year)
for(y in 23:28) {
    report_init[y] <- rbeta(1,report_hyp_y$alpha[y - 22],report_hyp_y$beta[y-22])
}

#########################################################################
###
### Fecundity/FDR Preliminaries
###
#########################################################################

##########################################
### moment matching functions
##########################################

# lognormal_moments <- function(barx,s){
# 	mu <- log(barx / sqrt((s^2) / (barx^2) + 1))
# 	sigma <- sqrt(log((s^2) / (barx^2) + 1))
# 	return(list(mu=mu,sigma=sigma))
# }

gamma_moments <- function(mu,sigma){
	alpha <- (mu^2)/(sigma^2)
	beta <- mu/(sigma^2)
	return(list(alpha=alpha,beta=beta))
}
##########################################
### calculate moments 
##########################################

##########################
### Option 3: lognormal
##########################

# fdr_ct_moments_2002_2016 <- lognormal_moments(fawndoe_df$overall_fd,mean(df_camtrap_fd$fdr_sd)*2)
# fdr_ct_moments_2017_2021 <- lognormal_moments(df_camtrap_fd$fdr_mean,df_camtrap_fd$fdr_sd)

# #how to set the sd for the years without uncertainty?
# #approximate using the mean of the estimates from Jen's method and doubling it?
# # mean(df_camtrap_fd$fdr_sd)*2

# obs_ct_fd_mu  <- c(fdr_ct_moments_2002_2016$mu,fdr_ct_moments_2017_2021$mu)
# obs_ct_fd_sd <- c(fdr_ct_moments_2002_2016$sigma,fdr_ct_moments_2017_2021$sigma)

###################################################################
### Option 4: gamma for camera trap, poisson for earlier data
###################################################################

# fdr_ct_gam_moments_2002_2016 <- gamma_moments(fawndoe_df$overall_fd,mean(df_camtrap_fd$fdr_sd)*2)
# fdr_ct_gam_moments_2017_2021 <- gamma_moments(df_camtrap_fd$fdr_mean,df_camtrap_fd$fdr_sd)
# fdr_ct_gam_moments_2002_2016 <- gamma_moments(fawndoe_df$overall_fd,mean(df_camtrap_fd$fdr_sd)*2)
fdr_ct_gam_moments_1992_2016 <- gamma_moments(fawndoe_df$overall_fd,mean(df_camtrap_fd$fdr_sd)*2)
fdr_ct_gam_moments_2017_2021 <- gamma_moments(df_camtrap_fd$fdr_mean,df_camtrap_fd$fdr_sd)
# obs_ct_fd_alpha  <- c(fdr_ct_gam_moments_2002_2016$alpha,fdr_ct_gam_moments_2017_2021$alpha)
# obs_ct_fd_beta <- c(fdr_ct_gam_moments_2002_2016$beta,fdr_ct_gam_moments_2017_2021$beta)
obs_ct_fd_alpha  <- c(fdr_ct_gam_moments_1992_2016$alpha,fdr_ct_gam_moments_2017_2021$alpha)
obs_ct_fd_beta <- c(fdr_ct_gam_moments_1992_2016$beta,fdr_ct_gam_moments_2017_2021$beta)
fec_init <- c(fawndoe_df$overall_fd,df_camtrap_fd$fdr_mean)
mu_fec_init <-  mean(log(fec_init))
fec_eps_init <- log(fec_init) - mean(log(fec_init))
n_year_fec_early <- nrow(fawndoe_df)

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






######################################
###
### checking for initial values and reasonable priors
###
#######################################

# mu_sn_inf <-  rnorm(1,cloglog(.2),2)

# cll_sn_inf <- c()
# sn_inf <- c()
#     for (t in 1:n_year) {
#       cll_sn_inf[t] = rnorm(1,mu_sn_inf, .2)
      
#       #change in variable to probability scale
#       sn_inf[t] <- exp(-exp(cll_sn_inf[t]))
#     }
# cll_sn_inf
# plot(sn_inf)
# round(sn_inf,2)  

##########################################################################
# preliminaries for survival model using GPS collar
###########################################################################
# n_year_precollar <- length(1992:2016)
n_year_precollar <- length(1994:2016)
n_year_collar <- length(2017:2021)

which(1994:2021 == 2017)

##########################################################################
#loading age and period effects from imputation version of S/FOI model
###########################################################################

load("~/Documents/integrate_s_foi/s_foi_v3/mcmcout.Rdata")

age_effect <- mcmcout$summary$all.chains[grep("sus_age_effect",rownames(mcmcout$summary$all.chains)), 1]
period_effect <- mcmcout$summary$all.chains[grep("sus_period_effect",rownames(mcmcout$summary$all.chains)),1]
sus_beta0 <- mcmcout$summary$all.chains[grep("sus_beta0",rownames(mcmcout$summary$all.chains)),1]
sus_beta_sex <- mcmcout$summary$all.chains[grep("sus_beta_sex",rownames(mcmcout$summary$all.chains)),1]
inf_beta0 <- mcmcout$summary$all.chains[grep("inf_beta0",rownames(mcmcout$summary$all.chains)),1]
inf_beta_sex <- mcmcout$summary$all.chains[grep("inf_beta_sex",rownames(mcmcout$summary$all.chains)),1]
age_effect <- unname(age_effect)
period_effect <- unname(period_effect)
sus_beta0 <- unname(sus_beta0)
sus_beta_sex <- unname(sus_beta_sex)
inf_beta0 <- unname(inf_beta0)
inf_beta_sex <- unname(inf_beta_sex)
nT_age <- length(age_effect)
nT_period <- length(period_effect)






tau_sn_sus  <- rgamma(1, 1,1)
mu_sn_sus  <- rnorm(2,cloglog(.2), 1/sqrt(3))
cll_sn_sus <- array(NA,c(2,n_agef,n_year-1))
sn_sus <- array(NA,c(2,n_agef,n_year-1))
for (t in 1:(n_year - 1)) {
    for(a in 1:n_agef) {
      cll_sn_sus[1, a, t]  <-  rnorm(1,mu_sn_sus[1], tau_sn_sus)
      sn_sus[1, a, t] <- exp(-exp(cll_sn_sus[1, a, t]))
    }
    for(a in 1:n_agem) {
        cll_sn_sus[2, a, t] ~ dnorm(mu_sn_sus[2], tau_sn_sus)
        sn_sus[2, a, t] <- exp(-exp(cll_sn_sus[2, a, t]))
    }
}

cll_sn_sus
sn_sus
