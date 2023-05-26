#########################################################################################################################3
###
### Formatting/combining of data
###
#########################################################################################################################3

# n_year <- length(2002:2021)
# n_year <- length(1992:2021)
# n_year <- length(1994:2021)
n_ageclass <- 7
# n_ageclassm <- 6
# n_ageclassf <- 7
# n_sex <- 2
n_agem <- 7
n_agef <- 10


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

####################################
###
### Calculating sex ratio prior to 
### study starting
###
####################################

df_age_before_female <- df_age_before %>% group_by(age,sex) %>% summarise(n=sum(n)) %>% filter(sex=="Female")
df_age_before_male <- df_age_before %>% group_by(age,sex) %>% summarise(n=sum(n)) %>% filter(sex=="Male")


df_age_before_antlerless <- df_age_before_female
df_age_before_antlered <- df_age_before_male

df_age_before_antlerless$n[df_age_before_antlerless$age==0] <- 
              df_age_before_female$n[df_age_before_female$age==0] +
              df_age_before_male$n[df_age_before_male$age==0]

df_age_before_antlered <- df_age_before_male %>% filter(age != 0)
df_age_before_female_nofawn <- df_age_before_female %>% filter(age != 0)

doe_per <- df_age_before_female_nofawn$n[df_age_before_female_nofawn$age == 1]/sum(df_age_before_female_nofawn$n)
buck_per <- df_age_before_antlered$n[df_age_before_antlered$age == 1]/sum(df_age_before_antlered$n)

sex_ratio_early <- doe_per / buck_per

df_age_before_antlerless$proportion <- df_age_before_antlerless$n/sum(df_age_before_antlerless$n)
df_age_before_antlered$proportion <- df_age_before_antlered$n/sum(df_age_before_antlered$n)
df_age_before_female$proportion <- df_age_before_female$n/sum(df_age_before_female$n)
df_age_before_male$proportion <- df_age_before_male$n/sum(df_age_before_male$n)

########################################
###
### Hyper prior for  sex-age
### structured initial population size
###
########################################

###################
### East
###################

prevalence_f_east <- (apply(Cage_inf[1,1,,],2,sum)/apply(Cage[1,1,,9:28],2,sum))
prevalence_m_east <- (apply(Cage_inf[1,2,,],2,sum)/apply(Cage[1,2,,9:28],2,sum))

df_prev_f_east <- data.frame(x=2002:2021,prevalence_f_east=log(prevalence_f_east))
lm_f_east <- lm(prevalence_f_east~x,data=df_prev_f_east)
# summary(lm_f_east)
df_prev_m_east <- data.frame(x=2002:2021,prevalence_m_east=log(prevalence_m_east))
lm_m_east <- lm(prevalence_m_east~x,data=df_prev_m_east)
# summary(lm_m_east)

pred_prev_f_east <- exp(predict(lm_f_east,newdata=data.frame(x=1994:2001)))
pred_prev_m_east_east <- exp(predict(lm_m_east,newdata=data.frame(x=1994:2001)))

df_prev_f_east <- data.frame(x=2002:2021,prevalence_f_east=log(prevalence_f_east))
lm_f_east <- lm(prevalence_f_east~x,data=df_prev_f_east)
# summary(lm_f_east)
df_prev_m_east <- data.frame(x=2002:2021,prevalence_m_east=log(prevalence_m_east))
lm_m_east <- lm(prevalence_m_east~x,data=df_prev_m_east)
# summary(lm_m_east)

pred_prev_f_east <- exp(predict(lm_f_east,newdata=data.frame(x=1994:2001)))
pred_prev_m_east_east <- exp(predict(lm_m_east,newdata=data.frame(x=1994:2001)))

###################
### West
###################

prevalence_f_west <- (apply(Cage_inf[2,1,,],2,sum)/apply(Cage[2,1,,9:28],2,sum))
prevalence_m_west <- (apply(Cage_inf[2,2,,],2,sum)/apply(Cage[2,2,,9:28],2,sum))
#tricking prevalence to be super small rather than 0 to fit the log linear model. 
prevalence_f_west[prevalence_f_west==0] <- .0000001

df_prev_f_west <- data.frame(x=2002:2021,prevalence_f_west=log(prevalence_f_west))
lm_f_west <- lm(prevalence_f_west~x,data=df_prev_f_west)
# summary(lm_f_west)
df_prev_m_west <- data.frame(x=2002:2021,prevalence_m_west=log(prevalence_m_west))
lm_m_west <- lm(prevalence_m_west~x,data=df_prev_m_west)
# summary(lm_m_west)

pred_prev_f_west <- exp(predict(lm_f_west,newdata=data.frame(x=1994:2001)))
pred_prev_m_east_west <- exp(predict(lm_m_west,newdata=data.frame(x=1994:2001)))

df_prev_f_west <- data.frame(x=2002:2021,prevalence_f_west=log(prevalence_f_west))
lm_f_west <- lm(prevalence_f_west~x,data=df_prev_f_west)
# summary(lm_f_west)
df_prev_m_west <- data.frame(x=2002:2021,prevalence_m_west=log(prevalence_m_west))
lm_m_west <- lm(prevalence_m_west~x,data=df_prev_m_west)
# summary(lm_m_west)

pred_prev_f_west <- exp(predict(lm_f_west,newdata=data.frame(x=1994:2001)))
pred_prev_m_east_west <- exp(predict(lm_m_west,newdata=data.frame(x=1994:2001)))


pred_prev_f_east
pred_prev_m_east_east
pred_prev_f_west
pred_prev_m_east_west

########################################
###
### Hyper prior for  sex-age
### structured initial population size
###
########################################

initN_sus <- array(0, dim = c(n_study_area,n_sex,n_agef))
initN_inf <- array(0, dim = c(n_study_area,n_sex,n_agef))

#######################
### East
#######################

### Total population initialized from SAK estimate from 1994
init0_east <- c()
init0_east$female <- sex_ratio_early * pop_estimate_east
init0_east$male <- (1 - sex_ratio_early) * pop_estimate_east


###susceptible initial population
###females
initN_sus[1, 1, 1] <- init0_east$female *
                   .5 *
                   df_age_before_antlerless$proportion[1] *
                   (1 - pred_prev_f_east[1])   #F
for(a in 2:4){
      initN_sus[1, 1, a] <- init0_east$female *
                         df_age_before_antlerless$proportion[a] *
                         (1 - pred_prev_f_east[1])   #1,2,3
}
initN_sus[1, 1, 5] <- init0_east$female * 
                   df_age_before_antlerless$proportion[5] *
                   (2/3) *
                   (1 - pred_prev_f_east[[1]])  #4
initN_sus[1, 1, 6] <- init0_east$female *
                   df_age_before_antlerless$proportion[5] *
                   (1/3) * (1 - pred_prev_f_east[[1]])  #5
initN_sus[1, 1, 7] <- init0_east$female *
                   df_age_before_antlerless$proportion[6] * 
                   (1/2) * 
                   (1 - pred_prev_f_east[[1]])  #6
initN_sus[1, 1, 8] <- init0_east$female *
                   df_age_before_antlerless$proportion[6] *
                   (1/4) *
                   (1 - pred_prev_f_east[[1]]) #7
initN_sus[1, 1, 9] <- init0_east$female * 
                   df_age_before_antlerless$proportion[6]*
                   (1/6) *
                   (1 - pred_prev_f_east[[1]]) #8
initN_sus[1, 1, 10] <- init0_east$female *
                    df_age_before_antlerless$proportion[6] *
                    (1/12) *
                    (1 - pred_prev_f_east[[1]])  #9+,same as 6-8, decaying from 6-8

###antlered
initN_sus[1, 2, 1] <- init0_east$female *
                   .5 *
                   df_age_before_antlerless$proportion[1] *
                   (1 - pred_prev_m_east[1]) 
for(a in 2:4){
      initN_sus[1, 2, a] <- init0_east$male *
                         df_age_before_antlered$proportion[a - 1] *
                         (1 - pred_prev_m_east[1])  #1,2,3,
}
initN_sus[1, 2, 5] <- 1 * (1 - pred_prev_m_east[1]) # 4
initN_sus[1, 2, 6] <- 1 * (1 - pred_prev_m_east[1]) # 5
initN_sus[1, 2, 7] <- 1 * (1 - pred_prev_m_east[1]) # 6+

####################################
###infected initial population
####################################

###antlerless
initN_inf[1, 1, 1] <- init0_east$female *
                   .5 *
                   df_age_before_antlerless$proportion[1] *
                   (pred_prev_f_east[1])   #F
for(a in 2:4){
      initN_inf[1, 1, a] <- init0_east$female *
                         df_age_before_antlerless$proportion[a] *
                         (pred_prev_f_east[1])   #1,2,3
}
initN_inf[1, 1, 5] <- init0_east$female *
                   df_age_before_antlerless$proportion[5] *
                   (2 / 3) *
                   (pred_prev_f_east[[1]])  #4
initN_inf[1, 1, 6] <- init0_east$female * 
                   df_age_before_antlerless$proportion[5] *
                   (1 / 3) *
                   (pred_prev_f_east[[1]])  #5
initN_inf[1, 1, 7] <- init0_east$female *
                   df_age_before_antlerless$proportion[6] *
                   (1 / 2) *
                   (pred_prev_f_east[[1]])  #6
initN_inf[1, 1, 8] <- init0_east$female *
                   df_age_before_antlerless$proportion[6] *
                   (1 / 4) *
                   (pred_prev_f_east[[1]]) #7
initN_inf[1, 1, 9] <- init0_east$female *
                   df_age_before_antlerless$proportion[6] *
                   (1 / 6) *
                   (pred_prev_f_east[[1]]) #8
initN_inf[1, 1, 10] <- init0_east$female *
                    df_age_before_antlerless$proportion[6] *
                    (1 / 12) *
                    (pred_prev_f_east[[1]])  # 9+, set as same proportion from 6-8

###antlered
initN_inf[1, 2, 1] <- init0_east$female *
                   .5 *
                   df_age_before_antlerless$proportion[1] *
                   (pred_prev_m_east[1])  #F
for(a in 2:4) {
      initN_inf[1, 2, a] <- init0_east$male *
                         df_age_before_antlered$proportion[a - 1] *
                         (pred_prev_m_east[1])  #1,2,3
}
initN_inf[1, 2, 5] <- 1  * (pred_prev_m_east[1])# 4
initN_inf[1, 2, 6] <- 1  * (pred_prev_m_east[1])# 5
initN_inf[1, 2, 7] <- 1  * (pred_prev_m_east[1])# 6+



#######################
### West
#######################

### Total population initialized from SAK estimate from 1994
init0_west <- c()
init0_west$female <- sex_ratio_early * pop_estimate_west
init0_west$male <- (1 - sex_ratio_early) * pop_estimate_west

###susceptible initial population
###females
initN_sus[2, 1, 1] <- init0_west$female *
                   .5 *
                   df_age_before_antlerless$proportion[1] *
                   (1 - pred_prev_f_west[1])   #F
for(a in 2:4){
      initN_sus[2, 1, a] <- init0_west$female *
                         df_age_before_antlerless$proportion[a] *
                         (1 - pred_prev_f_west[1])   #1,2,3
}
initN_sus[2, 1, 5] <- init0_west$female * 
                   df_age_before_antlerless$proportion[5] *
                   (2/3) *
                   (1 - pred_prev_f_west[[1]])  #4
initN_sus[2, 1, 6] <- init0_west$female *
                   df_age_before_antlerless$proportion[5] *
                   (1/3) * (1 - pred_prev_f_west[[1]])  #5
initN_sus[2, 1, 7] <- init0_west$female *
                   df_age_before_antlerless$proportion[6] * 
                   (1/2) * 
                   (1 - pred_prev_f_west[[1]])  #6
initN_sus[2, 1, 8] <- init0_west$female *
                   df_age_before_antlerless$proportion[6] *
                   (1/4) *
                   (1 - pred_prev_f_west[[1]]) #7
initN_sus[2, 1, 9] <- init0_west$female * 
                   df_age_before_antlerless$proportion[6]*
                   (1/6) *
                   (1 - pred_prev_f_west[[1]]) #8
initN_sus[2, 1, 10] <- init0_west$female *
                    df_age_before_antlerless$proportion[6] *
                    (1/12) *
                    (1 - pred_prev_f_west[[1]])  #9+,same as 6-8, decaying from 6-8

###antlered
initN_sus[2, 2, 1] <- init0_west$female *
                   .5 *
                   df_age_before_antlerless$proportion[1] *
                   (1 - pred_prev_m_west[1]) 
for(a in 2:4){
      initN_sus[2, 2, a] <- init0_west$male *
                         df_age_before_antlered$proportion[a - 1] *
                         (1 - pred_prev_m_west[1])  #1,2,3,
}
initN_sus[2, 2, 5] <- 1 * (1 - pred_prev_m_west[1]) # 4
initN_sus[2, 2, 6] <- 1 * (1 - pred_prev_m_west[1]) # 5
initN_sus[2, 2, 7] <- 1 * (1 - pred_prev_m_west[1]) # 6+

####################################
###infected initial population
####################################

###antlerless
initN_inf[2, 1, 1] <- init0_west$female *
                   .5 *
                   df_age_before_antlerless$proportion[1] *
                   (pred_prev_f_west[1])   #F
for(a in 2:4){
      initN_inf[2, 1, a] <- init0_west$female *
                         df_age_before_antlerless$proportion[a] *
                         (pred_prev_f_west[1])   #1,2,3
}
initN_inf[2, 1, 5] <- init0_west$female *
                   df_age_before_antlerless$proportion[5] *
                   (2 / 3) *
                   (pred_prev_f_west[[1]])  #4
initN_inf[2, 1, 6] <- init0_west$female * 
                   df_age_before_antlerless$proportion[5] *
                   (1 / 3) *
                   (pred_prev_f_west[[1]])  #5
initN_inf[2, 1, 7] <- init0_west$female *
                   df_age_before_antlerless$proportion[6] *
                   (1 / 2) *
                   (pred_prev_f_west[[1]])  #6
initN_inf[2, 1, 8] <- init0_west$female *
                   df_age_before_antlerless$proportion[6] *
                   (1 / 4) *
                   (pred_prev_f_west[[1]]) #7
initN_inf[2, 1, 9] <- init0_west$female *
                   df_age_before_antlerless$proportion[6] *
                   (1 / 6) *
                   (pred_prev_f_west[[1]]) #8
initN_inf[2, 1, 10] <- init0_west$female *
                    df_age_before_antlerless$proportion[6] *
                    (1 / 12) *
                    (pred_prev_f_west[[1]])  # 9+, set as same proportion from 6-8

###antlered
initN_inf[2, 2, 1] <- init0_west$female *
                   .5 *
                   df_age_before_antlerless$proportion[1] *
                   (pred_prev_m_west[1])  #F
for(a in 2:4) {
      initN_inf[2, 2, a] <- init0_west$male *
                         df_age_before_antlered$proportion[a - 1] *
                         (pred_prev_m_west[1])  #1,2,3
}
initN_inf[2, 2, 5] <- 1  * (pred_prev_m_west[1])# 4
initN_inf[2, 2, 6] <- 1  * (pred_prev_m_west[1])# 5
initN_inf[2, 2, 7] <- 1  * (pred_prev_m_west[1])# 6+


################################################
f_logpop_sus_east <- log(initN_sus[1, 1,])
f_logpop_inf_east <- log(initN_inf[1, 1,])
m_logpop_sus_east <- log(initN_sus[1, 2,1:n_agem])
m_logpop_inf_east <- log(initN_inf[1, 2,1:n_agem])


f_logpop_sus_west <- log(initN_sus[2, 1,])
f_logpop_inf_west <- log(initN_inf[2, 1,])
m_logpop_sus_west <- log(initN_sus[2, 2,1:n_agem])
m_logpop_inf_west <- log(initN_inf[2, 2,1:n_agem])



f_logpop_sus <- rbind(f_logpop_sus_east,
                      f_logpop_sus_west)

f_logpop_inf <- rbind(f_logpop_inf_east,
                      f_logpop_inf_west)

m_logpop_sus <- rbind(m_logpop_sus_east,
                      m_logpop_sus_west)
                      
m_logpop_inf <- rbind(m_logpop_inf_east,
                      m_logpop_inf_west)

### Total population initialized from 1993
# initO <- df_harvest[df_harvest$year==1993,c(3,2)]
# initN_sus <- array(0, dim = c(n_sex,n_agef))
# initN_inf <- array(0, dim = c(n_sex,n_agef))

###susceptible initial population
###antlerless
# initN_sus[1, 1, 1] <- initO$antlerless * .5 * df_age_before_antlerless$proportion[1] * (1 - pred_prev_f[1])   #F,1,2,3,
# for(a in 2:4){
#       initN_sus[1, 1, a] <- initO$antlerless * df_age_before_antlerless$proportion[a] * (1 - pred_prev_f[1])   #F,1,2,3,
# }
# initN_sus[1, 1, 5] <- initO$antlerless * df_age_before_antlerless$proportion[5]/2 * (1 - pred_prev_f[[1]])  #4
# initN_sus[1, 1, 6] <- initO$antlerless * df_age_before_antlerless$proportion[5]/2 * (1 - pred_prev_f[[1]])  #5
# initN_sus[1, 1, 7] <- initO$antlerless * df_age_before_antlerless$proportion[6]/3 * (1 - pred_prev_f[[1]])  #6
# initN_sus[1, 1, 8] <- initO$antlerless * df_age_before_antlerless$proportion[6]/3 * (1 - pred_prev_f[[1]]) #7
# initN_sus[1, 1, 9] <- initO$antlerless * df_age_before_antlerless$proportion[6]/3 * (1 - pred_prev_f[[1]]) #8
# initN_sus[1, 1, 10] <- initO$antlerless * df_age_before_antlerless$proportion[6]/3 * (1 - pred_prev_f[[1]])  #9+,same as 6-8

# ###antlered
# initN_sus[1, 2, 1] <- initO$antlerless * .5 * df_age_before_antlerless$proportion[1] * (1 - pred_prev_m_east[1])  #1,2,3,
# for(a in 2:4){
#       initN_sus[1, 2, a] <- initO$antlered * df_age_before_antlered$proportion[a-1] * (1 - pred_prev_m_east[1])  #1,2,3,
# }
# initN_sus[1, 2, 5] <- 1  # 4
# initN_sus[1, 2, 6] <- 1  # 5
# initN_sus[1, 2, 7] <- 1  # 6+

# ###infected initial population
# ###antlerless
# initN_inf[1, 1] <- initO$antlerless * .5 * df_age_before_antlerless$proportion[1] * (pred_prev_f[1])   #F,1,2,3,
# for(a in 2:4){
#       initN_inf[1, a] <- initO$antlerless * df_age_before_antlerless$proportion[a] * (pred_prev_f[1])   #F,1,2,3,
# }
# initN_inf[1, 5] <- initO$antlerless * df_age_before_antlerless$proportion[5]/2 * (pred_prev_f[[1]])  #4
# initN_inf[1, 6] <- initO$antlerless * df_age_before_antlerless$proportion[5]/2 * (pred_prev_f[[1]])  #5
# initN_inf[1, 7] <- initO$antlerless * df_age_before_antlerless$proportion[6]/3 * (pred_prev_f[[1]])  #6
# initN_inf[1, 8] <- initO$antlerless * df_age_before_antlerless$proportion[6]/3 * (pred_prev_f[[1]]) #7
# initN_inf[1, 9] <- initO$antlerless * df_age_before_antlerless$proportion[6]/3 * (pred_prev_f[[1]]) #8
# initN_inf[1, 10] <- initO$antlerless * df_age_before_antlerless$proportion[6]/3 * (pred_prev_f[[1]])  # 9+, set to same as 6-8

# ###antlered
# initN_inf[1, 2, 1] <- initO$antlerless * .5 * df_age_before_antlerless$proportion[1] * (pred_prev_m_east[1])  #F
# for(a in 2:4){
#       initN_inf[1, 2, a] <- initO$antlered * df_age_before_antlered$proportion[a-1] * (pred_prev_m_east[1])  #1,2,3,
# }
# initN_inf[1, 2, 5] <- 1  # 4
# initN_inf[1, 2, 6] <- 1  # 5
# initN_inf[1, 2, 7] <- 1  # 6+

# f_logpop_sus <- log(initN_sus[1, 1,])
# f_logpop_inf <- log(initN_inf[1,])
# m_logpop_sus <- log(initN_sus[1, 2,1:n_agem])
# m_logpop_inf <- log(initN_inf[1, 2,1:n_agem])



########################################
###
### Earn-A-Buck (EAB correction factor)
###
########################################


# antlerless_corr <- c(1.55, 2.39)
# mn_antlerless <- mean(antlerless_corr)
# var_antlerless  <- ((2.39 - 1.55) / 2) ^ 2

# antlered_corr <- c(.29, .36)
# mn_antlered <- mean(antlered_corr)
# var_antlered <- ((.36 - .29) / 2) ^ 2

# antlerless_param <- gamma_moments(mn_antlerless,var_antlerless)
# antlered_param <- gamma_moments(mn_antlered,var_antlered)

# png("correction_factors_hunterharvest.png")
# par(mfrow=c(2,1))
# hist(rgamma(10000,antlerless_param$alpha,antlerless_param$beta), main = "Antleress")
# hist(rgamma(10000,antlered_param$alpha,antlered_param$beta), main = "Antlered")
# dev.off()


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

##############
### East
##############

#adding antlerless male fawns to the Cage_less data
Cage_less_east <- rbind(Cage[1, 1,,], Cage[1, 2, 1,])
Cage_ant_east <- Cage[1, 2, 2:6,]

sizeCage_f_east <- apply(Cage_less_east,2,sum)
sizeCage_m_east <- apply(Cage_ant_east,2,sum)

pCage_ant_east <- matrix(NA,nr=5,nc=n_year)
for(i in 1:n_year){
      pCage_ant_east[,i] <- Cage_ant_east[,i]/sum(Cage_ant_east[,i])
}


##############
### West
##############

#adding antlerless male fawns to the Cage_less data
Cage_less_west <- rbind(Cage[2, 1,,], Cage[2, 2, 1,])
Cage_ant_west <- Cage[2, 2, 2:6,]

sizeCage_f_west <- apply(Cage_less_west,2,sum)
sizeCage_m_west <- apply(Cage_ant_west,2,sum)

pCage_ant_west <- matrix(NA,nr=5,nc=n_year)
for(i in 1:n_year){
      pCage_ant_west[,i] <- Cage_ant_west[,i]/sum(Cage_ant_west[,i])
}

#### aggregating into single array
Cage_less <- array(NA,c(n_study_area,dim(Cage_less_east)))
Cage_less[1,,] <- Cage_less_east
Cage_less[2,,] <- Cage_less_west

Cage_ant <- array(NA,c(n_study_area,dim(Cage_ant_east)))
Cage_ant[1,,] <- Cage_ant_east
Cage_ant[2,,] <- Cage_ant_west

sizeCage_f <- rbind(sizeCage_f_east, sizeCage_f_west)
sizeCage_m <- rbind(sizeCage_m_east, sizeCage_m_west)


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

n_year_precollar <- length(1994:2016)
n_year_collar <- length(2017:2021)

# which(1994:2021 == 2017)

##########################################################################
### Survival Parameters
### loading age and period effects from imputation version of S/FOI model
###########################################################################

sus_beta0_survival <- -8.75
sus_beta_sex_survival <- -.75
inf_beta0_survival <- -8
inf_beta_sex_survival <- -.5

nT_age_surv <- 962
nT_period_surv <- 1564

age_effect_survival <- 2*exp(-.09*seq(1:nT_age_surv))
age_effect_survival[600:nT_period_surv] <- .00001*exp(.009*seq(600:nT_period_surv))
age_effect_survival <- age_effect_survival- mean(age_effect_survival)
# plot(age_effect_survival)

# head(round(age_effect_survival,2),100)

period_effect_survival <- 1 * sin(2/52 * pi * (1:nT_period_surv)) + rnorm(nT_period_surv,0,.1)
period_effect_survival <- period_effect_survival - mean(period_effect_survival)
# plot(period_effect_survival[1:104],type="l")
# nT_age_surv <- length(age_effect_survival)
# nT_period_surv <- length(period_effect_survival)
# period_effect_survival[c(nT_period_surv - 1,nT_period_surv)] <- period_effect_survival[nT_period_surv - 2]

# period_effect_survival <- rnorm(nT_period_surv,0,.5)
# period_effect_survival <- period_effect_survival-mean(period_effect_survival)
##########################################################################
### FOI Parameters
### loading age and period effects from Transmission v3 w/o Survival
###########################################################################

load("datafiles/f_age_foi.Rdata")
load("datafiles/m_age_foi.Rdata")
load("datafiles/f_period_foi.Rdata")
load("datafiles/m_period_foi.Rdata")
load("datafiles/age_lookup_f.Rdata")
load("datafiles/age_lookup_m.Rdata")

Nage_lookup <- length(age_lookup_f)
x <- 2002:2022
lmfperiod <- lm(f_period_foi~x)
lmmperiod <- lm(m_period_foi~x)
pred_foiperiod_f <- predict(lmfperiod,newdata=data.frame(x=1994:2001))
pred_foiperiod_m <- predict(lmmperiod,newdata=data.frame(x=1994:2001))
f_period_foi <- c(pred_foiperiod_f,f_period_foi)
m_period_foi <- c(pred_foiperiod_m,m_period_foi)
f_period_foi <- f_period_foi[-length(f_period_foi)]
m_period_foi <- m_period_foi[-length(m_period_foi)]
nT_period_foi <- length(f_period_foi) #number of years == n_year

#making lower b/c this is monthly rather than weekly
# f_age_foi <- f_age_foi -3
# m_age_foi  <- m_age_foi -3

##########################################################################
### Testing survival generated parameters
###########################################################################

# tau_sn_sus  <- rgamma(1, 1,1)
# mu_sn_sus  <- rnorm(2,cloglog(.2), 1/sqrt(3))
# cll_sn_sus <- array(NA,c(2,n_agef,n_year-1))
# sn_sus <- array(NA,c(2,n_agef,n_year-1))
# for (t in 1:(n_year - 1)) {
#     for(a in 1:n_agef) {
#       cll_sn_sus[1, a, t]  <-  rnorm(1,mu_sn_sus[1], tau_sn_sus)
#       sn_sus[1, a, t] <- exp(-exp(cll_sn_sus[1, a, t]))
#     }
#     for(a in 1:n_agem) {
#         cll_sn_sus[2, a, t] ~ dnorm(mu_sn_sus[2], tau_sn_sus)
#         sn_sus[2, a, t] <- exp(-exp(cll_sn_sus[2, a, t]))
#     }
# }

# cll_sn_sus
# sn_sus


################################################################
###
### Preliminaries for calculating prob(survival) for AAH model
###
#################################################################

###
### The number of integrals within a year, for weekly or monthly
###
intvl_step_yr <- 52
# intvl_step_yr <- 12

load("datafiles/d_fit_season.Rdata")

# d_fit_season$yr_end

################################################################
###
### Load p_hunt results from cause-specific analysis
###
#################################################################

load("datafiles/p_hunt.Rdata")

p_ng_f <- p_hunt$p_ng_f
p_gun_f <- p_hunt$p_gun_f
p_ng_m <- p_hunt$p_ng_m
p_gun_m <- p_hunt$p_gun_m


################################################################
###
### Load eab prior allocation
###
#################################################################

# antlerless_correction = (1.55)
# Sd = (.55*(2.3/2.04) - .49*(1.79/2.04))/2 =  0.06875
# antlered_correction = (1 -.29)
# Sd = -(.29*(-.49/-.6) - .29*(-.7/-.6))/2 = 0.05075


eab_antlerless_alpha <- gamma_moments(1.49, .1225^2)$alpha
eab_antlerless_beta <- gamma_moments(1.49, .1225^2)$beta

eab_antlered_alpha <- gamma_moments(.71, .1015^2)$alpha
eab_antlered_beta <- gamma_moments(.71, .1015^2)$beta

pdf("figures/eab_prior_plot.pdf")
hist(rgamma(10000,eab_antlerless_alpha,eab_antlerless_beta))
hist(rgamma(10000,eab_antlered_alpha,eab_antlered_beta))
dev.off()

df_temp <- data.frame(year=1994:2021,tot=Ototal$antlerless,totant=Ototal$antlered,eab=df_eab$EAB)
df_temp$eab <- as.factor(df_temp$eab)
eab_indicator_plot <- ggplot(data=df_temp) + 
      geom_point(aes(x = year,
                        y = tot,
                        color = eab),
                  size = 8) +
      geom_point(aes(x = year,
                     y = totant,
                     color = eab),
                  size = 8) +
      geom_line(aes(x = year,y = tot), size = 1) + 
      geom_line(aes(x = year, y = totant),
                    size = 1,color = "green4") +
      theme_bw()

ggsave("eab_indicator_plot.png", eab_indicator_plot, height = 6, width = 6)

eab_indicator_plot

######################################################################
###
### fixing the FOI age lookup vector
###
#######################################################################

# age_lookup_m_conv <- age_lookup_m
# age_lookup_f_conv <- c(age_lookup_f,rep(7,52))

age_lookup_f_conv <- c(rep(1:4,each=52),rep(5,104),rep(6,156),rep(7,52))
age_lookup_m_conv <- c(rep(1:4,each=52),rep(5,104),rep(6,156))
Nage_lookup_conv <- length(age_lookup_f_conv)

Nage_lookup_conv
