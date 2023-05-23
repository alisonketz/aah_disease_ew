
#######################################################################
###
### Function to calculate Annual survival probability based on 
### age effects and period effects
###
#######################################################################

calc_surv_aah <- nimble::nimbleFunction(
    run = function(
        ### argument type declarations
        nT_age = double(0),
        nT_period = double(0),
        beta0 = double(0),
        beta_sex = double(0),
        age_effect = double(1),
        period_effect = double(1),
        yr_start = double(1),
        yr_end = double(1),
        intvl_step_yr = double(0),
        n_year = double(0),
        n_agef = double(0),
        n_agem = double(0)
        ) {

    ###################################################################
    ###
    ### General Survival Surfaces for Susceptible/Infected Individuals
    ###
    ###################################################################

    # s_aah[sex,age,period]
    S0 <- nimArray(NA,c(2,nT_age,nT_period))
    UCH <- nimArray(NA,c(2,nT_age,nT_period))
    #UCH_fawn <- nimArray(NA,c(2,intvl_step_yr,nT_period))
    s_aah <- nimArray(NA,c(2,n_agef,n_year))

 #starttime <- Sys.time()
   for(i in 1:nT_age) {
        for(j in 1:nT_period) {
            UCH[1, i, j] <- exp(beta0 + beta_sex + age_effect[i] + period_effect[j])
            UCH[2, i, j] <- exp(beta0 + age_effect[i] + period_effect[j])
            S0[1, i, j] <- exp(-sum(UCH[1,1:i,1:j]))
            S0[2, i, j] <- exp(-sum(UCH[2,1:i,1:j]))
        }
		#print(c(i,j))
    }
#(endtime <- Sys.time() - starttime)
#  for(i in 1:intvl_step_yr) {
#        for(j in 1:nT_period) {
#            UCH_fawn[1, i, j] <- exp(beta0 + beta_sex + age_effect[i] + period_effect[j])
#            UCH_fawn[2, i, j] <- exp(beta0 + age_effect[i] + period_effect[j])
#        }
#		#print(c(i,j))
#    }
#(endtime <- Sys.time() - starttime)

    # fawns across all years
    # s_aah[sex, age(years), period(years)]
    for (t in 1:n_year) {
        s_aah[1, 1, t] <- exp(-sum(UCH[1,1:yr_end[1],yr_start[t]:yr_end[t]])) #antlerless fawns
        s_aah[2, 1, t] <- exp(-sum(UCH[2,1:yr_end[1],yr_start[t]:yr_end[t]]))  #antlered fawns
    }
    # first year across all ages from sex-specific process model
    # antlerless yearlings and older first year
    for(a in 2:n_agef) {
        #!#! s_aah[1, a, 1] <- S0[1, yr_end_indx[1], a * intvl_step_yr]
        s_aah[1, a, 1] <- S0[1, yr_end[a], yr_end[1]]
    }
    #antlered yearlings and older first year
    for(a in 2:n_agem) {
        #!#! s_aah[1, a, 1] <- S0[1, yr_end_indx[1], a * intvl_step_yr]
        s_aah[2, a, 1] <- S0[2, yr_end[a], yr_end[1]]
    }

    #subsequent years all ages from process model
    for (t in 2:n_year) {
	# age = 2 (denominator must come from fawns survival calculation)
		s_aah[1, 2, t] <- S0[1, yr_end[2], yr_end[t]]/
                                s_aah[1, 1, t-1]
        s_aah[2, 2, t] <- S0[2, yr_end[2], yr_end[t]]/
                                s_aah[2, 1, t-1]
	# age > 2
        for (a in 3:n_agef) {
            s_aah[1, a, t] <- S0[1, yr_end[a], yr_end[t]]/
                                S0[1, yr_end[a-1], yr_end[t - 1]]
        }
        for(a in 3:n_agem) {
            s_aah[2, a, t] <- S0[2, yr_end[a], yr_end[t]]/
                                S0[2, yr_end[a-1], yr_end[t - 1]]
        }
    }

  returnType(double(3))
  return(s_aah[1:2,1:n_agef,1:n_year])
})

Ccalc_surv_aah <- compileNimble(calc_surv_aah)

nT_age = nT_age_surv
nT_period = nT_period_surv
beta0 = sus_beta0_survival
beta_sex = sus_beta_sex_survival
age_effect = age_effect_survival    
period_effect = period_effect_survival
yr_start = d_fit_season$yr_start
yr_end = d_fit_season$yr_end
intvl_step_yr = intvl_step_yr
n_year = n_year
n_agef = n_agef
n_agem = n_agem




# starttime <- Sys.time()
# sn_sus <- Ccalc_surv_aah(
#     nT_age = nT_age_surv,
#     nT_period = nT_period_surv,
#     beta0 = sus_beta0_survival,
#     beta_sex = sus_beta_sex_survival,
#     age_effect = age_effect_survival,        # length = 962
#     period_effect = period_effect_survival,  # length = 1564
#	  yr_start = d_fit_season$yr_start,
#     yr_end = d_fit_season$yr_end,
#     intvl_step_yr = intvl_step_yr,
#     n_year = n_year,
#     n_agef = n_agef,
#     n_agem = n_agem)
# (endtime1 <- Sys.time() - starttime)


# starttime <- Sys.time()
# sn_inf <- calc_surv_aah(
#         nT_age = nT_age_surv,
#         nT_period = nT_period_surv,
#         beta0 = inf_beta0_survival,
#         beta_sex = sus_beta_sex_survival,
#         age_effect = age_effect_survival,
#         period_effect = period_effect_survival,
#         yr_end_indx = d_fit_season$yr_end,
#         intvl_step_yr = intvl_step_yr,
#         n_year = n_year,
#         n_agef = n_agef,
#         n_agem = n_agem)
# endtime2 <- Sys.time() - starttime


# sn_sus[1:2,1:n_agef,1:n_year] <- Ccalc_surv_aah(
#     nT_age = nT_age_surv,
#     nT_period = nT_period_surv,
#     beta0 = sus_beta0_survival,
#     beta_sex = sus_beta_sex_survival,
#     age_effect = age_effect_survival,
#     period_effect = period_effect_survival,
#     yr_end_indx = d_fit_season$yr_end,
#     intvl_step_yr = intvl_step_yr,
#     n_year = n_year,
#     n_agef = n_agef,
#     n_agem = n_agem)

# sn_inf[1:2,1:n_agef,1:n_year] <- calc_surv_aah(
#         nT_age = nT_age_surv,
#         nT_period = nT_period_surv,
#         beta0 = inf_beta0_survival,
#         beta_sex = sus_beta_sex_survival,
#         age_effect = age_effect_survival,
#         period_effect = period_effect_survival,
#         yr_end_indx = d_fit_season$yr_end,
#         intvl_step_yr = intvl_step_yr,
#         n_year = n_year,
#         n_agef = n_agef,
#         n_agem = n_agem)

#######################################################################
###
### Function to calculate annual survival based on age effects and 
### period effects
###
#######################################################################

calc_surv_harvest <- nimble::nimbleFunction(
    run = function(
        ### argument type declarations
        nT_age = double(0),
        nT_period = double(0),
        beta0 = double(0),
        beta_sex = double(0),
        age_effect = double(1),
        period_effect = double(1),
        #yr_end_indx = double(1),
        intvl_step_yr = double(0),
        n_year = double(0),
        n_agef = double(0),
        n_agem = double(0),
        pre_hunt_end = double(1),
        ng_start = double(1),
        gun_start = double(1),
        gun_end = double(1),
        ng_end = double(1),
        yr_start = double(1),
        yr_end = double(1),
        p_nogun_f = double(0),
        p_nogun_m = double(0),
        p_gun_f = double(0),
        p_gun_m = double(0)
        ) {

    ###################################################################
    ###
    ### General Survival Surfaces for Susceptible/Infected Individuals
    ###
    ###################################################################

    # s_hunt[sex,age,period]
    S0 <- nimArray(NA,c(2,nT_age,nT_period))
    UCH <- nimArray(NA,c(2,nT_age,nT_period))
    UCH_hunt <- nimArray(NA,c(2,nT_age,nT_period))
    S0_hunt <- nimArray(NA,c(2,nT_age,nT_period))
    s_hunt <- nimArray(NA,c(2,n_agef,n_year))

    for(i in 1:nT_age) {
        for(j in 1:nT_period) {
            UCH[1,i,j] <- exp(beta0 + beta_sex + age_effect[i] + period_effect[j])
            UCH[2,i,j] <- exp(beta0 + age_effect[i] + period_effect[j])
        }
    }
    for(i in 1:n_year){
        for(j in yr_start[i]:(ng_start[i]-1)){
            UCH_hunt[1,1:nT_age,j] <- UCH[1,1:nT_age,j]
            UCH_hunt[2,1:nT_age,j] <- UCH[2,1:nT_age,j]
        }
        for(j in (ng_end[i] + 1):(yr_end[i])){
            UCH_hunt[1,1:nT_age,j] <- UCH[1,1:nT_age,j]
            UCH_hunt[2,1:nT_age,j] <- UCH[2,1:nT_age,j]
        }
        for(j in ng_start[i]:(gun_start[i]-1)){
            UCH_hunt[1,1:nT_age,j] <- UCH[1,1:nT_age,j] * p_nogun_f
            UCH_hunt[2,1:nT_age,j] <- UCH[2,1:nT_age,j] * p_nogun_m
        }
        #for(j in gun_start[i]:(gun_end[i]-1)){
        for(j in gun_start[i]:(gun_end[i])){
            UCH_hunt[1,1:nT_age,j] <- UCH[1,1:nT_age,j] * p_gun_f
            UCH_hunt[2,1:nT_age,j] <- UCH[2,1:nT_age,j] * p_gun_m
        }
        for(j in (gun_end[i] + 1):(ng_end[i])){
            UCH_hunt[1,1:nT_age,j] <- UCH[1,1:nT_age,j] * p_nogun_f
            UCH_hunt[2,1:nT_age,j] <- UCH[2,1:nT_age,j] * p_nogun_m
       }
    }
    for(i in 1:nT_age) {
        for(j in 1:nT_period) {
            S0[1,i,j] <- exp(-sum(UCH[1,1:i,1:j]))
            S0[2,i,j] <- exp(-sum(UCH[2,1:i,1:j]))
            S0_hunt[1,i,j] <- exp(-sum(UCH_hunt[1,1:i,1:j]))
            S0_hunt[2,i,j] <- exp(-sum(UCH_hunt[2,1:i,1:j]))
        }
    }

    # fawns across all years
    # s_aah[sex, age(years), period(years)]
    for (t in 1:n_year) {
        #antlerless fawns
        s_hunt[1, 1, t] <- exp(-sum(UCH_hunt[1,
                                             1:ng_end[1],
                                             yr_start[t]:ng_end[t]]))
        #antlered fawns
        s_hunt[2, 1, t] <- exp(-sum(UCH_hunt[2,
                                             1:ng_end[1],
                                             yr_start[t]:ng_end[t]]))
    }
    # first year across all ages from sex-specific process model
    # antlerless yearlings and older first year
    for(a in 2:n_agef) {
        s_hunt[1, a ,1] <- S0_hunt[1, ng_end[a], ng_end[1]]
    }
    #antlered yearlings and older first year harvest survival
    for(a in 2:n_agem) {
        s_hunt[2, a, 1] <- S0_hunt[2, ng_end[a], ng_end[1]]
   }

    #subsequent years all ages from process model
    for (t in 2:n_year) {
	# age == 2 (denominator must come from fawns survival calculation)
#############GES fixed
        # s_hunt[1,2,t] <- exp(log(S0_hunt[1, ng_end[2], ng_end[t]]) - 
        #                         sum(UCH[1, 1:yr_end[1], yr_start[t - 1]:yr_end[t - 1]]) + 
        #                         sum(UCH_hunt[1, 1:yr_end[1], yr_start[t - 1]:yr_end[t - 1]]))/
        #                  exp(-sum(UCH_hunt[1, 1:yr_end[1], yr_start[t - 1]:yr_end[t - 1]]))

        # s_hunt[2,2,t] <- exp(log(S0_hunt[2 ,ng_end[2], ng_end[t]]) - 
        #                      sum(UCH[2, 1:yr_end[1], yr_start[t - 1]:yr_end[t - 1]]) +
        #                      sum(UCH_hunt[2, 1:yr_end[1], yr_start[t - 1]:yr_end[t - 1]]))/
        #                  exp(-sum(UCH_hunt[2, 1:yr_end[1], yr_start[t - 1]:yr_end[t - 1]]))

#############ACK fixed
        s_hunt[1,2,t] <- exp(log(S0_hunt[1, ng_end[2], ng_end[t]]) - 
                                sum(UCH[1, 1:yr_end[1], yr_start[t - 1]:yr_end[t - 1]]) + 
                                sum(UCH_hunt[1, 1:yr_end[1], yr_start[t - 1]:yr_end[t - 1]]))/
                          S0[1, ng_end[1], yr_end[t - 1]]

        s_hunt[2,2,t] <- exp(log(S0_hunt[2 ,ng_end[2], ng_end[t]]) - 
                             sum(UCH[2, 1:yr_end[1], yr_start[t - 1]:yr_end[t - 1]]) +
                             sum(UCH_hunt[2, 1:yr_end[1], yr_start[t - 1]:yr_end[t - 1]]))/
                          S0[2, ng_end[1], yr_end[t - 1]]


	# age > 2
    for (a in 3:n_agef) {

            s_hunt[1, a, t] <- exp(log(S0_hunt[1, ng_end[a], ng_end[t]]) +
                                 log(S0[1, yr_end[a - 1], yr_end[t - 1]]) -
                                 log(S0_hunt[1, yr_end[a - 1], yr_end[t - 1]]))/
                              S0[1, yr_end[a - 1], yr_end[t - 1]]
        }
        for(a in 3:n_agem) {
            s_hunt[2,a,t] <- exp(log(S0_hunt[2, ng_end[a], ng_end[t]]) +
                                 log(S0[2, yr_end[a - 1],yr_end[t - 1]]) -
                                 log(S0_hunt[2, yr_end[a - 1], yr_end[t - 1]]))/
                             S0[2, yr_end[a - 1], yr_end[t - 1]]
       }
    }
  returnType(double(3))
  return(s_hunt[1:2, 1:n_agef, 1:n_year])
})
Ccalc_surv_harvest <- compileNimble(calc_surv_harvest)



        pre_hunt_end = d_fit_season$pre_hunt_end
        ng_start = d_fit_season$ng_start
        gun_start = d_fit_season$gun_start
        gun_end = d_fit_season$gun_end
        # gun_end = d_fit_season$gun_start
        ng_end = d_fit_season$ng_end
        yr_start = d_fit_season$yr_start
        yr_end = d_fit_season$yr_end
        p_nogun_f = p_ng_f
        p_nogun_m = p_ng_m
        p_gun_f = p_gun_f
        p_gun_m = p_gun_m

# starttime <- Sys.time()
# sh_sus <- Ccalc_surv_harvest(nT_age = nT_age_surv,
#         nT_period = nT_period_surv,
#         beta0 = sus_beta0_survival,
#         beta_sex = sus_beta_sex_survival,
#         age_effect = age_effect_survival,
#         period_effect = period_effect_survival,
#         #yr_end_indx = d_fit_season$yr_end,
#         intvl_step_yr = intvl_step_yr,
#         n_year = n_year,
#         n_agef = n_agef,
#         n_agem = n_agem,
#         pre_hunt_end = d_fit_season$pre_hunt_end,
#         ng_start = d_fit_season$ng_start,
#         gun_start = d_fit_season$gun_start,
#         #gun_end = d_fit_season$gun_end,
#         gun_end = d_fit_season$gun_start,
#         ng_end = d_fit_season$ng_end,
#         yr_start = d_fit_season$yr_start,
#         yr_end = d_fit_season$yr_end,
#         p_nogun_f = p_ng_f,
#         p_nogun_m = p_ng_m,
#         p_gun_f = p_gun_f,
#         p_gun_m = p_gun_m
#         )
# (endtime3 <- Sys.time() - starttime)



# sh_sus[1:2,1:n_agef,1:n_year] <- Ccalc_surv_harvest(nT_age = nT_age_surv,
#         nT_period = nT_period_surv,
#         beta0 = sus_beta0_survival,
#         beta_sex = sus_beta_sex_survival,
#         age_effect = age_effect_survival,
#         period_effect = period_effect_survival,
#         yr_end_indx = d_fit_season$yr_end,
#         intvl_step_yr = intvl_step_yr,
#         n_year = n_year,
#         n_agef = n_agef,
#         n_agem = n_agem,
#         pre_hunt_end = d_fit_season$pre_hunt_end,
#         ng_start = d_fit_season$ng_start,
#         gun_start = d_fit_season$gun_start,
#         gun_end = d_fit_season$gun_end,
#         ng_end = d_fit_season$ng_end,
#         yr_start = d_fit_season$yr_start,
#         yr_end = d_fit_season$yr_end,
#         p_nogun_f = p_ng_f,
#         p_nogun_m = p_nogun_m,
#         p_gun_f = p_gun_f,
#         p_gun_m = p_gun_m
#         )

#######################################################################
###
### Function to calculatw probability of infection
### based on FOI age and period effects
### Weekly Version
###
#######################################################################

calc_infect_prob <- nimbleFunction(
  run = function(age_lookup_f = double(1),
                 age_lookup_m = double(1),
                 Nage_lookup = double(0),
                 f_age = double(1),
                 m_age = double(1),
                 f_period = double(1),
                 m_period = double(1),
                 nT_period_foi = double(0)) {

    p <- nimArray(value = 0, c(2,Nage_lookup,nT_period_foi))
    gam <-nimArray(value = 0, c(2,Nage_lookup,nT_period_foi))

    for (t in 1:nT_period_foi) {
      for (a in 1:Nage_lookup) {
        gam[1,a,t] <- f_age[age_lookup_f[a]] + f_period[t]
        gam[2,a,t] <- m_age[age_lookup_m[a]] + m_period[t]
        p[1,a,t] <- 1 - exp(-sum(exp(gam[1,1:a,t])))
        p[2,a,t] <- 1 - exp(-sum(exp(gam[2,1:a,t])))
      }
    }

    # browser()
    returnType(double(3))
    return(p[1:2,1:Nage_lookup,1:nT_period_foi])
  })

#testing state.transition function as R function
# psi <- calc_infect_prob(age_lookup_f = age_lookup_f,
#                         age_lookup_m = age_lookup_m,
#                         Nage_lookup = Nage_lookup,
#                         f_age = f_age_foi,
#                         m_age = m_age_foi,
#                         f_period = f_period_foi,
#                         m_period = m_period_foi,
#                         nT_period_foi = nT_period_foi)
# head(psi[1,,])
