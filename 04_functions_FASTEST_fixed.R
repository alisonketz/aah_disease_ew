
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

	#---------------------------------------------------------------
	# calculate hazards
	#---------------------------------------------------------------
    #S0 <- nimArray(NA,c(2,n_agef,n_year)) # don't need this anymore
    UCH <- nimArray(NA,c(2,nT_age,nT_period))
    s_aah <- nimArray(NA,c(2,n_agef,n_year))

   for(i in 1:nT_age) {
        for(j in 1:nT_period) {
            UCH[1, i, j] <- exp(beta0 + beta_sex + age_effect[i] + period_effect[j])
            UCH[2, i, j] <- exp(beta0 + age_effect[i] + period_effect[j])
        }
    }
	#---------------------------------------------------------------
	# calculate survival from cum haz
	#---------------------------------------------------------------
    for (t in 1:n_year) {	
        for (a in 1:n_agef) {
            s_aah[1, a, t] <- exp(-sum(UCH[1,yr_start[a]:yr_end[a],yr_start[t]:yr_end[t]]))
        }
        for(a in 1:n_agem) {
            s_aah[2, a, t] <- exp(-sum(UCH[2,yr_start[a]:yr_end[a],yr_start[t]:yr_end[t]]))
        }
    }
  returnType(double(3))
  return(s_aah[1:2,1:n_agef,1:n_year])
})

Ccalc_surv_aah <- compileNimble(calc_surv_aah)


# starttime <- Sys.time()
# sn_sus <- Ccalc_surv_aah(
# 	nT_age = n_agef*intvl_step_yr,
#     nT_period = n_year*intvl_step_yr,
#     beta0 = sus_beta0_survival,
#     beta_sex = sus_beta_sex_survival,
#     age_effect = age_effect_survival,        # length = 962
#     period_effect = period_effect_survival,  # length = 1564
# 	  yr_start = d_fit_season$yr_start,
#     yr_end = d_fit_season$yr_end,
#     intvl_step_yr = intvl_step_yr,
#     n_year = n_year,
#     n_agef = n_agef,
#     n_agem = n_agem)
# (endtime1 <- Sys.time() - starttime)
# sn_sus[1,1:5,]
# sn_sus[2,1:5,]

# starttime <- Sys.time()
# sn_inf <- Ccalc_surv_aah(
# 	      nT_age = n_agef*intvl_step_yr,
#         nT_period = n_year*intvl_step_yr,
#         beta0 = inf_beta0_survival,
#         beta_sex = inf_beta_sex_survival,
#         age_effect = age_effect_survival,
#         period_effect = period_effect_survival,
#         yr_end = d_fit_season$yr_end,
#         yr_start = d_fit_season$yr_start,
#         intvl_step_yr = intvl_step_yr,
#         n_year = n_year,
#         n_agef = n_agef,
#         n_agem = n_agem)
# (endtime2 <- Sys.time() - starttime)
# sn_inf[1,3,]
# sn_inf[2,3,]


# sn_sus[1:2,1:n_agef,1:n_year] <- Ccalc_surv_aah(
#	  nT_age = n_agef*intvl_step_yr,
#     nT_period = n_year*intvl_step_yr,
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
#	      nT_age = n_agef*intvl_step_yr,
#         nT_period = n_year*intvl_step_yr,
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

	#---------------------------------------------------------------
	# initialize hazard array
	#---------------------------------------------------------------
    UCH <- nimArray(NA,c(2,nT_age,nT_period))
    UCH_hunt <- nimArray(NA,c(2,nT_age,nT_period))
    s_hunt <- nimArray(NA,c(2,n_agef,n_year))

	#---------------------------------------------------------------
	# calculate hazards
	#---------------------------------------------------------------
    for(i in 1:nT_age) {
        for(j in 1:nT_period) {
            UCH[1,i,j] <- exp(beta0 + beta_sex + age_effect[i] + period_effect[j])
            UCH[2,i,j] <- exp(beta0 + age_effect[i] + period_effect[j])
        }
    }

	#---------------------------------------------------------------
	# adjust hazards to remove harvest hazards
	#---------------------------------------------------------------
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
        for(j in gun_start[i]:(gun_end[i])){
            UCH_hunt[1,1:nT_age,j] <- UCH[1,1:nT_age,j] * p_gun_f
            UCH_hunt[2,1:nT_age,j] <- UCH[2,1:nT_age,j] * p_gun_m
        }
		if(gun_end[i]<ng_end[i]){
        for(j in (gun_end[i] + 1):(ng_end[i])){
            UCH_hunt[1,1:nT_age,j] <- UCH[1,1:nT_age,j] * p_nogun_f
            UCH_hunt[2,1:nT_age,j] <- UCH[2,1:nT_age,j] * p_nogun_m
       }}
    }
	
	#---------------------------------------------------------------
	# calculate survival from cum haz
	#---------------------------------------------------------------
    for (t in 1:n_year) {	
        for (a in 1:n_agef) {
            s_hunt[1, a, t] <- exp(-sum(UCH_hunt[1,yr_start[a]:yr_end[a],yr_start[t]:yr_end[t]]))
        }
        for(a in 1:n_agem) {
            s_hunt[2, a, t] <- exp(-sum(UCH_hunt[2,yr_start[a]:yr_end[a],yr_start[t]:yr_end[t]]))
        }
    }
	
  returnType(double(3))
  return(s_hunt[1:2,1:n_agef,1:n_year])
})
Ccalc_surv_harvest <- compileNimble(calc_surv_harvest)

# starttime <- Sys.time()
# sh_sus <- Ccalc_surv_harvest(nT_age = n_agef*intvl_step_yr,
#         nT_period = n_year*intvl_step_yr,
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
#         gun_end = d_fit_season$gun_end,
#         #gun_end = d_fit_season$gun_start,
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
#         p_nogun_m = p_ng_m,
#         p_gun_f = p_gun_f,
#         p_gun_m = p_gun_m
#         )

#######################################################################
###
### Function to calculate probability of infection
### based on FOI age and period effects
### Weekly Version
###
#######################################################################

calc_infect_prob <- nimbleFunction(
  run = function(age_lookup_f = double(1),
                 age_lookup_m = double(1),
                 Nage_lookup_f = double(0),
                 Nage_lookup_m = double(0),
                 n_agef = double(0),
                 n_agem = double(0),
                 yr_end = double(1),
                 f_age = double(1),
                 m_age = double(1),
                 f_period = double(1),
                 m_period = double(1),
                 n_year = double(0),
                 n_sex = double(0),
                 n_study_area = double(0),
                 space = double(0)) {

    p <- nimArray(value = 0, c(n_study_area, n_sex, Nage_lookup_f, n_year))
    gam <- nimArray(value = 0, c(n_study_area, n_sex,Nage_lookup_f, n_year))
    p_inf <- nimArray(value = 0, c(n_study_area, n_sex, n_agef, n_year))

    for (t in 1:n_year) {
        for (i in 1:Nage_lookup_f) {
            gam[1, 1, i, t] <- f_age[age_lookup_f[i]] + f_period[t]
            p[1, 1, i, t] <- exp(-sum(exp(gam[1, 1, 1:i, t])))
            gam[2, 1, i, t] <- f_age[age_lookup_f[i]] + f_period[t] + space
            p[2, 1, i, t] <- exp(-sum(exp(gam[1, 1, 1:i, t])))
        }
        for (i in 1:Nage_lookup_m) {
            gam[1, 2, i, t] <- m_age[age_lookup_m[i]] + m_period[t]
            p[1, 2, i, t] <- exp(-sum(exp(gam[1, 2, 1:i, t])))
            gam[2, 2, i, t] <- m_age[age_lookup_m[i]] + m_period[t] + space
            p[2, 2, i, t] <- exp(-sum(exp(gam[2, 2, 1:i, t])))
        }
    }


    for (t in 1:n_year) {
      for (i in 1:Nage_lookup_f) {
        gam[1, 1, i, t] <- f_age[age_lookup_f[i]] + f_period[t]
        p[1, 1, i, t] <- exp(-sum(exp(gam[1, 1, 1:i, t])))
        gam[2, 1, i, t] <- f_age[age_lookup_f[i]] + f_period[t] + space
        p[2, 1, i, t] <- exp(-sum(exp(gam[2, 1, 1:i, t])))
      }
      for (i in 1:Nage_lookup_m) {
        gam[1, 2, i, t] <- m_age[age_lookup_m[i]] + m_period[t]
        p[1, 2, i, t] <- exp(-sum(exp(gam[1, 2, 1:i, t])))
        gam[2, 2, i, t] <- m_age[age_lookup_m[i]] + m_period[t] + space
        p[2, 2, i, t] <- exp(-sum(exp(gam[2, 2, 1:i, t])))
      }

    }

    for(k in 1:n_study_area){
        #fawn probability of infection all years
        #both sexes
        for(t in 1:n_year){
            p_inf[k, 1, 1, t] <- 1 - p[k, 1, yr_end[1], t]
            p_inf[k, 2, 1, t] <- 1 - p[k, 2, yr_end[1], t]
        }
        #all non-fawn prob of infection first year
        for (a in 2:n_agef) {
            p_inf[k, 1, a, 1] <- 1 - p[k, 1,yr_end[a],1]
        }
        for (a in 2:n_agem) {
            p_inf[k, 2, a, 1] <- 1 - p[k, 2,yr_end[a],1]
        }

        #non-fawn infection probability all years except first year
        for (t in 2:n_year) {
            for (a in 2:n_agef) {
                p_inf[k, 1, a, t] <- 1 - (p[k, 1,yr_end[a],t]/p[k, 1, yr_end[a-1], t])
            }
            for (a in 2:n_agem) {
                p_inf[k, 2, a, t] <- 1 - (p[k, 2,yr_end[a],t]/p[k, 1, yr_end[a-1], t])
            }
        }
    }

    returnType(double(4))
    return(p_inf[1:n_study_area, 1:n_sex, 1:n_agef, 1:n_year])
  })

Ccalc_infect_prob <- compileNimble(calc_infect_prob)

# age_lookup_f = age_lookup_f_conv
# age_lookup_m = age_lookup_m_conv
# Nage_lookup = Nage_lookup_conv
# yr_end = d_fit_season$yr_end
# f_age = f_age_foi-2
# m_age = m_age_foi-2
# f_period = f_period_foi-2
# m_period = m_period_foi-2
###testing state.transition function as R function
# starttime <- Sys.time()
psi <- calc_infect_prob(age_lookup_f = age_lookup_f_conv,
                        age_lookup_m = age_lookup_m_conv,
                        Nage_lookup_f = Nage_lookup_conv,
                        Nage_lookup_m = length(age_lookup_m_conv),
                        n_agef = n_agef,
                        n_agem = n_agem,
                        yr_end = d_fit_season$yr_end,
                        f_age = f_age_foi-.5,
                        m_age = m_age_foi-.5,
                        f_period = f_period_foi-.5,
                        m_period = m_period_foi-.5,
                        n_year = n_year,
                        n_sex = n_sex,
                        n_study_area = n_study_area, 
                        space = -.55
                        )

# (endtime5 <- Sys.time() - starttime)
# psi[1,,,]
