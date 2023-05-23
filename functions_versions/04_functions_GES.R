
#######################################################################
###
### Function to calculate Annual survival probability based on 
### age effects and period effects
###
#######################################################################

calc_surv_aah <- nimble::nimbleFunction(
    run = function(
        ### argument type declarations
        #nT_age = double(0),
        #nT_period = double(0),
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
    S0 <- nimArray(NA, c(2, n_agef, n_year))
    s_aah <- nimArray(NA, c(2, n_agef, n_year))
		
    # for age 1
    for(j in 1:n_year) {
        cll_uch <- beta0 +
                    rep(age_effect[1:yr_end[1]],
                        times = intvl_step_yr) +
                    rep(period_effect[yr_start[j]:yr_end[j]],
                        each = intvl_step_yr)

        S0[1, 1, j] <- exp(-sum(exp(cll_uch + beta_sex)))
        S0[2, 1, j] <- exp(-sum(exp(cll_uch)))
    }

    # for age 2+
    for(i in 2:n_agef) {
        for(j in 1:n_year) {
			cll_uch <- beta0 +
                       rep(age_effect[1:yr_end[i]],
                           times = yr_end[j]) +
                       rep(period_effect[1:yr_end[j]],
                           each = yr_end[i])

            S0[1, i, j] <- exp(-sum(exp(cll_uch + beta_sex)))
            S0[2, i, j] <- exp(-sum(exp(cll_uch)))
       }
	}
    # fawns across all years
    # s_aah[sex, age(years), period(years)]
    for (t in 1:n_year) {
        s_aah[1, 1, t] <- S0[1, 1, t] #antlerless fawns
        s_aah[2, 1, t] <- S0[2, 1, t] #antlered fawns
    }
    # first year across all ages from sex-specific process model
    # antlerless yearlings and older first year
    for(a in 2:n_agef) {
        s_aah[1, a, 1] <- S0[1, a, 1]
    }
    #antlered yearlings and older first year
    for(a in 2:n_agem) {
        s_aah[2, a, 1] <- S0[2, a, 1]
    }
    #first year all ages from process model
    for (t in 2:n_year) {
        for (a in 2:n_agef) {
            s_aah[1, a, t] <- S0[1, a, t] /
                                S0[1, a - 1, t - 1]
        }
        for(a in 2:n_agem) {
            s_aah[2, a, t] <- S0[2, a, t]/
                                S0[2, a - 1, t - 1]
        }
    }

  returnType(double(3))
  return(s_aah[1:2, 1:n_agef, 1:n_year])
})

Ccalc_surv_aah <- compileNimble(calc_surv_aah)

# starttime <- Sys.time()
# sn_sus <- Ccalc_surv_aah(
#     #nT_age = nT_age_surv,
#     #nT_period = nT_period_surv,
#     beta0 = sus_beta0_survival,
#     beta_sex = sus_beta_sex_survival,
#     age_effect = age_effect_survival,
#     period_effect = period_effect_survival,
#     yr_start = d_fit_season$yr_start,
#     yr_end = d_fit_season$yr_end,
#     intvl_step_yr = intvl_step_yr,
#     n_year = n_year,
#     n_agef = n_agef,
#     n_agem = n_agem)
# (endtime1 <- Sys.time() - starttime)

# save(endtime1,file="endtime1.Rdata")

# starttime <- Sys.time()
# sn_inf <- Ccalc_surv_aah(
#         #nT_age = nT_age_surv,
#         #nT_period = nT_period_surv,
#         beta0 = inf_beta0_survival,
#         beta_sex = inf_beta_sex_survival,
#         age_effect = age_effect_survival,
#         period_effect = period_effect_survival,
#         yr_start = d_fit_season$yr_start,
#         yr_end = d_fit_season$yr_end,
#         intvl_step_yr = intvl_step_yr,
#         n_year = n_year,
#         n_agef = n_agef,
#         n_agem = n_agem)
# endtime2 <- Sys.time() - starttime

# save(endtime2,file="endtime2.Rdata")


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
        #nT_age = double(0),
        #nT_period = double(0),
        beta0 = double(0),
        beta_sex = double(0),
        age_effect = double(1),
        period_effect = double(1),
        #yr_end_indx = double(1),
        intvl_step_yr = double(0),
        n_year = double(0),
        n_agef = double(0),
        n_agem = double(0),
        #pre_hunt_end = double(1),
        ng_start = double(1),
        gun_start = double(1),
        gun_end = double(1),
        ng_end = double(1),
        yr_start = double(1),
        yr_end = double(1),
		#p_vec_f = double(1),
		#p_vec_m = double(1)
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

    # s_aah[sex,age,period]
    S0 <- nimArray(NA,c(2,n_agef,n_year))
    #UCH_hunt <- nimArray(NA,c(2,nT_age,nT_period))
    #S0_hunt <- nimArray(NA,c(2,nT_age,nT_period))
    S0_hunt <- nimArray(NA,c(2,n_agef,n_year))
    S0_hunt2 <- nimArray(NA,c(2,n_agef,n_year))
    s_hunt <- nimArray(NA,c(2,n_agef,n_year))
	p_vec_f <- nimNumeric(length = intvl_step_yr*n_year, init = FALSE)
	p_vec_m <- nimNumeric(length = intvl_step_yr*n_year, init = FALSE)
	
	for(i in 1:n_year){
		p_vec_f[yr_start[i]:yr_end[i]] <- c(rep(1, ng_start[i] - yr_start[i]),
                                            rep(p_nogun_f, gun_start[i] - ng_start[i]),
                                            rep(p_gun_f,length(gun_start[i]:gun_end[i])),
                                            rep(p_nogun_f,ng_end[i] - gun_end[i]),
                                            rep(1,yr_end[i] - ng_end[i]))
			
		p_vec_m[yr_start[i]:yr_end[i]] <- c(rep(1, ng_start[i] - yr_start[i]),
                                            rep(p_nogun_m, gun_start[i] - ng_start[i]),
                                            rep(p_gun_m,length(gun_start[i]:gun_end[i])),
                                            rep(p_nogun_m, ng_end[i] - gun_end[i]),
                                            rep(1, yr_end[i] - ng_end[i]))
	}
	
    # for age 1
    for(j in 1:n_year) {
        l_uch <- beta0 +
                    rep(age_effect[yr_start[1]:yr_end[1]],
                        times = intvl_step_yr) +
                    rep(period_effect[yr_start[j]:yr_end[j]],
                        each = intvl_step_yr)

        l_uch_hunt <- beta0 +
                      rep(age_effect[yr_start[1]:ng_end[1]],
                          times = ng_end[j] + 1 - yr_start[j]) +
                      rep(period_effect[yr_start[j]:ng_end[j]],
                          each = ng_end[1])

        S0[1,1,j] <- exp(-sum(exp(l_uch + beta_sex)))
        S0[2,1,j] <- exp(-sum(exp(l_uch)))
        S0_hunt[1,1,j] <- exp(-sum(exp(l_uch_hunt + beta_sex) * 
                                   rep(p_vec_f[yr_start[j]:ng_end[j]],
                                       each = ng_end[1])))
        S0_hunt[2,1,j] <- exp(-sum(exp(l_uch_hunt) * 
                                   rep(p_vec_m[yr_start[j]:ng_end[j]],
                                       each = ng_end[1])))
        
        S0_hunt2[1,1,j] <- exp(-sum(exp(l_uch + beta_sex) *
                                    rep(p_vec_f[yr_start[j]:yr_end[j]],
                                    each = yr_end[1])))

        S0_hunt2[2,1,j] <- exp(-sum(exp(l_uch) *
                                    rep(p_vec_m[yr_start[j]:yr_end[j]],
                                        each = yr_end[1])))
    }

# for age 2+
    for(i in 2:n_agef) {
        for(j in 1:n_year) {
			l_uch <- beta0 +
                     rep(age_effect[1:yr_end[i]],
                         times = yr_end[j]) +
                     rep(period_effect[1:yr_end[j]],
                         each = yr_end[i])

			l_uch_hunt <- beta0 +
                          rep(age_effect[1:ng_end[i]],
                              times = ng_end[j]) +
                          rep(period_effect[1:ng_end[j]],
                              each = ng_end[i])

            S0[1,i,j] <- exp(-sum(exp(l_uch + beta_sex)))
            S0[2,i,j] <- exp(-sum(exp(l_uch)))
            S0_hunt[1,i,j] <- exp(-sum(exp(l_uch_hunt + beta_sex) *
                                       rep(p_vec_f[1:ng_end[j]],
                                           each = ng_end[i])))
            S0_hunt[2,i,j] <- exp(-sum(exp(l_uch_hunt)*
                                       rep(p_vec_m[1:ng_end[j]],
                                           each = ng_end[i])))

			S0_hunt2[1,i,j] <- exp(-sum(exp(l_uch + beta_sex) *
                                        rep(p_vec_f[1:yr_end[j]], each = yr_end[i])))
            S0_hunt2[2,i,j] <- exp(-sum(exp(l_uch) *
                                        rep(p_vec_m[1:yr_end[j]],
                                            each = yr_end[i])))
       }
	}

    # fawns across all years
    # s_aah[sex, age(years), period(years)]
    for (t in 1:n_year) {
        s_hunt[1,1,t] <- S0_hunt[1,1,t] #antlerless fawns
        s_hunt[2,1,t] <- S0_hunt[2,1,t] #antlered fawns
    }
    # first year across all ages from sex-specific process model
    # antlerless yearlings and older first year
    for(a in 2:n_agef) {
        s_hunt[1,a,1] <- S0_hunt[1,a,1]
    }
    #antlered yearlings and older first year
    for(a in 2:n_agem) {
        s_hunt[2,a,1] <- S0_hunt[2,a,1]
    }

    #subsequent years all ages from process model
    for (t in 2:n_year) {
	# age = 2 (denominator must come from fawns survival calculation)
       #s_hunt[1,2,t] <- S0_hunt[1,2,t]/
       #                    s_hunt[1,1,t-1]
       #s_hunt[2,2,t] <- S0_hunt[2,2,t]/
       #                    s_hunt[2,1,t-1]
	   
	   #s_hunt[1,2,t] <- exp(log(S0_hunt[1,2,t])+log(S0[1,1,t-1])-log(S0_hunt2[1,1,t-1]))/S0_hunt2[1,1,t-1]
       #s_hunt[2,2,t] <- exp(log(S0_hunt[2,2,t])+log(S0[2,1,t-1])-log(S0_hunt2[2,1,t-1]))/S0_hunt2[2,1,t-1]

       s_hunt[1,2,t] <- exp(log(S0_hunt[1, 2, t]) +
                            log(S0[1, 1, t - 1]) -
	                        log(S0_hunt2[1, 1, t - 1])) /
	                    S0_hunt2[1, 1, t - 1]
	   
       s_hunt[2,2,t] <- exp(log(S0_hunt[2, 2, t]) +
                      	    log(S0[2, 1, t - 1]) -
                	        log(S0_hunt2[2, 1, t - 1]))/
                 	    S0_hunt2[2, 1, t - 1]

	# age > 2
        for (a in 3:n_agef) {
            #s_hunt[1,a,t] <- S0_hunt[1,a,t]/
            #                    S0[1,a-1,t-1]
            s_hunt[1,a,t] <- exp(log(S0_hunt[1, a, t]) +
                                 log(S0[1, a - 1, t - 1]) -
                                 log(S0_hunt2[1, a - 1, t - 1])) /
                             S0[1, a - 1, t - 1]
       }
        for(a in 3:n_agem) {
            #s_hunt[2,a,t] <- S0_hunt[2,a,t]/
            #                    S0[2,a-1,t-1]
            s_hunt[2,a,t] <- exp(log(S0_hunt[2, a, t]) +
                                 log(S0[2, a - 1, t - 1]) -
                                 log(S0_hunt2[2, a - 1, t - 1])) /
                             S0[2, a - 1, t - 1]
       }
    }
  returnType(double(3))
  return(s_hunt[1:2, 1:n_agef, 1:n_year])
})
Ccalc_surv_harvest <- compileNimble(calc_surv_harvest)

# starttime <- Sys.time()
# sh_sus <- Ccalc_surv_harvest(
# 		#nT_age = nT_age_surv,
#         #nT_period = nT_period_surv,
#         beta0 = sus_beta0_survival,
#         beta_sex = sus_beta_sex_survival,
#         age_effect = age_effect_survival,
#         period_effect = period_effect_survival,
#         #yr_end_indx = d_fit_season$yr_end,
#         intvl_step_yr = intvl_step_yr,
#         n_year = n_year,
#         n_agef = n_agef,
#         n_agem = n_agem,
#         #pre_hunt_end = d_fit_season$pre_hunt_end,
#         ng_start = d_fit_season$ng_start,
#         gun_start = d_fit_season$gun_start,
#         #gun_end = d_fit_season$gun_end,
#         gun_end = d_fit_season$gun_start,
#         ng_end = d_fit_season$ng_end,
#         yr_start = d_fit_season$yr_start,
#         yr_end = d_fit_season$yr_end,
# 		#p_vec_f = p_vec_f,
# 		#p_vec_m = p_vec_m
#         p_nogun_f = p_ng_f,
#         p_nogun_m = p_ng_m,
#         p_gun_f = p_gun_f,
#         p_gun_m = p_gun_m
#         )
# (endtime3 <- Sys.time() - starttime)

# save(endtime3,file="endtime3.Rdata")



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
                 n_year = double(0)) {

    p <- nimArray(value = 0, c(2,Nage_lookup,n_year))
    gam <-nimArray(value = 0, c(2,Nage_lookup,n_year))

    for (t in 1:n_year) {
      for (a in 1:Nage_lookup) {
        gam[1,a,t] <- f_age[age_lookup_f[a]] + f_period[t]
        gam[2,a,t] <- m_age[age_lookup_m[a]] + m_period[t]
        p[1,a,t] <- 1 - exp(-sum(exp(gam[1,1:a,t])))
        p[2,a,t] <- 1 - exp(-sum(exp(gam[2,1:a,t])))
      }
    }

    # browser()
    returnType(double(3))
    return(p[1:2,1:Nage_lookup,1:n_year])
  })

###testing state.transition function as R function
starttime <- Sys.time()
psi <- calc_infect_prob(age_lookup_f = age_lookup_f,
                        age_lookup_m = age_lookup_m,
                        Nage_lookup = Nage_lookup,
                        f_age = f_age_foi,
                        m_age = m_age_foi,
                        f_period = f_period_foi,
                        m_period = m_period_foi,
                        n_year = n_year)
endtime4 <- Sys.time() - starttime
save(endtime4,file="endtime4.Rdata")

# head(psi[1,,])