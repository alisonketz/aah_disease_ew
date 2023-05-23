#################################
###
### Run model
###
#################################

modelcode <- nimbleCode({

  #######################################
  #### Initial population, currently 
  ### based on empirical bayes approach 
  ### of using the first year of data
  ########################################

  #should this be different for pos/neg or m/f ?
  for(i in 1:2){
    tau_pop[i] ~ dgamma(1,1)

  }
  # tau_pop_sus ~ dgamma(1,1)
  # tau_pop_inf ~ dgamma(1,1)

  for (a in 1:n_agef) {
    #Initial population structure pop[sex,age,year] for susceptible deer
    llpop_sus[1, a, 1] ~ dnorm(f_logpop_sus[a], tau_pop[1])
    pop_sus[1, a, 1] <- exp(llpop_sus[1, a, 1])

    #Initial population structure pop[year=1,sex=i,age=a]
    llpop_inf[1, a, 1] ~ dnorm(f_logpop_inf[a], tau_pop[2])
    pop_inf[1, a, 1] <- exp(llpop_inf[1, a, 1])
  }

  for (a in 1:n_agem) {
    #Initial population structure pop[year=1,sex=i,age=a] for susceptible deer
    llpop_sus[2, a, 1] ~ dnorm(m_logpop_sus[a], tau_pop[1])
    pop_sus[2, a, 1] <- exp(llpop_sus[2, a, 1])

    #Initial population structure pop for infected deer
    llpop_inf[2, a, 1] ~ dnorm(m_logpop_inf[a], tau_pop[2])
    pop_inf[2, a, 1] <- exp(llpop_inf[2, a, 1])
  }

  ############################
  ####Reporting Rates
  ############################

  report_overall ~ dbeta(report_hyp_all[1], report_hyp_all[2])
  for(t in 1:22){#1992-2014
    report[t]  <- report_overall
  }
  for(t in 23:27){ #2015-2020
    report[t] ~ dbeta(report_hyp_y[t - 22, 1], report_hyp_y[t - 22, 2])
  }
  report[28]  <- report_overall #2021

  ############################
  #### Fecundity
  ############################
  # fec ~ dgamma(5, 4)
  
  ################
  # option 1
  ################

  mu_fec ~ dnorm(0, 1)
  fec_prec_eps ~ dgamma(1, 1)

  #Observations of fawns & does overall from the 3 counties
  for(t in 1:n_year_fec_early) {
    fec_epsilon[t] ~ dnorm(0, fec_prec_eps)
    fec[t] <- exp(mu_fec + fec_epsilon[t])
    Nfawn[t] ~ dpois(fec[t] * Ndoe[t])
  }

  #for 2017:2021
  for(t in (n_year_fec_early + 1):n_year){
    fec[t] ~ dgamma(obs_ct_fd_alpha[t], obs_ct_fd_beta[t])
    # fec[t] ~ dlnorm(obs_ct_fd[t-n_year_fec_early],1/(obs_ct_fdsd[t-n_year_fec_early]^2))
  }

  ###################################################
  #### Non-Hunting Season Survival Susceptibles
  ###################################################
  # age_effect = age_effect, 
  # period_effect = period_effect, 
  # sus_beta0 = sus_beta0,
  # sus_beta_sex = sus_beta_sex,
  # inf_beta0 = inf_beta0,
  # inf_beta_sex = inf_beta_sex

    ## constant test
    ## sn_sus ~ dbeta(300,100)

    ## long-term mean cloglog survival natural (sn)
    ## tau_sn_sus ~ dgamma(10, 1)
    # mu_sn_sus[1] ~ dnorm(cloglog(.05), 10)
    # mu_sn_sus[2] ~ dnorm(cloglog(.15), 10)
    # for (t in 1:n_year) {
    #     for(a in 1:n_agef) {
    #       cll_sn_sus[1, a, t] ~ dnorm(mu_sn_sus[1], 10)
    #       sn_sus[1, a, t] <- exp(-exp(cll_sn_sus[1, a, t]))
    #     }
    #     for(a in 1:n_agem) {
    #         cll_sn_sus[2, a, t] ~ dnorm(mu_sn_sus[2], 10)
    #         sn_sus[2, a, t] <- exp(-exp(cll_sn_sus[2, a, t]))
    #     }
    # }

  ###################################################
  #### Non-Hunting Season Survival Infected
  ###################################################
    
    ### sn_inf ~ dbeta(150,100)
    ### tau_sn_inf ~ dgamma(10, 1)
    # mu_sn_inf[1] ~ dnorm(cloglog(.2), 10)
    # mu_sn_inf[2] ~ dnorm(cloglog(.3), 10)
    # for (t in 1:n_year) {
    #     for(a in 1:n_agef){
    #       cll_sn_inf[1, a, t] ~ dnorm(mu_sn_inf[1], 10)
    #       sn_inf[1, a, t] <- exp(-exp(cll_sn_inf[1, a, t]))
    #     }
    #     for(a in 1:n_agem){
    #         cll_sn_inf[2, a, t] ~ dnorm(mu_sn_inf[2], 10)
    #         sn_inf[2, a, t] <- exp(-exp(cll_sn_inf[2, a, t]))
    #     }
    # }


  ###################################################
  #### Hunting Season Survival Susceptibles
  ###################################################

    # mu_sh_sus[1] ~ dnorm(cloglog(.4), 10)
    # mu_sh_sus[2] ~ dnorm(cloglog(.5), 10)
    # for (t in 1:n_year) {
    #     for(a in 1:n_agef) {
    #       cll_sh_sus[1, a, t] ~ dnorm(mu_sh_sus[1], 10)
    #       sh_sus[1, a, t] <- exp(-exp(cll_sh_sus[1, a, t]))
    #     }
    #     for(a in 1:n_agem) {
    #         cll_sh_sus[2, a, t] ~ dnorm(mu_sh_sus[2], 10)
    #         sh_sus[2, a, t] <- exp(-exp(cll_sh_sus[2, a, t]))
    #     }
    # }

  # # ###################################################
  # # #### Hunting Season Survival Infected
  # # ###################################################

  #   ### sh_inf ~ dbeta(150, 100)
  #   ### tau_sh_inf ~ dgamma(1, 1)
    # mu_sh_inf[1] ~ dnorm(cloglog(.4), 10)
    # mu_sh_inf[2] ~ dnorm(cloglog(.5), 10)
    # for (t in 1:(n_year)) {
    #     for(a in 1:n_agef) {
    #       cll_sh_inf[1, a, t] ~ dnorm(mu_sh_inf[1], 10)
    #       sh_inf[1, a, t] <- exp(-exp(cll_sh_inf[1, a, t]))
    #     }
    #     for(a in 1:n_agem) {
    #         cll_sh_inf[2, a, t] ~ dnorm(mu_sh_inf[2], 10)
    #         sh_inf[2, a, t] <- exp(-exp(cll_sh_inf[2, a, t]))
    #     }
    # }


  ###################################################
  #### Overall Survival Susceptibles
  ###################################################

  sn_sus[1:2,1:n_agef,1:n_year] <- calc_surv_aah(
          nT_age = nT_age_surv,
          nT_period = nT_period_surv,
          beta0 = sus_beta0_survival,
          beta_sex = sus_beta_sex_survival,
          age_effect = age_effect_survival[1:nT_age_surv],
          period_effect = period_effect_survival[1:nT_period_surv],
          yr_start = yr_start[1:n_year],
          yr_end = yr_end[1:n_year],
          intvl_step_yr = intvl_step_yr,
          n_year = n_year,
          n_agef = n_agef,
          n_agem = n_agem)

  ###################################################
  #### Overall Survival CWD Infected
  ###################################################

  sn_inf[1:2,1:n_agef,1:n_year] <- calc_surv_aah(
          nT_age = nT_age_surv,
          nT_period = nT_period_surv,
          beta0 = inf_beta0_survival,
          beta_sex = inf_beta_sex_survival,
          age_effect = age_effect_survival[1:nT_age_surv],
          period_effect = period_effect_survival[1:nT_period_surv],
          yr_start = yr_start[1:n_year],
          yr_end = yr_end[1:n_year],
          intvl_step_yr = intvl_step_yr,
          n_year = n_year,
          n_agef = n_agef,
          n_agem = n_agem)

  ###################################################
  #### Hunting Survival Susceptibles
  ###################################################

  sh_sus[1:2,1:n_agef,1:n_year] <- calc_surv_harvest(
          nT_age = nT_age_surv, 
          nT_period = nT_period_surv,
          beta0 = sus_beta0_survival,
          beta_sex = sus_beta_sex_survival,
          age_effect = age_effect_survival[1:nT_age_surv],
          period_effect = period_effect_survival[1:nT_period_surv],
          intvl_step_yr = intvl_step_yr,
          n_year = n_year,
          n_agef = n_agef,
          n_agem = n_agem,
          ng_start = ng_start[1:n_year],
          gun_start = gun_start[1:n_year],
          gun_end = gun_end[1:n_year],
          ng_end = ng_end[1:n_year],
          yr_start = yr_start[1:n_year],
          yr_end = yr_end[1:n_year],
          p_nogun_f = p_ng_f,
          p_nogun_m = p_ng_m,
          p_gun_f = p_gun_f,
          p_gun_m = p_gun_m
          )

  ###################################################
  #### Hunting Survival Infected
  ###################################################

  sh_inf[1:2,1:n_agef,1:n_year] <- calc_surv_harvest(
          nT_age = nT_age_surv,
          nT_period = nT_period_surv,
          beta0 = inf_beta0_survival,
          beta_sex = inf_beta_sex_survival,
          age_effect = age_effect_survival[1:nT_age_surv],
          period_effect = period_effect_survival[1:nT_period_surv],
          intvl_step_yr = intvl_step_yr,
          n_year = n_year,
          n_agef = n_agef,
          n_agem = n_agem,
          ng_start = ng_start[1:n_year],
          gun_start = gun_start[1:n_year],
          gun_end = gun_end[1:n_year],
          ng_end = ng_end[1:n_year],
          yr_start = yr_start[1:n_year],
          yr_end = yr_end[1:n_year],
          p_nogun_f = p_ng_f,
          p_nogun_m = p_ng_m,
          p_gun_f = p_gun_f,
          p_gun_m = p_gun_m
          )


  ###################################################
  #### Probability of Infection based on FOI hazards
  ###################################################

  psi[1:2, 1:n_agef, 1:n_year] <- calc_infect_prob(
                        age_lookup_f = age_lookup_f_conv[1:Nage_lookup_f],
                        age_lookup_m = age_lookup_m_conv[1:Nage_lookup_m],
                        Nage_lookup_f = Nage_lookup_f,
                        Nage_lookup_m = Nage_lookup_m,
                        n_agef = n_agef,
                        n_agem = n_agem,
                        yr_end = yr_end[1:n_year],
                        f_age = f_age_foi[1:n_ageclassf],
                        m_age = m_age_foi[1:n_ageclassm],
                        f_period = f_period_foi[1:n_year],
                        m_period = m_period_foi[1:n_year],
                        n_year = n_year)


  ###################################################
  #### Earn-a-buck correction factor
  #### based on Van Deelen et al (2010)
  ###################################################

  # eab_antlerless_temp ~ dgamma(eab_antlerless_alpha,eab_antlerless_beta)
  # eab_antlered_temp ~ dgamma(eab_antlered_alpha,eab_antlered_beta)
  # for(t in 1:n_year) {
  #   eab_antlerless[t] <- eab_antlerless_temp^x_eab[t]
  #   eab_antlered[t]  <- eab_antlered_temp^x_eab[t]
  # }

  ######################################################################
  ###
  ### Population Process Model
  ### Population Projection
  ### pop_proj temporarily holds the projected age class
  ###
  ######################################################################

    ##################
    ### Susceptible
    ##################

    for (t in 2:n_year) {
    ###########
    # Females
    ###########
    #Female: project forward anually
    for (a in 1:(n_agef - 1)) {
      pop_sus_proj[1, a, t] <- pop_sus[1, a, t - 1] * sn_sus[1, a, t - 1] * (1 - psi[1, a, t - 1])
    }

    #female max age class
    pop_sus_proj[1, n_agef, t] <- pop_sus_proj[1,(n_agef - 1), t] +
                                  pop_sus[1,  n_agef, t - 1] * sn_sus[1, n_agef,t - 1] * (1 - psi[1, n_agef, t - 1])

    #Female: set projection into population model matrix
    for (a in 2:n_agef) {
      pop_sus[1, a, t] <- pop_sus_proj[1, (a - 1), t]
    }

    #Male: fawn class = total #females * unisex fawns per female/2
    #(should this be divided by 2?)
    pop_sus[1, 1, t] <- (sum(pop_sus_proj[1, 1:n_agef, t]) +
                         sum(pop_inf_proj[1, 1:n_agef, t])) * fec[t] * (1 - psi[2, 1, t]) / 2 
    ###########
    # Males
    ###########

    #Male: project forward anually
    for (a in 1:(n_agem - 1)) {
      pop_sus_proj[2, a, t] <- pop_sus[2, a, t - 1] * sn_sus[2, a, t - 1] * (1 - psi[2, a, t - 1])
    }
    
    #Male: max age class
    pop_sus_proj[2, n_agem, t] <- pop_sus_proj[2, (n_agem - 1), t] +
                                  pop_sus[2, n_agem, t - 1] * sn_sus[2, n_agem, t - 1] * (1 - psi[2, n_agem, t - 1])


    #Male: set projection into population model matrix
    for (a in 2:n_agem) {
      pop_sus[2, a, t] <- pop_sus_proj[2, (a - 1), t]
    }

    # Male: fawn class = total #females * unisex fawns per female/2
    # (should this be divided by 2?)
    pop_sus[2, 1, t] <- (sum(pop_sus_proj[1, 1:n_agef, t]) +
                        sum(pop_inf_proj[1, 1:n_agef, t])) * fec[t] * (1 - psi[2, 1, t]) / 2 

    ###################################################
    ### Infected/Infectious
    ###################################################

    ###########
    # Females
    ###########

    #Female: project forward anually
    for (a in 1:(n_agef - 1)) {
      pop_inf_proj[1, a, t] <- pop_inf[1, a, t - 1] * sn_inf[1, a, t - 1] +
                               pop_sus[1, a, t - 1] * sn_sus[1, a, t - 1] * psi[1, a, t - 1]
    }
    #Female: max age = 9.5+ years
    pop_inf_proj[1, n_agef, t] <- pop_inf_proj[1, (n_agef - 1), t] +
                                  pop_inf[1, n_agef, t - 1] * sn_inf[1, n_agef, t - 1] +
                                  # pop_sus_proj[1, (n_agef - 1), t] * psi[1, (n_agef - 1), t] + #need to double check this
                                  pop_sus[1, n_agef, t - 1] * sn_sus[1, n_agef, t - 1] * psi[1, n_agef, t - 1]

    #Female: set projection into population model matrix
    for (a in 2:n_agef) {
      pop_inf[1, a, t] <- pop_inf_proj[1, (a - 1), t]
    }
    
    #Female: fawn class = total #females * unisex fawns per female/2
    #(should this be divided by 2?)
    pop_inf[1, 1, t] <- (sum(pop_sus_proj[1, 1:n_agef, t]) +
                         sum(pop_inf_proj[1, 1:n_agef, t])) * fec[t] * psi[1, 1, t] / 2

    ###########
    # Males
    ###########

    #Male: project forward anually
    for (a in 1:(n_agem - 1)) {
        pop_inf_proj[2, a, t] <- pop_inf[2, a, t - 1] * sn_inf[2, a, t - 1] +
                                 pop_sus[2, a, t - 1] * sn_sus[2, a, t - 1] * psi[2, a, t - 1]
    }

    #Male: max age class
    pop_inf_proj[2, n_agem, t] <- pop_inf_proj[2, (n_agem - 1), t] +
                                      pop_inf[2, n_agem, t - 1] * sn_inf[2, n_agem, t - 1] +
                                      # pop_sus_proj[2, (n_agem - 1), t] * psi +#need to double check this
                                      pop_sus[2, n_agem, t - 1] *  sn_sus[2, n_agem, t - 1] * psi[2, n_agem, t - 1]

    #Male: set projection into population model matrix
    for (a in 2:n_agem) {
      pop_inf[2, a, t] <- pop_inf_proj[2, (a - 1), t]
    }

    #Male: fawn class = total #females * unisex fawns per female/2#(should this be divided by 2?)
    pop_inf[2, 1, t] <- (sum(pop_sus_proj[1, 1:n_agef, t]) +
                         sum(pop_inf_proj[1, 1:n_agef, t])) * fec[t] * psi[2, 1, t] / 2

  }#end t
  
  ######################################################################
  ### Observation Model
  ######################################################################

  for (i in 1:n_sex) {
    # tau_obs[t, i] <- 1 / mu_obs[t, i]
    tau_obs[i] ~ dgamma(1, 1)
  }#end i

  for (t in 1:n_year) {
    for (a in 1:n_agef) {
      harv_pop[1, a, t] <- (pop_inf[1, a, t] * (1 - sh_inf[1, a, t]) + pop_sus[1, a, t] * (1 - sh_sus[1, a, t])) * report[t]
    }
    for (a in 1:n_agem) {
      harv_pop[2, a, t] <- (pop_inf[2, a, t] * (1 - sh_inf[2, a, t]) + pop_sus[2, a, t] * (1 - sh_sus[2, a, t])) * report[t]
    }

    #Total Antlerless Harvest
    #adding in male fawns
    mu_obs[1, t] <- (sum(harv_pop[1, 1:n_agef, t]) + harv_pop[2, 1, t]) # eab_antlerless[t] * 

    #Total Antlered Harvest
    mu_obs[2, t] <- sum(harv_pop[2, 2:n_agem, t])#excludes male fawns eab_antlered[t] * 

    ###################################
    #Likelihood for overall total
    ###################################
    
    for (j in 1:n_sex) {
      O[j, t] ~ dnorm(mu_obs[j, t], tau_obs[j])
    }#end i

    ###################################
    #Likelihood for overall total
    ###################################

    ###parameters for likelihood harvest data by antlerless group
    p_less[1, t] <- harv_pop[1, 1, t] / mu_obs[1, t]#proportion female fawns
    p_less[2, t] <- harv_pop[1, 2, t] / mu_obs[1, t]#1
    p_less[3, t] <- harv_pop[1, 3, t] / mu_obs[1, t]#2
    p_less[4, t] <- harv_pop[1, 4, t] / mu_obs[1, t]#3
    p_less[5, t] <- sum(harv_pop[1, 5:6, t]) / mu_obs[1, t]#4-5
    p_less[6, t] <- sum(harv_pop[1, 7:9, t]) / mu_obs[1, t]#6-8
    p_less[7, t] <- harv_pop[1, 10, t] / mu_obs[1, t]#9+
    p_less[8, t] <- 1 - sum(p_less[1:7,t])#proportion male fawns, antlerless

    #harvest data bt antlered group
    ### p_ant[t, 1] <- harv_pop[t, 2, 1] / (mu_obs[t, 2])#male fawns 
    p_ant[1, t] <- harv_pop[2, 2, t] / mu_obs[2, t]#1
    p_ant[2, t] <- harv_pop[2, 3, t] / mu_obs[2, t]#2
    p_ant[3, t] <- harv_pop[2, 4, t] / mu_obs[2, t]#3
    p_ant[4, t] <- sum(harv_pop[2, 5:6, t]) / mu_obs[2, t]#4-5
    p_ant[5, t] <- 1 - sum(p_ant[1:4, t]) #6+

  }# end t

  for(t in 1:n_year){

    #antlerless, male fawns
    Cage_less[1:(n_ageclassf + 1),t] ~ dmulti(prob = p_less[1:(n_ageclassf + 1),t], size = sizeCage_f[t])
 
    #antlered
    Cage_ant[1:(n_ageclassm - 1),t] ~ dmulti(prob = p_ant[ 1:(n_ageclassm - 1),t], size = sizeCage_m[t])
  }

})#end model statement


######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################

###################################
### Data/Constants/Inits
###################################

nimData <- list(Cage_less = Cage_less,
                Cage_ant = Cage_ant,
                O = t(Ototal[,3:2]),
                f_logpop_sus = f_logpop_sus,
                f_logpop_inf = f_logpop_inf,
                m_logpop_sus = m_logpop_sus,
                m_logpop_inf = m_logpop_inf,
                # obs_ct_fd_mu = obs_ct_fd_mu,
                # obs_ct_fd_sd = obs_ct_fd_sd,
                obs_ct_fd_alpha = obs_ct_fd_alpha,
                obs_ct_fd_beta = obs_ct_fd_beta,
                Nfawn = fawndoe_df$overall_fawn,
                Ndoe = fawndoe_df$overall_doe,
                x_eab = df_eab$EAB#,
                # cll_sn_sus = array(NA,c(2, n_agef, n_year)),
                # cll_sn_inf = array(NA,c(2, n_agef, n_year)),
                # cll_sh_sus = array(NA,c(2, n_agef, n_year)),
                # cll_sh_inf = array(NA,c(2, n_agef, n_year))
                )

#Constants
nimConsts <- list(
    n_year = n_year,
    n_agef = n_agef,
    n_agem = n_agem,
    n_ageclassf = n_ageclassf,
    n_ageclassm = n_ageclassm,
    n_sex = n_sex,
    sizeCage_f = sizeCage_f,
    sizeCage_m = sizeCage_m,
    report_hyp_all = report_hyp_all,
    report_hyp_y = report_hyp_y,
    # psi = array(runif(2*n_agef*n_year, .001, .01),c(2, n_agef, n_year)),
    # fec_init = fawndoe_df$overall_fd[1],
    n_year_fec_early = n_year_fec_early,
    # n_year_precollar = n_year_precollar,
    # n_year_collar = n_year_collar,
    age_effect_survival = age_effect_survival,
    period_effect_survival = period_effect_survival,
    sus_beta0_survival = sus_beta0_survival,
    sus_beta_sex_survival = sus_beta_sex_survival,
    inf_beta0_survival = inf_beta0_survival,
    inf_beta_sex_survival = inf_beta_sex_survival,
    nT_age_surv = nT_age_surv,
    nT_period_surv = nT_period_surv,
    ng_start = d_fit_season$ng_start,
    gun_start = d_fit_season$gun_start,
    gun_end = d_fit_season$gun_end,
    ng_end = d_fit_season$ng_end,
    yr_start = d_fit_season$yr_start,
    yr_end = d_fit_season$yr_end,
    p_ng_f = p_ng_f,
    p_ng_m = p_ng_m,
    p_gun_f = p_gun_f,
    p_gun_m = p_gun_m,
    age_lookup_f = age_lookup_f,
    age_lookup_m = age_lookup_m,
    Nage_lookup = Nage_lookup,
    age_lookup_f_conv = age_lookup_f_conv,
    age_lookup_m_conv = age_lookup_m_conv,
    Nage_lookup_f = Nage_lookup_conv,
    Nage_lookup_m = length(age_lookup_m_conv),
    f_age_foi = f_age_foi-.5,#original was for monthly haz
    m_age_foi = m_age_foi-.5,#original was for monthly haz
    f_period_foi = f_period_foi-.5,#original was for monthly haz
    m_period_foi = m_period_foi-.5,#original was for monthly haz
    # eab_antlerless_alpha = eab_antlerless_alpha,
    # eab_antlerless_beta = eab_antlerless_beta,
    # eab_antlered_temp = 1,
    # eab_antlerless_temp = 1,
    # eab_antlered_alpha = eab_antlered_alpha,
    # eab_antlered_beta = eab_antlered_beta,
    intvl_step_yr = intvl_step_yr
)

#Initial values
initsFun <- function()list(
  tau_obs = runif(2, 1, 3),
  # sn_sus = runif(1,.72, .78),
  # sn_inf = runif(1,.55,.65),
  # sh_inf = runif(1,.55,.65),
  # sh_sus = runif(1,.63,.73),
  # mu_sn_sus = rnorm(2,cloglog(.15), .001),
  # tau_sn_sus = rgamma(1, 10, 1),
  # mu_sn_inf = rnorm(2,cloglog(.3), .001),
  # tau_sn_inf = rgamma(1, 10, 1),
  # mu_sh_sus = rnorm(2,cloglog(.6), .001),
  # tau_sh_sus = rgamma(1,4, 6),
  # mu_sh_inf = rnorm(2,cloglog(.5), .001),
  # tau_sh_inf = rgamma(1,4, 6),
  # pop_sus = pop_sus_init,
  # pop_inf = pop_inf_init,
  tau_pop = runif(2, .5, 1),
  # llpop_sus = llpop_sus_init,
  # llpop_inf = llpop_inf_init,
  # tau_pop_inf = runif(1,.5,1),
  # p_less = matrix(1/8,nrow=n_year,ncol=n_ageclass+1),
  # p_ant = matrix(1/6,nrow=n_year,ncol=n_ageclass-1),
  # fec = rgamma(1,5,4),
  report_overall = report_overall_init,
  report = report_init,
  # fec = fec_init,
  fec_epsilon = fec_eps_init,#rnorm(n_year_fec_early, 0, sd = .01),
  mu_fec = rnorm(1, mu_fec_init, .01),
  fec_prec_eps = runif(1, 5, 10)#,
  # eab_antlerless_temp = rgamma(1,eab_antlerless_alpha,eab_antlerless_beta)#,
  # eab_antlered_temp = .71
)
nimInits <- initsFun()


Rmodel <- nimbleModel(code = modelcode,
                      constants = nimConsts,
                      data = nimData,
                      inits = initsFun(),
                      check = FALSE,
                      calculate = FALSE
                      )
Rmodel$initializeInfo()

### Identify params to monitor
parameters <- c("fec",
              "mu_fec",
              "fec_epsilon",
              "sn_inf",
              "sn_sus",
              "sh_inf",
              "sh_sus",
              # "mu_sn_sus",
              # "mu_sn_inf",
              # "mu_sh_sus",
              # "mu_sh_inf",
              # "tau_sn_sus",
              # "tau_sn_inf",
              # "tau_sh_sus",
              # "tau_sh_inf",
              # "psi",
              # "eab_antlerless",
              "report",
              "mu_obs",
              "tau_obs",
              "tau_pop"#,
              # "pop_inf",
              # "pop_sus"
              # "tau_pop_inf"
              )
n_thin <- 1
n_chains <- 3
reps <- 20000
bin <- reps*.5
starttime <- Sys.time()
confMCMC <- configureMCMC(Rmodel,
                          monitors = parameters,
                          thin = n_thin,
                          # enableWAIC = TRUE,
                          useConjugacy = FALSE)
# for(i in 1:10){beepr::beep()}

nimMCMC <- buildMCMC(confMCMC)
# for(i in 1:10){beepr::beep()}

Cnim <- compileNimble(Rmodel)
# for(i in 1:10){beepr::beep()}

CnimMCMC <- compileNimble(nimMCMC, project = Rmodel)
for(i in 1:10){beepr::beep()}

set.seed(12191591)
starttime <- Sys.time()
mcmcout <- runMCMC(CnimMCMC,
                   niter = reps,
                   nburnin = bin,
                   nchains = n_chains,
                   inits = initsFun,
                   samplesAsCodaMCMC = TRUE,
                   summary = TRUE
                  #  WAIC=TRUE
                   )
for(i in 1:10){beepr::beep()}

runtime <- difftime(Sys.time(),
                  starttime,
                  units = "min")


# assign('state.transition', state.transition, envir = .GlobalEnv)

# cl<-makeCluster(n.chains)
# clusterExport(cl, c("modelcode", "initsFun", "nimData", "nimConsts", "parameters","state.transition","reps","bin","n.thin"))
# for (j in seq_along(cl)) {
#   set.seed(j+1000)
#   init <- initsFun()
#   clusterExport(cl[j], "init")
# }
# mcmcout <-  mcmc.list(clusterEvalQ(cl, {
#   library(nimble)
#   library(coda)
#   Rmodel <- nimbleModel(code = modelcode, name = "modelcode",
#                            constants = nimConsts, data = nimData,
#                            inits = init)
#   confMCMC <- configureMCMC(Rmodel,
#                           monitors=parameters,
#                           thin=n.thin,
#                           useConjugacy = FALSE)
#   nimMCMC <- buildMCMC(confMCMC)
#   Cnim <- compileNimble(Rmodel)
#   CnimMCMC <- compileNimble(nimMCMC,project=Rmodel)
#   mcmcout <- runMCMC(CnimMCMC,
#                    niter=reps,
#                    nburnin=bin,
#                    samplesAsCodaMCMC=TRUE)
#   return(mcmcout)
# }))

save(mcmcout, file = "mcmcout.Rdata")
save(runtime, file = "runtime.Rdata")

# posteriorSamplesMatrix <- rbind(mcmcout[[1]], mcmcout[[2]], mcmcout[[3]])
# CnimMCMC$run(5)   ## non-zero number of iterations
# nimble:::matrix2mv(posteriorSamplesMatrix, CnimMCMC$mvSamples)
# # CnimMCMC$enableWAIC <- TRUE
# waic <- calculateWAIC(posteriorSamplesMatrix,Rmodel)
# waic <- mcmcout$WAIC
# save(waic,file="waic.Rdata")