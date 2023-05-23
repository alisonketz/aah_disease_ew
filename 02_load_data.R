###########################################################################################
###
### Load data for survival models
###
###########################################################################################

# rm(list=ls())

filepath <- "~/Documents/aah/aah_disease/datafiles/"

###
### Save Data 
###
# save(df_age_early_female,file=paste0(filepath,"df_age_early_female.Rdata"))
# save(df_age_early_male,file=paste0(filepath,"df_age_early_male.Rdata"))
# save(df_age_early,file=paste0(filepath,"df_age_early.Rdata"))
# save(df_age_before_female,file=paste0(filepath,"df_age_before_female.Rdata"))
# save(df_age_before_male,file=paste0(filepath,"df_age_before_male.Rdata"))
# save(df_age_before,file=paste0(filepath,"df_age_before.Rdata"))
# save(Cage,file=paste0(filepath,"Cage.Rdata"))
# save(Cage_inf,file=paste0(filepath,"Cage_inf.Rdata"))
# save(Cage_sus,file=paste0(filepath,"Cage_sus.Rdata"))
# save(df_harvest,file=paste0(filepath,"df_harvest.Rdata"))
# save(df_pop_estimate,file=paste0(filepath,"df_pop_estimate.Rdata"))
# # save(Cage_less,file=paste0(filepath,"Cage_less.Rdata"))
# # save(Cage_ant,file=paste0(filepath,"Cage_ant.Rdata"))
# save(report_hyp_all,file=paste0(filepath,"report_hyp_all.Rdata"))
# save(fawndoe_df,file=paste0(filepath,"fawndoe_df.Rdata"))
# save(df_camtrap_fd,file=paste0(filepath,"df_camtrap_fd.Rdata"))
# save(report_hyp_y,file=paste0(filepath,"report_hyp_y.Rdata"))
# save(Ototal,file=paste0(filepath,"Ototal.Rdata"))
# save(df_eab,file=paste0(filepath,"df_eab.Rdata"))

###
### Load Data 
###

load(paste0(filepath,"p_hunt.Rdata"))
load(paste0(filepath,"d_fit_season.Rdata"))
load(paste0(filepath,"f_age_foi.Rdata"))
load(paste0(filepath,"m_age_foi.Rdata"))
load(paste0(filepath,"f_period_foi.Rdata"))
load(paste0(filepath,"m_period_foi.Rdata"))
load(paste0(filepath,"age_lookup_f.Rdata"))
load(paste0(filepath,"age_lookup_m.Rdata"))
load(paste0(filepath,"period_effect_survival.Rdata"))
load(paste0(filepath,"age_effect_survival.Rdata"))
load(paste0(filepath,"sus_beta0_survival.Rdata"))
load(paste0(filepath,"sus_beta_sex_survival.Rdata"))
load(paste0(filepath,"inf_beta0_survival.Rdata"))
load(paste0(filepath,"inf_beta_sex_survival.Rdata"))
load(paste0(filepath,"df_age_before_female.Rdata"))
load(paste0(filepath,"df_age_before_male.Rdata"))
load(paste0(filepath,"df_age_before.Rdata"))
load(paste0(filepath,"Cage.Rdata"))
load(paste0(filepath,"Cage_inf.Rdata"))
load(paste0(filepath,"Cage_sus.Rdata"))
load(paste0(filepath,"df_harvest.Rdata"))
load(paste0(filepath,"df_pop_estimate.Rdata"))
# load(paste0(filepath,"Cage_less.Rdata"))
# load(paste0(filepath,"Cage_ant.Rdata"))
load(paste0(filepath,"report_hyp_all.Rdata"))
load(paste0(filepath,"fawndoe_df.Rdata"))
load(paste0(filepath,"df_camtrap_fd.Rdata"))
load(paste0(filepath,"df_age_early_female.Rdata"))
load(paste0(filepath,"df_age_early_male.Rdata"))
load(paste0(filepath,"df_age_early.Rdata"))
load(paste0(filepath,"report_hyp_y.Rdata"))
load(paste0(filepath,"Ototal.Rdata"))
load(paste0(filepath,"df_eab.Rdata"))