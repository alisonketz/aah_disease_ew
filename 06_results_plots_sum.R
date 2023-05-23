###
### Results - plotting and summary statistics
###

###########################################################
### Source summary function for posteriors
###########################################################

# source("summarize.R")
# # load("fit_sum.Rdata")
# load("mcmcout.Rdata")
# load("runtime.Rdata")
# load("out.Rdata")

# out <- mcmc.list(mcmcout$samples)
# fit_sum <- summarize(out)
# out<-mcmc.list(out)

fit_sum <- mcmcout$summary$all.chains
out <- mcmcout$samples

# cause.sum <- fit_sum[grep("cause",rownames(fit_sum)),]
# cause.sum <- cause.sum[-c(1:4),]
# n_c.effects<-dim(cause.sum)[1]/4
# n_c.effects/c.records
# cause1.mn <-as.numeric(cause.sum[1:n_c.effects, 1])
# cause2.mn <- as.numeric(cause.sum[(n_c.effects+1):(2*n_c.effects), 1])
# cause3.mn <- as.numeric(cause.sum[(n_c.effects*2+1):(3*n_c.effects), 1])
# cause4.mn <- as.numeric(cause.sum[(n_c.effects*3+1):(4*n_c.effects), 1])

# plot(cause1.mn)
# individual<-rep(1:c.records,cnT_age)
# age<-rep(1:cnT_age,c.records)
# cause<-rep(1:4,each=n_c.effects)
# cause.df=data.frame(individual,age,cause,cause.sum[,1])
# names(cause.df)[4]<-"cause.sum"
# cause.df$cause=as.factor(cause.df$cause)
# library(ggplot2)
# ggplot(data=cause.df,aes(y=cause.sum))+geom_point(aes(x=age,color=cause))+facet_wrap(.~cause)

gelman.diag(out[,"fec[18]"])
gelman.diag(out[,"fec[28]"])

# gelman.diag(out[,"mu_fec"])
# gelman.diag(out[,"sn_sus[2, 1, 1]"])
# gelman.diag(out[,"sn_inf[2, 1, 1]"])
# gelman.diag(out[,"sn_sus[20, 1, 1]"])
# gelman.diag(out[,"sn_inf[20, 1, 1]"])
gelman.diag(out[,"tau_obs[1]"])
gelman.diag(out[,"tau_obs[2]"])
gelman.diag(out[,"tau_pop[1]"])
gelman.diag(out[,"tau_pop[2]"])

# gelman.diag(out[,"tau_pop_pos"])

# pop_indx=grep("pop_neg",rownames(fit_sum))
# gelman.diag(out[,pop_indx],multivariate=FALSE)

# pop_indx=grep("pop_pos",rownames(fit_sum))
# gelman.diag(out[,pop_indx],multivariate=FALSE)


par(mfrow=c(1,1))
pdf("figures/traceplots_ahh.pdf")

traceplot(out[,"mu_obs[1, 1]"],ylab="mu_obs[1, 1]")
traceplot(out[,"mu_obs[2, 1]"],ylab="mu_obs[2, 1]")
traceplot(out[,"mu_fec"],ylab="mu_fec")
traceplot(out[,"fec[8]"],ylab="fec[8]")
traceplot(out[,"fec[15]"],ylab="fec[15]")
traceplot(out[,"fec[28]"],ylab="fec[28]")

# traceplot(out[,"sn_sus[1, 1, 1]"],ylab="sn_sus[1, 1, 1]")
# traceplot(out[,"sn_sus[1, 1, 21]"],ylab="sn_sus[1, 1, 21]")
# traceplot(out[,"sh_sus[1, 1, 1]"],ylab="sh_sus[1, 1, 1]")
# traceplot(out[,"sh_sus[1, 1, 21]"],ylab="sh_sus[1, 1, 21]")
# traceplot(out[,"sn_inf[1, 1, 21]"],ylab="sn_inf[1, 1, 21]")
# traceplot(out[,"sn_inf[1, 1, 2]"],ylab="sn_inf[1, 1, 2]")

traceplot(out[,"tau_obs[1]"],ylab="tau_obs[1]: antlerless")
traceplot(out[,"tau_obs[2]"],ylab="tau_obs[2]: antlered")
# traceplot(out[,"tau_pop_inf_f"],ylab="tau_pop_inf_f")

traceplot(out[,"tau_pop[1]"],ylab="tau_pop[1]")
traceplot(out[,"tau_pop[2]"],ylab="tau_pop[2]")
# traceplot(out[,"tau_pop[3]"],ylab="tau_pop[3]")
# traceplot(out[,"tau_pop[4]"],ylab="tau_pop[4]")


# traceplot(out[,"tau_pop_pos"],ylab="tau_pop_pos")
traceplot(out[,"report[1]"],ylab="report_overall")
traceplot(out[,"report[14]"],ylab="report[14]: 2015")
traceplot(out[,"report[15]"],ylab="report[15]: 2016")
traceplot(out[,"report[16]"],ylab="report[16]: 2017")
traceplot(out[,"report[17]"],ylab="report[17]: 2018")
traceplot(out[,"report[18]"],ylab="report[18]: 2019")
traceplot(out[,"report[19]"],ylab="report[19]: 2020")
traceplot(out[,"eab_antlerless[3]"],ylab="eab_antlerless[3]")
dev.off()

round(fit_sum[grep("fec",rownames(fit_sum)),],2)
fit_sum[grep("eab",rownames(fit_sum)),]

###################################################################
###
### print plot S0_age
###
####################################################################

# The Colorblind palette with grey:
cbPalette <- c( "#0072B2", "#D55E00", "#CC79A7","#999999", "#E69F00", "#56B4E9")


##################################
###
### Plotting the population model
###
##################################

pop_indx <- grep("pop_neg",rownames(fit_sum))
age_effect_mean <- fit_sum[te_indx,1]
age_effect_lower <- fit_sum[te_indx,4]
age_effect.upper <- fit_sum[te_indx,5]
weeks <- 1:nT_age
pred_age_effect <- data.frame(weeks,age_effect_mean,age_effect_lower,age_effect.upper)
pred_age_effect_plot <- ggplot(data = pred_age_effect,aes(x = weeks)) +
  geom_line(aes(x = weeks,y=age_effect_mean),size=1) +
  geom_ribbon(aes(ymin=age_effect_lower,ymax=age_effect.upper),alpha=.2,linetype=0) +
  ggtitle("Predation Age Effect Posterior") +
  xlab("Age") +
  ylab("Effect Size") +
  theme_bw()
pred_age_effect_plot

ggsave("figures/predation_age_effect.pdf",pred_age_effect_plot)

#disease
te_indx=grep("age_effect",rownames(fit_sum))[(nT_age+1):(2*nT_age)]
age_effect_mean = fit_sum[te_indx,1]
age_effect_lower = fit_sum[te_indx,4]
age_effect.upper = fit_sum[te_indx,5]
weeks=1:nT_age
disease_age_effect = data.frame(weeks,age_effect_mean,age_effect_lower,age_effect.upper)
disease_age_effect_plot=ggplot(data =disease_age_effect,aes(x = weeks))+
  geom_line(aes(x = weeks,y=age_effect_mean),size=1)+
  geom_ribbon(aes(ymin=age_effect_lower,ymax=age_effect.upper),alpha=.2,linetype=0)+
  ggtitle("Disease Age Effect Posterior")+xlab("age")+ylab("Effect Size")+
  theme_bw()

disease_age_effect_plot
ggsave("figures/disease_age_effect.pdf",disease_age_effect_plot)

#anthro
te_indx=grep("period_effect",rownames(fit_sum))[(2*nT_yr1+1):(3*nT_yr1)]
period_effect_mean = fit_sum[te_indx,1]
period_effect_lower = fit_sum[te_indx,4]
period_effect.upper = fit_sum[te_indx,5]
weeks=1:nT_yr1
anthro_period_effect = data.frame(weeks,period_effect_mean,period_effect_lower,period_effect.upper)
anthro_period_effect_plot=ggplot(data = anthro_period_effect,aes(x = weeks))+
  geom_line(aes(x = weeks,y=period_effect_mean),size=1)+
  geom_ribbon(aes(ymin=period_effect_lower,ymax=period_effect.upper),alpha=.2,linetype=0)+
  ggtitle("Anthro Period Effect Posterior")+xlab("time")+ylab("Effect Size")+
  theme_bw()

anthro_period_effect_plot
ggsave("figures/anthro_time_effect.pdf",anthro_period_effect_plot)

#hunt
pdf("figures/density_beta_hunt.pdf")
densityplot(out[,"beta_gun"],ylab="beta_hunt")
dev.off()

# starvation
te_indx=grep("age_effect",rownames(fit_sum))[(4*nT_age+1):(5*nT_age)]
age_effect_mean = fit_sum[te_indx,1]
age_effect_lower = fit_sum[te_indx,4]
age_effect.upper = fit_sum[te_indx,5]
weeks=1:nT_age
starve_age_effect = data.frame(weeks,age_effect_mean,age_effect_lower,age_effect.upper)
starve_age_effect_plot=ggplot(data =starve_age_effect,aes(x = weeks))+
  geom_line(aes(x = weeks,y=age_effect_mean),size=1)+
  geom_ribbon(aes(ymin=age_effect_lower,ymax=age_effect.upper),alpha=.2,linetype=0)+
  ggtitle("Starvation Age Effect Posterior")+xlab("Age")+ylab("Effect Size")+
  theme_bw()

starve_age_effect_plot
ggsave("figures/starvation_age_effect.pdf",starve_age_effect_plot)

# wound
pdf("figures/density_beta_wound.pdf")
  densityplot(out[,"beta_cause[6]"],ylab="beta_wound")
dev.off()


###
### Posteriors of beta_causes
###
pdf("figures/density_beta_causes.pdf")
for(j in 2:n_causes){
  print(densityplot(out[,paste0("beta_cause[",j,"]")],ylab=paste0("beta_cause[",j,"]")))
}
dev.off()

###
### Posteriors of p_int
###

pdf("figures/density_prob_causes.pdf")
for(j in 1:n_causes){
  print(densityplot(out[,paste0("p_int[",j,"]")],ylab=paste0("p_int[",j,"]")))
}
dev.off()
###############################################
###
### Save results
###
##############################################

sink("results_fawn_spline_cause_time.txt")
print(fit_sum[c(grep("beta",rownames(fit_sum)),
          grep("b_",rownames(fit_sum)),
          grep("tau",rownames(fit_sum)),
          grep("p_int",rownames(fit_sum))),])
print(gelman.diag(out[,c(grep("beta",rownames(fit_sum)),
                         grep("b_",rownames(fit_sum)),
                         grep("tau",rownames(fit_sum)),
                         grep("p_int",rownames(fit_sum)))],multivariate=FALSE))
print(effectiveSize(out[,c(grep("beta",rownames(fit_sum)),
                           grep("b_",rownames(fit_sum)),
                           grep("p_int",rownames(fit_sum)),
                           grep("tau",rownames(fit_sum)))]))
cat("Runtime: ", runtime,"\n")
cat("Reps: ", reps,"\n")
sink()


print(fit_sum[c(grep("beta",rownames(fit_sum)),
          grep("b_age",rownames(fit_sum)),
          grep("tau",rownames(fit_sum))),])
class(out)
length(out)
fit_sum[grep("beta",rownames(fit_sum)),]
gelman.diag(out[,grep("beta",rownames(fit_sum))])
gelman.diag(out[,c(grep("beta",rownames(fit_sum)),
                   grep("b_age",rownames(fit_sum)),grep("tau",rownames(fit_sum)))],multivariate=FALSE)
print(effectiveSize(out[,c(grep("beta",rownames(fit_sum)),grep("b_age",rownames(fit_sum)),grep("tau",rownames(fit_sum)))]))
cat("Runtime: ",runtime,"\n")
cat("Reps: ",reps,"\n")
cat("WAIC: ",mcmcout$WAIC,"\n")



############################
###
### plotting betas
###
############################
beta_df=do.call(rbind,out[,grep('beta',rownames(fit_sum))])
beta_df <- beta_df[,c(1,3:7)]
head(beta_df)

# densityplot(out[,"beta0"])

# densityplot(out[,"beta[1]"])
# densityplot(out[,"beta[2]"])
# densityplot(out[,"beta[3]"])


beta_out=melt(beta_df)
head(beta_out)
names(beta_out)=c("iteration","model","value")
head(beta_out)

beta_density <- ggplot(beta_out, aes(x=value, y=model, fill=model)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + theme(legend.position='none') + ggtitle(paste(expression(beta[0]), "Posteriors"))+
  scale_fill_viridis_d()

ggsave('figures/beta_density.pdf',beta_density)


###########
###
### why the uptick right side
###
##############

# pdf("figures/right_age_censor.pdf")
# hist(d.fit$right_age[d.fit$censor==1],breaks=100)
# hist(d.fit$right_age[d.fit$censor==0],breaks=100)
# dev.off()


###########
###
### why the uptick right side
###
##############

p.df=do.call(rbind,out[,grep('p_int',rownames(fit_sum))])
head(p.df)

colnames(p.df) <- causes
p.out <- melt(p.df)
head(p.out)
names(p.out) <- c("iteration","causeProb","value")
head(p.out)

p.density <- ggplot(p.out, aes(x=value, y=causeProb, fill=causeProb)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + theme(legend.position='none') + ggtitle(paste(expression(p[0]), "Posteriors"))+
  scale_fill_viridis_d()
p.density

ggsave('figures/p_density.pdf',p.density)




##################################
###
### log hazard - time axis
###
##################################

# ###
# ### year 1
# ###

# te_indx=grep("causehaz",rownames(fit_sum))[1:(n_causes * nT_yr1)]
# causehaz_mean = fit_sum[te_indx,1]
# causehaz_lower = fit_sum[te_indx,4]
# causehaz.upper= fit_sum[te_indx,5]
# head(fit_sum[te_indx,])
# cause=rep(causes,nT_yr1)

# weeks=rep(1:(nT_yr1),n_causes)
# out.causehaz_yr1 = data.frame(weeks,causehaz_mean,causehaz_lower,causehaz.upper,cause)

# causehaz_plot_yr1=ggplot(data =out.causehaz_yr1,aes(x = weeks,color=cause,fill=cause))+
#   geom_line(aes(x = weeks,y=causehaz_mean),size=1)+
#   geom_ribbon(aes(ymin=causehaz_lower,ymax=causehaz.upper),alpha=.2,linetype=0)+
#   ggtitle("Causehazard- Year 1")+xlab("Time")+ylab("Hazard")+
#   theme_bw()+
#   scale_color_brewer("Year",palette = 'Set1')+
#   scale_fill_brewer("Year",palette = 'Set1')+
#   facet_wrap(.~cause)+ylim(0,.02)+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

# causehaz_plot_yr1

# ggsave("figures/causehaz_yr1.pdf",causehaz_plot_yr1)


# ###
# ### year 2
# ###

# te_indx=grep("causehaz",rownames(fit_sum))[(n_causes * nT_yr1 + 1):(n_causes * nT_yr1 * 2)]

# causehaz_mean = fit_sum[te_indx,1]
# causehaz_lower = fit_sum[te_indx,4]
# causehaz.upper= fit_sum[te_indx,5]

# weeks=rep(1:(nT_yr1),n_causes)
# out.causehaz_yr2 = data.frame(weeks,causehaz_mean,causehaz_lower,causehaz.upper,cause)

# causehaz_plot_yr2=ggplot(data =out.causehaz_yr2,aes(x = weeks,color=cause,fill=cause))+
#   geom_line(aes(x = weeks,y=causehaz_mean),size=1)+
#   geom_ribbon(aes(ymin=causehaz_lower,ymax=causehaz.upper),alpha=.2,linetype=0)+
#   ggtitle("Causehaz - Year 2")+xlab("Time")+ylab("Effect Size")+
#   theme_bw()+
#   scale_color_brewer("Year",palette = 'Set1')+
#   scale_fill_brewer("Year",palette = 'Set1')+
#   facet_wrap(.~cause)+ylim(0,.02)+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))


# causehaz_plot_yr2

# ggsave("figures/causehaz_yr2.pdf",causehaz_plot_yr2)


# ###
# ### year 3
# ###

# te_indx=grep("causehaz",rownames(fit_sum))[(2 * n_causes * nT_yr1 + 1):(n_causes * nT_yr1 * 3)]

# causehaz_mean = fit_sum[te_indx,1]
# causehaz_lower = fit_sum[te_indx,4]
# causehaz.upper= fit_sum[te_indx,5]

# weeks=rep(1:(nT_yr1),n_causes)
# out.causehaz_yr3 = data.frame(weeks,causehaz_mean,causehaz_lower,causehaz.upper,cause)

# causehaz_plot_yr3=ggplot(data =out.causehaz_yr3,aes(x = weeks,color=cause,fill=cause))+
#   geom_line(aes(x = weeks,y=causehaz_mean),size=1)+
#   geom_ribbon(aes(ymin=causehaz_lower,ymax=causehaz.upper),alpha=.2,linetype=0)+
#   ggtitle("Time Effect Posterior - Year 1")+xlab("Time")+ylab("Effect Size")+
#   theme_bw()+
#   scale_color_brewer("Year",palette = 'Set1')+
#   scale_fill_brewer("Year",palette = 'Set1')+
#   facet_wrap(.~cause)+ylim(0,.02)+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

# causehaz_plot_yr3

# ggsave("figures/causehaz_yr3.pdf",causehaz_plot_yr3)



