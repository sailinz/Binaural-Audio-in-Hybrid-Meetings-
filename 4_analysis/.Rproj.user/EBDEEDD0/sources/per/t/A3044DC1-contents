library(faraway)
library(car)
library(lmerTest)
library(dplyr)
library(doBy)
library(emmeans)
library(sjlabelled)
library(lme4)# for glmer
library(lmerTest)
library(ordinal) # for clmm 
library(RVAideMemoire) # for Anova.clmm 
library(fitdistrplus)
library(coin)
library(logspline)
library(MASS) 
library(glmmADMB)
library(glmmTMB)

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

# random effects: https://ourcodingclub.github.io/tutorials/mixed-models/
# nice explaination: https://mfviz.com/hierarchical-models/
# model selection: http://depts.washington.edu/acelab/proj/Rstats/Rstats.pdf
# check distribution: https://stats.stackexchange.com/questions/132652/how-to-determine-which-distribution-fits-my-data-best

all_measures_per_participant <- read.csv("~/Desktop/Loic/Thesis_new/4_analysis/eyetracking_analysis/all_measures_per_participant_patche3.csv", comment.char="#")

all_measures_per_participant <- within(all_measures_per_participant, {
  mask <- factor(mask)
  sound <- factor(Sound)
  pid <- factor(PID)
})
par(cex = .6)


# ScoreMemTest
lm_ScoreMemTest <- lm(ScoreMemTest ~ mask*sound, data = all_measures_per_participant)
emmip(lm_ScoreMemTest, mask ~ sound,CIs=TRUE, xlab="Sound", ylab ="Mean of number of correct answer (max 6)")

glmm_ScoreMemTest <- glmer(ScoreMemTest ~ mask * sound + (1 | pid), data = all_measures_per_participant, family=poisson)
Anova(glmm_ScoreMemTest, type=3)

# MemConfidence
lm_MemConfidence <- lm(MemConfidence ~ mask*sound, data = all_measures_per_participant)
emmip(lm_MemConfidence, mask ~ sound,CIs=TRUE, xlab="Sound", ylab ="Likert's scale (1 - 5)")

all_measures_per_participant$MemConfidence = ordered(all_measures_per_participant$MemConfidence) # ordinal response 
glmm_MemConfidence <- clmm(MemConfidence ~ mask * sound + (1 | pid), data = all_measures_per_participant) 
Anova.clmm(glmm_MemConfidence, type=3)

# ScoreCompTest 
lm_ScoreCompTest <- lm(ScoreCompTest ~ mask*sound, data = all_measures_per_participant)
emmip(lm_ScoreCompTest, mask ~ sound,CIs=TRUE, xlab="Sound", ylab ="Mean of number of correct answer (max 6)")

contrasts(all_measures_per_participant$mask) <-"contr.sum"
contrasts(all_measures_per_participant$sound) <-"contr.sum"
glmm_ScoreCompTest <- glmer(ScoreCompTest ~ mask * sound + ( 1 | pid), data = all_measures_per_participant, family=poisson)
Anova(glmm_ScoreCompTest, type=3)
overdisp_fun(glmm_ScoreCompTest) # no overdispersion as the ratio is lower than 1


## let's use the binomial model results
# summaryBy(ScoreCompTest_normalized ~ mask+sound, data = all_measures_per_participant, 
#           FUN = list(mean, sd))
# all_measures_per_participant$ScoreCompTest_normalized <- all_measures_per_participant$ScoreCompTest/max(all_measures_per_participant$ScoreCompTest, na.rm = TRUE)
# glmm_ScoreCompTest <- glmer(ScoreCompTest_normalized ~ mask * sound + (1|pid), data = all_measures_per_participant)
# Anova(glmm_ScoreCompTest, type=3)


# all_measures_per_participant$pid <- as.numeric(all_measures_per_participant$pid) 
# all_measures_per_participant$mask <- as.numeric(all_measures_per_participant$mask) 
# friedman.test(ScoreCompTest ~ mask | pid, data=all_measures_per_participant)


# CompConfidence
lm_CompConfidence <- lm(CompConfidence ~ mask*sound, data = all_measures_per_participant)
emmip(lm_CompConfidence, mask ~ sound,CIs=TRUE, xlab="Sound", ylab ="Likert's scale (1 - 5)")

all_measures_per_participant$CompConfidence_ordinal = ordered(all_measures_per_participant$CompConfidence) # ordinal response
contrasts(all_measures_per_participant$mask) <-"contr.sum"
contrasts(all_measures_per_participant$sound) <-"contr.sum"
df2 <-as.data.frame(all_measures_per_participant) # Anova.clmmfails without this
glmm_CompConfidence <- clmm(CompConfidence_ordinal ~ mask * sound + (1 | pid), data = df2) 
Anova.clmm(glmm_CompConfidence, type=3)

# PostSpeakerIdent
lm_PostSpeakerIdent<- lm(PostSpeakerIdent~ mask*sound, data = all_measures_per_participant)
emmip(lm_PostSpeakerIdent, mask ~ sound,CIs=TRUE, xlab="Sound", ylab ="Likert's scale (1 - 5)")

all_measures_per_participant$PostSpeakerIdent_ordinal = ordered(all_measures_per_participant$PostSpeakerIdent) # ordinal response 
glmm_PostSpeakerIdent<- clmm(PostSpeakerIdent_ordinal ~ mask * sound + (1 | pid), data = all_measures_per_participant) 
Anova.clmm(glmm_PostSpeakerIdent, type=3)

# PostOverallComprehension
lm_PostOverallComprehension<- lm(PostOverallComprehension~ mask*sound, data = all_measures_per_participant)
emmip(lm_PostOverallComprehension, mask ~ sound,CIs=TRUE, xlab="Sound", ylab ="Likert's scale (1 - 5)")

all_measures_per_participant$PostOverallComprehension_ordinal = ordered(all_measures_per_participant$PostOverallComprehension) # ordinal response 
glmm_PostOverallComprehension<- clmm(PostOverallComprehension_ordinal ~ mask * sound + (1 | pid), data = all_measures_per_participant) 
Anova.clmm(glmm_PostOverallComprehension, type=3)

# PostConcentration 
lm_PostConcentration<- lm(PostConcentration~ mask*sound, data = all_measures_per_participant)
emmip(lm_PostConcentration, mask ~ sound,CIs=TRUE, xlab="Sound", ylab ="Likert's scale (1 - 5)")

all_measures_per_participant$PostConcentration_ordinal = ordered(all_measures_per_participant$PostConcentration) # ordinal response 
glmm_PostConcentration<- clmm(PostConcentration_ordinal ~ mask * sound + (1 | pid), data = all_measures_per_participant) 
Anova.clmm(glmm_PostConcentration, type=3)

# PostPresence 
lm_PostPresence<- lm(PostPresence~ mask*sound, data = all_measures_per_participant)
emmip(lm_PostPresence, mask ~ sound,CIs=TRUE, xlab="Sound", ylab ="Likert's scale (1 - 5)")

all_measures_per_participant$PostPresence_ordinal = ordered(all_measures_per_participant$PostPresence) # ordinal response 
glmm_PostPresence<- clmm(PostPresence_ordinal ~ mask * sound + (1 | pid), data = all_measures_per_participant) 
Anova.clmm(glmm_PostPresence, type=3)

# PostAudioHelp 
lm_PostAudioHelp<- lm(PostAudioHelp~ mask*sound, data = all_measures_per_participant)
emmip(lm_PostAudioHelp, mask ~ sound,CIs=TRUE, xlab="Sound", ylab ="Likert's scale (1 - 5)")

all_measures_per_participant$PostAudioHelp_ordinal = ordered(all_measures_per_participant$PostAudioHelp) # ordinal response 
glmm_PostAudioHelp<- clmm(PostAudioHelp_ordinal ~ mask * sound + (1 | pid), data = all_measures_per_participant) 
Anova.clmm(glmm_PostAudioHelp, type=3)

########### eye-tracking ############
# nb_fixation 
x= all_measures_per_participant$nb_fixation
df=all_measures_per_participant
descdist(x, discrete = FALSE) # log normal or gama
fit.norm <- fitdist(x, "norm")
gamma <- fitdist(x, "gamma")
fit.norm$aic
gamma$aic
fa = fitdistr(df[df$Sound == "mono",]$nb_fixation, "normal")$estimate # create fit for X.a ks.test(df[df$X == "a",]$Y, "pnorm", mean=fa[1], sd=fa[2])
fb = fitdistr(df[df$Sound == "binaural",]$nb_fixation, "normal")$estimate # create fit for X.a ks.test(df[df$X == "a",]$Y, "pnorm", mean=fa[1], sd=fa[2])
fc = fitdistr(df[df$Sound == "head",]$nb_fixation, "normal")$estimate # create fit for X.a ks.test(df[df$X == "a",]$Y, "pnorm", mean=fa[1], sd=fa[2])
ks.test(df[df$Sound == "mono",]$nb_fixation, "pnorm",mean=fa[1], sd=fa[2])
ks.test(df[df$Sound == "binaural",]$nb_fixation, "pnorm",mean=fb[1], sd=fb[2])
ks.test(df[df$Sound == "head",]$nb_fixation, "pnorm",mean=fb[1], sd=fb[2]) # not normal

fa = fitdistr(df[df$Sound == "mono",]$nb_fixation, "gamma")$estimate # create fit for X.a ks.test(df[df$X == "a",]$Y, "pnorm", mean=fa[1], sd=fa[2])
fb = fitdistr(df[df$Sound == "binaural",]$nb_fixation, "gamma")$estimate # create fit for X.a ks.test(df[df$X == "a",]$Y, "pnorm", mean=fa[1], sd=fa[2])
fc = fitdistr(df[df$Sound == "head",]$nb_fixation, "gamma")$estimate # create fit for X.a ks.test(df[df$X == "a",]$Y, "pnorm", mean=fa[1], sd=fa[2])
ks.test(df[df$Sound == "mono",]$nb_fixation, "pgamma", shape=fa[1], rate=fa[2])
ks.test(df[df$Sound == "binaural",]$nb_fixation, "pgamma", shape=fb[1], rate=fb[2])
ks.test(df[df$Sound == "head",]$nb_fixation, "pgamma", shape=fc[1], rate=fc[2]) # not gamma

lm_nb_fixation<- lm(nb_fixation~ mask*sound, data = all_measures_per_participant)
emmip(lm_nb_fixation, mask ~ sound,CIs=TRUE, xlab="Sound", ylab ="count/ms")

glmm_nb_fixation = glmer(nb_fixation ~ mask * sound + (1 | pid), data = all_measures_per_participant, family=Gamma)
Anova(glmm_nb_fixation, type=3)

library(multcomp) # for glht
library(emmeans) # for emm, emmeans
summary(glht(glmm_nb_fixation, emm(pairwise ~ mask * sound)), test=adjusted(type="bonferroni"))

# mean_fix_duration 
x= all_measures_per_participant$mean_fix_duration
df=all_measures_per_participant
descdist(x, discrete = FALSE) # beta or gamma
 

lm_mean_fix_duration<- lm(mean_fix_duration~ mask*sound, data = all_measures_per_participant)
emmip(lm_mean_fix_duration, mask ~ sound,CIs=TRUE, xlab="Sound", ylab ="ms")

glmm_mean_fix_duration = glmer(mean_fix_duration ~ mask * sound + (1 | pid), data = all_measures_per_participant, family=Gamma)
Anova(glmm_mean_fix_duration, type=3)


# total_fix_duration
#following distributions : "norm", "lnorm", "pois", "exp", "gamma","nbinom", "geom", "beta", "unif" and "logis"
x= all_measures_per_participant$total_fix_duration
df=all_measures_per_participant
descdist(x, discrete = FALSE)
fit.norm <- fitdist(x, "norm")
fit.logistic <- fitdist(x, "logis")
fit.norm$aic
fit.logistic$aic

lm_total_fix_duration<- lm(total_fix_duration~ mask*sound, data = all_measures_per_participant)
emmip(lm_total_fix_duration, mask ~ sound,CIs=TRUE, xlab="Sound", ylab ="ms/ms")

# use lmm results

# glmm_total_fix_duration = glmer(total_fix_duration ~ mask * sound + (1 | pid), data = all_measures_per_participant, family=Gamma)
# Anova(glmm_total_fix_duration, type=3)


# mean_saccade_amplitude 
x= all_measures_per_participant$mean_saccade_amplitude
df=all_measures_per_participant
descdist(x, discrete = FALSE)

lm_mean_saccade_amplitude<- lm(mean_saccade_amplitude~ mask*sound, data = all_measures_per_participant)
emmip(lm_mean_saccade_amplitude, mask ~ sound,CIs=TRUE, xlab="Sound", ylab ="px")

glmm_mean_saccade_amplitude = glmer(mean_saccade_amplitude ~ mask * sound + (1 | pid), data = all_measures_per_participant, family=Gamma)
Anova(glmm_mean_saccade_amplitude, type=3)


# mean_dissimilarity 
x= all_measures_per_participant$mean_dissimilarity
df=all_measures_per_participant
descdist(x, discrete = FALSE)

lm_mean_dissimilarity<- lm(mean_dissimilarity~ mask*sound, data = all_measures_per_participant)
emmip(lm_mean_dissimilarity, mask ~ sound,CIs=TRUE, xlab="Sound", ylab ="count/count")

# use LMM results
# glmm_mean_dissimilarity = glmer(mean_dissimilarity ~ mask * sound + (1 | pid), data = all_measures_per_participant, family=Gamma)
# Anova(glmm_mean_dissimilarity, type=3)

# ratio_aoi_transitions 
x= all_measures_per_participant$ratio_aoi_transitions
df=all_measures_per_participant
descdist(x, discrete = FALSE)

lm_ratio_aoi_transitions<- lm(ratio_aoi_transitions~ mask*sound, data = all_measures_per_participant)
emmip(lm_ratio_aoi_transitions, mask ~ sound,CIs=TRUE, xlab="Sound", ylab ="count/count")

# use LMM results
# glmm_ratio_aoi_transitions = glmer(ratio_aoi_transitions ~ mask * sound + (1 | pid), data = all_measures_per_participant, family=Gamma)
# Anova(glmm_ratio_aoi_transitions, type=3)

# mean_dispersion 
mean_dispersion_data <- read.csv("~/Desktop/Loic/Thesis_new/4_analysis/eyetracking_analysis/mean_dispersion.csv", comment.char="#")
mean_dispersion_data <- na.omit(mean_dispersion_data)
descdist(mean_dispersion_data$mean_dispersion, discrete = FALSE) # log normal
mean_dispersion_data <- within(mean_dispersion_data, {
  mask <- factor(mask)
  sound <- factor(Sound)
})
par(cex = .6)
# mean_dispersion$index <- row.names(mean_dispersion)

contrasts(mean_dispersion_data$mask) <-"contr.sum"
contrasts(mean_dispersion_data$sound) <-"contr.sum"
m = glm(mean_dispersion ~ mask*sound, data=mean_dispersion_data, family=Gamma)
# family=Gamma(link="log") is often used
Anova(m, type=3)

lm_mean_dispersion= lm(mean_dispersion ~ mask * sound , data=mean_dispersion_data)
anova(lm_mean_dispersion)
emmip(lm_mean_dispersion, mask ~ sound,CIs=TRUE, xlab="Sound", ylab ="px/frame")



# mean_dispersion_all_transitions 
mean_dispersion_all_transitions_data <- read.csv("~/Desktop/Loic/Thesis_new/4_analysis/eyetracking_analysis/mean_dispersion_all_transitions.csv", comment.char="#")
mean_dispersion_all_transitions_data <- na.omit(mean_dispersion_all_transitions_data)
descdist(mean_dispersion_all_transitions_data$mean_dispersion_all_transitions, discrete = FALSE) # beta
mean_dispersion_all_transitions_data <- within(mean_dispersion_all_transitions_data, {
  mask <- factor(mask)
  sound <- factor(Sound)
})
par(cex = .6)

contrasts(mean_dispersion_all_transitions_data$mask) <-"contr.sum"
contrasts(mean_dispersion_all_transitions_data$sound) <-"contr.sum"
m = glm(mean_dispersion_all_transitions ~ mask*sound, data=mean_dispersion_all_transitions_data, family=Gamma)
# family=Gamma(link="log") is often used
Anova(m, type=3)

lm_mean_dispersion_all_transitions= lm(mean_dispersion_all_transitions ~ mask * sound , data=mean_dispersion_all_transitions_data)
anova(lm_mean_dispersion_all_transitions)
emmip(lm_mean_dispersion_all_transitions, mask ~ sound,CIs=TRUE, xlab="Sound", ylab ="px/frame")


# mean_dispersion_interruption 
mean_dispersion_interruption_data <- read.csv("~/Desktop/Loic/Thesis_new/4_analysis/eyetracking_analysis/mean_dispersion_interruption.csv", comment.char="#")
mean_dispersion_interruption_data <- na.omit(mean_dispersion_interruption_data)
descdist(mean_dispersion_interruption_data$mean_dispersion_interruption, discrete = FALSE) # beta
mean_dispersion_interruption_data <- within(mean_dispersion_interruption_data, {
  mask <- factor(mask)
  sound <- factor(Sound)
})
par(cex = .6)

contrasts(mean_dispersion_interruption_data$mask) <-"contr.sum"
contrasts(mean_dispersion_interruption_data$sound) <-"contr.sum"
m = glm(mean_dispersion_interruption ~ mask*sound, data=mean_dispersion_interruption_data, family=Gamma)
# family=Gamma(link="log") is often used
Anova(m, type=3)

lm_mean_dispersion_interruption= lm(mean_dispersion_interruption ~ mask * sound , data=mean_dispersion_interruption_data)
anova(lm_mean_dispersion_interruption)
emmip(lm_mean_dispersion_interruption, mask ~ sound,CIs=TRUE, xlab="Sound", ylab ="px/frame")


## HMM states
all_states <- read.csv("/Users/zhongs/Desktop/Loic/Thesis_new/4_analysis/states/states_all.csv", comment.char="#")

all_states <- within(all_states, {
  mask <- factor(mask)
  sound <- factor(sound)
  pid <- factor(PID)
})
par(cex = .6)

lmm_state_ratio <- lmer(state1_to_2_ratio ~ mask * sound + (1 | pid), data = all_states)
anova(lmm_state_ratio)

summaryBy(state1_to_2_ratio ~ sound, data = all_states, 
          FUN = list(mean, median, sd))

summaryBy(state1_to_2_ratio ~ mask+sound, data = all_states, # should be state 2 to one
          FUN = list(mean, median, sd))

