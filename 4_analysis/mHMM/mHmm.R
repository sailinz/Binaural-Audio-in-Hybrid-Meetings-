library(readr)
library(mHMMbayes)
library(devtools)
library(RColorBrewer)
library(mcmcse)

#https://stats.stackexchange.com/questions/49570/effective-sample-size-for-posterior-inference-from-mcmc-sampling/224244#224244
minESS(p = 18, alpha = .05, eps = .05)

dialog_id = "5"

Sound_lab <- c("Same speaker(S)", "Different speaker(D)", "Exterior(E)","No fixation(N)")
Speech_lab <-  c("Same speaker(S)", "Different speaker(D)", "Either speaker(B)", "Exterior(E)","No fixation/speech(N)")

# load observation per dialog 
# consolidate_obs_2 <- read_csv("eyetracking_analysis/hmm_observations/all/consolidate_obs_3.csv")
consolidate_obs_2 <- read_csv(paste("eyetracking_analysis/hmm_observations/all/consolidate_obs_", dialog_id, ".csv",sep = ""))

## creating HMM data
# id <- c(0,1,2,3,4)
id <- unique(consolidate_obs_2$pid)
input1 <- paste("eyetracking_analysis/hmm_observations/dialog", dialog_id, "/obs_dialog", dialog_id, "_p", id, ".csv", sep = "")
# input1 <- paste("eyetracking_analysis/hmm_observations/dialog2/test2/obs_dialog2_p", id, ".csv", sep = "")

HMM_dyad_data <- vector("list", 42)
workdata <- numeric(1)
  

for(i in 1:42){
  workdata <- read.csv2(input1[i], header = TRUE, dec = ",", sep = ",")[,3:4]
  # make sure factors levels are in correct order
  workdata$o_sound <- factor(workdata$o_sound, levels = c("S", "D", "E","N"))
  workdata$o_sound <- unclass(workdata$o_sound)
  workdata$o_speech_act <- factor(workdata$o_speech_act, levels = c("S","D","B","E","N"))
  workdata$o_speech_act <- unclass(workdata$o_speech_act)
  
  HMM_dyad_data[[i]] <- list(y = as.matrix(workdata), rownames.force = FALSE)
}
length(HMM_dyad_data)

obs_2 <- cbind(1,HMM_dyad_data[[1]]$y)
for(i in 2:42){
  obs_2 <- rbind(obs_2,cbind(i,HMM_dyad_data[[i]]$y))
}
colnames(obs_2) <- c("id", "o_sound", "o_speech_act")

# Determining the most likely state sequence
state_seq <- vit_mHMM(out5, s_data = obs_2)
head(state_seq)
write.csv(state_seq, paste("./states/dialog", dialog_id,".csv", sep=""))

# s_data = obs_2
# id         <- unique(s_data[,1])
# n_subj     <- length(id)

## set HMM initial parameters
m <-2 # number of states
n_dep <- 2 # number of dependent variables
q_emiss <- c(4, 5) # number of categorical outcomes of each DV

start_TM <- diag(.8, m)
start_TM[lower.tri(start_TM) | upper.tri(start_TM)] <- .2
start_EM <- list(matrix(c(0.56,	0.34,	0.03,	0.07,
                          0.21,	0.12,	0.18,	0.49
                          ), byrow = TRUE, nrow = m, ncol = q_emiss[1]), # sound direction
                 matrix(c(0.50,	0.34,	0.06,	0.03,	0.07,
                          0.17,	0.12,	0.02,	0.18,	0.51
                          ), byrow = TRUE, nrow = m, ncol = q_emiss[2])) # speech act
                

# Run a model without covariate(s):
set.seed(10)
out2 <- mHMM(s_data = obs_2,
             gen = list(m = m, n_dep = n_dep, q_emiss = q_emiss),
             start_val = c(list(start_TM), start_EM),
             mcmc = list(J = 30, burn_in = 3))
             # mcmc = list(J = 1000, burn_in = 200))
save(out2, file = "dialog2_2st_1000it.rda")

# show estimated parameters: the point estimates of the posterior distribution for the transition probability matrix and the emission distribution of each of the dependent variables at the group level
summary(out2)

# show subject level parameters:
gamma_subj <- obtain_gamma(out2, level = "subject")
gamma_subj
emiss_subj<- obtain_emiss(out2, level = "subject")
emiss_subj


## Graphically displaying outcomes
# plot the posterior densities for the transition and emission probabilities
plot(out2, component = "gamma", col =c("darkslategray3", "goldenrod"))

# sound direction'
Sound_col <- c(brewer.pal(4,"Set1")[c(1,2,3,4)])
Sound_lab <- c("Same speaker(S)", "Different speaker(D)", "Exterior(E)","No fixation(N)")
plot_emission(out2, component = "emiss", dep = 1, col = Sound_col, 
     dep_lab = c("ROI and Sound direction"), cat_lab = Sound_lab)

# speech act
Speech_col <- c(brewer.pal(5,"Set1")[c(1,2,3,4,5)])
Speech_lab <- c("Same speaker(S)", "Different speaker(D)", "Either speaker(B)", "Exterior(E)","No fixation/speech(N)")
plot_emission(out2, component = "emiss", dep = 2, col = Speech_col, 
          dep_lab = c("ROI and Speech act"), cat_lab = Speech_lab)
# legend('topright',Speech_lab , lty=1, col=Speech_col, bty='n', cex=.75)

# Transition probabilities at the group level and for subject number 1, respectively:
gamma_pop <- obtain_gamma(out2)
plot(gamma_pop, col = rep(rev(brewer.pal(3,"PiYG"))[-2], each = m))
gamma_subj <- obtain_gamma(out2, level = "subject")
plot(gamma_subj, subj_nr = 1, col = rep(rev(brewer.pal(3,"PiYG"))[-2], each = m))

## Determining the most likely state sequence
state_seq <- vit_mHMM(out2, s_data = obs_2)
head(state_seq)

## Checking model convergence and label switching
## set HMM initial parameters
m <-2 # number of states
n_dep <- 2 # number of dependent variables
q_emiss <- c(4, 5) # number of categorical outcomes of each DV

start_TM <- diag(.8, m)
start_TM[lower.tri(start_TM) | upper.tri(start_TM)] <- .2
start_EM_b <- list(matrix(c(0.4,	0.2,	0.15,	0.25,
                          0.15,	0.2,	0.25,	0.4
                  ), byrow = TRUE, nrow = m, ncol = q_emiss[1]), # sound direction
                  matrix(c(0.4,	0.3,	0.1,	0.1,	0.1,
                           0.25,	0.2,	0.1,	0.2,	0.45
                  ), byrow = TRUE, nrow = m, ncol = q_emiss[2])) # speech act


# Run a model without covariate(s):
set.seed(20)
out2_b <- mHMM(s_data = obs_2,
             gen = list(m = m, n_dep = n_dep, q_emiss = q_emiss),
             start_val = c(list(start_TM), start_EM_b),
             mcmc = list(J = 30, burn_in = 3))
save(out2_b, file = "dialog2_2st_1000it_b.rda")

#label switching
par(mfrow = c(m,q_emiss[2]))
for(i in 1:m){
  for(q in 1:q_emiss[2]){
    plot(x = 1:30, y = out2$emiss_prob_bar[[2]][,(i-1) * q_emiss[2] + q], 
         ylim = c(0,1.4), yaxt = 'n', type = "l", ylab = "Transition probability",
         xlab = "Iteration", main = paste("AOI v.s. Speech act:", Speech_lab[q], "in state", i), col = "#8da0cb") 
    axis(2, at = seq(0,1, .2), las = 2)
    lines(x = 1:30, y = out2_b$emiss_prob_bar[[2]][,(i-1) * q_emiss[2] + q], col = "#e78ac3")
    legend("topright", col = c("#8da0cb", "#e78ac3"), lwd = 2, 
           legend = c("Starting value set 1", "Starting value set 2"), bty = "n")
  }
}





########### visualize all dialogs ##############
load("~/Desktop/Loic/Thesis_new/4_analysis/figures_300it/dialog2_2st_300it.rda")
load("~/Desktop/Loic/Thesis_new/4_analysis/figures_300it/dialog3_2st_300it.rda")
load("~/Desktop/Loic/Thesis_new/4_analysis/figures_300it/dialog4_2st_300it.rda")
load("~/Desktop/Loic/Thesis_new/4_analysis/figures_300it/dialog5_2st_300it.rda")

summary(out2)
summary(out3)
summary(out4)
summary(out5)

# state
# speech act
par(mfrow = c(m,q_emiss[2]))
for(i in 1:m){
  for(q in 1:q_emiss[2]){
    plot(x = 1:300, y = out2$emiss_prob_bar[[2]][,(i-1) * q_emiss[2] + q], 
         ylim = c(0,1.4), yaxt = 'n', type = "l", ylab = "Transition probability",
         xlab = "Iteration", main = paste("Speech act:", Speech_lab[q], "in state", i), col = "#8da0cb") 
    axis(2, at = seq(0,1, .2), las = 2)
    lines(x = 1:300, y = out3$emiss_prob_bar[[2]][,(i-1) * q_emiss[2] + q], col = "#228B22")
    
    axis(3, at = seq(0,1, .2), las = 2)
    lines(x = 1:300, y = out4$emiss_prob_bar[[2]][,(i-1) * q_emiss[2] + q], col = "#e56119")
    
    axis(4, at = seq(0,1, .2), las = 2)
    lines(x = 1:300, y = out5$emiss_prob_bar[[2]][,(i-1) * q_emiss[2] + q], col = "#f4c34d")
    
    legend("topright", col = c("#8da0cb", "#228B22", "#e78ac3", "#f4c34d"), lwd = 2, 
           legend = c("head (mask)", "binaural (no mask)", "head (no mask)", "binaural (mask)"), bty = "n")
  }
}

# sound
par(mfrow = c(m,q_emiss[1]))
for(i in 1:m){
  for(q in 1:q_emiss[1]){
    plot(x = 1:300, y = out2$emiss_prob_bar[[1]][,(i-1) * q_emiss[1] + q], 
         ylim = c(0,1.4), yaxt = 'n', type = "l", ylab = "Transition probability",
         xlab = "Iteration", main = paste("Sound:", Speech_lab[q], "in state", i), col = "#8da0cb") 
    axis(2, at = seq(0,1, .2), las = 2)
    lines(x = 1:300, y = out3$emiss_prob_bar[[1]][,(i-1) * q_emiss[1] + q], col = "#228B22")
    
    axis(3, at = seq(0,1, .2), las = 2)
    lines(x = 1:300, y = out4$emiss_prob_bar[[1]][,(i-1) * q_emiss[1] + q], col = "#e56119")
    
    axis(4, at = seq(0,1, .2), las = 2)
    lines(x = 1:300, y = out5$emiss_prob_bar[[1]][,(i-1) * q_emiss[1] + q], col = "#f4c34d")
    
    legend("topright", col = c("#8da0cb", "#228B22", "#e78ac3", "#f4c34d"), lwd = 2, 
           legend = c("head (mask)", "binaural (no mask)", "head (no mask)", "binaural (mask)"), bty = "n")
  }
}

## Graphically displaying outcomes
mycols_sound <- c("#00A5E3", "#8DD7BF", "#FF96C5", "#FF5768")
mycols_speech_act <- c("#00A5E3", "#8DD7BF", "#ffbf65", "#FF96C5", "#FF5768")

output = out5
## emission probability
# speech act
Speech_col <- c(mycols_speech_act[c(1,2,3,4,5)])
Speech_lab <- c("Same speaker(S)", "Different speaker(D)", "Either speaker(B)", "Exterior(E)","No fixation/speech(N)")
plot_emission(output, component = "emiss", dep = 2, col = Speech_col, 
              dep_lab = c("ROI and Speech act"), cat_lab = Speech_lab)

# sound direction'
Sound_col <- c(mycols_sound[c(1,2,3,4)])
Sound_lab <- c("Same speaker(S)", "Different speaker(D)", "Exterior(E)","No fixation(N)")
plot_emission(output, component = "emiss", dep = 1, col = Sound_col, 
              dep_lab = c("ROI and Sound direction"), cat_lab = Sound_lab)



## not much difference for transition probabilities among participants so ignore this
gamma_subj <- obtain_gamma(output, level = "subject")
gamma_subj
## emission probabilities per subject 
emiss_subj<- obtain_emiss(out5, level = "subject")
emiss_subj[["o_sound"]]
emiss_subj[["o_speech_act"]]
write.csv(emiss_subj[["o_sound"]], "./emiss/sound/dialog5.csv")
write.csv(emiss_subj[["o_speech_act"]], "./emiss/speech_act/dialog5.csv")




#patch the plot function
plot_emission <- function(x, component = "gamma", dep = 1, col, cat_lab,
                          dep_lab, lwd1 = 2, lwd2 = 1, lty1 = 1, lty2 = 3,
                          legend_cex, burn_in, ...){
  if (component != "gamma" & component != "emiss"){
    stop("The input specified under component should be a string, restrectid to state either gamma or emiss.")
  }
  object <- x
  input   <- x$input
  n_subj  <- input$n_subj
  
  # print(n_subj)
  
  if (missing(burn_in)){
    burn_in <- input$burn_in
  }
  J       <- input$J
  if (burn_in >= (J-1)){
    stop(paste("The specified burn in period should be at least 2 points smaller
               compared to the number of iterations J, J =", J))
  }
  old_par <- graphics::par(no.readonly =TRUE)
  on.exit(graphics::par(old_par))
  m       <- input$m
  q_emiss <- input$q_emiss
  n_dep   <- input$n_dep
  
  if(component == "gamma"){
    if (missing(col)){
      state_col <- grDevices::rainbow(m)
    } else {
      state_col <- col
    }
    if(m > 3){
      graphics::par(mfrow = c(2,ceiling(m/2)), mar = c(4,2,3,1) + 0.1, mgp = c(2,1,0))
    } else {
      graphics::par(mfrow = c(1,m), mar = c(4,2,3,1) + 0.1, mgp = c(2,1,0))
    }
    for(i in 1:m){
      max <- 0
      for(j in 1:m){
        new <- max(stats::density(object$gamma_prob_bar[burn_in:J, m * (i-1) + j])$y)
        if(new > max){max <- new}
      }
      graphics::plot.default(x = 1, ylim = c(0, max), xlim = c(0,1), type = "n", cex = .8,  main =
                               paste("From state", i, "to state ..."), yaxt = "n", ylab = "",
                             xlab = "Transition probability", ...)
      graphics::title(ylab="Density", line=.5)
      for(j in 1:m){
        graphics::lines(stats::density(object$gamma_prob_bar[burn_in:J,m * (i-1) + j]),
                        type = "l", col = state_col[j], lwd = lwd1, lty = lty1)
        for(s in 1:n_subj){
          graphics::lines(stats::density(object$PD_subj[[s]][burn_in:J,(sum(q_emiss * m) + m * (i-1) + j)]),
                          type = "l", col = state_col[j], lwd = lwd2, lty = lty2)
        }
      }
      graphics::legend("topright", col = state_col, legend = paste("To state", 1:m),
                       bty = 'n', lty = 1, lwd = 2, cex = .8)
    }
  } else if (component == "emiss"){
    if (missing(cat_lab)){
      cat_lab <- paste("Category", 1:q_emiss[dep])
    }
    if (missing(dep_lab)){
      dep_lab <- input$dep_labels[dep]
    }
    start <- c(0, q_emiss * m)
    start2 <- c(0, seq(from = (q_emiss[dep]-1) * 2, to = (q_emiss[dep]-1) * 2 * m, by = (q_emiss[dep]-1) * 2))
    if (missing(col)){
      cat_col <- grDevices::rainbow(q_emiss[dep])
    } else {
      cat_col <- col
    }
    if(m > 3){
      graphics::par(mfrow = c(2,ceiling(m/2)), mar = c(4,2,3,1) + 0.1, mgp = c(2,1,0))
    } else {
      graphics::par(mfrow = c(1,m), mar = c(4,2,3,1) + 0.1, mgp = c(2,1,0))
    }
    for(i in 1:m){
      # determining the scale of the y axis
      max <- 0
      for(q in 1:q_emiss[dep]){
        new <- max(stats::density(object$emiss_prob_bar[[dep]][burn_in:J,q_emiss[dep] * (i-1) + q])$y)
        if(new > max){max <- new}
      }
      print(max)
      max = 250
      # set plotting area
      graphics::plot.default(x = 1, ylim = c(0, max), xlim = c(0,1), type = "n",
                             # main = paste(dep_lab, ", state", i),
                             main = "",
                             yaxt = "n", ylab = "", xlab = "Conditional probability", ...)
      graphics::title(ylab="Density", line=.5)
      for(q in 1:q_emiss[dep]){
        # add density curve for population level posterior distribution
        graphics::lines(stats::density(object$emiss_prob_bar[[dep]][burn_in:J,q_emiss[dep] * (i-1) + q]),
                        type = "l", col = cat_col[q], lwd = lwd1, lty = lty1)
        # add density curves for subject posterior distributions
        for(s in 1:n_subj){
          # print(s)
          graphics::lines(stats::density(object$PD_subj[[s]][burn_in:J,(sum(start[1:dep])
                                                                        + (i-1)*q_emiss[dep] + q)]),
                          type = "l", col = cat_col[q], lwd = lwd2, lty = lty2)
        }
      }
      graphics::legend("topright", col = cat_col, legend = cat_lab, bty = 'n', lty = 1, lwd = 2, cex = .7)
    }
  }
}




