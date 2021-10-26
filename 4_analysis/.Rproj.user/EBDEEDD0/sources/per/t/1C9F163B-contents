library(readr)
library(mHMMbayes)
# library(devtools)
library(RColorBrewer)
library(mcmcse)

#https://stats.stackexchange.com/questions/49570/effective-sample-size-for-posterior-inference-from-mcmc-sampling/224244#224244

### dialog 1 ####
minESS(p = 18, alpha = .05, eps = .05)

dialog_id = "1"
num_part = 42

Speech_lab <-  c("Same speaker(S)", "Different speaker(D)", "Either speaker(B)", "Exterior(E)","No fixation/speech(N)")

# load observation per dialog 
consolidate_obs_1 <- read_csv(paste("eyetracking_analysis/hmm_observations/all/consolidate_obs_", dialog_id, ".csv",sep = ""))

## creating HMM data
id <- unique(consolidate_obs_1$pid)
input1 <- paste("eyetracking_analysis/hmm_observations/dialog", dialog_id, "/obs_dialog", dialog_id, "_p", id, ".csv", sep = "")

HMM_dyad_data <- vector("list", num_part)
workdata <- numeric(1)

for(i in 1:num_part){
  workdata <- read.csv2(input1[i], header = TRUE, dec = ",", sep = ",")[,4] # only the speech act
  workdata<- factor(workdata, levels = c("S","D","B","E","N"))
  workdata<- unclass(workdata)
  
  HMM_dyad_data[[i]] <- list(y = as.matrix(workdata), rownames.force = FALSE)
}
length(HMM_dyad_data)

obs_1 <- cbind(1,HMM_dyad_data[[1]]$y)
for(i in 2:num_part){
  obs_1 <- rbind(obs_1,cbind(i,HMM_dyad_data[[i]]$y))
}
colnames(obs_1) <- c("id", "o_speech_act")

## Determining the most likely state sequence
# state_seq <- vit_mHMM(out1, s_data = obs_1)
# head(state_seq)
# write.csv(state_seq, paste("./states/dialog", dialog_id,".csv", sep=""))


## set HMM initial parameters
m <-2 # number of states
n_dep <- 1 # number of dependent variables
q_emiss <- c(5) # number of categorical outcomes of each DV

start_TM <- diag(.8, m)
start_TM[lower.tri(start_TM) | upper.tri(start_TM)] <- .2
start_EM <- list(matrix(c(0.50,	0.34,	0.06,	0.03,	0.07,
                          0.17,	0.12,	0.02,	0.18,	0.51
                          ), byrow = TRUE, nrow = m, ncol = q_emiss[1])) # speech act
                

# Run a model without covariate(s):
set.seed(678)
out1 <- mHMM(s_data = obs_1,
             gen = list(m = m, n_dep = n_dep, q_emiss = q_emiss),
             start_val = c(list(start_TM), start_EM),
             mcmc = list(J = 6, burn_in = 2))
save(out1, file = "dialog1_1st_300it.rda")

# Determining the most likely state sequence
state_seq <- vit_mHMM(out1, s_data = obs_1)
head(state_seq)
write.csv(state_seq, paste("./states/dialog", "1",".csv", sep=""))

# show estimated parameters: the point estimates of the posterior distribution for the transition probability matrix and the emission distribution of each of the dependent variables at the group level
summary(out1)
emiss_subj<- obtain_emiss(out1, level = "subject")
emiss_subj


### dialog 6 ###
minESS(p = 18, alpha = .05, eps = .05)

dialog_id = "6"
num_part = 42

Speech_lab <-  c("Same speaker(S)", "Different speaker(D)", "Either speaker(B)", "Exterior(E)","No fixation/speech(N)")

# load observation per dialog 
consolidate_obs_6 <- read_csv(paste("eyetracking_analysis/hmm_observations/all/consolidate_obs_", dialog_id, ".csv",sep = ""))

## creating HMM data
id <- unique(consolidate_obs_6$pid)
input1 <- paste("eyetracking_analysis/hmm_observations/dialog", dialog_id, "/obs_dialog", dialog_id, "_p", id, ".csv", sep = "")

HMM_dyad_data <- vector("list", num_part)
workdata <- numeric(1)

for(i in 1:num_part){
  workdata <- read.csv2(input1[i], header = TRUE, dec = ",", sep = ",")[,4] # only the speech act
  workdata<- factor(workdata, levels = c("S","D","B","E","N"))
  workdata<- unclass(workdata)
  
  HMM_dyad_data[[i]] <- list(y = as.matrix(workdata), rownames.force = FALSE)
}
length(HMM_dyad_data)

obs_6 <- cbind(1,HMM_dyad_data[[1]]$y)
for(i in 2:num_part){
  obs_6 <- rbind(obs_6,cbind(i,HMM_dyad_data[[i]]$y))
}
colnames(obs_6) <- c("id", "o_speech_act")


## set HMM initial parameters
m <-2 # number of states
n_dep <- 1 # number of dependent variables
q_emiss <- c(5) # number of categorical outcomes of each DV

start_TM <- diag(.8, m)
start_TM[lower.tri(start_TM) | upper.tri(start_TM)] <- .2
start_EM <- list(matrix(c(0.50,	0.34,	0.06,	0.03,	0.07,
                          0.17,	0.12,	0.02,	0.18,	0.51
), byrow = TRUE, nrow = m, ncol = q_emiss[1])) # speech act


# Run a model without covariate(s):
set.seed(868)
out6 <- mHMM(s_data = obs_6,
             gen = list(m = m, n_dep = n_dep, q_emiss = q_emiss),
             start_val = c(list(start_TM), start_EM),
             mcmc = list(J = 300, burn_in = 30))
save(out6, file = "dialog6_1st_300it.rda")

state_seq <- vit_mHMM(out6, s_data = obs_6)
head(state_seq)
write.csv(state_seq, paste("./states/dialog", "6",".csv", sep=""))

# show estimated parameters: the point estimates of the posterior distribution for the transition probability matrix and the emission distribution of each of the dependent variables at the group level
summary(out6)
emiss_subj<- obtain_emiss(out6, level = "subject")
emiss_subj




load("~/Desktop/Loic/Thesis_new/4_analysis/figures_300it/dialog1_1st_300it.rda")
load("~/Desktop/Loic/Thesis_new/4_analysis/figures_300it/dialog6_1st_300it.rda")




## Graphically displaying outcomes
output = out6
mycols_sound <- c("#00A5E3", "#8DD7BF", "#FF96C5", "#FF5768")
mycols_speech_act <- c("#00A5E3", "#8DD7BF", "#ffbf65", "#FF96C5", "#FF5768")

Speech_col <- c(mycols_speech_act[c(1,2,3,4,5)])
Speech_lab <- c("Same speaker(S)", "Different speaker(D)", "Either speaker(B)", "Exterior(E)","No fixation/speech(N)")
plot_emission(output, component = "emiss", dep = 1, col = Speech_col, 
              dep_lab = c("ROI and Speech act"), cat_lab = Speech_lab)





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
      # graphics::legend("topright", col = cat_col, legend = cat_lab, bty = 'n', lty = 1, lwd = 2, cex = .7)
    }
  }
}




