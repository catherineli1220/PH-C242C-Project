################################################################################################
# NAME: Longitudinal Data Analysis Final Project
# AUTHORS: Kevin Chen, Matt Lee, Catherine Li
# DATE STARTED: 11/25/2018
# PURPOSE: Analysis of the Framingham Teaching Data Set for PH C242C Final Project.
# UPDATES: 11/25/2018: CL added code to import data set
#
################################################################################################

####### PROGRAM START

# Clear workspace
rm(list=ls())

# Load Packages. Be sure your local computer has these installed before running. No need to re-install each time you run.
library(ggplot2)
library(gee) # for modified poisson
library(lme4)

# Set Working Directory
#setwd("") # Kevin's directory
#setwd("/Users/matthewlee/Box/School/MS Y1/Fall 2018/PH C242C/Final Project Data/") # Matt's directory
setwd("C:/Users/Catherine/Desktop/PB HLTH C242C/Final Project") # Catherine's directory

########################
# IMPORT & CLEAN FRAMINGHAM DATA
########################

  # Import Framingham teaching dataset
  data <- read.csv("frmgham2.csv")
	# Recode people who smoke more than 2/packs a day to 2 packs
	data$CIGPDAY <- ifelse(data$CIGPDAY >= 40, 40, data$CIGPDAY)

########################
# Exploratory visualization
########################

ggplot(data[data$RANDID %in% c(
	sample(unique(data$RANDID[data$educ == 1]), 50),
	sample(unique(data$RANDID[data$educ == 2]), 50),
	sample(unique(data$RANDID[data$educ == 3]), 50),
	sample(unique(data$RANDID[data$educ == 4]), 50)
	),],
		aes(PERIOD, TOTCHOL, group = RANDID)) +
		# geom_path() +
		geom_smooth(method = 'lm' , se = F, aes(col = CIGPDAY), size = 0.5) +
		facet_grid(. ~ educ) +
		xlab('Visit') +
		theme_bw()+ theme(legend.pos = 'bottom')


########################
# OLS WITH BOOTSTRAP
########################

	chol.ols <- lm(
		TOTCHOL ~ factor(educ) + CIGPDAY + factor(educ)*CIGPDAY + AGE + PERIOD,
		data = data)

	clusbootreg <- function(formula, data, cluster, reps=1000) {
	 reg1 <- lm(as.formula(formula), data)
	 clusters <- names(table(cluster))
	 sterrs <- matrix(NA, nrow=reps, ncol=length(coef(reg1)))
	 for(i in 1:reps){
	  index <- sample(1:length(clusters), length(clusters), replace=TRUE)
	  aa <- clusters[index]
	  bb <- table(aa)
	  bootdat <- NULL
	  for(j in 1:max(bb)){
	   cc <- data[cluster %in% names(bb[bb %in% j]),]
	   for(k in 1:j){
	    bootdat <- rbind(bootdat, cc)
	   }
	  }
	  sterrs[i,] <- coef(lm(formula, bootdat))
	 }
	 val <- cbind(coef(reg1),apply(sterrs,2,sd))
	 colnames(val) <- c("Estimate","Std. Error")
	 return(val)
	}

	chol.ols.BS <- clusbootreg(
		'TOTCHOL ~ factor(educ) + CIGPDAY + factor(educ)*CIGPDAY + AGE + PERIOD',
		data = data, cluster = data$RANDID, reps = 1000)

	dplyr::mutate(as.data.frame(chol.ols.BS),
								Names = rownames(chol.ols.BS),
								z = `Estimate`/`Std. Error`,
								`2.5th` = `Estimate` - qnorm(0.975) * `Std. Error`,
								`97.5th` = `Estimate` - qnorm(0.975) * `Std. Error`,
								`Pr(Z>|z|)` = pnorm(abs(`Estimate`/`Std. Error`),
																			lower.tail = F) * 2
								)[,c(3,1,2,4,5,6,7)]


########################
# GEE
########################

gee.fit <- gee(data = data,
               formula = TOTCHOL ~ as.factor(educ) + CIGPDAY + as.factor(educ)*CIGPDAY + AGE + PERIOD,
               id = RANDID,
               corstr = 'exchangeable')
summary(gee.fit)

gee.out <- summary(gee.fit)
gee.est <- as.data.frame(gee.out$coefficients)
gee.rownames <- rownames(gee.est)
gee.est <-
  gee.est %>%
  mutate(
    variable = gee.rownames,
    naive.cilow  = Estimate-1.96*`Naive S.E.`,
    naive.cihigh = Estimate+1.96*`Naive S.E.`,
    naive.p = 2*pnorm(-abs(`Naive z`)),
    robust.cilow  = Estimate-1.96*`Robust S.E.`,
    robust.cihigh = Estimate+1.96*`Robust S.E.`,
    robust.p = 2*pnorm(-abs(`Robust z`))
    ) %>%
  select(variable, everything())




########################
# MIXED MODEL
########################

#RANDOM INTERCEPT (RAND ID)
randint.fit <- lmer(TOTCHOL ~ as.factor(educ) + CIGPDAY + as.factor(educ):CIGPDAY + AGE + PERIOD + (1|RANDID), data = data)
summary(randint.fit)

#RANDOM SLOPE
#randcoeff.fit <- lmer(TOTCHOL ~ CIGPDAY + as.factor(educ):CIGPDAY + AGE + PERIOD + (as.factor(educ)|RANDID), data = data)
randcoeff.fit <- lmer(TOTCHOL ~ factor(educ) + (factor(educ)|RANDID), data = data)
summary(randcoeff.fit)

#END

