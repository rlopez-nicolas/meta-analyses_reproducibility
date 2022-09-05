## ============================================================
## This file contains R code for reproducing the analyses from:
##
## Woll, C.F.J. & Schönbrodt, F.D. (2018). A Series of Meta-Analytic Tests of the Efficacy of Long-Term Psychoanalytic Psychotherapy.
##
## This script file is licensed under a CC-BY 4.0 license. 
## For a human-readable summary of that license, 
## see http://creativecommons.org/licenses/by/4.0/
##
## (c) 2018 Christian Woll (Christian.Woll@psy.lmu.de). Last update: 2018-03-03
## ============================================================

## ============================================================
## This file computes pre-post-controlled Hedges' g for our main outcome (post data based on means and standard deviations AND other metrics!), 
## runs tests for publication bias and conducts sensitivity analyses.
## THIS IS THE CENTRAL SCRIPT FOR THE MAIN RESULTS REPORTED IN THE PAPER
## ============================================================

# (1) install packages if necessary
# install.packages("metafor") 
# install.packages("Matrix")
# install.packages("compute.es")
# install.packages("openxlsx")
# install.packages("plyr")
# install.packages("foreign")
# install.packages("lme4")
# install.packages("lmerTest")

# install.packages("weightr")
# install.packages("devtools")
# devtools::install_github("RobbievanAert/puniform")

# load packages:
#library(puniform)
#library(weightr)
#library(Matrix)
#library(metafor)
#library(compute.es)
#library(openxlsx)
#library(stats)
#library(plyr)
#library(foreign)
#library(lme4)
#library(lmerTest)

pacman::p_load(metafor, openxlsx, plyr)

sum_standard_g <- read.xlsx(here::here("data", "primary_data", 
                              "dat_075.xlsx"), sheet = 2)
sum_standard_g
# now the sum_standard_g variable represents the mixed excel chart (ppc Hedges' g plus yi and vi from special cases and plus 
# standard g for missing ppc g cases)




# example for referencing: bsp4 [(bsp4$Geschlecht=="weiblich") & (bsp4$Einkommen>1500) ,]
# divide data set in pre_post and pre_follow_up data:

# standard Hedges' g:
standard_pre_post_data <- sum_standard_g[(sum_standard_g$Time_of_measurement=="pre_post"),]


## ======================================================================
## Pre-post-data: 
## ======================================================================

# calculate standard hedges' g per outcome category per author:
# psych_sympt_standard:
psych_sympt_standard <- standard_pre_post_data[(standard_pre_post_data$Outcome_category=="psych_sympt"),]

means_standard_psych_sympt_per_author <- aggregate(psych_sympt_standard$yi ~ psych_sympt_standard$Author, FUN = mean)

# target_prob_standard:
target_prob_standard <- standard_pre_post_data[(standard_pre_post_data$Outcome_category=="target_prob"),]

means_standard_target_prob_per_author <- aggregate(target_prob_standard$yi ~ target_prob_standard$Author, FUN = mean)

# social_funct_standard:
social_funct_standard <- standard_pre_post_data[(standard_pre_post_data$Outcome_category=="social_funct"),]

means_standard_social_funct_per_author <- aggregate(social_funct_standard$yi ~ social_funct_standard$Author, FUN = mean)

# pers_funct_standard:
pers_funct_standard <- standard_pre_post_data[(standard_pre_post_data$Outcome_category=="pers_funct"),]

means_standard_pers_funct_per_author <- aggregate(pers_funct_standard$yi ~ pers_funct_standard$Author, FUN = mean)
means_standard_pers_funct_per_author


# overall_effectiveness_standard:
# name the single tables identically to combine them:
#pers funct:
names(means_standard_pers_funct_per_author) <- c("ID", "mean")
names(means_standard_social_funct_per_author) <- c("ID", "mean")
names(means_standard_psych_sympt_per_author) <- c("ID", "mean")
names(means_standard_target_prob_per_author) <- c("ID", "mean")


# combine social funct and pers funct and psych sympt in one data frame:
overall_combined_g <- rbind(means_standard_social_funct_per_author, means_standard_pers_funct_per_author, 
                          means_standard_psych_sympt_per_author)
overall_combined_g

# calulate means of means per study to get overall effectiveness:
overall_effectiveness_g <- ddply(overall_combined_g, .(ID), summarize, mean_value = mean(mean))
overall_effectiveness_g


# VARIANCES to standard Hedges' g:
standard_pre_post_data <- sum_standard_g[(sum_standard_g$Time_of_measurement=="pre_post"),]

standard_pre_follow_up_data <- sum_standard_g[(sum_standard_g$Time_of_measurement=="pre_follow_up"),]

# Pre-post-data:
# calculate VARIANCE of standard hedges' g per outcome category per author:
# psych_sympt_standard:
psych_sympt_standard <- standard_pre_post_data[(standard_pre_post_data$Outcome_category=="psych_sympt"),]

variance_standard_psych_sympt_per_author <- aggregate(psych_sympt_standard$vi ~ psych_sympt_standard$Author, FUN = mean)

# target_prob_standard:
target_prob_standard <- standard_pre_post_data[(standard_pre_post_data$Outcome_category=="target_prob"),]

variance_standard_target_prob_per_author <- aggregate(target_prob_standard$vi ~ target_prob_standard$Author, FUN = mean)

# social_funct_standard:
social_funct_standard <- standard_pre_post_data[(standard_pre_post_data$Outcome_category=="social_funct"),]

variance_standard_social_funct_per_author <- aggregate(social_funct_standard$vi ~ social_funct_standard$Author, FUN = mean)

# pers_funct_standard:
pers_funct_standard <- standard_pre_post_data[(standard_pre_post_data$Outcome_category=="pers_funct"),]

variance_standard_pers_funct_per_author <- aggregate(pers_funct_standard$vi ~ pers_funct_standard$Author, FUN = mean)



# overall_effectiveness_standard:
# name the single tables identically to combine them:
names(variance_standard_pers_funct_per_author) <- c("ID", "variance")
names(variance_standard_social_funct_per_author) <- c("ID", "variance")
names(variance_standard_psych_sympt_per_author) <- c("ID", "variance")
names(variance_standard_target_prob_per_author) <- c("ID", "variance")

# combine social funct and pers funct and psych sympt in one data frame:
overall_combined_variance <- rbind(variance_standard_social_funct_per_author, variance_standard_pers_funct_per_author, 
                          variance_standard_psych_sympt_per_author)
overall_combined_variance

# calulate means of means per study to get overall effectiveness:
overall_effectiveness_variance <- ddply(overall_combined_variance, .(ID), summarize, variance_value = mean(variance))
overall_effectiveness_variance


# Random-effects models:
# psychiatric symptoms:
psych_sympt_ready_for_rma <- merge(means_standard_psych_sympt_per_author, variance_standard_psych_sympt_per_author, by="ID")
psych_sympt_ready_for_rma

# important for forest plot later:
psych_sympt_ready_for_rma$ID
psych_sympt_ready_for_rma$ID <- c("Bachar et al. (1999)", "Bateman & Fonagy (1999)", "Bateman & Fonagy (2009)", 
                                  "Bressi et al. (2010)", 
                                  "Clarkin et al. (2007)", "Doering et al. (2010)", "Fonagy et al. (2015)", 
                                  "van Asselt et al. (2008)", 
                                  "Gregory et al. (2008)", "Huber et al. (2012)", "Jørgensen et al. (2013)", 
                                  "Knekt et al. (2008)",
                                  "Poulsen et al. (2014)", "Svartberg et al. (2004)")
psych_sympt_ready_for_rma
sorted_psych_sympt_ready_for_rma    <- psych_sympt_ready_for_rma[order(psych_sympt_ready_for_rma$ID),]
sorted_psych_sympt_ready_for_rma


rma_psych_sympt <- rma(mean, variance, slab = paste(ID), data=sorted_psych_sympt_ready_for_rma)
rma_psych_sympt

# target problems:
target_prob_ready_for_rma <- merge(means_standard_target_prob_per_author, variance_standard_target_prob_per_author, by="ID")
target_prob_ready_for_rma


# important for forest plot later:
target_prob_ready_for_rma$ID
target_prob_ready_for_rma$ID <- c("Bachar et al. (1999)", "Bateman & Fonagy (1999)", "Bateman & Fonagy (2009)", 
                                  "Bressi et al. (2010)", 
                                  "Clarkin et al. (2007)", "Doering et al. (2010)", "Fonagy et al. (2015)", 
                                  "van Asselt et al. (2008)", 
                                  "Gregory et al. (2008)", "Huber et al. (2012)", "Jørgensen et al. (2013)", 
                                  "Knekt et al. (2008)",
                                  "Poulsen et al. (2014)", "Svartberg et al. (2004)")
target_prob_ready_for_rma
sorted_target_prob_ready_for_rma   <- target_prob_ready_for_rma[order(target_prob_ready_for_rma$ID),]
sorted_target_prob_ready_for_rma


rma_target_prob <- rma(mean, variance, slab = paste(ID), data=sorted_target_prob_ready_for_rma)
rma_target_prob



# social functioning:
social_funct_ready_for_rma <- merge(means_standard_social_funct_per_author, variance_standard_social_funct_per_author, by="ID")
social_funct_ready_for_rma

# important for forest plot later:
social_funct_ready_for_rma$ID
social_funct_ready_for_rma$ID <- c("Bateman & Fonagy (1999)", "Bateman & Fonagy (2009)", 
                                  "Bressi et al. (2010)", 
                                  "Clarkin et al. (2007)", "Doering et al. (2010)", "Fonagy et al. (2015)", 
                                  "van Asselt et al. (2008)", 
                                  "Gregory et al. (2008)", "Huber et al. (2012)", "Jørgensen et al. (2013)", 
                                  "Knekt et al. (2008)",
                                  "Poulsen et al. (2014)", "Svartberg et al. (2004)")

social_funct_ready_for_rma

sorted_social_funct_ready_for_rma   <- social_funct_ready_for_rma[order(social_funct_ready_for_rma$ID),]
sorted_social_funct_ready_for_rma


rma_social_funct <- rma(mean, variance, slab = paste(ID), data=sorted_social_funct_ready_for_rma)
rma_social_funct

# personality functioning:
pers_funct_ready_for_rma <- merge(means_standard_pers_funct_per_author, variance_standard_pers_funct_per_author, by="ID")
pers_funct_ready_for_rma

# important for forest plot later:
pers_funct_ready_for_rma$ID
pers_funct_ready_for_rma$ID <- c("Bachar et al. (1999)", "Bateman & Fonagy (1999)", 
                                  "Levy et al. (2006)", "Doering et al. (2010)", 
                                  "Gregory et al. (2008)", "Huber et al. (2012)", "Jørgensen et al. (2013)", 
                                  "Knekt et al. (2008)",
                                  "Poulsen et al. (2014)", "Svartberg et al. (2004)")
pers_funct_ready_for_rma
sorted_pers_funct_ready_for_rma   <- pers_funct_ready_for_rma[order(pers_funct_ready_for_rma$ID),]
sorted_pers_funct_ready_for_rma


rma_pers_funct <- rma(mean, variance, slab = paste(ID), data=sorted_pers_funct_ready_for_rma)
rma_pers_funct

# rma_pers_funct <- rma(mean, variance, data=pers_funct_ready_for_rma, verbose=TRUE, digits=5, control=list(maxiter=1000))
# rma_pers_funct


# overall_effectiveness:
overall_effectiveness_ready_for_rma <- merge(overall_effectiveness_g, overall_effectiveness_variance, by="ID")
overall_effectiveness_ready_for_rma

# important for forest plot later:
overall_effectiveness_ready_for_rma$ID
overall_effectiveness_ready_for_rma$ID <- c("Bachar et al. (1999)", "Bateman & Fonagy (1999)", "Bateman & Fonagy (2009)", 
                                  "Bressi et al. (2010)", 
                                  "Clarkin et al. (2007)", "Doering et al. (2010)", "Fonagy et al. (2015)", 
                                  "van Asselt et al. (2008)", 
                                  "Gregory et al. (2008)", "Huber et al. (2012)", "Jørgensen et al. (2013)", 
                                  "Knekt et al. (2008)",
                                  "Poulsen et al. (2014)", "Svartberg et al. (2004)")

overall_effectiveness_ready_for_rma
sorted_overall_effectiveness_ready_for_rma   <- overall_effectiveness_ready_for_rma[order(overall_effectiveness_ready_for_rma$ID),]
sorted_overall_effectiveness_ready_for_rma


rma_overall_effectiveness <- rma(mean_value, variance_value, slab = paste(ID), data=sorted_overall_effectiveness_ready_for_rma)
rma_overall_effectiveness

rm(means_standard_pers_funct_per_author, means_standard_psych_sympt_per_author, means_standard_social_funct_per_author, means_standard_target_prob_per_author, 
     overall_combined_g, overall_combined_variance, overall_effectiveness_g, overall_effectiveness_ready_for_rma, overall_effectiveness_variance,
     pers_funct_ready_for_rma, pers_funct_standard, social_funct_ready_for_rma, social_funct_standard, sorted_overall_effectiveness_ready_for_rma, sorted_pers_funct_ready_for_rma, sorted_psych_sympt_ready_for_rma,
     sorted_social_funct_ready_for_rma, sorted_target_prob_ready_for_rma, standard_pre_follow_up_data, standard_pre_post_data,
     sum_standard_g, target_prob_ready_for_rma, target_prob_standard, variance_standard_pers_funct_per_author, variance_standard_psych_sympt_per_author, variance_standard_social_funct_per_author,
     variance_standard_target_prob_per_author, psych_sympt_ready_for_rma, psych_sympt_standard)

