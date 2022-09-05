####Importing data####

load(file = here::here("data", "first_stage", "data.Rdata"))

####Imputation of non-available analytic details through the software used.####

df_mas<- df_mas %>% 
  filter(orign_data!="not_available") %>% 
  mutate(tau2_estimator=replace(tau2_estimator, 
                                Model== "random-effects" &
                                  tau2_estimator=="not_available" &           #REML metafor's default tau2 estimator
                                  software=="R (metafor)", 
                                "REML")) %>% 
  mutate(tau2_estimator=replace(tau2_estimator, 
                                Model=="random-effects" &
                                  tau2_estimator=="not_available" &
                                  software=="HLM6 statistical software", 
                                "ML")) %>%                                      #I could not find the default tau2 estimator of HLM6, but https://doi.org/10.1002/jrsm.1164
  #mentions ML and REML as the only available options. 
  mutate(tau2_estimator=replace(tau2_estimator,
                                Model== "random-effects" &
                                  tau2_estimator=="not_available" &             #All others use DL by default.
                                  (software!= "R (metafor)" | software !="HLM6 statistical software"), 
                                "DL")) %>%
  mutate(weight_factor=replace(weight_factor, 
                               weight_factor=="not_available", 
                               "inverse_variance")) %>% 
  mutate(AC=UL_CI-LL_CI) 


#Checking the different combinations to be programmed. 

unique_methods_combinations<- crossing(nesting(df_mas$Model, df_mas$weight_factor, df_mas$tau2_estimator, df_mas$ci_method))


####Computational reproducibility####

#Running the original analysis script code of the papers with analysis script availability 

source(here::here("analysis", "script_075.r"))

#Store the computational reproduced results 

computational_rep_mas<- list(rma_overall_effectiveness, rma_psych_sympt, rma_target_prob, rma_social_funct, rma_pers_funct)

####Analytic reproduction loop####

rep_mas<-vector("list", nrow(df_mas)) #Empty list to store reproduced models

rep_k<- rep(0, nrow(df_mas))          #Empty vectors to store reproduced statistics
rep_g<-rep(0, nrow(df_mas))
rep_ll_g<-rep(0, nrow(df_mas))
rep_ul_g<-rep(0, nrow(df_mas))
rep_ac_g<-rep(0, nrow(df_mas))
rep_z<-rep(0, nrow(df_mas))
rep_q<-rep(0, nrow(df_mas))
rep_i2<-rep(0, nrow(df_mas))

t<-0
h<-0

for (j in 1:nrow(df_mas)) {
  
  if(df_mas[j,]$Model=="fixed-effect" & df_mas[j,]$weight_factor=="inverse_variance" & df_mas[j,]$script == 2) {
    
    h<-h+1
    rep_mas[[j]]<- rma(yi, sei=sei, method = "FE", data = mas_df[[h]])
    
    rep_k[j]<- rep_mas[[j]]$k
    rep_g[j]<- rep_mas[[j]]$b[1]
    rep_ll_g[j]<- rep_mas[[j]]$ci.lb
    rep_ul_g[j]<- rep_mas[[j]]$ci.ub
    rep_ac_g[j]<- rep_mas[[j]]$ci.ub-rep_mas[[j]]$ci.lb
    rep_z[j]<- rep_mas[[j]]$zval
    rep_q[j]<- rep_mas[[j]]$QE
    rep_i2[j]<- rep_mas[[j]]$I2
    
  }
  
  else if (df_mas[j,]$Model=="fixed-effect" & df_mas[j,]$weight_factor=="sample_size" & df_mas[j,]$script == 2)
  { 
    h<-h+1
    rep_mas[[j]]<- rma(yi, sei=sei, method = "FE", weights = ni+nc, data = mas_df[[h]])
    
    rep_k[j]<- rep_mas[[j]]$k
    rep_g[j]<- rep_mas[[j]]$b[1]
    rep_ll_g[j]<- rep_mas[[j]]$ci.lb
    rep_ul_g[j]<- rep_mas[[j]]$ci.ub
    rep_ac_g[j]<- rep_mas[[j]]$ci.ub-rep_mas[[j]]$ci.lb
    rep_z[j]<- rep_mas[[j]]$zval
    rep_q[j]<- rep_mas[[j]]$QE
    rep_i2[j]<- rep_mas[[j]]$I2
    
    
  }
  
  else if (df_mas[j,]$Model=="Hunter and Schmidt's bare bones" & df_mas[j,]$script == 2)
  {
    h<-h+1
    rep_mas[[j]]<- rma(yi, sei=sei, method = "HS", weights = ni+nc, data = mas_df[[h]])
    
    rep_k[j]<- rep_mas[[j]]$k
    rep_g[j]<- rep_mas[[j]]$b[1]
    rep_ll_g[j]<- rep_mas[[j]]$b[1]-(1.28*sqrt(rep_mas[[j]]$tau2))
    rep_ul_g[j]<- rep_mas[[j]]$b[1]+(1.28*sqrt(rep_mas[[j]]$tau2))
    rep_ac_g[j]<- rep_ul_g[j]-rep_ll_g[j]
    rep_z[j]<- rep_mas[[j]]$zval
    rep_q[j]<- rep_mas[[j]]$QE
    rep_i2[j]<- rep_mas[[j]]$I2
    
    
  }
  
  else if (df_mas[j,]$Model=="random-effects" & df_mas[j,]$weight_factor=="inverse_variance"
           & df_mas[j,]$tau2_estimator=="DL" & df_mas[j,]$ci_method==1 & df_mas[j,]$script == 2){
    
    h<-h+1
    rep_mas[[j]]<- rma(yi, sei=sei, method = "DL", test = "knha", data = mas_df[[h]])
    
    rep_k[j]<- rep_mas[[j]]$k
    rep_g[j]<- rep_mas[[j]]$b[1]
    rep_ll_g[j]<- rep_mas[[j]]$ci.lb
    rep_ul_g[j]<- rep_mas[[j]]$ci.ub
    rep_ac_g[j]<- rep_mas[[j]]$ci.ub-rep_mas[[j]]$ci.lb
    rep_z[j]<- rep_mas[[j]]$zval
    rep_q[j]<- rep_mas[[j]]$QE
    rep_i2[j]<- rep_mas[[j]]$I2
    
    
    
    
  }
  
  else if (df_mas[j,]$Model=="random-effects" & df_mas[j,]$weight_factor=="inverse_variance"
           & df_mas[j,]$tau2_estimator=="DL" & df_mas[j,]$ci_method==2 & df_mas[j,]$script == 2){
    
    h<-h+1
    rep_mas[[j]]<- rma(yi, sei=sei, method = "DL", data = mas_df[[h]])
    
    rep_k[j]<- rep_mas[[j]]$k
    rep_g[j]<- rep_mas[[j]]$b[1]
    rep_ll_g[j]<- rep_mas[[j]]$ci.lb
    rep_ul_g[j]<- rep_mas[[j]]$ci.ub
    rep_ac_g[j]<- rep_mas[[j]]$ci.ub-rep_mas[[j]]$ci.lb
    rep_z[j]<- rep_mas[[j]]$zval
    rep_q[j]<- rep_mas[[j]]$QE
    rep_i2[j]<- rep_mas[[j]]$I2
    
    
    
    
  }
  
  else if (df_mas[j,]$Model=="random-effects" & df_mas[j,]$weight_factor=="inverse_variance"
           & df_mas[j,]$tau2_estimator=="ML" & df_mas[j,]$ci_method==2 & df_mas[j,]$script == 2){
    
    h<-h+1
    rep_mas[[j]]<- rma(yi, sei=sei, method = "ML", data = mas_df[[h]])
    
    rep_k[j]<- rep_mas[[j]]$k
    rep_g[j]<- rep_mas[[j]]$b[1]
    rep_ll_g[j]<- rep_mas[[j]]$ci.lb
    rep_ul_g[j]<- rep_mas[[j]]$ci.ub
    rep_ac_g[j]<- rep_mas[[j]]$ci.ub-rep_mas[[j]]$ci.lb
    rep_z[j]<- rep_mas[[j]]$zval
    rep_q[j]<- rep_mas[[j]]$QE
    rep_i2[j]<- rep_mas[[j]]$I2
    
    
    
    
  }
  
  else if (df_mas[j,]$Model=="random-effects" & df_mas[j,]$weight_factor=="inverse_variance"
           & df_mas[j,]$tau2_estimator=="REML" & df_mas[j,]$ci_method==1 & df_mas[j,]$script == 2){
    
    h<-h+1
    rep_mas[[j]]<- rma(yi, sei=sei, method = "REML", test = "knha", data = mas_df[[h]])
    
    rep_k[j]<- rep_mas[[j]]$k
    rep_g[j]<- rep_mas[[j]]$b[1]
    rep_ll_g[j]<- rep_mas[[j]]$ci.lb
    rep_ul_g[j]<- rep_mas[[j]]$ci.ub
    rep_ac_g[j]<- rep_mas[[j]]$ci.ub-rep_mas[[j]]$ci.lb
    rep_z[j]<- rep_mas[[j]]$zval
    rep_q[j]<- rep_mas[[j]]$QE
    rep_i2[j]<- rep_mas[[j]]$I2
    
    
    
    
  }
  
  else if (df_mas[j,]$Model=="random-effects" & df_mas[j,]$weight_factor=="inverse_variance"
           & df_mas[j,]$tau2_estimator=="REML" & df_mas[j,]$ci_method==2 & df_mas[j,]$script == 2){
    
    h<-h+1
    rep_mas[[j]]<- rma(yi, sei=sei, method = "REML", data = mas_df[[h]])
    
    rep_k[j]<- rep_mas[[j]]$k
    rep_g[j]<- rep_mas[[j]]$b[1]
    rep_ll_g[j]<- rep_mas[[j]]$ci.lb
    rep_ul_g[j]<- rep_mas[[j]]$ci.ub
    rep_ac_g[j]<- rep_mas[[j]]$ci.ub-rep_mas[[j]]$ci.lb
    rep_z[j]<- rep_mas[[j]]$zval
    rep_q[j]<- rep_mas[[j]]$QE
    rep_i2[j]<- rep_mas[[j]]$I2
    
    
    
    
  }
  
  else if (df_mas[j,]$Model=="random-effects" & df_mas[j,]$weight_factor=="inverse_variance"
           & df_mas[j,]$tau2_estimator=="SJ" & df_mas[j,]$ci_method==1 & df_mas[j,]$script == 2){
    
    h<-h+1
    rep_mas[[j]]<- rma(yi, sei=sei, method = "SJ", test = "knha", data = mas_df[[h]])
    
    rep_k[j]<- rep_mas[[j]]$k
    rep_g[j]<- rep_mas[[j]]$b[1]
    rep_ll_g[j]<- rep_mas[[j]]$ci.lb
    rep_ul_g[j]<- rep_mas[[j]]$ci.ub
    rep_ac_g[j]<- rep_mas[[j]]$ci.ub-rep_mas[[j]]$ci.lb
    rep_z[j]<- rep_mas[[j]]$zval
    rep_q[j]<- rep_mas[[j]]$QE
    rep_i2[j]<- rep_mas[[j]]$I2
    
    
    
    
  }
  
  else if (df_mas[j,]$Model=="random-effects" & df_mas[j,]$weight_factor=="sample_size"
           & df_mas[j,]$tau2_estimator=="DL" & df_mas[j,]$ci_method==2 & df_mas[j,]$script == 2){
    
    h<-h+1
    rep_mas[[j]]<- rma(yi, sei=sei, method = "DL", weights = ni+nc, data = mas_df[[h]])
    
    rep_k[j]<- rep_mas[[j]]$k
    rep_g[j]<- rep_mas[[j]]$b[1]
    rep_ll_g[j]<- rep_mas[[j]]$ci.lb
    rep_ul_g[j]<- rep_mas[[j]]$ci.ub
    rep_ac_g[j]<- rep_mas[[j]]$ci.ub-rep_mas[[j]]$ci.lb
    rep_z[j]<- rep_mas[[j]]$zval
    rep_q[j]<- rep_mas[[j]]$QE
    rep_i2[j]<- rep_mas[[j]]$I2
    
    
    
    
  }
  
  else if (df_mas[j,]$script == 1) {
    
    t<- t+1 
    rep_k[j]<- computational_rep_mas[[t]]$k
    rep_g[j]<- computational_rep_mas[[t]]$b[1]
    rep_ll_g[j]<- computational_rep_mas[[t]]$ci.lb
    rep_ul_g[j]<- computational_rep_mas[[t]]$ci.ub
    rep_ac_g[j]<- computational_rep_mas[[t]]$ci.ub-computational_rep_mas[[t]]$ci.lb
    rep_z[j]<- computational_rep_mas[[t]]$zval
    rep_q[j]<- computational_rep_mas[[t]]$QE
    rep_i2[j]<- computational_rep_mas[[t]]$I2
    
    
    
  }
  
}


#Merge the original results and reproduced results

df_mas<- df_mas %>% 
  cbind(rep_k, rep_g,
        rep_ll_g, rep_ul_g, rep_ac_g,
        rep_z, rep_q, rep_i2) %>% 
  mutate_if(is.numeric, round, 3) %>%  #Round the rep values? 
  mutate(diff_k=k-rep_k) %>% 
  mutate(diff_g=((summary_effect-rep_g)/summary_effect)*100) %>% #Percentage error
  mutate(diff_ac=((AC-rep_ac_g)/AC)*100) %>% 
  mutate(diffi2=I2-rep_i2) %>% 
  mutate(original_ci_test=ifelse((LL_CI*UL_CI)>0, "sig", "no_sig")) %>% #Check original and reproduced CI test
  mutate(rep_ci_test=ifelse((rep_ll_g * rep_ul_g)>0, "sig", "no_sig")) %>% 
  mutate(numerical_error=ifelse(abs(diff_g)>=5 | abs(diff_ac)>=5, "error", "minor")) %>% 
  mutate(numerical_error=ifelse(diff_g==0 & diff_ac == 0, "no_error", numerical_error)) %>%
  mutate(numerical_error = factor(numerical_error)) %>% 
  mutate(decision_error = ifelse(original_ci_test==rep_ci_test, "no_error", "error")) %>% 
  mutate(decision_error = factor(decision_error))

####Selecting meta-analysis to be revised due to issues in reproductions####

df_to_rev<- df_mas %>% 
  filter(numerical_error == "error" | decision_error =="error") 


#Selecting number of papers to be revised

#df_to_split<- df_to_rev %>% 
#  group_by(order, Coder) %>% 
#  tally() %>% 
#  ungroup()

#Splitting the papers to be revised between the coders 

#n_each<-nrow(df_to_split)/4

#df_coder2<-df_to_split %>% filter(Coder!=2) %>% sample_n(n_each)

#df_coder3<-df_to_split %>% filter(Coder!=3 & !order %in% c(df_coder2$order)) %>% sample_n(n_each)

#df_coder4<- df_to_split %>% filter(Coder !=4 & !order %in% c(df_coder2$order, df_coder3$order)) %>% sample_n(n_each)

#df_coder5<- df_to_split %>% filter(!order %in% c(df_coder2$order, df_coder3$order, df_coder4$order))


#df_coder2<- df_to_rev %>% filter(order %in% c(df_coder2$order))

#write.xlsx(df_coder2, here("results", "first_stage", "df_to_rev_coder2.xlsx"))

#df_coder3<- df_to_rev %>% filter(order %in% c(df_coder3$order))

#write.xlsx(df_coder3, here("results", "first_stage", "df_to_rev_coder3.xlsx"))

#df_coder4<- df_to_rev %>% filter(order %in% c(df_coder4$order))

#write.xlsx(df_coder4, here("results", "first_stage", "df_to_rev_coder4.xlsx"))

#df_coder5<- df_to_rev %>% filter(order %in% c(df_coder5$order))

#write.xlsx(df_coder5, here("results", "first_stage", "df_to_rev_coder5.xlsx"))
