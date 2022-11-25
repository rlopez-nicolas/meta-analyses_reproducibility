####Loading data####

source(here::here("analysis", "00_functions.R"))

df_to_rev2<- read_xlsx(here::here("results", "second_stage", "df_to_rev2.xlsx"))

df_to_rev2<- df_to_rev2 %>% 
  mutate(clarification_request_results = ifelse(ID == "Mitchell2019" | ID == "Braun2013", "Reply", "No reply"))

df_success_request<- df_to_rev2 %>% 
  group_by(ID) %>% 
  filter(row_number()==1) %>% 
  group_by(clarification_request_results) %>% 
  tally() %>%   
  cbind(rbind(BinomCI(.$n[1], .$n[1]+.$n[2], conf.level = 0.95, method = "wilson"),
                       BinomCI(.$n[2], .$n[1]+.$n[2], conf.level = 0.95, method = "wilson")))%>%
  dplyr::rename(perc=est)%>%
  mutate_if(is.numeric, round, 2)


df_to_reanalyse<- df_to_rev2 %>% 
  filter(clarification_request_results == "Reply")


dat_019<- read_csv(here::here("data", "third_stage", "BBDD_019.csv")) %>% 
  escalc(n1i=tN, n2i=cN, m1i=tMean, m2i=cMean, sd1i = tSD, sd2i = cSD, data=., measure = "SMD", append = TRUE) %>% 
  mutate(sei=sqrt(vi)) %>% 
  select(Study, Outcome, Measurement, yi, sei)


dat_061<- read_xlsx(here::here("data", "third_stage", "BBDD_061.xlsx"),
                    col_names = TRUE) %>% 
          ind_ma(., type = "gs_ci", n_mas = 9)

dat_to_rev<- list(filter(dat_019, Outcome == "SOL" & Measurement == "Actigraphy"),
                  filter(dat_019, Outcome == "WASO" & Measurement == "Actigraphy"),
                  filter(dat_019, Outcome == "SOL" & Measurement == "Diary"),
                  filter(dat_019, Outcome == "SE" & Measurement == "Actigraphy"),
                  dat_061[[1]], dat_061[[3]], dat_061[[6]], dat_061[[7]], dat_061[[9]])



unique_methods_combinations_rev<- crossing(nesting(df_to_reanalyse$Model, df_to_reanalyse$weight_factor, df_to_reanalyse$tau2_estimator, df_to_reanalyse$ci_method))

####Analytic reproduction loop####

rep_mas_rev<-vector("list", nrow(df_to_reanalyse)) #Empty list to store reproduced models

rep_k<- rep(0, nrow(df_to_reanalyse))          #Empty vectors to store reproduced statistics
rep_g<-rep(0, nrow(df_to_reanalyse))
rep_ll_g<-rep(0, nrow(df_to_reanalyse))
rep_ul_g<-rep(0, nrow(df_to_reanalyse))
rep_ac_g<-rep(0, nrow(df_to_reanalyse))
rep_z<-rep(0, nrow(df_to_reanalyse))
rep_q<-rep(0, nrow(df_to_reanalyse))
rep_i2<-rep(0, nrow(df_to_reanalyse))


for (j in 1:nrow(df_to_reanalyse)) {
  
    rep_mas_rev[[j]]<- rma(yi, sei=sei, method = "REML", data = dat_to_rev[[j]])
    
    rep_k[j]<- rep_mas_rev[[j]]$k
    rep_g[j]<- rep_mas_rev[[j]]$b[1]
    rep_ll_g[j]<- rep_mas_rev[[j]]$ci.lb
    rep_ul_g[j]<- rep_mas_rev[[j]]$ci.ub
    rep_ac_g[j]<- rep_mas_rev[[j]]$ci.ub-rep_mas_rev[[j]]$ci.lb
    rep_z[j]<- rep_mas_rev[[j]]$zval
    rep_q[j]<- rep_mas_rev[[j]]$QE
    rep_i2[j]<- rep_mas_rev[[j]]$I2
    
  }
  
#Merge the original results and reproduced results

df_to_reanalyse<- df_to_reanalyse %>% 
  select(-c(rep_k, rep_g, rep_ll_g, rep_ul_g, rep_ac_g, rep_z, rep_q, rep_i2,
            diff_k, diff_g, diff_ac, diffi2, rep_ci_test, numerical_error, decision_error)) %>% 
  cbind(rep_k, rep_g,
        rep_ll_g, rep_ul_g, rep_ac_g,
        rep_z, rep_q, rep_i2) %>% 
  mutate_if(is.numeric, round, 2) %>%  #Round the rep values? 
  mutate(diff_k=k-rep_k) %>% 
  mutate(diff_g=((summary_effect-rep_g)/summary_effect)*100) %>% #Percentage error
  mutate(diff_ac=((AC-rep_ac_g)/AC)*100) %>% 
  mutate(diffi2=I2-rep_i2) %>% 
  mutate(original_ci_test=ifelse((LL_CI*UL_CI)>0, "sig", "no_sig")) %>% #Check original and reproduced CI test
  mutate(rep_ci_test=ifelse((rep_ll_g * rep_ul_g)>0, "sig", "no_sig")) %>% 
  mutate(numerical_error=ifelse(abs(diff_g)>=5 | abs(diff_ac)>=5, "error", "minor")) %>% 
  mutate(numerical_error=ifelse(diff_g==0 & diff_ac == 0, "no_error", numerical_error)) %>%
  mutate(decision_error = ifelse(original_ci_test==rep_ci_test, "no_error", "error")) %>% 
  relocate(c(issues, qualitative_check), .after = last_col()) %>% 
  mutate(qualitative_check2= c(rep("rep", 7), "no_rep", "rep"))

df_results_stage_3<- df_to_rev2 %>% 
  filter(clarification_request_results != "Reply") %>% 
  mutate(qualitative_check2 = c(rep(NA, nrow(.)))) %>% 
  rbind(df_to_reanalyse) %>% 
  arrange(order)


#save(df_results_stage_3, file = here::here("results", "third_stage", "third_stage_results.Rdata"))





