#####Loading data####

pacman::p_load(openxlsx, grid)
source(here::here("analysis", "00_functions.R"))

df_mas_revised2<-read_xlsx(here::here("data", "second_stage", "df_to_rev_coder2.xlsx"),
                           col_names = TRUE)

df_mas_revised3<-read_xlsx(here::here("data", "second_stage", "df_to_rev_coder3.xlsx"),
                           col_names = TRUE)

df_mas_revised4<-read_xlsx(here::here("data", "second_stage", "df_to_rev_coder4.xlsx"),
                           col_names = TRUE)

df_mas_revised5<-read_xlsx(here::here("data", "second_stage", "df_to_rev_coder5.xlsx"),
                           col_names = TRUE)

df_mas_revised<- rbind(df_mas_revised2, df_mas_revised3, df_mas_revised4, df_mas_revised5) %>% 
  arrange(order)

df_npapers<- df_mas_revised %>% 
  select(ID, order) %>% 
  group_by(ID, order) %>% 
  tally()


df_to_reanalyse<- df_mas_revised %>% filter(qualitative_check=="re-analyse")

#Number of meta-analyses to be reproduced in each paper. 

df_nmas<- df_to_reanalyse %>% 
  select(ID, order, type_of_data) %>% 
  group_by(order, ID, type_of_data) %>%
  tally()


####Importing primary data####


#Importing the coded primary data

dat_to_rev<- list()

t<-0

for (j in c(df_nmas$order)) {
  
  t<- t+1  
  
  dat_to_rev[[t]]<-read_xlsx(here::here("data", "second_stage", paste0("BBDD_0", j, ".xlsx")), 
                             col_names = TRUE)
  
  
}

#Tidying the primary data 

mas_dat_to_rev<- list()


for (l in 1:length(dat_to_rev)) {
  
  
  mas_dat_to_rev[[l]]<- ind_ma(dat= dat_to_rev[[l]], type = paste0(df_nmas$type_of_data[l]), n_mas = df_nmas$n[l])
  
}



#Removing the hierarchy of the list 

mas_df_to_rev<- flatten(mas_dat_to_rev)

#Checking the different combinations to be programmed. 

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
  
  if(df_to_reanalyse[j,]$Model=="fixed-effect" & df_to_reanalyse[j,]$weight_factor=="inverse_variance" & df_to_reanalyse[j,]$script == 2) {
    
    rep_mas_rev[[j]]<- rma(yi, sei=sei, method = "FE", data = mas_df_to_rev[[j]])
    
    rep_k[j]<- rep_mas_rev[[j]]$k
    rep_g[j]<- rep_mas_rev[[j]]$b[1]
    rep_ll_g[j]<- rep_mas_rev[[j]]$ci.lb
    rep_ul_g[j]<- rep_mas_rev[[j]]$ci.ub
    rep_ac_g[j]<- rep_mas_rev[[j]]$ci.ub-rep_mas_rev[[j]]$ci.lb
    rep_z[j]<- rep_mas_rev[[j]]$zval
    rep_q[j]<- rep_mas_rev[[j]]$QE
    rep_i2[j]<- rep_mas_rev[[j]]$I2
    
  }
  
  
  else if (df_to_reanalyse[j,]$Model=="random-effects" & df_to_reanalyse[j,]$weight_factor=="inverse_variance"
           & df_to_reanalyse[j,]$tau2_estimator=="DL" & df_to_reanalyse[j,]$ci_method==2 & df_to_reanalyse[j,]$script == 2){
    
    rep_mas_rev[[j]]<- rma(yi, sei=sei, method = "DL", data = mas_df_to_rev[[j]])
    
    rep_k[j]<- rep_mas_rev[[j]]$k
    rep_g[j]<- rep_mas_rev[[j]]$b[1]
    rep_ll_g[j]<- rep_mas_rev[[j]]$ci.lb
    rep_ul_g[j]<- rep_mas_rev[[j]]$ci.ub
    rep_ac_g[j]<- rep_mas_rev[[j]]$ci.ub-rep_mas_rev[[j]]$ci.lb
    rep_z[j]<- rep_mas_rev[[j]]$zval
    rep_q[j]<- rep_mas_rev[[j]]$QE
    rep_i2[j]<- rep_mas_rev[[j]]$I2
    
    
    
    
  }
  
  else if (df_to_reanalyse[j,]$Model=="random-effects" & df_to_reanalyse[j,]$weight_factor=="inverse_variance"
           & df_to_reanalyse[j,]$tau2_estimator=="REML" & df_to_reanalyse[j,]$ci_method==2 & df_to_reanalyse[j,]$script == 2){
    
    rep_mas_rev[[j]]<- rma(yi, sei=sei, method = "REML", data = mas_df_to_rev[[j]])
    
    rep_k[j]<- rep_mas_rev[[j]]$k
    rep_g[j]<- rep_mas_rev[[j]]$b[1]
    rep_ll_g[j]<- rep_mas_rev[[j]]$ci.lb
    rep_ul_g[j]<- rep_mas_rev[[j]]$ci.ub
    rep_ac_g[j]<- rep_mas_rev[[j]]$ci.ub-rep_mas_rev[[j]]$ci.lb
    rep_z[j]<- rep_mas_rev[[j]]$zval
    rep_q[j]<- rep_mas_rev[[j]]$QE
    rep_i2[j]<- rep_mas_rev[[j]]$I2
    
    
    
    
  }
  
  else if (df_to_reanalyse[j,]$Model=="random-effects" & df_to_reanalyse[j,]$weight_factor=="sample_size"
           & df_to_reanalyse[j,]$tau2_estimator=="DL" & df_to_reanalyse[j,]$ci_method==2 & df_to_reanalyse[j,]$script == 2){
    
    rep_mas_rev[[j]]<- rma(yi, sei=sei, method = "DL", weights = ni, data = mas_df_to_rev[[j]])
    
    rep_k[j]<- rep_mas_rev[[j]]$k
    rep_g[j]<- rep_mas_rev[[j]]$b[1]
    rep_ll_g[j]<- rep_mas_rev[[j]]$ci.lb
    rep_ul_g[j]<- rep_mas_rev[[j]]$ci.ub
    rep_ac_g[j]<- rep_mas_rev[[j]]$ci.ub-rep_mas_rev[[j]]$ci.lb
    rep_z[j]<- rep_mas_rev[[j]]$zval
    rep_q[j]<- rep_mas_rev[[j]]$QE
    rep_i2[j]<- rep_mas_rev[[j]]$I2
    
  } 
  
  
}

#Merge the original results and reproduced results

df_to_reanalyse<- df_to_reanalyse %>% 
  select(-c(rep_k, rep_g, rep_ll_g, rep_ul_g, rep_ac_g, rep_z, rep_q, rep_i2,
            diff_k, diff_g, diff_ac, diffi2, rep_ci_test, numerical_error, decision_error)) %>% 
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
  mutate(decision_error = ifelse(original_ci_test==rep_ci_test, "no_error", "error")) %>% 
  relocate(c(issues, qualitative_check), .after = last_col())


re_analyzed_w_error<- df_to_reanalyse %>% 
  filter(numerical_error == "error" | decision_error =="error") 

df_to_rev2<- df_mas_revised %>% 
  filter(qualitative_check != "re-analyse" & qualitative_check != "rep") %>% 
  rbind(re_analyzed_w_error) %>% 
  arrange(order)

write.xlsx(df_to_rev2, here::here("results", "second_stage", "df_to_rev2.xlsx"))

df_to_scatter<- df_mas_revised %>% 
  filter(qualitative_check != "re-analyse") %>% 
  rbind(df_to_reanalyse) %>% 
  mutate(diff_g = ifelse(qualitative_check == "re-analyse", diff_g, 
                         ((summary_effect-rep_g)/summary_effect)*100)) %>% 
  mutate(diff_ac = ifelse(qualitative_check == "re-analyse", diff_ac,
                          ((AC-rep_ac_g)/AC)*100)) %>%
  mutate(original_ci_test = ifelse(qualitative_check == "re-analyse", original_ci_test,
                                   ifelse((LL_CI*UL_CI)>0, "sig", "no_sig"))) %>% 
  mutate(rep_ci_test = ifelse(qualitative_check == "re-analyse", rep_ci_test,
                              ifelse((rep_ll_g * rep_ul_g)>0, "sig", "no_sig"))) %>% 
  mutate(decision_error = ifelse(qualitative_check == "re-analyse", decision_error,
                                 ifelse(original_ci_test==rep_ci_test, "no_error", "error"))) %>% 
  mutate(diffi2 = ifelse(qualitative_check == "re-analyse", diffi2,
                         I2-rep_i2)) %>% 
  arrange(order)


df_stage2_results<-df_to_scatter
save(df_stage2_results, file = here::here("results", "second_stage", "second_stage_results.Rdata"))


####Figure 4#####

df_to_scatter_original_ci<- df_to_scatter %>% 
  select(LL_CI, UL_CI, decision_error, diff_ac) %>% 
  pivot_longer(cols = c(LL_CI, UL_CI), names_to =  "original_bound") %>% 
  mutate(n_ma = seq(1:104))

df_to_scatter_rep_ci<- df_to_scatter %>% 
  select(rep_ll_g, rep_ul_g, decision_error, diff_ac) %>% 
  pivot_longer(cols = c(rep_ll_g, rep_ul_g), names_to =  "rep_bound")%>% 
  mutate(n_ma = seq(1:104))

df_to_scatter_ci<- inner_join(df_to_scatter_original_ci, df_to_scatter_rep_ci, by = "n_ma")



scatter_g<- scatter_match(df_to_scatter, x=df_to_scatter$summary_effect, y=df_to_scatter$rep_g, 
                          shape = df_to_scatter$decision_error, shape_name = "Decision error:", shape_labels = c("Error", "No error"),
                          size = 15,
                          title = "Summary effect") + 
  theme(plot.margin = unit(c(0, 0, 3, 0), "lines"))




scatter_ci<- scatter_match2(df_to_scatter_ci, x=df_to_scatter_ci$value.x, y=df_to_scatter_ci$value.y, 
                           shape = df_to_scatter_ci$decision_error.x, shape_name = "Decision error:", shape_labels = c("Error", "No error"), 
                           color = df_to_scatter_ci$original_bound, color_name = "Bound", color_labels = c("Lower", "Upper"),
                           size = 15,
                           title = "Confidence Interval") +
  theme(plot.margin = unit(c(0, 0, 0, 0), "lines"))

figure4<- ggarrange(scatter_g,
                    scatter_ci,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2, font.label = list(size = 15),
                    common.legend = FALSE, legend = "bottom")

ggsave(here::here("results","second_stage", 'Figure 4.tiff'), width = 7, height = 14, units = "in", dpi = 600, compression = "lzw+p")


####Secondary analysis, Figure 4####


scatter_i2<-scatter_match(df_to_scatter, x=df_to_scatter$I2, y=df_to_scatter$rep_i2, 
                          shape = df_to_scatter$decision_error, shape_name = "Decision error:", shape_labels = c("Error", "No error"),
                          size = abs(df_to_scatter$diff_g),
                          title = "Heterogeneity statistic (I2)")

figure5<- ggarrange(scatter_i2,
                    legend = "bottom")

ggsave(here::here("results","second_stage", 'Figure 5.tiff'), width = 7, height = 6, units = "in", dpi = 600, compression = "lzw+p")