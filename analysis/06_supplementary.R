if(!require(tidyverse)){
  install.packages("tidyverse")
}

library(tidyverse)

####Importing data####

load(here::here("results", "first_stage", "first_stage_results.Rdata"))

#####Figure S1#####



df_mas <- df_mas %>% 
  mutate(diff_g_abs = summary_effect - rep_g) %>% 
  mutate(diff_ac_abs =  AC - rep_ac_g) %>%   
  mutate(numerical_error1 = ifelse(abs(diff_g_abs) >= 0.01 | abs(diff_ac_abs) >= 0.02 , "error", "no_error")) %>% 
  mutate(numerical_error2 = ifelse(abs(diff_g_abs) >= 0.02 | abs(diff_ac_abs) >= 0.04 , "error", "no_error")) %>% 
  mutate(numerical_error3 = ifelse(abs(diff_g_abs) >= 0.03 | abs(diff_ac_abs) >= 0.06 , "error", "no_error")) %>% 
  mutate(numerical_error4 = ifelse(abs(diff_g_abs) >= 0.04 | abs(diff_ac_abs) >= 0.08 , "error", "no_error")) %>% 
  mutate(numerical_error5 = ifelse(abs(diff_g_abs) >= 0.05 | abs(diff_ac_abs) >= 0.10 , "error", "no_error")) %>% 
  mutate(numerical_error =  ifelse(numerical_error == "error", "error", "no_error"))


df_plot<- df_mas %>%
  pivot_longer(c(numerical_error, numerical_error1, numerical_error2, numerical_error3, numerical_error4, numerical_error5), 
               names_to = "criteria") %>% 
  select(51, 52) %>% 
  filter(value == "error")



figure_s1<-ggplot(aes(x=criteria), data = df_plot) + 
  geom_bar(colour = "black", fill = "#b00b13", width = .8) +
  scale_y_continuous(name = "Meta-analyses lablled as numerical error", limits = c(0,150), expand=c(0,0),breaks = c(0, 50, 100, 146))+
  scale_x_discrete(name = "Criteria", labels = c("5%", "0.01", "0.02", "0.03", "0.04", "0.05"))+  
  theme_minimal(base_size = 12)+
  theme(panel.grid.major = element_blank(),  
        axis.line = element_line(colour = "black"),
        axis.text = element_text(vjust = 0, size = 12),
        axis.title = element_text(size = 14, face = "bold"))

ggsave(here::here("results", 'Figure S1.tiff'), width = 7, height = 7, units = "in", dpi = 600, compression = "lzw+p")
