if(!require(pacman)){
  install.packages("pacman")
}

library(pacman)

pacman::p_load(tidyverse,
               wesanderson,
               ggpubr)


####Importing data####

load(here::here("results", "first_stage", "first_stage_results.Rdata"))
load(here::here("results", "second_stage", "second_stage_results.Rdata"))
load(here::here("results", "third_stage", "third_stage_results.Rdata"))


source(here::here("analysis", "00_functions.R"))


#####Figure S1#####

stage_1<- df_mas %>% 
  mutate(numerical_error = ifelse(numerical_error == "error", "error", "no_error")) %>% 
  group_by(numerical_error, decision_error) %>% 
  tally() %>% 
  mutate(Item = paste0(numerical_error," ", decision_error)) %>% 
  mutate(Item = factor(Item)) %>% 
  mutate(Item = fct_recode(Item, "Numerical and decision error" = "error error", "Numerical error" = "error no_error", "No error" = "no_error no_error")) %>% 
  mutate(perc=n)


stage_2<- df_stage2_results %>% 
  mutate(numerical_error = ifelse(numerical_error == "error", "error", "no_error")) %>% 
  mutate(Item = ifelse(qualitative_check ==  "re-analyse" & numerical_error == "no_error", "Reproduced after fixing a coding error",
                       ifelse(qualitative_check == "rep", "Reproduced after minor adjustment", "Error"))) %>% 
  group_by(Item) %>% 
  tally() %>% 
  mutate(Results = factor(Item)) %>% 
  mutate(perc=n)


stage_3<- df_results_stage_3 %>% 
  mutate(Item = factor(clarification_request_results)) %>% 
  group_by(ID, Item) %>% 
  tally() %>% 
  group_by(Item) %>% 
  tally() %>% 
  mutate(perc=n)

Moonrise3 <-rev(wes_palette("Moonrise3", n=5))

plt_stage1<-customized_barplot(stage_1, pal = Moonrise3[3:5], title = "", legend = "Result", width = .35, rate = FALSE) +
  theme(plot.margin = unit(c(3, 0, 3, 0), "lines"))

plt_stage2<-customized_barplot(stage_2, pal = Moonrise3[3:5], title = "", legend = "Result", width = .35, rate = FALSE) +
  theme(plot.margin = unit(c(3, 0, 3, 0), "lines"))

plt_stage3<-customized_barplot(stage_3, pal = Moonrise3[4:5], title = "", legend = "Result", width = .35, rate = FALSE) +
  theme(plot.margin = unit(c(3, 0, 3, 0), "lines"))



figure5b<- ggarrange(plt_stage1,
                      plt_stage2,
                      plt_stage3,
                      ncol = 1, nrow = 3)


#ggsave(here::here("results", 'Figure 6b.tiff'), width = 8, height = 12, units = "in", dpi = 600, compression = "lzw+p")
