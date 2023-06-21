if(!require(tidyverse)){
  install.packages("pacman")
}
library(pacman)

pacman::p_load(readxl, tidyverse, kableExtra, here, questionr, pscl, irr)


#####Inter-coder agreement#####

#Importing data

path<-here::here("data", "double_coding", "check_primary_data.xlsx") 

dat<- lapply(excel_sheets(path), function(x) read_xlsx(path, sheet = x, col_names = F))

dat_4_irr<- lapply(dat, function(x) data.frame(A = unlist(c(x[, 1:as.integer(ncol(x)/2 - 0.5)]), use.names = F), 
                                               B = unlist(c(x[, as.integer(ncol(x)/2 + 1.5):ncol(x)]), use.names = F)))

datasets_wo_error<- sapply(dat_4_irr, function(x) all(x$A == x$B, na.rm = T)) 

icc_results<- lapply(dat_4_irr, function(x) icc(x, model = "twoway", type = "consistency", unit = "single"))

icc_table<- data.frame(Paper = seq(1:21),
                       ICC = round(unlist(lapply(icc_results, function (x) x$value)), 3),
                       "Full agreement" = datasets_wo_error)


####Sensibility analysis####

#Importing data

load(here::here("results", "first_stage", "first_stage_results.Rdata"))


#Figure S1

df_mas2 <- df_mas2 %>% 
  mutate(diff_g_abs = summary_effect - rep_g) %>% 
  mutate(diff_ac_abs =  AC - rep_ac_g) %>%   
  mutate(numerical_error1 = ifelse(abs(diff_g_abs) >= 0.01 | abs(diff_ac_abs) >= 0.02 , "error", "no_error")) %>% 
  mutate(numerical_error2 = ifelse(abs(diff_g_abs) >= 0.02 | abs(diff_ac_abs) >= 0.04 , "error", "no_error")) %>% 
  mutate(numerical_error3 = ifelse(abs(diff_g_abs) >= 0.03 | abs(diff_ac_abs) >= 0.06 , "error", "no_error")) %>% 
  mutate(numerical_error4 = ifelse(abs(diff_g_abs) >= 0.04 | abs(diff_ac_abs) >= 0.08 , "error", "no_error")) %>% 
  mutate(numerical_error5 = ifelse(abs(diff_g_abs) >= 0.05 | abs(diff_ac_abs) >= 0.10 , "error", "no_error")) %>% 
  mutate(numerical_error =  ifelse(numerical_error == "error", "error", "no_error"))


df_plot<- df_mas2 %>%
  pivot_longer(c(numerical_error, numerical_error1, numerical_error2, numerical_error3, numerical_error4, numerical_error5), 
               names_to = "criteria") %>% 
  select(53, 54) %>% 
  filter(value == "error")



figure_s1<-ggplot(aes(x=criteria), data = df_plot) + 
  geom_bar(colour = "black", fill = "#b00b13", width = .8) +
  scale_y_continuous(name = "Meta-analyses lablled as numerical error", limits = c(0,102), expand=c(0,0),breaks = c(0, 50, 100))+
  scale_x_discrete(name = "Criteria", labels = c("5%", "0.01", "0.02", "0.03", "0.04", "0.05"))+  
  theme_minimal(base_size = 12)+
  theme(panel.grid.major = element_blank(),  
        axis.line = element_line(colour = "black"),
        axis.text = element_text(vjust = 0, size = 12),
        axis.title = element_text(size = 14, face = "bold"))

ggsave(here::here("results", 'Figure S1.tiff'), width = 7, height = 7, bg = "white", units = "in", dpi = 600, compression = "lzw+p")

#Correlation summary effect and heterogeneity statistic error

corr<- cor.test(abs(df_mas2$diff_g_abs), abs(df_mas2$diffi2))

#####Year trend analysis#####

df_mas_pyear1<-read_xlsx(here::here("data", 
                                    "mas_original_results.xlsx"),
                         col_names = TRUE) %>% 
  mutate(publication_year = as.double(str_sub(ID, -4))) %>% 
  mutate(publication_year = ifelse(is.na(publication_year), 2017, publication_year)) %>% 
  mutate(publication_year_centered = scale(publication_year)) %>% 
  mutate(process_rep = ifelse(orign_data!="not_available", 1, 0))

  

df_mas_pyear2<- df_mas_pyear1 %>% 
  group_by(order) %>% 
  mutate(process_rep2 = ifelse(all(orign_data=="not_available"), 0, ifelse(any(orign_data=="not_available"), 2, 1))) %>% 
  slice(1) %>% 
  arrange(order)


log_reg_malevel<- glm(process_rep ~ publication_year, df_mas_pyear1, family = binomial())

log_reg_paperlvl<- glm(process_rep2 ~ publication_year, subset(df_mas_pyear2, process_rep2 != 2), family = binomial())

log_reg_malevel_10<- glm(process_rep ~ publication_year, subset(df_mas_pyear1, publication_year >= 2010), family = binomial())

log_reg_paperlvl_10<- glm(process_rep2 ~ publication_year, subset(df_mas_pyear2, process_rep2 != 2 & publication_year >= 2010), family = binomial())


log_reg_results<- data.frame(Level = c("Meta-analysis", "Paper", "Meta-analysis", "Paper"),
                             Exclusion = c("No", "No", "Yes", "Yes"),
                             Slope = c(coef(summary(log_reg_malevel))[2], coef(summary(log_reg_paperlvl))[2], coef(summary(log_reg_malevel_10))[2], coef(summary(log_reg_paperlvl_10))[2]), 
                             OR = c(odds.ratio(log_reg_malevel)$OR[2], odds.ratio(log_reg_paperlvl)$OR[2], odds.ratio(log_reg_malevel_10)$OR[2], odds.ratio(log_reg_paperlvl_10)$OR[2]),
                             "OR LL" = c(odds.ratio(log_reg_malevel)$`2.5 %`[2], odds.ratio(log_reg_paperlvl)$`2.5 %`[2], odds.ratio(log_reg_malevel_10)$`2.5 %`[2], odds.ratio(log_reg_paperlvl_10)$`2.5 %`[2]),
                             "OR UL" = c(odds.ratio(log_reg_malevel)$`97.5 %`[2], odds.ratio(log_reg_paperlvl)$`97.5 %`[2], odds.ratio(log_reg_malevel_10)$`97.5 %`[2], odds.ratio(log_reg_paperlvl_10)$`97.5 %`[2]),
                             p = c(coef(summary(log_reg_malevel))[8], coef(summary(log_reg_paperlvl))[8], coef(summary(log_reg_malevel_10))[8], coef(summary(log_reg_paperlvl_10))[8])) %>% 
  mutate("Percentage change" = ifelse(OR < 1, (1/OR - 1)*-100, (OR - 1)*100  )) %>% 
  mutate_if(is.numeric, round, 3)
  

#####Qualitative check results####

load(here::here("results", "second_stage", "second_stage_results.Rdata"))

quali_rep<- df_stage2_results %>% 
  filter(qualitative_check=="rep")

table_qual<- data.frame("Meta-analysis" = c(seq(1:15)),
                   Reason = c(quali_rep$reason))


