if(!require(pacman)){
  install.packages("pacman")
}
library(pacman)

pacman::p_load(readxl, tidyverse, papaja, kableExtra, english, knitr, here, DescTools, wesanderson, ggpubr)

source(here::here("analysis", "00_functions.R"))

####Importing original results and methods####

#This dataset contains the summary results, characteristics,  and original reported methodology of each included meta-analysis

df_mas<- read_xlsx(here::here("data", 
                              "mas_original_results.xlsx"),
                   col_names = TRUE)


####Descriptives####

n_mas_total<- nrow(df_mas)
k_mean<- round(mean(df_mas$k),3)
k_median<- median(df_mas$k)
k_sd<- round(sd(df_mas$k), 3)
range<- c(min(df_mas$k), max(df_mas$k))
k_iqr<- IQR(df_mas$k)
k_q1<-quantile(df_mas$k)[2]
k_q3<-quantile(df_mas$k)[4]
n_mas_less_10<- df_mas %>% filter(k < 10) %>% 
  nrow(.)


#Process reproducibility

df_process_rep<- df_mas %>% 
  select(order, ID, orign_data) %>% 
  mutate(Item = ifelse(orign_data!="not_available", "Yes", "No")) %>% 
  group_by(Item) %>% 
  tally() %>% 
  cbind(rbind(BinomCI(.$n[1], .$n[1]+.$n[2], conf.level = 0.95, method = "wilson"),
              BinomCI(.$n[2], .$n[1]+.$n[2], conf.level = 0.95, method = "wilson")))%>%
  rename(perc=est)%>%
  mutate_if(is.numeric, round, 2)


#Source of primary data

df_source_primary_data<- df_mas %>% 
  select(order, ID, Item=orign_data) %>% 
  filter(Item!="not_available") %>% 
  mutate(Item=ifelse(str_detect(Item, "xls"), "Machine-readable data file", Item)) %>% 
  group_by(Item) %>% 
  tally() %>%   
  cbind(MultinomCI(.$n, conf.level = 0.95, method = "sisonglaz"))%>%
  rename(perc=est)%>%
  mutate_if(is.numeric, round, 2) %>% 
  mutate(Item=fct_recode(Item, "Upon request"="from_request", "Forest plot from paper itself"="paper_forest", 
                         "Table from paper itself"="paper_table", "Personal webpage"="personal_webpage", 
                         "Supplementary file"="supp_file")) %>% 
  mutate(Item=fct_relevel(Item, "Personal webpage", "Upon request", "Machine-readable data file", "Table from paper itself", "Supplementary file", "Forest plot from paper itself"))

#Results of data request

df_request<- df_mas %>% 
  select(order, ID, orign_data, results, reminder_results) %>% 
  filter(orign_data=="not_available" | orign_data == "from_request") %>% 
  group_by(ID) %>% 
  filter(row_number()==1) %>% 
  mutate(final_results = ifelse(is.na(reminder_results), results, reminder_results)) %>% 
  mutate(Item = ifelse(orign_data == "from_request", "Reply with data", 
                       ifelse(final_results == "no reply", "No reply", "Reply with reason"))) %>% 
  group_by(Item) %>% 
  tally() %>% 
  cbind(MultinomCI(.$n, conf.level = 0.95, method = "sisonglaz"))%>%
  rename(perc=est)%>%
  mutate_if(is.numeric, round, 2) %>% 
  mutate(Item =  fct_relevel(Item, "Reply with reason", "No reply", "Reply with data"))

#Reasons for not sharing data

df_reasons<- df_mas %>% 
  select(ID, orign_data, results, reminder_results) %>% 
  filter(orign_data=="not_available") %>% 
  group_by(ID) %>% 
  filter(row_number()==1) %>%
  mutate(final_results = ifelse(is.na(reminder_results), results, reminder_results)) %>% 
  filter(final_results!="no reply") %>% 
  mutate(final_results = ifelse(final_results == "data discarded" | final_results == "not be able to track down the data" 
                                | final_results == "author no longer has the data", "The author no longer has the data.",
                                ifelse(final_results == "the co-author's email no longer exists", "Data held by a co-author, and do not have his contact details", 
                                       ifelse(final_results == "propietary_data", "Propietary dataset", 
                                              ifelse(final_results == "again asking to supervisor and no reply", 
                                                     "The author requested more information and a written agreement including possible authorship. 
                                                      Additional details were sent and after some email exchanges there was no further response."))))) %>% 
  group_by(final_results) %>% 
  tally() %>% 
  cbind(MultinomCI(.$n, conf.level = 0.95, method = "sisonglaz"))%>%
  mutate(porc = sprintf("%0.0f%%", est * 100)) %>% 
  rename("Reason" = final_results, "N" = n, "%" = porc) %>% 
  select(Reason, N, "%")
  



#Interoperable data

df_data_downloaded<- df_mas %>% 
  select(order, ID, Item=orign_data) %>% 
  filter(Item!="not_available") %>% 
  mutate(Item=str_detect(Item, "xls")) %>% 
  group_by(Item, ID) %>% 
  tally() 

#Available data types

df_type_of_data<-df_mas %>% 
  filter(orign_data!="not_available") %>% 
  select(ID, order, type_of_data) %>% 
  mutate(type_of_data = ifelse(type_of_data == "raw_means", "means_smd", type_of_data)) %>% 
  group_by(type_of_data) %>% 
  tally()  %>% 
  cbind(MultinomCI(.$n, conf.level = 0.95, method = "sisonglaz"))%>%
  rename(perc=est)%>%
  mutate_if(is.numeric, round, 2)

#Number of meta-analyses to be reproduced in each paper. 

df_nmas_wdata<- df_mas %>% 
  filter(orign_data!="not_available") %>% 
  select(ID, order, type_of_data) %>% 
  group_by(order, ID, type_of_data) %>%
  tally()

#Number of meta-anlyses to be analytical reproduced because the lack of script analysis

df_nmas_analytc_data<- df_mas %>% 
  filter(orign_data!="not_available" & script == 2) %>% 
  select(ID, order, type_of_data) %>% 
  group_by(order, ID, type_of_data) %>%
  tally()

#Meta-analysis withouth primary data available

df_nas_data<- df_mas %>% 
  select(ID, order, orign_data, results, reminder_results) %>% 
  group_by(ID, orign_data, results, reminder_results) %>% 
  tally() %>% 
  filter(orign_data=="not_available" | orign_data == "from_request")


n_papers_w_some_ma_wo_data<- nrow(df_nas_data)  #Number of papers with some MA without primary data available

n_mas_wo_data<- sum(df_nas_data$n)  #Number of meta-analyses withouth primary data available

#Script availability

df_script<- df_mas %>% 
  filter(orign_data!="not_available") %>% 
  select(ID, order, script) %>% 
  group_by(ID, order, script) %>% 
  tally()

n_mas_w_script<- sum(subset(df_script, script==1)$n)

n_papers_w_script<- nrow(subset(df_script, script==1))

#Use frequency of each software

df_software<- df_mas %>% 
  select(ID, software) %>% 
  group_by(software) %>% 
  tally()

####Figures####

#Figure 2

hist_primary_studies<- ggplot(df_mas, aes(x = k)) + 
  geom_histogram(alpha = 0.75, aes(y=..count..), position = "identity",
                 bins = 50, colour = "#b00b13", fill = "lavenderblush") +
  labs(x="Number of primary studies included",
       y= "Count")+
  geom_vline(aes(xintercept=k_median), colour= "#3444d9",
             linetype="dashed", size=1)+
  geom_vline(xintercept = c(k_q1, k_q3), colour = "#3444d9",
             linetype="dashed", size=0.75, alpha = 0.5)+
  theme_minimal(base_size = 10)+
  theme_classic()+
  theme(
    axis.title.y = element_text(colour = "black", size = 13, hjust = 0.5, margin = margin(t=5, b=10)),
    axis.title.x = element_text(colour = "black", size = 13, hjust = 0.5, margin = margin(t=5, b=10)),
    axis.text = element_text(colour = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank())

#ggsave(here::here("results", 'Figure 2.tiff'), width = 6 , height = 6 , units = "in", dpi = 600, compression = "lzw+p")


#Figure 3

#Definition of palette values:

Moonrise3 <-rev(wes_palette("Moonrise3", n=5))

plt_process_rep<- customized_barplot(df_process_rep, pal = Moonrise3[4:5], title = "Process reproducibility", legend = "Successful:", width = .35)

plt_source_primary_data<- customized_barplot(df_source_primary_data, pal = c("#E1EFC4", Moonrise3), title = "Primary data source", legend = "Source:", width = .35)

plt_results_request<- customized_barplot(df_request, pal = Moonrise3[3:5], title = "Results of data requests", legend = "Result:", width = .35)


figure3<- ggarrange(plt_process_rep,
                    plt_source_primary_data,
                    plt_results_request,
                    labels = c("A", "B", "C"),
                    ncol = 1, nrow = 3, font.label = list(size = 14))

#ggsave(here::here("results", 'Figure 3.tiff'), width = 10, height = 11, units = "in", dpi = 600, compression = "lzw+p")


####Secondary analysis####

#Differences in number of primary studies between MAs with available data and MAs without it

#df_differences<- df_mas %>% 
#  select(order, ID, k, orign_data) %>% 
#  mutate(orign_data=ifelse(orign_data!="not_available", "available", orign_data)) %>% 
#  mutate(orign_data=factor(orign_data))

#T.test

#ttest_res<-t.test(k ~ orign_data, data = df_differences)

#Summary

#summary<- df_differences %>% 
#  group_by(orign_data) %>% 
#  summarise(mean_primary_studies=mean(k), sd_primary_studies=sd(k), n=n())

#Performance of Kolmogorov-Smirnov test

#ks_res<-ks.test(subset(df_differences, orign_data=="available")$k, subset(df_differences, orign_data=="not_available")$k)

#Histogram plotting:

#hist_primary_studies<- ggplot(df_differences, aes(k, fill = orign_data)) + 
#  geom_histogram(alpha = 0.5, aes(y=..density..), position = "identity",
#                 bins = 20) +
#  labs(title = "Number of primary studies",
#       x="k")+
#  scale_fill_manual(values = c("#E05656", "#5C56E0"), labels = c("Available", "Not available"))+
#  geom_density(alpha = 0.4)+
#  geom_vline(data=summary, aes(xintercept=mean_primary_studies), colour= c("#E05656", "#5C56E0"),
#             linetype="dashed", size=1)+
#  geom_text(x=100,
#            y=.04,
#            label=sprintf("Kolmogorov-Smirnov test 
#                      D= %.3f; p= %.3f", 
#                          ks_res$statistic, ks_res$p.value),
#            check_overlap = TRUE,
#            fontface = "bold")+
#  theme_minimal(base_size = 10)+
#  theme(
#    plot.title = element_text(face = "bold", size = 13, hjust = 0.5, colour = "black", margin = margin(t = 5, b = 10)),
#    axis.title.y = element_blank(),
#    axis.title.x = element_text(colour = "black", size = 12, hjust = 0.5, margin = margin(t=5, b=10)),
#    axis.text = element_text(colour = "black"),
#    panel.grid.major.y = element_blank(),
#    panel.grid.major.x = element_blank(),
#    legend.position="bottom",
#    legend.title = element_blank(),
#    legend.text = element_text(size = 8, face = "bold", colour = "black"),
#    legend.key.size = unit(.2, "cm"))


####Importing primary data####


#Importing the coded primary data

dat<- list()

t<- 0


for (j in c(df_nmas_analytc_data$order)) {
  
  t<-t+1
  
  dat[[t]]<-read_xlsx(here::here("data", "primary_data", 
                                 paste0("BBDD_0", j, ".xlsx")), 
                      col_names = TRUE)
  
  
}

#Tidying the primary data with the function available in "tidy_data_function.R"

mas_dat<- list()


for (l in 1:length(dat)) {
  
  
  mas_dat[[l]]<- ind_ma(dat = dat[[l]], 
                        type = paste0(df_nmas_analytc_data$type_of_data[l]), 
                        n_mas = df_nmas_analytc_data$n[l])
  
}



#Removing the hierarchy of the list 

mas_df<- flatten(mas_dat)

#Saving primary data

#save(df_mas, mas_df,
#     file = here::here("data", "first_stage", "data.Rdata"))
