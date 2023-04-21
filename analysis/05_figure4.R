if(!require(pacman)){
  install.packages("pacman")
}

library(pacman)

if(!require(ggsankey)){
  devtools::install_github("davidsjoberg/ggsankey")
}

library(pacman)


pacman::p_load(tidyverse,
               ggsankey)


####Importing data####

load(here::here("data", "first_stage", "data.Rdata"))
load(here::here("results", "first_stage", "first_stage_results.Rdata"))
load(here::here("results", "second_stage", "second_stage_results.Rdata"))
load(here::here("results", "third_stage", "third_stage_results.Rdata"))

#####Figure 4#####

stage_0 <- df_mas %>% 
  mutate("Process-reproduciblity (Data availability)" = ifelse(orign_data != "not_available", "Yes", "No")) %>% 
  select(order, ID, ma_x, "Process-reproduciblity (Data availability)")


stage_1<- df_mas2 %>% 
  mutate(numerical_error = ifelse(numerical_error == "error", "Numerical error", "No error")) %>% 
  mutate("Numerical and decision errors" = ifelse(decision_error == "error", "Decision error", numerical_error)) %>% 
  select(order, ID, ma_x, "Numerical and decision errors")


stage_2<- df_stage2_results %>% 
  mutate(numerical_error = ifelse(numerical_error == "error", "Numerical error", "No error")) %>% 
  mutate("Coding error and qualitative assessment" = ifelse(qualitative_check ==  "re-analyse" & numerical_error == "No error", "Reproduced after fixing a coding error",
                       ifelse(qualitative_check == "rep", "Minor adjustment or approximately reproduced", 
                              ifelse(decision_error == "error", "Decision error ", "Numerical error ")))) %>% 
  select(order, ID, ma_x, "Coding error and qualitative assessment")


stage_3<- df_stage3_results %>% 
  select(order, ID, ma_x, clarification_request_results) %>% 
  rename("Clarification request" = clarification_request_results)


sankey_df<- left_join(stage_0, stage_1) %>% 
  left_join(stage_2) %>% 
  left_join(stage_3)

# Select stage variables and make it long format
sankey_df <- sankey_df %>%
  select(`Process-reproduciblity (Data availability)`, `Numerical and decision errors`, 
         `Coding error and qualitative assessment`, `Clarification request`) %>%
  mutate(`Coding error and qualitative assessment` = ifelse(`Coding error and qualitative assessment` ==  "Reproduced after fixing a coding error", 
                                                            " Reproduced after\nfixing a coding error", `Coding error and qualitative assessment`)) %>%
  mutate(`Coding error and qualitative assessment` = ifelse(`Coding error and qualitative assessment` ==  "Minor adjustment or approximately reproduced", 
                                                            "Minor adjustment\nor approximately\nreproduced", `Coding error and qualitative assessment`))

sankey_df_long <- sankey_df %>%
  make_long(`Process-reproduciblity (Data availability)`, `Numerical and decision errors`, 
            `Coding error and qualitative assessment`, `Clarification request`)

# Tally it
sankey_df_agg <- sankey_df_long%>%
  dplyr::group_by(node)%>%
  tally()

# Merge long format and tally
sankey_df_merged <- merge(sankey_df_long, sankey_df_agg, by.x = 'node', by.y = 'node', all.x = TRUE)%>%
  filter(!is.na(node))%>%
  mutate(node=fct_relevel(node,"No", "Yes", "Decision error", 
                          "Numerical error", "No error", "Decision error ",
                          "Numerical error ", "Minor adjustment\nor approximately\nreproduced", 
                          " Reproduced after\nfixing a coding error", "No reply", "Reply"))%>%
  mutate(next_node=fct_relevel(next_node,"Decision error", 
                               "Numerical error", "No error", "Decision error ",
                               "Numerical error ", "Minor adjustment\nor approximately\nreproduced", 
                               " Reproduced after\nfixing a coding error", "No reply", "Reply"))

# Figure 4
figure4 <- ggplot(sankey_df_merged, aes(x = x
                                   , next_x = next_x
                                   , node = node
                                   , next_node = next_node
                                   , fill = factor(node)
                                   , label = paste0(node," n = ", n))) +
  geom_sankey(flow.alpha = 0.21, width = 0.05,
                      node.color = "white", show.legend = TRUE, space = NULL) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.title = element_blank()
                  , axis.text.y = element_blank()
                  , axis.ticks = element_blank()  
                  , panel.grid = element_blank()) +
  scale_fill_manual(values = c("sienna2", "lightskyblue3", "lightskyblue3", "sienna2", 
                                        "lightskyblue3", "tomato","lightskyblue3", "lightskyblue3", "lightskyblue3", "sienna2","tomato")) + 
  geom_sankey_label(size = 4, 
                            color = "black", 
                            fill = "white",
                            hjust = -0.1)



#ggsave(here::here("results", 'figure4a.tiff'), width = 12, height = 6, units = "in", dpi = 1200, compression = "lzw+p")

