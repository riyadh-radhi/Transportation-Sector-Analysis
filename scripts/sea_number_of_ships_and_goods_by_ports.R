rm(list = ls())

library(here)
library(readxl)
library(dplyr)
library(xlsx)
library(stringr)
library(glue)
library(tidyr)
library(esquisse)
library(ggplot2)
library(ggtext)
library(scales)
library(ggbump)
library(forcats)
library(cowplot)
library(ggchicklet)
library(patchwork)

options(scipen = 999)
sea_df <- read_xlsx(here("clean_data","ports.xlsx"),
                    sheet = "general_company_of_iraqiports")

colnames(sea_df)



sea_df |> 
  rename("Ships_Um Qasr" = "Umm Qasr_ships",
         "Goods_Um Qasr" = "Umm Qasr_goods1000Ton",
         "Ships_Khour Al Zubeer" = "Khour al- zubeer_ships",
         "Goods_Khour Al Zubeer" = "Khour al- zubeer_goods1000Ton",
         "Ships_Abo Flous" = "Abo-flous_ships",
         "Goods_Abo Flous" = "Abo-flous_goods1000Ton",
         
         "Ships_AL-Maka" = "AL-Maka_ships",
         "Goods_AL-Maka" = "AL-Maka_goods1000Ton") |> 
  pivot_longer(cols = c(contains("Ships"),contains("Goods")),
               names_to = "metric",values_to = "value") -> tidy_df
# KAPITA Colors -----------------------------------------------------------

ka_col <- list(
  green = c("normal"= "#accb46","1" = "#d6e3a3","2" = "#cdde91","3" = "#c4d87f","4" = "#bdd46b",
            "5" = "#b4cf5a","6" = "#9cb73f","7" = "#8ba237","8" = "#788e31","9" = "#687a2a","10" = "#576725"),
  turquaz =c("normal"= "#0098a0","1" = "#81cbcf","2" = "#66c2c6","3" = "#4bb7bc","4" = "#33adb3",
             "5" = "#18a2a9","6" = "#048990","7" = "#007a80","8" = "#006a71","9" = "#005c61","10" = "#004c50"),
  orange = c("normal"= "#f9b036","1" = "#fcd89b","2" = "#fcd086","3" = "#fbc871","4" = "#fac05d",
             "5" = "#fab84b","6" = "#e09e30","7" = "#c78e2a","8" = "#ae7b24","9" = "#956a20","10" = "#7d581b"),
  purple = c("normal"= "#ab3d76","1" = "#d69fbb","2" = "#cd8bad","3" = "#c4779f","4" = "#bd6491",
             "5" = "#b45085","6" = "#9b376a","7" = "#8a315e","8" = "#792b54","9" = "#672547","10" = "#561f3b"),
  title = c("1"= "#3d3d3d"),
  source = c("1" = "#194354")
)

# Plot Theme --------------------------------------------------------------

theme(
  #####################                     TITLES, SUBTITLE, CAPTION               ################
  plot.title = element_markdown(size = 14,hjust =0.5,vjust = 2,color = ka_col$title["1"],
                                margin=margin(0,0,0,0)),
  plot.subtitle = element_blank(),
  
  plot.caption = element_markdown(color = ka_col$title["1"],face = "italic",
                                  size = 10,hjust = 0),
  
  #####################                     AXIS                                  ################
  #axis.title.x = element_text(color = ka_col$title["1"],size = 12),
  axis.text.x = element_blank(),
  axis.text.y=element_text(size=13,hjust = 0.5),
  axis.title.y =element_blank(),
  axis.title.x= element_text(size=13,colour = ka_col$title, vjust = 12),
  axis.ticks= element_blank(),
  #####################                     Panel                                  ################
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border  = element_blank(),
  
  
  #####################                     LEGEND                                  ################
  legend.text=element_text(size=12),
  legend.title = element_blank(),
  legend.direction = "horizontal",
  legend.position = c(0.5, 0.95)
  
  
) -> riyadh_theme

# Plot Parameters ---------------------------------------------------------

subtitle <- ""
glue(" <span style = 'color: {ka_col$purple[['10']]}'>Number of Ships</span>") -> title

caption <- ""
# caption <-glue("<br></br><span style = 'color: {ka_col$source['1']}'>Source:</span>
#                  {unique(data$platform)} Job Matching Platform
#                  <br></br><span style = 'color: {ka_col$source['1']}'>Note:</span>
#                  Data collection is from the 1st of January 2021 until the 1st of August")

y_title <- glue("Count of Ships")
x_title <- ""

#Plotting--------------------------------------------------------------
#plot 1

unique(tidy_df$metric)
colnames(tidy_df)
tidy_df |>
  filter(str_detect(metric,"Ships")) |> 
  mutate(metric = case_when(metric %in% "Ships_Um Qasr" ~ "Umm Qasr",
                            metric %in% "Ships_Khour Al Zubeer" ~ "Khor Al-Zubayr",
                            metric %in% "Ships_Abo Flous" ~ "Abu Flous",
                            metric %in% "Ships_AL-Maka" ~ "Al-Maqil")) |> 
  mutate(metric = factor(metric),
         year = factor(year),
         metric = fct_reorder(metric,
                               value,
                               max)) -> ships_df


# ships_df |> 
#   mutate(y_position = case_when(metric %in% "Erbil International Airport" ~ -1800,
#                                 TRUE ~ 1200),
#          color = case_when(Airport %in% "Erbil International Airport" ~ "white",
#                            TRUE ~ ka_col$title[[1]])) -> df_2019


ships_df |> 
  ggplot(aes(x=metric,y= value))+
  geom_chicklet(width = 0.45,aes(fill = year),position = "dodge2")+
  scale_fill_manual(values = c(ka_col$purple[[3]],
                               ka_col$purple[[7]],
                               ka_col$purple[[10]]))+
  ggtitle(label = title,
          subtitle = subtitle)+
  ylab(y_title)+
  xlab(x_title)+
  labs(caption = caption)+
  scale_y_continuous(breaks = pretty_breaks(8))+
  theme_bw()+
  riyadh_theme+
  coord_flip()+
  geom_text(aes(x=metric,
                y= value + 100,
                label = glue("{comma(value,1)}"),
                group = year),
            size = 4.5,
            position = position_dodge2(width = 0.45),
            color= ka_col$title[[1]],
            fontface = "bold") -> p1

#plot 2

# Plot Parameters ---------------------------------------------------------
subtitle <- ""

title <- 
  glue("<span style = 'color: {ka_col$turquaz['7']}'>Quantity of Transported Goods</span>") 


caption <- ""
# caption <-glue("<br></br><span style = 'color: {ka_col$source['1']}'>Source:</span>
#                  {unique(data$platform)} Job Matching Platform
#                  <br></br><span style = 'color: {ka_col$source['1']}'>Note:</span>
#                  Data collection is from the 1st of January 2021 until the 1st of August")

y_title <- glue("Goods in Thousands Tons")
x_title <- ""

unique(tidy_df$metric)
colnames(tidy_df)
tidy_df |>
  filter(str_detect(metric,"Goods")) |> 
  mutate(metric = case_when(metric %in% "Goods_Um Qasr" ~ "Umm Qasr",
                            metric %in% "Goods_Khour Al Zubeer" ~ "Khor Al-Zubayr",
                            metric %in% "Goods_Abo Flous" ~ "Abu Flous",
                            metric %in% "Goods_AL-Maka" ~ "Al-Maqil")) |> 
  mutate(metric = factor(metric),
         year = factor(year),
         metric = fct_relevel(metric,
                               c("Al-Maqil",
                                 "Abu Flous", 
                                 "Umm Qasr",
                                 "Khor Al-Zubayr"))) -> goods_df


# ships_df |> 
#   mutate(y_position = case_when(metric %in% "Erbil International Airport" ~ -1800,
#                                 TRUE ~ 1200),
#          color = case_when(Airport %in% "Erbil International Airport" ~ "white",
#                            TRUE ~ ka_col$title[[1]])) -> df_2019


goods_df |> 
  ggplot(aes(x=metric,y= -value))+
  geom_chicklet(width = 0.45,aes(fill = year),position = "dodge2")+
  scale_fill_manual(values = c(ka_col$turquaz[[3]],
                               ka_col$turquaz[[7]],
                               ka_col$turquaz[[10]]))+
  ggtitle(label = title,
          subtitle = subtitle)+
  ylab(y_title)+
  xlab(x_title)+
  labs(caption = caption)+
  scale_y_continuous(breaks = pretty_breaks(8))+
  theme_bw()+
  theme(  #####################                     TITLES, SUBTITLE, CAPTION               ################
          plot.title = element_markdown(size = 14,hjust =0.5,vjust = -1,color = ka_col$title["1"],
                                        margin=margin(0,0,0,0)),
          plot.subtitle = element_blank(),
          plot.caption = element_markdown(color = ka_col$title["1"],face = "italic",
                                          size = 10,hjust = 0),
          
          #####################                     AXIS                                  ################
          axis.title.y =element_blank(),
          axis.title.x= element_text(size=13,colour = ka_col$title,vjust = 12),
          axis.text=element_blank(),
          axis.ticks= element_blank(),
          #####################                     Panel                                  ################
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border  = element_blank(),
          
          
          #####################                     LEGEND                                  ################
          legend.text=element_text(size=12),
          legend.title = element_blank(),
          legend.direction = "horizontal",
          legend.position = c(0.5, 0.95)
          )+
  coord_flip()+
  geom_text(aes(x=metric,
                y= -value - 1200,
                label = glue("{comma(round(value,2),1)}"),
                group = year),
            size = 4.5,
            position = position_dodge2(width = 0.45),
            color= ka_col$title[[1]],
            fontface = "bold") -> p2
p2
glue("Number of Ships Used in Carrying Goods & Volume of Imported and Exported Goods") -> title

caption <- "<span style = 'color: #194354'>Source:</span>
                   Central Organization of Statistics & Information Technology (COSIT), Iraqi Ministry of Planning."


p2 + p1 + plot_annotation(title = title,
                          caption = caption,
                          theme = theme(
                            plot.title = element_markdown(size = 17,hjust =0.5,vjust = 0.2,color = ka_col$title["1"]),
                            plot.caption = element_markdown(color = "#3d3d3d",
                                                            face = "italic",
                                                            size = 12,
                                                            hjust = 0))
)

ggsave(filename = glue("sea_number_of_ships_and_goods_by_ports.png"),
       path = here("results"),
       width=13,height= 6,units="in",dpi=300)

