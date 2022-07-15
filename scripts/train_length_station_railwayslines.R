########################
# TRUST in the Collegue System
#########################
rm(list = ls())

library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(patchwork)
library(ggtext)
library(ggchicklet)
library(glue)
library(forcats)
library(cowplot)
library(stringr)

train_df <- read_xlsx(here("clean_data","trains.xlsx"),
                      sheet = "railway_lines")


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
  #plot.title = element_markdown(size = 14,hjust =0.5,vjust = -0.7,color = ka_col$title["1"]),
  plot.title = element_markdown(size = 14,hjust =0.5,vjust = 0.2,color = ka_col$title["1"],
                                margin=margin(0,0,-30,0)),
  plot.subtitle = element_blank(),
  
  plot.caption = element_markdown(color = ka_col$title["1"],face = "italic",
                                  size = 10,hjust = 0,padding = unit(c(10, 0, 0, 0), "pt")),
  
  #####################                     AXIS                                  ################
  #axis.title.x = element_text(color = ka_col$title["1"],size = 12),
  axis.title.y =element_blank(),
  axis.title.x =element_blank(),
  
  axis.text.y=element_text(size=13,hjust = 0.5),
  axis.text.x=element_blank(),
  axis.ticks= element_blank(),
  #####################                     Panel                                  ################
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border  = element_blank(),
  
  
  #####################                     LEGEND                                  ################
  legend.text=element_text(size=12),
  legend.title = element_blank(),
  legend.position = "none"
  
) -> riyadh_theme

# Plot Parameters ---------------------------------------------------------

subtitle <- ""
glue(" <span style = 'color: {ka_col$turquaz[['10']]}'>Number of Operating Stations</span>") -> title

caption <- ""
# caption <- "<span style = 'color: #194354'>Source:</span>
#                    Central Organization of Statistics & Information Technology (COSIT),<br/> Iraqi Ministry of Planning."

y_title <- glue("Number of Operating Stations")
x_title <- ""

#Plotting--------------------------------------------------------------
#plot 1
unique(train_df$Railway_line)
colnames(train_df)
train_df |>
  mutate(Railway_line = case_when(Railway_line %in% "Baghdad -Basrah -  Maqal" ~"Baghdad - Basrah - Maqal",
                                       TRUE ~ Railway_line)) |> 
  select(Railway_line,`Number of Operating Stations`) |> 
  mutate(Railway_line = factor(Railway_line),
         Railway_line = fct_reorder(Railway_line,
                                    `Number of Operating Stations`,
                                    max)) -> df_operating_station


df_operating_station |> 
  ggplot(aes(x=Railway_line,y= `Number of Operating Stations`))+
  geom_chicklet(width = 0.45,aes(fill = Railway_line))+
  scale_fill_manual(values = c(ka_col$turquaz[[5]],
                               ka_col$turquaz[[5]],
                               ka_col$turquaz[[1]],
                               ka_col$turquaz[[10]]))+
  ggtitle(label = title,
          subtitle = subtitle)+
  ylab(y_title)+
  xlab(x_title)+
  labs(caption = caption)+
  scale_y_continuous(breaks = pretty_breaks(8))+
  theme_bw()+
  riyadh_theme+
  coord_flip()+
  geom_text(aes(x=Railway_line,
                y= `Number of Operating Stations`-2,
                label = glue("{`Number of Operating Stations`}")),
            nudge_y = c(0,0,0,0),
            size = 4.5,
            color= c(rep("#ffffff",4)),
            fontface = "bold") -> p1

#plot 2

# Plot Parameters ---------------------------------------------------------
subtitle <- ""

title <- 
  glue("<span style = 'color: {ka_col$purple['7']}'>Railways Lines Lengths in km</span>") 


caption <- "<span style = 'color: #194354'>Source:</span>
                   Central Organization of Statistics & Information Technology (COSIT),<br/> Iraqi Ministry of Planning."

# caption <-glue("<br></br><span style = 'color: {ka_col$source['1']}'>Source:</span>
#                  {unique(data$platform)} Job Matching Platform
#                  <br></br><span style = 'color: {ka_col$source['1']}'>Note:</span>
#                  Data collection is from the 1st of January 2021 until the 1st of August")

y_title <- glue("")
x_title <- ""

colnames(train_df)
train_df |>
  mutate(Railway_line = case_when(Railway_line %in% "Baghdad -Basrah -  Maqal" ~"Baghdad - Basrah - Maqal",
                                  TRUE ~ Railway_line)) |> 
  select(Railway_line,`Lengths of Railways`) |> 
  mutate(Railway_line = factor(Railway_line),
         Railway_line = fct_relevel(Railway_line,
                               c("Kirkuk - Beygee - Haditha", 
                                 "Baghdad - AL-Kaim - Husaiba",
                                 "Baghdad - Mosul - Rabiah",
                                 "Baghdad - Basrah - Maqal"))) -> df_length_of_railways

df_length_of_railways |> 
  ggplot(aes(x=Railway_line,y= -`Lengths of Railways`))+
  geom_chicklet(width = 0.45,aes(fill = Railway_line))+
  scale_fill_manual(values = c(ka_col$purple[[5]],
                               ka_col$purple[[5]],
                               ka_col$purple[[1]],
                               ka_col$purple[[10]]))+
  ggtitle(label = title,
          subtitle = subtitle)+
  ylab(y_title)+
  xlab(x_title)+
  labs(caption = caption)+
  scale_y_continuous(breaks = pretty_breaks(8))+
  theme_bw()+
  theme(  #####################                     TITLES, SUBTITLE, CAPTION               ################
          plot.title = element_markdown(size = 14,hjust =0.5,vjust = 0.2,color = ka_col$title["1"],
                                        margin=margin(0,0,-30,0)),
          plot.subtitle = element_blank(),
          plot.caption = element_markdown(color = ka_col$title["1"],face = "italic",
                                          size = 10,hjust = 0,padding = unit(c(10, 0, 0, 0), "pt")),
          
          #####################                     AXIS                                  ################
          axis.title =element_blank(),
          
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
          legend.position = "none")+
  coord_flip()+
  geom_text(aes(x=Railway_line,
                y= -`Lengths of Railways`+70,
                label = glue("{comma(`Lengths of Railways`)}")),
            nudge_y = 0,
            size = 4.5,
            color= c(rep("#ffffff",4)),
            fontface = "bold") -> p2

p2
glue("Railway Lines in 2020")  -> title

p2 + p1 + plot_annotation(title = title,
                          caption = y_title,
                          theme = theme(
                            plot.title = element_markdown(size = 17,hjust =0.5,vjust = 0.2,color = ka_col$title["1"],
                                                          margin=margin(0,0,-20,0)),
                            plot.caption = element_markdown(color = ka_col$title["1"],size = 15,
                                                            hjust = 0.5,margin=margin(0,0,30,0)))
)

ggsave(filename = glue("Train Stations Railway Lines in 2020.png"),
       path = here("results"),
       width=13,height= 6,units="in",dpi=300)


