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
library(janitor)
library(ggrepel)
options(scipen = 999)
air_df <- read_xlsx(here("clean_data","air_transportation_2020.xlsx"),
                    sheet = "Fleets Data")

air_df <- clean_names(air_df)
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
  plot.title = element_markdown(size = 14,hjust =0.5,vjust = 0.5,color = ka_col$title["1"],
                                margin=margin(0,0,0,0)),
  plot.subtitle = element_blank(),
  
  plot.caption = element_markdown(color = ka_col$title["1"],face = "italic",
                                  size = 12,hjust = 0),
  
  #####################                     AXIS                                  ################
  axis.title.y =element_text(size=12,hjust = 0.5,color = ka_col$title["1"]),
  axis.title.x =element_text(size=12,hjust = 0.5,color = ka_col$title["1"]),
  
  axis.text.y=element_text(size=13,hjust = 0.5),
  axis.text.x=element_text(size=13,hjust = 0.5),
  axis.ticks= element_blank(),
  #####################                     Panel                                  ################
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border  = element_blank(),
  
  
  #####################                     LEGEND                                  ################
  legend.text=element_text(size=12),
  legend.title = element_blank(),
  legend.position = "top"
  
) -> riyadh_theme

#plots details 
height <- 6
width <- 13
dpi <- 300


# Plot Parameters ---------------------------------------------------------


caption <- "<span style = 'color: #194354'>Source:</span> Planespotters"

# caption <-glue("<br></br><span style = 'color: {ka_col$source['1']}'>Source:</span>
#                  {unique(data$platform)} Job Matching Platform
#                  <br></br><span style = 'color: {ka_col$source['1']}'>Note:</span>
#                  Data collection is from the 1st of January 2021 until the 1st of August")

y_title <- "Average Aircraft Age (Years)"
x_title <- "Fleet Size (Total Number of Aircrafts)"


air_df |>
  ggplot(aes(x=number_of_aircrafts,
             y=average_aircraft_age,
             color= airlines))+
  geom_point(size=2.7,
             alpha = 0.55)+
  ggtitle(label = "Fleet Size & Average Age of Aircrafts in Iraqi Airways and Neighboring Countries Major Airlines",
          subtitle = "")+
  ylab(y_title)+
  xlab(x_title)+
  labs(caption = caption)+
  scale_y_continuous(breaks = c(seq(5,13,2)),limits = c(5,13))+
  scale_x_continuous(breaks = c(seq(30,320,30),320),limits = c(30,320))+
  theme_bw()+
  riyadh_theme+
  geom_text_repel(aes(label = paste0("Fleet Size: ",number_of_aircrafts,"\n",
                               "Aircrafts Age: ",average_aircraft_age)),
                  lineheight = 0.7,
                  color = ka_col$title[[1]],
                  size = 3)


#OUTPUTTING THE FILE
ggsave(filename = "air_airlines_fleetsize.png",
       path = here("results","fleet size"),
       width=width, height=height, units="in", dpi=dpi)


ggsave(filename = "air_airlines_fleetsize.pdf",
       path = here("results","fleet size"),
       width=width, height=height, units="in", dpi=dpi)


