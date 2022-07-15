rm(list = ls())

library(here)
library(readxl)
library(dplyr)
library(xlsx)
library(skimr)
library(stringr)
library(glue)
library(tidyr)
library(esquisse)
library(ggplot2)
library(ggtext)
library(scales)
library(ggchicklet)

## mapping data
sea_df <- read_xlsx(here("clean_data","Maritime_State Company for Maritime Transport.xlsx"),
                      sheet = "River Taxi")

colnames(sea_df)
sea_df$Year

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
  plot.title = element_markdown(size = 16,hjust =0.5,vjust = 0.5,color = ka_col$title["1"],
                                margin=margin(0,0,-30,0)),
  plot.subtitle = element_blank(),
  
  plot.caption = element_markdown(color = ka_col$title["1"],face = "italic",
                                  size = 12,hjust = 0),
  
  #####################                     AXIS                                  ################
  #axis.title.x = element_text(color = ka_col$title["1"],size = 12),
  axis.title.y =element_text(size=13,colour = ka_col$title),
  axis.title.x =element_text(size=13,colour = ka_col$title,vjust = 8),
  
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
  legend.position = "right",
  aspect.ratio = 1/4
  
) -> riyadh_theme

# Plot Parameters ---------------------------------------------------------

subtitle <- ""
glue("Number of Passengers of the River Taxi in Najaf & Basra") -> title

caption <- glue("<span style = 'color: #194354'>Source:</span>
Central Organization of Statistics & Information Technology (COSIT), Iraqi Ministry of Planning.")

# caption <-glue("<br></br><span style = 'color: {ka_col$source['1']}'>Source:</span>
#                  {unique(data$platform)} Job Matching Platform
#                  <br></br><span style = 'color: {ka_col$source['1']}'>Note:</span>
#                  Data collection is from the 1st of January 2021 until the 1st of August")

y_title <- glue("Passengers Count")
x_title <- ""

########### data preprocessing 



library(ggplot2)
library(forcats)
sea_df |> 
  mutate(Year = factor(Year),
         City = factor(City),
         City = fct_relevel(City, 
                            c("Basra","Najaf","Unknown"))) -> sea_df

sea_df$Year
sea_df$City
sea_df |> 
  ggplot(aes(x = Year, y = Passenger_Count))+
  geom_chicklet(width = 0.4,aes(fill = City),position = "dodge2")+
  scale_fill_manual(values = c(ka_col$purple[[10]],
                               ka_col$purple[[4]],
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
  geom_text(aes(x=Year,
                y= Passenger_Count + 250,
                label = glue("{comma(Passenger_Count)}"),
                group = City),
            size = 4,
            position = position_dodge2(width = 0.4),
            color= ka_col$title[[1]],
            fontface = "bold")




height <- 3
width <- 13
dpi <- 300

#OUTPUTTING THE FILE
ggsave(filename = "sea_river_taxi.png",
       path = here("results"),
       width=width, height=height, units="in", dpi=dpi)


