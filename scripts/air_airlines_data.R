
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

baghdad_df <- read_xlsx(here("clean_data","air_transportation_2020.xlsx"),
                    sheet = "Baghdad Airlines")

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
                                  size = 10,hjust = 0),
  
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
glue(" <span style = 'color: {ka_col$turquaz[['10']]}'>Flights According to Airlines in 2020</span>") -> title

caption <- ""
# caption <- "<span style = 'color: #194354'>Source:</span>
#                    Central Organization of Statistics & Information Technology (COSIT),<br/> Iraqi Ministry of Planning."


y_title <- glue("Number of Flights")
x_title <- ""

#Plotting--------------------------------------------------------------
#plot 1


baghdad_df |> 
  slice_max(order_by = Flights_Number,n = 10) -> baghdad_df

baghdad_df |> 
  ggplot(aes(x=reorder(Airlines,Flights_Number),y=Flights_Number))+
  geom_chicklet(width = 0.45,aes(fill = Flights_Number))+
  scale_fill_gradient(low = ka_col$purple[[4]],
                      high = ka_col$purple[[10]])+
  ggtitle(label = "Baghdad",
          subtitle = subtitle)+
  ylab(y_title)+
  xlab(x_title)+
  labs(caption = caption)+
  scale_y_continuous(breaks = pretty_breaks(8),
                     expand = expansion(mult = c(0, .1)))+
  theme_bw()+
  riyadh_theme+
  coord_flip()+
  geom_text(aes(x=Airlines,
                y= Flights_Number + 380,
                label = glue("{comma(Flights_Number,1)}")),
            nudge_y = c(0,0,0,0,0,0),
            size = 3,
            color=ka_col$title[1],
            fontface = "bold") -> bgw_p

####### Plot 2 #######

erbil_df <- read_xlsx(here("clean_data","air_transportation_2020.xlsx"),
                    sheet = "Erbil Airlines")


erbil_df |> 
  slice_max(order_by = Flights_Number,n = 10) -> erbil_df

erbil_df |> 
  ggplot(aes(x=reorder(Airlines,Flights_Number),y=Flights_Number))+
  geom_chicklet(width = 0.45,aes(fill = Flights_Number))+
  scale_fill_gradient(low = ka_col$purple[[4]],
                      high = ka_col$purple[[10]])+
  ggtitle(label = "Erbil",
          subtitle = subtitle)+
  ylab(y_title)+
  xlab(x_title)+
  labs(caption = caption)+
  scale_y_continuous(breaks = pretty_breaks(8),
                     expand = expansion(mult = c(0, .1)))+
  theme_bw()+
  riyadh_theme+
  coord_flip()+
  geom_text(aes(x=Airlines,
                y= Flights_Number + 150,
                label = glue("{comma(Flights_Number,1)}")),
            nudge_y = c(0,0,0,0,0,0),
            size = 3,
            color=ka_col$title[1],
            fontface = "bold") -> erbil_p

####### Plot 3 #######

suly_df <- read_xlsx(here("clean_data","air_transportation_2020.xlsx"),
                      sheet = "Sulaimaniya Airlines")


suly_df |> 
  slice_max(order_by = Flights_Number,n = 9) -> suly_df

suly_df |> 
  ggplot(aes(x=reorder(Airlines,Flights_Number),y=Flights_Number))+
  geom_chicklet(width = 0.45,aes(fill = Flights_Number))+
  scale_fill_gradient(low = ka_col$purple[[4]],
                      high = ka_col$purple[[10]])+
  ggtitle(label = "Sulaimaniya",
          subtitle = subtitle)+
  ylab(y_title)+
  xlab(x_title)+
  labs(caption = caption)+
  scale_y_continuous(breaks = pretty_breaks(8),
                     expand = expansion(mult = c(0, .1)))+
  theme_bw()+
  riyadh_theme+
  coord_flip()+
  geom_text(aes(x=Airlines,
                y= Flights_Number + 80,
                label = glue("{comma(Flights_Number,1)}")),
            nudge_y = c(0,0,0,0,0,0),
            size = 3,
            color=ka_col$title[1],
            fontface = "bold") -> suly_p


####### Plot 4 #######

basra_df <- read_xlsx(here("clean_data","air_transportation_2020.xlsx"),
                      sheet = "Basra Airlines")


basra_df |> 
  slice_max(order_by = Flights_Number,n = 10) -> basra_df

basra_df |> 
  ggplot(aes(x=reorder(Airlines,Flights_Number),y=Flights_Number))+
  geom_chicklet(width = 0.45,aes(fill = Flights_Number))+
  scale_fill_gradient(low = ka_col$purple[[4]],
                      high = ka_col$purple[[10]])+
  ggtitle(label = "Basra",
          subtitle = subtitle)+
  ylab(y_title)+
  xlab(x_title)+
  labs(caption = caption)+
  scale_y_continuous(breaks = pretty_breaks(8),
                     expand = expansion(mult = c(0, .1)))+
  theme_bw()+
  riyadh_theme+
  coord_flip()+
  geom_text(aes(x=Airlines,
                y= Flights_Number + 130,
                label = glue("{comma(Flights_Number,1)}")),
            nudge_y = c(0,0,0,0,0,0),
            size = 3,
            color=ka_col$title[1],
            fontface = "bold") -> basra_p

####### Plot 5 #######

najaf_df <- read_xlsx(here("clean_data","air_transportation_2020.xlsx"),
                      sheet = "Najaf Airlines")


najaf_df |> 
  slice_max(order_by = Flights_Number,n = 10) -> najaf_df

najaf_df |> 
  ggplot(aes(x=reorder(Airlines,Flights_Number),y=Flights_Number))+
  geom_chicklet(width = 0.45,aes(fill = Flights_Number))+
  scale_fill_gradient(low = ka_col$purple[[4]],
                      high = ka_col$purple[[10]])+
  ggtitle(label = "Najaf",
          subtitle = subtitle)+
  ylab(y_title)+
  xlab(x_title)+
  labs(caption = caption)+
  scale_y_continuous(breaks = pretty_breaks(8),
                     expand = expansion(mult = c(0, .1)))+
  theme_bw()+
  riyadh_theme+
  coord_flip()+
  geom_text(aes(x=Airlines,
                y= Flights_Number + 60,
                label = glue("{comma(Flights_Number,1)}")),
            nudge_y = c(0,0,0,0,0,0),
            size = 3,
            color=ka_col$title[1],
            fontface = "bold") -> najaf_p


caption <- "<span style = 'color: #194354'>Source:</span>
                   Central Organization of Statistics & Information Technology (COSIT), Iraqi Ministry of Planning. <br/><span style = 'color: #194354'>Note:</span> The data shows the top ten airlines according to flights' number of each airport."


bgw_p + erbil_p + basra_p + suly_p + najaf_p +
  plot_annotation(title = "Number of Flights According to Airlines & Airports in 2020",
                  caption = caption,
                  theme = theme(
                    plot.title = element_markdown(size = 17,hjust =0.5,vjust = 0.2,color = ka_col$title["1"],
                                                  margin=margin(0,0,10,0)),
                    plot.caption = element_markdown(color = ka_col$title["1"],size = 12,
                                                    hjust = 0,margin=margin(0,0,0,0)))
  )


ggsave(filename = glue("air_number_of_flights_by_airport.png"),
       path = here("results"),
       width=13,height= 6,units="in",dpi=300)

