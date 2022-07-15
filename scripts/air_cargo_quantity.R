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
air_df <- read_xlsx(here("clean_data","air_transportation_2020.xlsx"),
                      sheet = "gargo_by_kg_by_airport")

colnames(air_df)

air_df |> 
  mutate(Unloaded = Unloaded/1000000,
         Loaded = Loaded/1000000,
         Total = Total/1000000) -> air_df

air_df

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
                                margin=margin(0,0,0,0)),
  plot.subtitle = element_blank(),
  
  plot.caption = element_markdown(color = ka_col$title["1"],face = "italic",
                                  size = 12,hjust = 0,padding = unit(c(10, 0, 0, 0), "pt")),
  
  #####################                     AXIS                                  ################
  #axis.title.x = element_text(color = ka_col$title["1"],size = 12),
  axis.title.y =element_text(size=13,colour = ka_col$title,vjust = -30),
  axis.title.x =element_text(size=13,colour = ka_col$title),
  
  axis.text.y=element_blank(),
  axis.text.x = element_text(size = 12,color = ka_col$title),
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

# Plot Parameters ---------------------------------------------------------

subtitle <- ""
glue("Quantity of Cargo through the Iraqi Airports in 2020") -> title

caption <- "<span style = 'color: #194354'>Source:</span>
                   Central Organization of Statistics & Information Technology (COSIT), Iraqi Ministry of Planning."

y_title <- glue("Million Kilogram (kg) of Cargo")
x_title <- ""

########### data preprocessing 
air_df |> 
  select(-Total) |> 
  pivot_longer(cols = c(Unloaded, Loaded),names_to = "metric",values_to = "value") -> air_df


air_df |> 
  mutate(metric = case_when(metric %in% "Unloaded" ~ "Imported Cargo",
                            metric %in% "Loaded" ~ "Exported Cargo")) -> air_df

############3

library(ggplot2)

air_df |> 
  ggplot(aes(x = reorder(Airport,value), y = value))+
  geom_chicklet(width = 0.45,aes(fill = metric),position = "dodge2")+
  scale_fill_manual(values = c(ka_col$purple[[4]],ka_col$purple[[10]]))+
  ggtitle(label = title,
          subtitle = subtitle)+
  ylab(y_title)+
  xlab(x_title)+
  labs(caption = caption)+
  scale_y_continuous(breaks = pretty_breaks(8))+
  theme_bw()+
  riyadh_theme+
  geom_text(aes(x=Airport,
                y= value +1,
                label = glue("{round(value,3)} \n Million kg"),
                group = metric),
            size = 4.5,lineheight = 0.8,
            position = position_dodge2(width = 0.45),
            color= ka_col$title[[1]],
            fontface = "bold")




height <- 6
width <- 13
dpi <- 300

#OUTPUTTING THE FILE
ggsave(filename = "air_cargo_quantity.png",
       path = here("results"),
       width=width, height=height, units="in", dpi=dpi)


