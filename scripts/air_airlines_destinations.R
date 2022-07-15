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

options(scipen = 999)
air_df <- read_xlsx(here("clean_data","air_transportation_2020.xlsx"),
                    sheet = "Airlines Destinations Number")

colnames(air_df)



air_df |> 
  pivot_longer(cols = c("Number of Destinations","Number of Countries"),
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

#plots details 
height <- 6
width <- 13
dpi <- 300
params = list(legend_size = 16,
              axis_size = 10,
              title_size = 15,
              x_title ="",
              axis_title_size = 14,
              axis_label_size = 10,
              legend_title = 12,
              legend_text = 12,
              
              text_size = 3.5,
              title = title,
              caption_size = 12)


# Plot Parameters ---------------------------------------------------------

subtitle <- ""
glue("Iraqi Airways Comparison With Airlines From Neighboring Countries") -> title

caption <- "<span style = 'color: #194354'>Source:</span> FlightConnections"

# caption <-glue("<br></br><span style = 'color: {ka_col$source['1']}'>Source:</span>
#                  {unique(data$platform)} Job Matching Platform
#                  <br></br><span style = 'color: {ka_col$source['1']}'>Note:</span>
#                  Data collection is from the 1st of January 2021 until the 1st of August")

y_title <- "Count"
x_title <- ""

########### data preprocessing 

tidy_df
tidy_df |> 
  mutate(y_values = case_when(metric %in% "Number of Destinations" ~ 20,
                              metric %in% "Number of Countries" ~ -20)) -> tidy_df

tidy_df %>% 
  ggplot(aes(x=reorder(Airline, -value),
             y=value,
             color=metric,
             fill = metric,
             group = metric)) + 
  geom_bump(aes(group = metric, color =metric),alpha = 0.4, size = 2)+
  geom_point(size=4, shape = 21)+
  theme_bw()+
  labs(title = title, y = y_title, x= x_title,
       caption = caption) + 
  theme(plot.title = element_text(size = params$title_size,
                                  hjust =0.5,
                                  vjust = -0.7,
                                  color = "#3d3d3d"),
        plot.caption = element_markdown(color = "#3d3d3d",
                                        face = "italic",
                                        size = params$caption_size,
                                        hjust = 0,
                                        padding = unit(c(10, 0, 0, 0), "pt")),
        axis.title.y = element_text(size = params$axis_title_size,
                                    color = ka_col$title),
        axis.title.x = element_text(size = params$axis_title_size,
                                    color = ka_col$title),
        legend.position = "top",
        legend.title = element_text(size=params$legend_title), #change legend title font size
        legend.text = element_text(size=params$legend_text),
        
        panel.grid  = element_blank(),
        panel.border  = element_blank(),
        axis.text.x = element_text(size = params$axis_label_size, vjust = 1.2),
        axis.text.y = element_blank(),
        axis.ticks= element_blank(),
        panel.grid.major.x = element_blank()
        
  )+ 
  scale_fill_manual(values = c(ka_col$purple[["8"]],
                               ka_col$turquaz[["8"]]),
                    name = "")+
  scale_color_manual(values = c(ka_col$purple[["8"]],
                                ka_col$turquaz[["8"]]),
                     name = "")+
  scale_y_continuous(breaks = pretty_breaks(7),
                     expand = expansion(mult = c(0.2, 0.2)))+
  geom_text(aes(x=Airline,
                y= value + y_values,
                label = glue("{value}")),
            size = 3,
            color= ka_col$title[[1]],
            fontface = "bold")+
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1)))



#OUTPUTTING THE FILE
ggsave(filename = "air_airlines_destinations.png",
       path = here("results"),
       width=width, height=height, units="in", dpi=dpi)



