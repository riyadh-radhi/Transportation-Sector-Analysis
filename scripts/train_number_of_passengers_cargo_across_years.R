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
library(patchwork)

options(scipen = 9999)

train_df <- read_xlsx(here("clean_data","trains.xlsx"),
                     sheet = "metrics_over_years")

colnames(train_df)

train_df |> 
  rename( "Paying Passengers" = "Number of passengers (thousand)_with charge",
          "Non-paying Passengers" = "Number of passengers (thousand)_free",
          "volume_of_goods" = "volume  of Goods transported with  charge (1000.ton)") |> 
  select(Year,`Paying Passengers`,`Non-paying Passengers`,volume_of_goods) |> 
  mutate(`Paying Passengers` = `Paying Passengers` *1000,
         `Non-paying Passengers` = `Non-paying Passengers` *1000,
         volume_of_goods = volume_of_goods *1000) -> train_df
############################
train_df |> 
  select(Year,`Paying Passengers`,`Non-paying Passengers`) |> 
  pivot_longer(cols = c(`Paying Passengers`,`Non-paying Passengers`),
               names_to = "Metric", values_to = "Value") -> passengers_df

##########################
train_df |> 
  select(Year,volume_of_goods) |> 
  pivot_longer(cols = c(volume_of_goods),
               names_to = "Metric", values_to = "Value") -> volume_df






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
params = list(
  axis_size = 10,
  title_size = 13,
  x_title ="",
  axis_title_size = 12,
  axis_label_size = 12,
  legend_title = 10,
  legend_text = 12,
  
  text_size = 3,
  title = title,
  caption_size = 10)


# Plot Parameters ---------------------------------------------------------

## PLOT 1 ####
##############

subtitle <- ""
glue("Number of Passengers in the Railways Activities over the Years") -> title

caption <- "<span style = 'color: #194354'>Source:</span>
                   Central Organization of Statistics & Information Technology (COSIT),<br/> Iraqi Ministry of Planning."


y_title <- "Number of Passengers"
x_title <- ""

########### data preprocessing 

passengers_df %>% 
  ggplot(aes(x=Year,
             y=Value,
             color=Metric,
             fill = Metric,
             group = Metric)) + 
  geom_bump(aes(group = Metric, color =Metric ),alpha = 0.4, size = 2)+
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
        axis.text=element_text(size=params$axis_size),
        legend.position = "top",
        legend.title = element_blank(), #change legend title font size
        legend.text = element_text(size=params$legend_text),
        
        panel.grid  = element_blank(),
        panel.border  = element_blank(),
        axis.text.x = element_text(size = params$axis_label_size, vjust = 1.2),
        axis.text.y = element_text(size = params$axis_label_size),
        
        axis.ticks= element_blank(),
        
        panel.grid.major.x = element_blank()
        
  )+ 
  scale_fill_manual(values = c(ka_col$purple[["8"]],
                               ka_col$turquaz[["8"]]),
                    name = "Indicator")+
  scale_color_manual(values = c(ka_col$purple[["8"]],
                                ka_col$turquaz[["8"]]),
                     name = "Indicator")+
  scale_y_continuous(breaks = pretty_breaks(7),labels = comma)+
  geom_text(aes(x=Year,
                y= Value + 25000,
                label = glue("{comma(Value)}")),
            size = 3,
            color= ka_col$title[[1]],
            fontface = "bold") -> plot1

## PLOT 2 ####
##############

subtitle <- ""
glue("Volume of Goods Transported in the Railway Activities") -> title
# 
# caption <- "<span style = 'color: #194354'>Source:</span>
#                    Central Organization of Statistics & Information Technology (COSIT), Iraqi Ministry of Planning."

caption <- ""
y_title <- "Volume in Ton"
x_title <- ""

########### data preprocessing 

volume_df |> 
  mutate(left_margin = case_when(Year %in% 2020 ~ -0.1,
                                 TRUE ~ 0)) -> volume_df

volume_df %>% 
  ggplot(aes(x=Year,
             y=Value,
             color=Metric,
             fill = Metric,
             group = Metric)) + 
  geom_bump(aes(group = Metric, color =Metric ),alpha = 0.4, size = 2)+
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
        axis.text=element_text(size=params$axis_size),
        legend.position = "none",
        legend.title = element_blank(), #change legend title font size
        legend.text = element_text(size=params$legend_text),
        
        panel.grid  = element_blank(),
        panel.border  = element_blank(),
        axis.text.x = element_text(size = params$axis_label_size, vjust = 1.2),
        axis.text.y = element_text(size = params$axis_label_size),
        
        axis.ticks= element_blank(),
        
        panel.grid.major.x = element_blank()
        
  )+ 
  scale_fill_manual(values = c(ka_col$turquaz[["8"]]),
                    name = "Indicator")+
  scale_color_manual(values = c(ka_col$turquaz[["8"]]),
                     name = "Indicator")+
  scale_y_continuous(breaks = pretty_breaks(7), label = comma,
                     position = "right")+
  geom_text(aes(x=Year,
                y= Value + 55000,
                label = glue("{comma(Value)}")),
            size = 3,nudge_x = volume_df$left_margin,
            color= ka_col$title[[1]],
            fontface = "bold") -> plot2

#scale_x_continuous(labels = 1:10,breaks = 1:10)

plot1 +  plot2


#OUTPUTTING THE FILE
ggsave(filename = "train_number_passengers_cargo_years.png",
       path = here("results"),
       width=width, height=height, units="in", dpi=dpi)



