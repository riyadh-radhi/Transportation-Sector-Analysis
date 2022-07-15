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

air_df <- read_xlsx(here("clean_data","air_transportation_2020.xlsx"),
                     sheet = "revenue_and_wages")

colnames(air_df)


air_df |> 
  rename("2018" = "2018.0",
         "2019" = "2019.0",
         "2020" = "2020.0",
         "Metric" = "Details") |> 
  pivot_longer(cols = c("2018","2019","2020"),
               names_to = "year",values_to = "value") -> tidy_df


tidy_df |> 
  mutate(Metric = case_when(Metric %in% "Total revenues civil aviation authority" ~ "Civil Aviation Authority Revenue",
                            Metric %in% "Total revenues Iraqi airways responsible" ~ "Iraqi Airways Revenue",
                            Metric %in% "Total revenues The General Company for Air Navigation Services" ~ "The General Company for Air Navigation Services Revenue",
                            Metric %in% "Total Wages and bonuses paid for the Employees in Civil Aviation Authority" ~ "Civil Aviation Authority Wages",
                            Metric %in% "Total Wages and bonuses paid for the Employees in  Iraqi Airways" ~ "Iraqi Airways Wages",
                            Metric %in% "Total Wages and bonuses paid for the Employees in The General Company for Air Navigation Services" ~ "The General Company for Air Navigation Services Wages")) -> tidy_df




tidy_df |> 
  filter(str_detect(Metric, "Civil Aviation Authority")) -> civil_df

civil_df$Metric
civil_df |> 
  mutate(Metric = case_when(Metric %in% "Civil Aviation Authority Revenue" ~ "Revenue",
                            Metric %in% "Civil Aviation Authority Wages" ~ "Wages")) -> civil_df

civil_df$Value_in_billion <- (civil_df$value * 1000000) / 1000000000

######################################
tidy_df |> 
  filter(str_detect(Metric, "Iraqi Airways")) -> iraqi_ariways_df

iraqi_ariways_df$Metric
iraqi_ariways_df |> 
  mutate(Metric = case_when(Metric %in% "Iraqi Airways Revenue" ~ "Revenue",
                            Metric %in% "Iraqi Airways Wages" ~ "Wages")) -> iraqi_ariways_df


iraqi_ariways_df$Value_in_billion <- (iraqi_ariways_df$value * 1000000) / 1000000000

######################################
tidy_df |> 
  filter(str_detect(Metric, "General Company for Air")) -> general_company_df

general_company_df$Metric
general_company_df |> 
  mutate(Metric = case_when(Metric %in% "The General Company for Air Navigation Services Revenue" ~ "Revenue",
                            Metric %in% "The General Company for Air Navigation Services Wages" ~ "Wages")) -> general_company_df


general_company_df$Value_in_billion <- (general_company_df$value * 1000000) / 1000000000


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
  caption_size = 12)


# Plot Parameters ---------------------------------------------------------

## PLOT 1 ####
##############
air_df$Details
subtitle <- ""
glue("Iraqi Airways") -> title

caption <- ""


y_title <- "Billion IQD"
x_title <- ""

########### data preprocessing 

iraqi_ariways_df %>% 
  ggplot(aes(x=year,
             y=Value_in_billion,
             color=Metric,
             fill = Metric,
             group = Metric)) + 
  geom_bump(aes(group = Metric, color =Metric ),alpha = 0.4, size = 2)+
  geom_point(size=9, shape = 21)+
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
        axis.text.y = element_blank(),
        
        axis.ticks= element_blank(),
        
        panel.grid.major.x = element_blank()
        
  )+ 
  scale_fill_manual(values = c(ka_col$purple[["8"]],
                               ka_col$turquaz[["8"]]),
                    name = "Indicator")+
  scale_color_manual(values = c(ka_col$purple[["8"]],
                                ka_col$turquaz[["8"]]),
                     name = "Indicator")+
  scale_y_continuous(breaks = pretty_breaks(7))+
  geom_text(aes(x=year,
                y= Value_in_billion,
                label = glue("{round(Value_in_billion,1)}")),
            size = 2.5,
            color= "white",
            fontface = "bold")+
  guides(color = guide_legend(override.aes = list(size=3)))-> plot1

## PLOT 2 ####
##############
air_df$Details
subtitle <- ""
glue("Civil Aviation Authority") -> title
# 
# caption <- "<span style = 'color: #194354'>Source:</span>
#                    Central Organization of Statistics & Information Technology (COSIT), Iraqi Ministry of Planning."

caption <- ""
y_title <- "Billion IQD"
x_title <- ""

########### data preprocessing 

civil_df %>% 
  ggplot(aes(x=year,
             y=Value_in_billion,
             color=Metric,
             fill = Metric,
             group = Metric)) + 
  geom_bump(aes(group = Metric, color =Metric ),alpha = 0.4, size = 2)+
  geom_point(size=9, shape = 21)+
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
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = params$axis_title_size,
                                    color = ka_col$title),
        axis.text=element_text(size=params$axis_size),
        legend.position = "top",
        legend.title = element_blank(), #change legend title font size
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
                    name = "Indicator")+
  scale_color_manual(values = c(ka_col$purple[["8"]],
                                ka_col$turquaz[["8"]]),
                     name = "Indicator")+
  scale_y_continuous(breaks = pretty_breaks(7),
                     position = "right")+
  geom_text(aes(x=year,
                y= Value_in_billion,
                label = glue("{round(Value_in_billion,1)}")),
            size = 2.5,
            color= "white",
            fontface = "bold") +
  guides(color = guide_legend(override.aes = list(size=3))) -> plot2



## PLOT 3 ####
##############
air_df$Details
subtitle <- ""
glue("General Company for Air Navigation Services") -> title
# 
# caption <- "<span style = 'color: #194354'>Source:</span>
#                    Central Organization of Statistics & Information Technology (COSIT), Iraqi Ministry of Planning."

caption <- ""
y_title <- "Billion IQD"
x_title <- ""

########### data preprocessing 

general_company_df |> 
  filter(!year %in% "2018") -> general_company_df

general_company_df %>% 
  ggplot(aes(x=year,
             y=Value_in_billion,
             color=Metric,
             fill = Metric,
             group = Metric)) + 
  geom_bump(aes(group = Metric, color =Metric ),alpha = 0.4, size = 2)+
  geom_point(size=9, shape = 21)+
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
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = params$axis_title_size,
                                    color = ka_col$title),
        axis.text=element_text(size=params$axis_size),
        legend.position = "top",
        legend.title = element_blank(), #change legend title font size
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
                    name = "Indicator")+
  scale_color_manual(values = c(ka_col$purple[["8"]],
                                ka_col$turquaz[["8"]]),
                     name = "Indicator")+
  scale_y_continuous(breaks = pretty_breaks(7),
                     position = "right")+
  geom_text(aes(x=year,
                y= Value_in_billion,
                label = glue("{round(Value_in_billion,1)}")),
            size = 2.5,
            color= "white",
            fontface = "bold") +
  guides(color = guide_legend(override.aes = list(size=3))) -> plot3

#scale_x_continuous(labels = 1:10,breaks = 1:10)

plot1 +  plot2 + plot3 +
  plot_annotation(title = "Revenue & Wages in Billion IQD across the Years",
                  caption = "<span style = 'color: #194354'>Source:</span>
                   Central Organization of Statistics & Information Technology (COSIT), Iraqi Ministry of Planning.",
                  theme = theme(
                    plot.title = element_markdown(size = 17,hjust =0.5,vjust = 0.2,color = ka_col$title["1"],
                                                  margin=margin(0,0,0,0)),
                    plot.caption = element_markdown(color = "#3d3d3d",
                                                    face = "italic",
                                                    size = params$caption_size,
                                                    hjust = 0,
                                                    padding = unit(c(10, 0, 0, 0), "pt")))
  )


#OUTPUTTING THE FILE
ggsave(filename = "air_revenues_wages.png",
       path = here("results"),
       width=width, height=height, units="in", dpi=dpi)



