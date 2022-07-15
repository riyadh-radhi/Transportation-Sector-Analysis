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
library(sf)
library(ggrepel)
library(purrr)

#reading the data

bridge_df <- read_xlsx(here("clean_data","private_cars_2020.xlsx"),
                     sheet = "bridges_by_types_Dec2020")

map_data <- st_read("C:\\Users\\Riyadh\\Desktop\\KAPITA\\Building and Construction\\main\\clean_data\\map_data\\irq_admbnda_adm1_cso_20190603.shp")


#data preprocessing

#renaming column
bridge_df |> 
  rename("City" = "Governorate") -> bridge_df
bridge_df |> 
  rowwise() |> 
  mutate(Total = sum(c(`Concrete Bridge\r\n`,`an iron bridge`,`Floating bridge`))) -> bridge_df 
  

#finding centriod
map_data <- map_data  %>% 
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))

bridge_df %>% 
  dplyr::mutate(City = case_when(City %in% "Ninevah"~ "Nineveh",
                                 City %in% "Kirkuk"~ "Kirkuk",
                                 City %in% "Diala"~ "Diyala",
                                 City %in% "AL-Anbar"~ "Anbar",
                                 City %in% "Baghdad"~ "Baghdad",
                                 City %in% "Babylon"~ "Babil",
                                 City %in% "Kerbela"~ "Kerbala",
                                 City %in% "Wasit"~ "Wassit",
                                 City %in% "Salah Al-Deen"~ "Salah Al-Din",
                                 City %in% "Najaf"~ "Najaf",
                                 City %in% "AL- Qadisiya"~ "Diwaniyah",
                                 City %in% "AL- Muthanna"~ "Muthanna",
                                 City %in% "Thi- Qar"~ "Thi Qar",
                                 City %in% "Missan"~ "Maysan",
                                 City %in% "Basrah"~ "Basrah")) -> bridge_df


map_data %>% 
  dplyr::mutate(ADM1_EN = case_when(ADM1_EN %in% "Al-Qadissiya"~ "Diwaniyah",
                                    ADM1_EN %in% "Al-Basrah"~ "Basrah",
                                    ADM1_EN %in% "Al-Najaf"~ "Najaf",
                                    ADM1_EN %in% "Al-Anbar"~ "Anbar",
                                    ADM1_EN %in% "Al-Muthanna"~ "Muthanna",
                                    ADM1_EN %in% "Al-Sulaymaniyah"~ "Sulaymaniyah",
                                    ADM1_EN %in% "Ninewa"~ "Nineveh",
                                    TRUE ~ ADM1_EN)) -> map_data

merge(x = map_data,y = bridge_df,by.x = "ADM1_EN", by.y = "City", all.x = T) -> merged_data


merged_data %>% 
  mutate(nudge_y = case_when(ADM1_EN %in% c("Basrah") ~ 0.2,
                             ADM1_EN %in% c("Diwaniyah") ~ 0.05,
                             ADM1_EN %in% c("Salah Al-Din") ~ -0.2,
                             ADM1_EN %in% c("Maysan") ~ -0.1,
                             ADM1_EN %in% c("Babil") ~ -0.1,
                             ADM1_EN %in% c("Diyala") ~ 0,
                             ADM1_EN %in% c("Baghdad") ~ -0.05,
                             ADM1_EN %in% c("Thi Qar") ~ -0.1,
                             TRUE ~ 0),
         nudge_x = case_when(ADM1_EN %in% c("Diyala") ~ 0.1,
                             TRUE ~ 0),
         label_size = case_when(ADM1_EN %in% c("Baghdad") ~ 2.6,
                                ADM1_EN %in% c("Babil") ~ 2.6,
                                ADM1_EN %in% c("Sulaymaniyah") ~ 2.3,
                                ADM1_EN %in% c("Diwaniyah") ~ 2.8,
                                ADM1_EN %in% c("Kerbala") ~ 2.8,
                                TRUE ~ 3.5)) -> merged_data


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
              axis_label_size = 14,
              legend_title = 12,
              legend_text = 12,
              
              text_size = 2,
              title = title,
              caption_size = 10)


# Plot Parameters ---------------------------------------------------------

subtitle <- ""
glue("Total Number of Bridges across Iraqi Cities in 2020") -> title

caption <- "<span style = 'color: #194354'>Source:</span>
                   Central Organization of Statistics & Information Technology (COSIT), Iraqi Ministry of Planning."

# caption <-glue("<br></br><span style = 'color: {ka_col$source['1']}'>Source:</span>
#                  {unique(data$platform)} Job Matching Platform
#                  <br></br><span style = 'color: {ka_col$source['1']}'>Note:</span>
#                  Data collection is from the 1st of January 2021 until the 1st of August")

y_title <- "Count of Cars in Millions"
x_title <- "Year"

ggplot(merged_data) + 
  geom_sf(aes(fill = Total))+
  theme_void()+
  scale_fill_gradient(low = "#f5e6e6", high = ka_col$purple[[7]])+
  geom_text(aes(x =lon ,y =lat, label = ifelse(!is.na(Total),
                                               paste0(ADM1_EN,
                                                      "\n",
                                                      Total),
                                               paste0(ADM1_EN)
  )),
  
  size = merged_data$label_size,
  nudge_y = merged_data$nudge_y,
  nudge_x = merged_data$nudge_x,
  color = "black",
  lineheight = .9)+
  labs(title = title,
       caption = caption)+
  theme(plot.title = element_markdown(size = params$title_size,
                                      hjust =0.5,
                                      vjust = -0.7,
                                      color = "#3d3d3d"),
        plot.caption = element_markdown(color = "#3d3d3d",
                                        face = "italic",
                                        size = params$caption_size,
                                        hjust = 0,
                                        padding = unit(c(10, 0, 0, 0), "pt")),
        axis.text=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text=element_text(size=params$legend_size),
        # legend.title = element_blank(),
        legend.position = "right",
        #legend.key.size = unit(0.5, 'cm'),
        #plot.caption = element_text(hjust = 0, size = 16),
        panel.border  = element_blank(),
        axis.title = element_blank(),
        axis.ticks= element_blank())+
  guides(fill=guide_colourbar(title="Bridges Count",
                              size = 14,
                              colour = ka_col$title[[1]],
                              label.theme = element_text(size = 12)))


#OUTPUTTING THE FILE
ggsave(filename = "bridges_numbers.png",
       path = here("results"),
       width=width, height=height, units="in", dpi=dpi)
