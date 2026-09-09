## CC213### climate chage#
library(tidyverse)
library(glue)
library(scales)
library(purrr)
#temp_url <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/T_AIRS/GLB.Ts+dSST.csv"
#global_temp <- read_csv(temp_url, skip = 1, na = "***")

temp <- 
  
  read_csv("GLB.Ts+dSST (2).csv", skip = 1 , na = "***",show_col_types = FALSE) %>% 
  select(year = Year, t_diff = `J-D`) %>% 
  ggplot(aes(x = year, y = t_diff))+
  geom_line(color = "grey", size = 0.5)+
  geom_point(fill = "white", color = "grey", shape = 21)+
  geom_smooth(se = FALSE, color = "black", size = 0.5, span = 0.15)+
  scale_x_continuous(breaks = seq(1880, 2023, 20),expand = c(0,0))+
  scale_y_continuous(limits = c(-0.5, 1.5),expand = c(0,0))+
  theme_light()+
  labs( x = "Year", y = "Temperature Anomaly (C)",
        title = "Global land Ocean Temp index",
        subtitle = "NASA(GISS)")+
  theme(axis.ticks = element_blank(),
        plot.title.position = "panel",
        plot.title = element_text(margin = margin(8), color = "Red", face = "bold"),
        plot.subtitle = element_text(margin = margin(2), size = 10, face = "bold"))

ggsave("figure/temp_index_plot.png", width = 6, height = 4)

# still in this need legend positions and
################################################################
## bar plot , cc215

library(tidyverse)
library(glue)
library(scales)
library(purrr)
library(lubridate)

t_data <- read_csv("GLB.Ts+dSST (2).csv", skip = 1 , na = "***",show_col_types = FALSE)%>% 
  select(year = Year, t_diff = `J-D`) %>% drop_na() 


 annotation <- t_data %>% 
  arrange(year) %>% 
  slice(1,n()) %>% 
  mutate(t_diff = 0,
         x = year + c(-5, 5)) 
 max_t_diff <- format(round(max(t_data$t_diff), 1), nsmall=1)
  
 t_data %>%
   ggplot(aes(x=year, y=t_diff, fill=t_diff)) +
   geom_col(show.legend=FALSE) +
   geom_text(data = annotation, aes(x=x, label=year), color="white") +
   geom_text(x=1880, y=1, hjust=0,
             label=glue("Global temperatures have increased by over {max_t_diff}\u00B0C since {min(t_data$year)}"),
             color="white") +
   # scale_fill_gradient2(low="darkblue", mid="white", high="darkred",
   #                     midpoint = 0, limits= c(-0.5, 1.5)) +
   # scale_fill_gradientn(colors=c("darkblue", "white", "darkred"),
   #                      values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
   #                      limits = c(min(t_data$t_diff), max(t_data$t_diff))) +
   scale_fill_stepsn(colors=c("darkblue", "white", "darkred"),
                     values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
                     limits = c(min(t_data$t_diff), max(t_data$t_diff)),
                     n.breaks=9) +
   theme_void() +
   theme(
     plot.background = element_rect(fill="black"),
     legend.text = element_text(color="white")
   )

  ggsave("figure/temp_index_barplot.png", width = 6, height = 4)
###########################################################################
## Climate barline gradient( warming strips) CC216##
  
  library(tidyverse)
  library(glue)
  library(scales)
  library(purrr) 
  
  t_data1 <- read_csv("GLB.Ts+dSST (2).csv", skip = 1 , na = "***",show_col_types = FALSE)%>% 
    select(year = Year, t_diff = `J-D`) %>% drop_na() 
  
  t_data1 %>% 
    ggplot(aes(x = year, y = 1, fill = t_diff))+
    geom_tile(show.legend = FALSE)+
    scale_fill_stepsn(colors = c("darkblue", "white", "red"),
                      values = rescale(c(min(t_data1$t_diff),0, max(t_data1$t_diff))),
                                       n.breaks = 6)+
    coord_cartesian(expand = 0)+
    scale_x_continuous(breaks = seq( 1890, 2022, 20))+
    theme_void()+
    theme(
      axis.text.x = element_text(color = "white",
                                  margin = margin( t=5,b = 10)),
      plot.background = element_rect(fill = "black")
    )
  
  
  
  ggsave("figure/temp_index_warmstrip.png", width = 6, height = 4)
    
#############################################################################
  ## Horizontal temperature line 
  
 t_diff1 <- read_csv("GLB.Ts+dSST (2).csv", skip = 1 , na = "***",show_col_types = FALSE)%>% 
    select(year = Year, month.abb) %>% 
    pivot_longer(-year, names_to = "month", values_to = "t_diff") %>% 
    drop_na() %>% 
  
  
  LD <- t_diff1 %>% 
    filter(month == "Dec") %>% 
    mutate(year = year +1, month = "LD")
  
  NJ <- t_diff1 %>% 
    filter(month == "Jan") %>% 
    mutate(year = year -1, month = "NJ")
  
  # we got 3 datafram for the temp
  
  temp_data <-  bind_rows(LD, t_diff1, NJ) %>% 
  mutate(month = factor(month, levels = c("LD", month.abb, "NJ")),
         month_number = as.numeric(month)-1) %>% 
  ggplot(aes(x = month_number, y = t_diff, group = year, colour = year))+
    geom_hline(yintercept = 0, color = "white")+
  geom_line()+
    scale_x_continuous( breaks = 1 : 12,
                        labels = month.abb)+
    scale_color_viridis_c(breaks = seq(1880, 2020, 20))+
    coord_cartesian(xlim =c(1,12))+
    labs( x = NULL,
          y = "Global Temperature change in [\u00B0C]",
      title = "Global temperature change since 1880 by month")+
        theme(
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "#333333"),
        panel.grid = element_blank(),
        axis.text = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        axis.ticks.length = unit(-5, "pt"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color="white", hjust = 0.5,size = 15),
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        legend.text = element_text(color="white"),
        legend.key.height = unit(55, "pt")
        )

  ggsave("figure/temp_index_Horizontallines.png", width = 6, height = 4)

##########################################################################
  
  ## spiral or coord cardination
  t_diff1 <- read_csv("GLB.Ts+dSST (2).csv", skip = 1 , na = "***",show_col_types = FALSE)%>% 
    select(year = Year, month.abb) %>% 
    pivot_longer(-year, names_to = "month", values_to = "t_diff") %>% 
    drop_na() 
    
    
  #   LD <- t_diff1 %>% 
  #   filter(month == "Dec") %>% 
  #   mutate(year = year +1, month = "LD")
  # 
  # NJ <- t_diff1 %>% 
  #   filter(month == "Jan") %>% 
  #   mutate(year = year -1, month = "NJ")
  
  # we got 3 datafram for the temp
  
  temp_data <- t_diff1 %>%  #bind_rows(LD, t_diff1, NJ) %>% 
    ggplot(aes(x = month_number, y = t_diff, group = year, colour = year))+
    geom_hline(yintercept = 0, color = "white")+
    scale_x_continuous( breaks = 1 : 12,
                        labels = month.abb)+
    scale_color_viridis_c(breaks = seq(1880, 2020, 20))+
    # coord_cartesian(xlim =c(1,12))+
    coord_polar()+
    labs( x = NULL,
          y = "Global Temperature change in [\u00B0C]",
          title = "Global temperature change since 1880 by month")+
    theme(
      panel.background = element_rect(fill = "black"),
      plot.background = element_rect(fill = "#333333"),
      panel.grid = element_blank(),
      axis.text = element_text(color = "white"),
      axis.ticks = element_line(color = "white"),
      axis.ticks.length = unit(-5, "pt"),
      axis.title = element_text(color = "white"),
      plot.title = element_text(color="white", hjust = 0.5,size = 15),
      legend.title = element_blank(),
      legend.background = element_rect(fill = NA),
      legend.text = element_text(color="white"),
      legend.key.height = unit(55, "pt")
    )
  
  
  ggsave("figure/temp_index_sprial_lines.png", width = 6, height = 4)
  
##################################
  








