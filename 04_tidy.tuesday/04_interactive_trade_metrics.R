#' 04 tidy.tuesday
#' interactive ggirpah plot highlighting two trade metrics
#' using ggiraph o enable interactivity across plots when selecting groupings


library(tidyverse)
library(janitor)
library(stringr)
library(readxl)
library(ggplot2)
library(ggthemes)
library(RColorBrewer) # for set colour palletes
library(ggalt)
library(ggiraph) # interactive ggplots
library(ggiraphExtra) # extention 
library(ggtext) # enable custom style for ggplot text
library(ggflags) # https://github.com/davidsjoberg/ggbump/wiki/geom_bump-with-flags
library(ggbump)
library(patchwork)

options(scipen=999)


path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]")) 

`%notin%` <- Negate(`%in%`) # Custom negate function
trq_data <- read_excel("trq_data.xlsx") %>% clean_names()


# plot 1 ------- 

# progress data

# TRQ usage grouped plot 

df <- trq_data %>% 
  filter(quota_origin == "Vietnam", quota_year == 2022, quota_type == "FTA") %>%
  group_by(quota_origin,quota_sector,quota_year) %>%
  summarise(volume = sum(quota_volume),
            bal = sum(remaining_balance)
  ) %>%
  mutate(usage = volume - bal) %>%
  filter(usage > 0) %>%
  mutate(
    tooltip =
      paste0(
        quota_sector, "<br>",
        scales::comma(volume), "<br>",
        scales::comma(usage)
         )
    )
  

# order bar from smallest to largest:

myLevels <- df %>% 
  group_by(quota_sector) %>% 
  summarise(usage = sum(usage)) %>%
  arrange(usage)

# apply order
df$quota_sector<- factor(df$quota_sector, levels=myLevels$quota_sector)


# create plot - stacked horizontal bar plot
plot1 <- 
  df %>% 
    ggplot(
      aes(
        x = quota_origin,
        y = usage,
        fill = quota_sector,
        data_id = quota_sector, # data id for ggiraph to select
        tooltip = tooltip # tool tip for interactive chart
      )
    ) +
    labs(
      title = "Vietnam quota usage in 2022",
      subtitle = "Usage broken down by sector, measured in Kilograms"
      )+
    geom_bar_interactive(stat = "identity",width=0.4) +
    coord_flip()+ # flips axis
    scale_fill_brewer(palette = "Blues")+
    scale_y_continuous("",n.breaks=5,labels = scales::comma)+
    theme_minimal()+ # basic theme formatting
    theme(
      axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom",
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size=10),
      plot.subtitle = element_text(size=14),
      plot.title = element_text(size=18)
    )

# ggiraph interactive plot:

  girafe(
    ggobj = plot1,
    options = list(
          opts_hover(css = ''), # remove css - otherwise custom colour for bars when selected chnages to default
          opts_tooltip(
          css = 'padding:5pt;background-color:white;font-family: verdana;font-size:12px'), # custom css to chnage format of hover box
          opts_hover_inv(css = "opacity:0.6;"), # opacity of other bars while tooltip is viewed
          opts_sizing(rescale = FALSE)
        ),
     height_svg = 7,
     width_svg = 9,
    fonts = list(sans = "verdana") # change plot font
    )

 
# plot 2 -------
  

#' plot to visualise all FTA agreements and their usage

# prepare data:
df2 <- trq_data %>% 
    filter(
      quota_year == 2022, 
      quota_type == "FTA", 
      quota_origin == "Vietnam"
      ) %>%
    group_by(quota_origin,quota_sector,quota_year) %>%
    summarise(volume = sum(quota_volume),
              bal = sum(remaining_balance)
    ) %>%
    mutate(usage = volume - bal) %>% # calc. allocation level usage
    mutate(fill_rate = usage / volume) %>% # calc. fill rate 
    filter(usage > 0) %>% # remove un-used sectors
    mutate(
      tooltip =
        paste0(
          quota_sector, "<br>",
          "Allocation volume: ", scales::comma(volume), "<br>",
          "Total usage: ", scales::comma(usage), "<br>",
          "Fill rate: ", scales::percent(fill_rate)
        )
    )
  
  

# create order for plot:
  
  myLevels <- df2 %>% 
    group_by(quota_sector) %>% 
    summarise(volume = sum(volume)) %>%
    arrange(volume)
  
  # apply order
  df2$quota_sector<- factor(df2$quota_sector, levels=myLevels$quota_sector)
  
  
plot2 <-
  df2 %>%
  ggplot()+
  geom_bar_interactive(
    aes(
      x=volume,
      y=quota_sector,
      data_id = quota_sector,
      tooltip = tooltip,
      fill = "#2171b5"
  ),
  stat = "identity"#,
  #fill = "#2171b5" # https://www.color-hex.com/color-palette/17597 - useful link for RColorBrewer Hex colours
  )+geom_bar_interactive(
    aes(
      x=usage,
      y=quota_sector,
      tooltip = tooltip,
      fill = "#bdd7e7" # not for custom legend, the fill must be within the plot aesthetic
    ),
    stat = "identity"#,
    #fill = "#bdd7e7"
  )+
  labs(
    x="",
    y="",
    title=" ",
    subtitle = "Vietnam sector quota volume and usage in 2022"
    #subtitle = "Data is measured in kilogrMS"
  )+
  scale_x_continuous(n.breaks=6,labels = scales::comma)+
  theme_minimal()+
  scale_fill_manual(
    name = '', 
    values =c('#2171b5','#bdd7e7'),
    labels = c('Volume','Usage')
  )+
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size=11),
    axis.text.x = element_text(size=10),
    plot.subtitle = element_text(size=15),
    legend.position = "bottom"
  )




# use patchwork to combine plots:
plot3 <- plot1 / plot2


# save plot image
ggsave("04_interactive_trade_metrics.png",plot3,
       height=12,width=16)
  
girafe(ggobj = plot3,
       options = list(
         opts_hover(css = ''),
         opts_tooltip(
           css = 'padding:5pt;background-color:white;font-family: verdana;font-size:12px'),
         opts_hover_inv(css = "opacity:0.6;"),
         opts_sizing(rescale = FALSE)
       ),
       height_svg = 7,
       width_svg = 9
       ,
       fonts = list(sans = "verdana")
)

# end 