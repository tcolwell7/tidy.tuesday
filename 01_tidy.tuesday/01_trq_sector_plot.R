
#' First TidyTuesday exercise to explore and experiment 
#' creating more aesthetic and complicated charts using R
#' and the incredible offer of packages R includes
#' The first is a relatively simple stacked bar plot
#' however this is my first opportunity to practice custom css style 
#' in ggplot text
#' combined with facet - ordering
#' and creating an interactive ggiraph plot - which I am exploring for inclusion in shiny apps. 

# set up ------------

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


options(scipen=999)


path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]")) 

`%notin%` <- Negate(`%in%`) # Custom negate function


#' Example UK TRQ usage plot broken down by sector grouping
#' utilising css style for title
#' and added ggiraph interactive plot
#' 


uk_trqs <- read_excel("uk_trq_data.xlsx") %>% clean_names()

# css help:
#' https://stackoverflow.com/questions/17083362/colorize-parts-of-the-title-in-a-plot
#' 

## data wrangling ----------


# one option is to present the chart using proportion 
# as the size of each groupings usage can greatly vary

# create df with counts of each grouping
# data needs to be filtered to countries with both groups (agri and non agri)

tally <- uk_trqs %>% 
  filter(quota_year == 2021) %>%
  group_by(grouping,type) %>% tally() %>% tally() %>% filter(n==1)

df <- uk_trqs %>% 
  filter(quota_year == 2021) %>%
  group_by(grouping,type) %>%
  summarise(
    total_usage = sum(quota_usage)
  ) %>%
  mutate(prop = total_usage/sum(total_usage)) %>%
  filter(prop != "NaN") %>%
  filter(grouping %notin% tally$grouping) %>%
  ungroup() 



#  plot --------------

## define variables --------

# define variables and titles for cleaner ggplot code

agri_col <- "#894843"
non_agri_col <- "#CC6666"
pfoods_col <- "#999999"

bg_color <- "#D7E0DA"
font_color <- "#1f3225"

title <- "UK-FTA TRQ usage by sector type"

# define subtitle using css with ggtext package which enables this
subtitle <- glue::glue(
          "Usage is broken down by 
          <span style = 'font-size:16pt;color:{agri_col}'>Agri</span> and
          <span style = 'font-size:16pt;color:{non_agri_col}'>Non-Agri</span> goods. <br>
          <span style = 'font-size:10pt;color:grey52'>Quota year: 2021</span></br>"
          )

# pre-define theme 

custom_theme <- 
  theme(
    text = element_text(color = font_color), 
    panel.grid.minor = element_blank(), # remove grid lines
    panel.grid.major = element_blank(), 
    plot.background = element_rect(fill="white"), # white background
    plot.subtitle = element_markdown(face="bold", size = 14), # chart titles
    plot.title = element_markdown(face="bold",size=16), # ensure markdown so css styling integrates
    plot.caption = element_markdown(size=12),
    legend.position = "none", # revmove legend
    axis.text.y = element_text(size=12,face="bold"), # axis text formatting 
    axis.text.x = element_text(size=12,face="bold"),
    axis.line.x.bottom = element_line(linewidth=0.5,color="grey52") # add horizontal line on x axis
  )


## create plot -----------

plot <-
  df %>%
  arrange(desc(type), prop) %>% # arrange and create factor order for plot so order isn't alphabetical
  mutate(grouping = factor(grouping, unique(grouping))) %>% 
    ggplot( 
      aes(
        data_id = grouping, # data_id for interactive ggiraph plot
        tooltip = grouping, # tooltip for interactive ggirpah plot (normal ggplot will run with this inputs)
        x=grouping,
        y=prop,
        fill=type) # grouping breakdown
    )+
    geom_col_interactive(position="stack",linewidth=1,color="white")+ # ggiraph interactive function
    theme_minimal()+ # defauly inbuilt theme for formatting 
    labs(
      title = title, # chart labels
      x = "", 
      y = "",
      subtitle = subtitle,
      caption = "source: HMRC, 2022"
    )+
    scale_y_continuous(labels = scales::percent) + # format percentage axis
    coord_flip() + # flip scales so country on y-axis
    custom_theme+ # apply full custom theme
    scale_fill_manual(values=c(agri_col,non_agri_col)) # manual colors


# save ggplot image

ggsave(plot=plot,filename="png/TidyTuesday_01_ggplot1.png")


## interactive plot ------


# create ggiraph interactive plot: 

  girafe(
    ggobj = plot, 
    options = list( # customise ggirpah tooltip
      opts_tooltip(
        opacity = 0.8, use_fill = TRUE,
        use_stroke = FALSE, 
        css = "padding:5pt;font-size:1rem;color:white"),
      opts_hover_inv(css = "opacity:0.4"), # opacity of bars when tooltip activated 
      opts_toolbar(saveaspng = FALSE), # toggle TRUE/FALSE to enable saving png image
      opts_zoom(max = 1),
      opts_hover(
        css = girafe_css(
          css = glue::glue(""),
          text = glue::glue("stroke:none;fill:{font_color};fill-opacity:;")
        ))
    ),
    height_svg = 6, # size of image
    width_svg = 9
  )
  






