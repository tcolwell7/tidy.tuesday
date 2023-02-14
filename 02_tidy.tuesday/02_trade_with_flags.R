
#' TidyTuesday to create simple and effective plots 
#' visualising the size of trade combining the use of 
#' images of each countries flags for effect 


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
library(ggflags) # https://github.com/davidsjoberg/ggbump/wiki/geom_bump-with-flags


options(scipen=999)


path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]")) 

`%notin%` <- Negate(`%in%`) # Custom negate function


uk_trqs <- read_excel("uk_trq_data.xlsx")


# chart 1 --------------

#' single year - fill-rate of UK TRQs utilising the ggalt package
#' for a lollipop chart 
#' this can be combined with ggiraph to create an interactive ggplot 

# compile data for 2021 removing specific origins

df <- uk_trqs %>%
  filter(quota_year==2021)%>%
  group_by(quota_origin, iso) %>%
  summarise(fill_rate=mean(fill_rate, na.rm = TRUE)) %>%
  filter(str_length(iso)<=3) %>% # remove origins with multiple countries such as Central America
  filter(fill_rate > 0) %>%
  filter(iso %notin% c("TN","XS"))


lol <-
    df %>%
    ggplot( 
      aes(
      y=reorder(quota_origin,fill_rate), # re-order y-axis 
      x=fill_rate,
      data_id = quota_origin,
      tooltip = quota_origin
    )
    ) +
    ggalt::geom_lollipop(
      size=1,
      color="grey52",
      point.size=3, 
      horizontal=TRUE
    )+
    theme_classic()+ # simple for axis borders
    labs(x="",y="")+
    scale_x_continuous(labels=scales::percent,n.breaks=7) # convert x-axis percentage format


# add flags to lollipop chart
lol <- 
  lol +
      geom_flag(
      aes(
          x=fill_rate,
          y=quota_origin,
          country = tolower(iso)
           ),
       size = 7
      ) +
  geom_point_interactive(alpha=0.01,size=2) + # add small opaque point to initite interactive plot which doesnt hide flags
  theme( # simple customisaiton of plot format
    axis.line.x.bottom = element_line(color="grey52"),
    axis.text.x.bottom = element_text(size=12,face="bold"),
    axis.line.y.left = element_line(color="grey52"),
    axis.text.y.left = element_text(size=12,face="italic"),
  )


ggsave("png/02_flag_lollipop_ggplot.png",lol)


## chart 1 ggiraph --------


girafe(
  ggobj = lol, 
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
        text = glue::glue("")
      ))
  ),
  height_svg = 6, # size of image
  width_svg = 9
)




# chart 2 ----------





