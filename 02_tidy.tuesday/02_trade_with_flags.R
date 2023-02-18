
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
library(ggimage)

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
  filter(iso %notin% c("TN","XS")) %>%
  mutate(
    tooltip = # cusotm tool tip text for interactive plot
      paste0(
        "Agreement: ",quota_origin,
        "<br>Utilisation: ", paste0(round(fill_rate,2)*100,"%")
      )
  )
        
# define custom colours for interactive plot specific to countries

lol <-
    df %>%
    ggplot( 
      aes(
      y=reorder(quota_origin,fill_rate), # re-order y-axis 
      x=fill_rate,
      data_id = quota_origin, # input id for ggiraph interactive function
      tooltip = tooltip # input id for ggiraph interactive function
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
  geom_point_interactive(aes(color=quota_origin),alpha=0.01,size=2) + # add small opaque point to initite interactive plot which doesnt hide flags
  theme( # simple customisaiton of plot format
    axis.line.x.bottom = element_line(color="grey52"),
    axis.text.x.bottom = element_text(size=12,face="bold"),
    axis.line.y.left = element_line(color="grey52"),
    axis.text.y.left = element_text(size=12,face="italic"),
    legend.position = "none"
  )

# add custom colours to points for interactive tooltip fill
# colours are orders based on A-Z df order 

clr <- c("brown1","brown1","darkgoldenrod1","cornsilk4","brown1","deepskyblue3",
         "cornflowerblue","darkolivegreen3","brown1","darkolivegreen3","brown2","darkgoldenrod1",
         "brown2","brown2","chartreuse4","brown","darkgoldenrod1","darkgoldenrod1"
)

# add colour style
lol <- lol + scale_color_manual(values=clr)

ggsave("png/02_flag_lollipop_ggplot.png",lol)


## chart 1 ggiraph --------


tooltip_css <- "padding:5pt;font-size:1rem;color:white;"

  girafe(
    ggobj = lol, 
    options = list( # customise ggirpah tooltip
      opts_tooltip(
        opacity = 0.8, use_fill = TRUE,
        use_stroke = FALSE, 
        css = tooltip_css),
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

# bar chart depicting change in trade since 2016

# data downloaded from UN comtrade highlight
# data is split into imports and exports
# plot is for total trade (imports + exports)
# simple data aggregation is required before plotting 

data <- read_csv("comtrade.csv") %>% clean_names()

# country filter: G7 and select OECD partners for comparison to UK
country_filt <- c("United Kingdom","Japan","Canada","Germany","France","Italy","USA","Netherlands","Spain","Sweden","Ireland","Finland","Switzerland")

data <- data %>%
  filter(reporter %in% country_filt) %>%
  filter(year == 2016 | year == 2021) %>%
  filter(grepl("Export",trade_flow)) %>%
  group_by(reporter,iso,year) %>%
  summarise(total = sum(trade_value_us,na.rm = TRUE)) %>%
  mutate(pct_change = (total/lag(total) - 1))


# theme and formatting

un_comtrade_colr <- "#2980B9"

theme <- 
 theme(
  plot.caption = element_markdown(),
  axis.text.y = element_text(face = "bold"),
  axis.text.x = 
    element_text(
      color = un_comtrade_colr,
      face = "bold",
      angle=35,
      hjust = 0.9,
      vjust=0.8,
      size=9), # format text for x axis so text fits plot
  plot.background = element_rect(fill="grey92"), # shading for background so flags can be distinguished 
  panel.background = element_rect(fill="grey92")
)

# custom text values for labels

caption <- glue::glue(
  "<span style = 'font-size:10pt;'>source:</span>",
  "<span style = 'font-size:11pt;font-family:serif;color:{un_comtrade_colr}'>UN comtrade</span>"
)
  
plot <- 
 data %>%
   filter(year == 2021) %>%
   ggplot(
     aes(
       x = reorder(reporter,pct_change),
       y = pct_change
     )
   )+
   geom_bar(
     stat="identity",
     color=un_comtrade_colr,
     fill="white", 
     width=0.8 # change width of bars
     )+
   theme_classic()+ # theme formatting for simple x/y axis grid
   labs(
     x = "",
     y = "",
     title = "Percentage change of goods exports since 2016 until 2021.",
     subtitle = "G7 and select OECD members",
     caption = caption
   )+
   scale_y_continuous(labels = scales::percent)

# add theme formatting
plot <- plot + theme

# add flags on top of bars
plot <-
  plot +
  ggimage::geom_flag(
    aes(
      y=(pct_change+0.02),
      image=iso
      ),
    size=0.07
    )


# add un comtrade image in background
# custom plot position 

plot <- plot + 
  ggimage::geom_image(
    aes(
      x="USA",
      y=0.5,
      image="UN-Comtrade-logo.png"
       ),
    size = 0.5,
    )

plot

ggsave("png/02_ggimage_bar_plot.png")

# end


