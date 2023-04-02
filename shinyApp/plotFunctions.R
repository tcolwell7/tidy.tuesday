# back end functions for plot


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
library(plotly)
library(ggflags) # https://github.com/davidsjoberg/ggbump/wiki/geom_bump-with-flags
library(ggimage)

options(scipen=999)


path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]")) 

`%notin%` <- Negate(`%in%`) # Custom negate function

uk_trqs <- read_excel("data/uk_trq_data.xlsx") %>% clean_names()


plot1 <- function(){

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


# create ggiraph interactive plot: 

plot2 <- 
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

return(plot2)

}



plot2 <- function(){
  
df <- uk_trqs %>%
  filter(quota_year==2021)%>%
  group_by(quota_origin, iso) %>%
  summarise(fill_rate=mean(fill_rate, na.rm = TRUE)) %>%
  filter(str_length(iso)<=3) %>% # remove origins with multiple countries such as Central America
  filter(fill_rate > 0) %>%
  filter(iso %notin% c("TN","RS")) %>%
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
  ggflags::geom_flag(
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

tooltip_css <- "padding:5pt;font-size:1rem;color:white;"

lol2 <- 
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

return(lol2)

}



plot3 <- function(){
# bar chart depicting change in trade since 2016

# data downloaded from UN comtrade highlight
# data is split into imports and exports
# plot is for total trade (imports + exports)
# simple data aggregation is required before plotting

data <- read_csv("data/comtrade.csv") %>% clean_names()

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
      image="data/UN-Comtrade-logo.png"
    ),
    size = 0.5,
  )

return(plot)

}