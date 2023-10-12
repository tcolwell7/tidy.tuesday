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


# load and create data ------------

#' create data for plot 1 
#' grouped data by country
#' to highlight "fill rate" for single year 2022. 


df <- read_excel("uk_trq_data.xlsx") %>% 
  clean_names() %>%
  filter(quota_year==2022, quota_type == "FTA")%>%
  group_by(quota_origin, iso) %>%
  summarise(fill_rate=mean(quota_fill_rate, na.rm = TRUE)) %>%
  filter(str_length(iso)<=3) %>% # remove origins with multiple countries such as Central America
  filter(fill_rate >= 0.01) %>%
  arrange(-fill_rate) %>%
  ungroup() %>%
  mutate(rank = 1:n())


# create plot --------

#' create lolipop chart using ggalt package
#' ordered higher average fill rate at top
#' # add flag icon as a point on chart to emphasise country
#' 


lol <-
  df %>%
  ggplot( 
    aes(
      y=reorder(quota_origin,fill_rate), # re-order y-axis 
      x=fill_rate
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
  scale_x_continuous(labels=scales::percent,n.breaks=5) # convert x-axis percentage format


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
    axis.text.x.bottom = element_text(size=12),#,face="bold"),
    axis.text.y.left = element_text(size=12,face="italic",color="black"),
    legend.position = "none"
  )

# css text for plot -----------

colr1 <- "#CF102D"
colr2 <-'#00285F'
colr3 <- "#004D44"
caption_color <- "#0000EE"

title_css <- 
  glue::glue("<span style = 'font-size:16pt;color:{colr2}'>UK-FTA partner Tariff Rate Quota Utilisation</span><br>
             <span style = 'font-size:11pt;'>Average TRQ fill-rates across quota year 2022</span></br>"
  )
#<span style = 'font-size:16pt;color:{colr2}'>Tariff Rate Quota Utilisation</span><br>



caption_css <-
  glue::glue(
    "<span style = 'font-size=16;color:black;'>Source:</span>
     <span style  = 'font-size16;color:{caption_color}'>HMRC trade-tariff service</span><span style ='color:black'> and </span><span style  = 'font-size16;color:{caption_color}'>Rural Payments Agency</span>"
  ) 


lol2 <- 
  lol + labs(
  title = title_css,
  caption = caption_css) + 
  theme(
    plot.title = element_markdown(),
    plot.caption = element_markdown(hjust = 0, size = 9)
  )



# css text -----------



#' Iceland has on average a 82% fill-rate across their FTA TRQ allocation. 
#' This is driven by their fully filled quoas n Salmon, Sole and Sheepmeet. 
#' 
#' Likewise, Norway's third highest fill-rate is driven by their in demand
#'  Fish quotas for shrimpts, lobester, swordfish and cod. 
#'  
#'  Morocco has the second highest average FTA parner fill-rate of 63%. 
#'  Morocco has highly in demand quotas covering corgettes, tomooas and cucumeber which are consistnely over subscribed both pre and post EU exit. 
#'  Serbia had one of the largest increases in average fill rates, jumping form 0% in 2021 to 12% in 2022. Tunisia had an average fill rate of 5% increasing to 10% in 2022.


# Save text data in a tibble
  tib_summary_text <- tibble(
    x = 0, 
    y = c(1.52, 1.25,0.75,0.34,0), 
    label = c(
              "<span style = 'font-size=15;color:#02529C'>**ICELAND'S**</span> <span style = 'color:grey10'>average FTA fill-rate jumped<br> from **62%** in 2021 to **83%** in 2022, <br>the largest increase of any FTA partner. <br>This was driven by their filled **Salmon**, **Sole** <br>and **Sheepmeat** quotas.<br> **Iceland had the highest average fill-rate <br>across all FTA partners in 2021 and 2022**.</span>",
              "<span style = 'font-size=15;color:#BA0C2F'>**NORWAY**</span> <span style = 'color:grey10'>utilises a significant proportion <br>of their fish quota allocation, fully filling their<br>**Shrimp** and **Swordfish and whitefish fillets**. </span> ",
              "<span style = 'font-size=15;color:#C1272D'>**MORROCO**</span> <span style = 'color:grey10'>has the second largest <br>average fill rate in 2022 of **51%**. <br> This is slightly down from **53%** in 2021, with <br> Morocco's TRQs for **Cucumbers**, <br>**Strawberries** and **Tomatoes**  being fully filled<br> across both years.</span> ",
              "<span style = 'font-size=15;color:#02529C'>**Serbia**</span> <span style = 'color:grey10'>had one of the largest <br> increases in average fill-rates <br>jumping from **0%** in 2021 to **13%** in 2022.<br> Likewise for</span> <span style = 'font-size=15;color:#BA0C2F'>**Tunisa**</span> <span>,who in 2021 had a <br> **0.1%** average fill-rate increase to **8%** in 2022.</span> ",
              "<span style = 'font-size=15;color:#0056B9'>**Ukraine**</span> <span style = 'color:grey10'>and</span> <span style = 'font-size=15;color:#ce1126'>**Egypt**</span> were the only two <br>agreements to see their average fill-rates drop <br>significanly from 2021 to 2022 with reductions <br> of **37%** and **45%** respectively.</span>"
              )#C1272D
  )
#02529C
  # Create text plot with geom_richtext() and theme_void()
  text_plot <- tib_summary_text %>% 
    ggplot() +
    geom_richtext(
      aes(x, y, label = label),
      size = 4,
      hjust = 0,
      vjust = 0,
      label.colour = NA
    ) +
    coord_cartesian(xlim = c(0, 1.2), ylim = c(0, 2), clip = 'off') +
    # clip = 'off' is important for putting it together later.
    theme_void()
  text_plot
  
  
  #lol+text_plot+lol2

# dumbbell plot ----------

#' the third plot isa dumbell plot
#' highlighing he chnage in average fill rate
#' again, using ggalt package
#' first the data needs creating
#' and ordered in the same positioning as he first plot
#' instead of label axis ggimage will be used 
#' 

  
df2 <- read_excel("uk_trq_data.xlsx") %>% 
    clean_names() %>%
    filter(
      quota_year<=2022, 
      quota_type == "FTA",
      iso %in% df$iso
      )%>%
    group_by(quota_origin, iso,quota_year) %>%
    summarise(fill_rate=mean(quota_fill_rate, na.rm = TRUE)) %>%
    filter(str_length(iso)<=3) %>% # remove origins with multiple countries such as Central America
    arrange(iso) 

df2$quota_year <- as.character(df2$quota_year)
  
myLevels <- df
  
  df2$quota_origin <- factor(df2$quota_origin, levels=myLevels$quota_origin)
  


x21 <- df2 %>% filter(quota_year=="2021")
x22 <- df2 %>% filter(quota_year=="2022")

plot3 <-
df2 %>%
  ggplot(aes(x = fill_rate, y = quota_origin))+
  geom_point(aes(color=quota_year),size=2)+
  scale_y_discrete(limits=rev)+
  geom_segment(
    data = x21,
    aes(
      x = fill_rate, 
      y = quota_origin,
      yend = x22$quota_origin, 
      xend = x22$fill_rate
    ), 
    color = "#aeb6bf",
    linewidth = 2, #Note that I sized the segment to fit the points
    alpha = .6)+
  geom_point(aes(color=quota_year),size=2)


# customisation 

title_css2 <- 
  glue::glue("<span style = 'font-size:15pt;color:{colr2}'>UK-FTA TRQ utilisation 2021-2022.</span><br>
             <span style = 'font-size:11pt;'>Average TRQ fill-rates compared for quota year 2021 and 2022.</span></br>"
  )

plot3 <- plot3+
  theme_classic()+
  scale_x_continuous(labels=scales::percent,n.breaks=7)+ # convert x-axis percentage format
  scale_color_manual(values = c(colr1, colr2))+
  labs(
    x="",
    y="",
    title = title_css2,
  )+
  theme(
    plot.title = element_markdown(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=10),
    axis.text.x.bottom = element_text(size=12)
  )+
  guides(color = guide_legend(override.aes = list(size = 3))) # increase size of legend shapes


# plot3 ggimage ----------------

# create distinct dataset with country/iso and create y axis
# create the x value as a negative so it is plot before y axis
# have to create single lines for custom axis lines
df3 <- df2 %>% distinct(., quota_origin, iso) %>%
  mutate(x_fill=-0.1)


plot4 <- plot3 +
  ggflags::geom_flag(
    data = df3,
    aes(
      x=x_fill,
      y=quota_origin,
      country = tolower(iso)
    ),
    size = 7
  )+
  theme(
    axis.line.y = element_blank()
  )+
  ggimage::geom_image(
    aes(
      y="Lebanon",
      x=0.70,
      image="DBT_Red2.jpg"
    ),
    size = 0.45
    #position = position_stack(vjust =1)
  )
  
# final plot and save -----------


final_plot <- lol2 + text_plot + plot4

ggsave("png/05_trq_patchwork_css_chart.png",final_plot,
       height=7,width=14)


# end
