

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


# process data -----------------


load("Trade_datasets.RData") # load trade data 

fta_country_input <- # UK-FTA country input
    read_excel("trade_agreement_country_input.xlsx") %>% 
    clean_names() %>%
    filter(tac == 1)


# aggregate trade/goods/services data to UK agreement

df_trade <- 
  trade_annual %>%
    filter(iso2 %in% fta_country_input$iso2) %>%
    filter(year >= 2010) %>%
    left_join(
        fta_country_input[,c("iso2","uk_agreement_name")],
        by = "iso2"
    ) %>%
    group_by(year,uk_agreement_name) %>%
    summarise(
      total = sum(total),
      import = sum(import),
      export = sum(export)
    )


df_goods <- 
  goods_annual %>%
  filter(iso2 %in% fta_country_input$iso2) %>%
  filter(year >= 2010) %>%
  left_join(
    fta_country_input[,c("iso2","uk_agreement_name")],
    by = "iso2"
  ) %>%
  group_by(year,uk_agreement_name) %>%
  summarise(
    total = sum(total),
    import = sum(import),
    export = sum(export)
  )

df_services <- 
  services_annual %>%
  filter(iso2 %in% fta_country_input$iso2) %>%
  filter(year >= 2010) %>%
  left_join(
    fta_country_input[,c("iso2","uk_agreement_name")],
    by = "iso2"
  ) %>%
  group_by(year,uk_agreement_name) %>%
  summarise(
    total = sum(total),
    import = sum(import),
    export = sum(export)
  )


# rank data ------------

# create country ranking for highest FTA trade partner with the UK

df <- 
    df_services %>%
    group_by(year) %>%
    #filter(uk_agreement_name != "Andean countries") %>%
    mutate(
        prop = total / sum(total),
        rank = rank(-total)
    ) %>%
    filter(year %in% c(2010, 2014, 2018, 2021)) %>% # filter select years
    filter(rank <= 10) %>% 
    group_by(uk_agreement_name) %>%
    mutate(min = min(year)) 


# create basic bump plot 

bump <- 
  df %>% 
    ggplot(aes(year, rank, color = uk_agreement_name)) +
    geom_bump(linewidth=1,smooth=15)+ # larger smooth value - steeper the curve
    geom_point(
        data = df %>% distinct(., min, .keep_all = TRUE),
        aes(x=min),
        size=3)+
    theme_minimal(base_size=14)+
    scale_y_reverse(n.breaks=13)+ # flip y-axis
    scale_x_continuous(n.breaks=5) +# 3 data points on x-axis
    theme(legend.position = "bottom")

# add dark background theme

theme <- 
    theme(
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill="black"),
        axis.text = element_blank(),
        panel.grid.minor = element_blank(), # remove grid lines
        panel.grid.major = element_blank(), 
        legend.position = "none"
    )


# add dark theme to plot

bump <- bump + theme

# add custom labeling

bump2 <- 
    bump +
    geom_text(data = tibble(x = 2008.5, y = 1:10), aes(x = x, y = y, label = y), 
              inherit.aes = F,
              color = "white")+
    geom_text(
        data = df %>% distinct(., min, .keep_all = TRUE),
        aes(x=year,y=rank+-0.5, label=uk_agreement_name),
        color="white",
        fontface = 2,
        size=3.5
    )+
    geom_point( # adds white circles around rank values
      data = tibble(x = 2008.5, y = 1:10), aes(x = x, y = y), 
      inherit.aes = F,
      color = "white",
      size = 10,
      pch = 21) + # hollows out point to create circle
    geom_text(
      data = tibble(x = c(2010, 2014, 2018, 2021), y = 11),
      aes(x=x,y=y,label=x),
      color="white",
      fontface = 2,
      size=4.5
    )


# add title

bump3 <- bump2 + 
  labs(
    title = "Top ranked UK-FTA trading partners covering total trade",
    subtitle = "Goods and services",
    caption = "source: ONS"
  ) +
  theme(
    plot.title = element_text(color = "white",size=12),
    plot.subtitle = element_text(color = "white",size=9),
    plot.caption = element_text(color = "white",size=9)
  )


bump33 + bump3 + bump333
# chart function --------------

# to create multiple plots the same process can be created
# as a function to produce plots for trade/goods/services 


bump_plotFunction <- function(.data,
                              .flow, 
                              .years, 
                              .rank.label = NULL,
                              .year.axis = NULL,
                              .title = NULL,
                              .subtitle = NULL, 
                              .caption = NULL
                              ){
  
  #' *test variables*
  
  # .data = df_trade
  # .flow = "import"
  # .type = "Trade"
  # .years = c(2010,2014,2018,2021)
  # .title = "TITLE"
  # .subtitle = "SUBTITLE TEST"
  # .caption = "CAPTION TEST"
  # .rank.label = "Y"
  
  
  # create country ranking for highest FTA trade partner with the UK
  
  df <- 
    .data %>%
    group_by(year) %>%
    mutate(
      prop = !!as.symbol(.flow) / sum(!!as.symbol(.flow)),
      rank = rank(-!!as.symbol(.flow))
    ) %>%
    filter(year %in% .years) %>% # filter select years
    filter(rank <= 10) %>% 
    group_by(uk_agreement_name) %>%
    mutate(min = min(year)) 
  
  
  # create basic bump plot 
  
  bump <- 
    df %>% 
    ggplot(aes(year, rank, color = uk_agreement_name)) +
    geom_bump(linewidth=1,smooth=15)+ # larger smooth value - steeper the curve
    geom_point(
      data = df %>% distinct(., min, .keep_all = TRUE),
      aes(x=min),
      size=3)+
    theme_minimal(base_size=14)+
    scale_y_reverse(n.breaks=13)+ # flip y-axis
    scale_x_continuous(n.breaks=5) +# 3 data points on x-axis
    theme(legend.position = "bottom")
  
  # add dark background theme
  
  theme <- 
    theme(
      plot.background = element_rect(fill = "black"),
      panel.background = element_rect(fill="black"),
      axis.text = element_blank(),
      panel.grid.minor = element_blank(), # remove grid lines
      panel.grid.major = element_blank(), 
      legend.position = "none"
    )
  
  
  # add dark theme to plot
  
  bump <- bump + theme
  
  # add custom labeling
  
  bump2 <- 
    bump +
    geom_text(
      data = df %>% distinct(., min, .keep_all = TRUE),
      aes(x=year,y=rank+-0.5, label=uk_agreement_name),
      color="white",
      fontface = 2,
      size=3.5
    )
  
  
  # logic to create custom x axis label
  if(!is.null(.year.axis)){
    
    bump2 <- bump2 +
    geom_text(
      data = tibble(x = c(2010, 2014, 2018, 2021), y = 11),
      aes(x=x,y=y,label=x),
      color="white",
      fontface = 2,
      size=6
    )
  }
  
  # logic to create custom y axis label
  if(!is.null(.rank.label)){

    bump2 <- bump2 +
    geom_text(data = tibble(x = 2008.5, y = 1:10), aes(x = x, y = y, label = y),
              inherit.aes = F,
              color = "white")+
    geom_point( # adds white circles around rank values
      data = tibble(x = 2008.5, y = 1:10), aes(x = x, y = y),
      inherit.aes = F,
      color = "white",
      size = 10,
      pch = 21)  # hollows out point to create circle
  } else{
    
    # create black text to create space for plot - bu can't be seen
    bump2 <- bump2 +
      geom_text(data = tibble(x = 2008.5, y = 1:10), aes(x = x, y = y, label = y),
                inherit.aes = F,
                color = "black")
  }
  
  # add title
  
  bump3 <- bump2 + 
    labs(
      title = .title,
      subtitle = .subtitle,
      caption = .caption
    ) +
    theme(
      plot.title = element_text(color = "white",size=16),
      plot.subtitle = element_text(color = "white",size=12),
      plot.caption = element_text(color = "white",size=12)
    )
  
  return(bump3)
  
}


bump_trade <- bump_plotFunction(
  .data = df_trade,
  .flow = "total",
  .years = c(2010,2014,2018,2021),
  .title = "Top ranked UK-FTA trading partners",
  .subtitle = "Goods and services total trade",
  .rank.label = "Y"
)

bump_imports <- bump_plotFunction(
  .data = df_trade,
  .flow = "import",
  .years = c(2010,2014,2018,2021),
  .subtitle = "Goods and services total imports"
)


bump_exports <- bump_plotFunction(
  .data = df_trade,
  .flow = "export",
  .years = c(2010,2014,2018,2021),
  .subtitle = "Goods and services total exports"
)


bump_goods <- bump_plotFunction(
  .data = df_goods,
  .flow = "total",
  .years = c(2010,2014,2018,2021),
  .subtitle = "Total goods trade",
  .rank.label = "Y"
)

bump_goods_imports <- bump_plotFunction(
  .data = df_goods,
  .flow = "import",
  .years = c(2010,2014,2018,2021),
  .subtitle = "Total goods imports"
)


bump_goods_exports <- bump_plotFunction(
  .data = df_goods,
  .flow = "export",
  .years = c(2010,2014,2018,2021),
  .subtitle = "Total goods exports"
)


bump_services <- bump_plotFunction(
  .data = df_services,
  .flow = "total",
  .years = c(2010,2014,2018,2021),
  .subtitle = "Total services trade",
  .rank.label = "Y",
  .year.axis = "Y"
)

bump_services_imports <- bump_plotFunction(
  .data = df_services,
  .flow = "import",
  .years = c(2010,2014,2018,2021),
  .subtitle = "Total services imports",
  .year.axis = "Y"
  
)


bump_services_exports <- bump_plotFunction(
  .data = df_goods,
  .flow = "export",
  .years = c(2010,2014,2018,2021),
  .subtitle = "Total services exports",
  .caption = "Source: ONS 2022 Q3",
  .year.axis = "Y"
  
)


trade <- bump_trade + bump_imports + bump_exports
goods <- bump_goods + bump_goods_imports + bump_goods_exports
services <- bump_services + bump_services_imports + bump_services_exports


top_fta_ranks_bump_plot <- trade / goods / services

ggsave("03_top_fta_ranks_bump_plot.png",top_fta_ranks_bump_plot,
       height=12,width=16)
