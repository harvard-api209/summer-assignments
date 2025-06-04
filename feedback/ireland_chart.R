
# Load packages ----------------------------------------------------------

library(tidyverse)
library(janitor)
library(scales)
library(hrbrthemes)
library(ggtext)
library(ggrepel)
library(ggpp)

# Load data --------------------------------------------------------------

df     <- read_csv("feedback/data/data-IIRu1.csv") 
games  <- read_csv("https://raw.githubusercontent.com/josephwccheng/olympedia_web_scraping/main/data/Olympics_Games.csv")

font = "Econ Sans Cnd"

games <- games |> 
  filter(str_detect(edition, "Summer")) |> 
  mutate(xaxis = glue::glue("{year}\n{city}")) |> 
  filter(between(year, 2000, 2024)) |> 
  select(Year = year, city, xaxis) 

# Cleaning ---------------------------------------------------------------

df |> 
  pivot_longer(-Year, values_to = "rank", names_to = "countries") |> 
  filter(
    countries %in% c("Ireland", "United States", "France"),
    Year >= 2000
  ) |> 
  left_join(games, by = "Year") |> 
  mutate(
    label = ordinal(rank),
    label = ifelse(countries == "Ireland", label, NA),
    seq   = cumsum(!is.na(label)), 
    label1 = ifelse(seq %% 2 == 0, label, NA),    
    label2 = ifelse(seq %% 2 != 0, label, NA),
    xaxis = as_factor(xaxis)
  ) |> 
  ggplot(
    aes(
      x = xaxis, 
      y = rank,
      group = countries,
      color = countries
    )
  ) +
  geom_line() +   
  geom_point(size = 5) +
  geom_point(size = 3, color = "white") + 
  geom_text(
    aes(label = label1), 
    color = "#c71e1d",
    nudge_y = -3,
    family = font
  ) +
  geom_text(
    aes(label = label2), 
    color = "#c71e1d",
    nudge_y = 3,
    family = font
  ) +  
  geom_richtext(
    aes(
      x = 3,
      y = 50, 
      label = "**Ireland**", 
    ),
    size = 6,
    fill = NA,
    color = "#c71e1d",
    label.color = NA,
    family = font
  ) + 
  geom_richtext(
    aes(
      x = 2,
      y = 70, 
      label = "no<br>medals<br>won", 
    ),
    size = 5,
    fill = "white",
    color = "grey",
    label.color = NA,
    family = font
  ) +   
  scale_y_reverse(limits = c(85, 1),  breaks = c(1, seq(10, 80, by = 10)), labels = label_ordinal(), expand = c(0.02,0)) +
  scale_color_manual(values = c("#cca285", "#c71e1d", "#c06f51")) + 
  labs(
    x = NULL, 
    y = NULL, 
    title = "<span style='color:#c71e1d'>**Ireland**</span>'s position in the Olympics Medals Table", 
    subtitle = "Compared to the position of the <span style='color:#c06f51'>**United States**</span> and <span style='color:#cca285'>**France**</span>",
    caption = "**Source**: Wikipedia via Kaggle · Created with GGPlot · **Original Chart**: @lisacmuth · **This Chart**: @rrmaximiliano"
  ) + 
  theme_ipsum_es(base_family = font) +
  theme(
    # Text adjustments
    plot.title = element_markdown(family = font),
    plot.subtitle = element_markdown(family = font),
    plot.caption = element_markdown(hjust = 0),
    
    # Grid
    panel.grid.minor = element_blank(),
    
    # Legend
    legend.position = "none",
    
    # Positions
    legend.location = "plot",
    plot.caption.position =  "plot",
    plot.title.position = "plot",    
  )

ggsave(
  "figs/Fig-ireland-olympics.png", 
  dpi = 320, height = 10, width = 16, units = "in", bg = "white", scale = 0.65
)
  



# Plot -------------------------------------------------------------------


