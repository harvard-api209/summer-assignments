# Original data from https://github.com/josephwccheng/olympedia_web_scraping/tree/main
# Load packages ----------------------------------------------------------

library(tidyverse)
library(hrbrthemes)
library(ggtext)
library(httr)
library(jsonlite)

# Get the data -----------------------------------------------------------

medals   <- read_csv("https://raw.githubusercontent.com/josephwccheng/olympedia_web_scraping/main/data/Olympic_Games_Medal_Tally.csv")
athletes <- read_csv("https://raw.githubusercontent.com/josephwccheng/olympedia_web_scraping/main/data/Olympic_Athlete_Event_Results.csv")
games    <- read_csv("https://raw.githubusercontent.com/josephwccheng/olympedia_web_scraping/main/data/Olympics_Games.csv")


# Get Paris 2024 data ----------------------------------------------------
response <- GET("https://api.olympics.kevle.xyz/medals")

data      <- content(response, as = "text")
json_data <- fromJSON(data, flatten = TRUE)
paris     <- as_tibble(json_data$results)

# Cleaning Paris to match the other data
paris <- paris |> 
  janitor::clean_names() |> 
  select(country = country_name, bronze = medals_bronze, gold = medals_gold, silver = medals_silver, total = medals_total) |> 
  mutate(
    city = "Paris",
    year = 2024
  )

# Cleaning ---------------------------------------------------------------

athletes |>
  # Extract the year from the 'edition' column and convert it to numeric
  mutate(year = as.numeric(str_extract(edition, "\\b\\d{4}\\b"))) |> 
  # Filter the data to only include entries for the year 2020 and where a medal was won
  filter(
    year == 2020,
    !is.na(medal)
  ) |> 
  # Group the data by country (using NOC code), medal type, and whether it's a team sport
  group_by(country_noc, medal) |>
  count() |> 
  ungroup() |> 
  filter(country_noc == "USA") |> 
  summarize(sum = sum(n))
  
# USA has 39 golds in 2020
athletes |>
  # Extract the year from the 'edition' column and convert it to numeric
  mutate(year = as.numeric(str_extract(edition, "\\b\\d{4}\\b"))) |> 
  # Filter the data to only include entries for the year 2020 and where a medal was won
  filter(
    year == 2020,
    !is.na(medal)
  ) |> 
  # Group the data by country (using NOC code), medal type, and whether it's a team sport
  group_by(country_noc, medal, isTeamSport) |>
  # Summarize the data by counting distinct events to avoid double-counting team sports
  summarise(medal_count = n_distinct(event)) |> 
  # Remove grouping to reset for the next summarization
  ungroup() |> 
  # Group again by country and medal type to sum up the medals
  group_by(country_noc, medal) |> 
  # Summarize the total medals by summing the medal counts for each country and medal type
  summarise(total_medals = sum(medal_count))  



# Plot -------------------------------------------------------------------

# Custom colors based on your list
custom_colors <- c(
  "United States"     = "#E3120B", 
  "China"             = "#FBA493", 
  "France"            = "#7892C5", 
  "Great Britain"     = "#666666", 
  "Russian Federation" = "#344499",
  "Other"             = "#BEBCAB"
)

games_order <- c(
  "Barcelona, 1992",
  "Atlanta, 1996", 
  "Sydney, 2000", 
  "Athina, 2004", 
  "Beijing, 2008", 
  "London, 2012", 
  "Rio de Janeiro, 2016", 
  "Tokyo, 2020",
  "**Paris, 2024**"
)


medals |> 
  filter(year <= 2020 & year >= 1992) |>
  filter(str_detect(edition, "Summer")) |> 
  left_join(select(games, edition, city), by = "edition") |> 
  bind_rows(paris) |> 
  mutate(
    city = glue::glue("{city}, {year}"), 
    city = ifelse(city == "Paris, 2024", "**Paris, 2024**", city),
    city = fct_relevel(city, games_order)
  ) |> 
  mutate(
    points = (gold * 3) + (silver * 2) + (bronze * 1),
    country   = case_when(
      country %in% c("ROC", "Unified Team", "Russian Federation") ~ "Russian Federation", 
      country == "People's Republic of China" ~ "China",
      TRUE ~ country
    ),
    color_lbl = ifelse(country %in% c("United States", "China", "France", "Great Britain", "Russian Federation"), country, "Other"),
    color_lbl = factor(color_lbl, levels = c("United States", "China", "France", "Great Britain", "Russian Federation", "Other"))
  ) |> 
  ggplot(
    aes(
      x = points, 
      y = city,
      color = color_lbl
    )
  ) +
  geom_vline(xintercept = 0) + 
  geom_point(size = 4.5) +
  scale_color_manual(values = custom_colors) +
  scale_x_continuous(position = "top", expand = c(0.01,0)) + 
  scale_y_discrete(expand = c(0,0)) + 
  coord_cartesian(clip = "off") +
  guides(
    color = guide_legend(nrow = 1)
  ) +   
  labs(
    x = NULL, 
    y = NULL,
    title = "American exceptionalism",
    subtitle = "Summer Olympics, medal points*",
    caption = "\n*Gold=3 points, Silver=2, Bronze=1 †Includes Unified Team in 1992 and Russian Olympic Committee in 2020\nSources: International Olympic Committee; Olympedia"
  ) + 
  theme_ipsum_es(base_family = "Econ Sans Cnd") +
  theme(
    # Background
    panel.background = element_blank(),
    plot.background = element_blank(),
    
    # Grid lines
    panel.grid.major.x = element_line(color = "lightgray", size = 0.5),
    panel.grid.major.y = element_line(color = "lightgray", size = 0.5),
    panel.grid.minor = element_blank(),
    
    # Axis text
    axis.text.y = element_markdown(size = 12, color = "gray30", hjust = 0),    
    axis.text = element_text(size = 12, color = "gray30"),
    
    # Axis titles
    axis.title = element_text(size = 14, color = "gray30"),
    
    # No legend title
    legend.title = element_blank(),
    legend.position = "top",
    legend.location = "plot", 
    legend.direction = "horizontal",
    legend.justification = "left", 
    legend.box.spacing = unit(10 , "pt"),
    legend.margin = margin(0,0,0,0),
    legend.text = element_text(size = 12, color = "black"),

    # Caption text
    plot.caption = element_text(size = 12, hjust = 0, family = "Econ Sans Cnd"),
    plot.caption.position =  "plot",
    plot.title.position = "plot",
    plot.title = element_text(family = "Econ Sans Cnd"),
    plot.subtitle = element_text(family = "Econ Sans Cnd")
  )  

ggsave(
  "figs/Fig-economist-olympics.png", 
  dpi = 720, height = 10, width = 14, units = "in", bg = "white", scale = 0.7
)
