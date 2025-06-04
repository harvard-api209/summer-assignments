
# Load packages ----------------------------------------------------------

library(qualtRics)
library(tidyverse)
library(ggtext)
library(ggstats)
library(glue)

# Read data --------------------------------------------------------------

# Define the paths
download_folder    <- "C:/Users/ifyou/Downloads"
destination_folder <- "feedback/data"

# Ensure the destination folder exists
if (!dir.exists(destination_folder)) {
  dir.create(destination_folder, recursive = TRUE)
}

# Define the file pattern
zip_file_pattern <- "API\\+209\\+-\\+Math\\+Camp_"

# Get the list of files in the Downloads folder
files_in_download <- list.files(download_folder, pattern = zip_file_pattern, full.names = TRUE)

# Get the current date
current_date <- Sys.Date()

# Check if the ZIP file exists, copy it, unzip it, rename the CSV, and then delete the ZIP file
if (length(files_in_download) > 0) {
  zip_file_to_copy <- files_in_download[1] 
  
  # Construct the full path for the destination
  destination_zip_path <- file.path(destination_folder, basename(zip_file_to_copy))
  
  # Copy the ZIP file to the destination folder
  copy_success <- file.copy(from = zip_file_to_copy, to = destination_zip_path)
  
  if (copy_success) {
    # Unzip the file in the destination folder
    unzip(zipfile = destination_zip_path, exdir = destination_folder)
    
    # Get the list of CSV files in the destination folder after unzipping
    csv_files_in_destination <- list.files(destination_folder, pattern = "\\.csv$", full.names = TRUE)
    
    # If there is a CSV file, rename it
    if (length(csv_files_in_destination) > 0) {
      original_csv_file <- csv_files_in_destination[1]  # Assuming there's only one CSV file
      new_csv_file_name <- paste0("feedback_", current_date, ".csv")
      new_csv_file_path <- file.path(destination_folder, new_csv_file_name)
      
      # Rename the CSV file
      file.rename(from = original_csv_file, to = new_csv_file_path)
      print(paste("CSV file renamed to:", new_csv_file_name))
    }
    
    # Delete the original ZIP file from both the Downloads folder and the destination folder
    file.remove(zip_file_to_copy)
    file.remove(destination_zip_path)
  }
}

# Load data --------------------------------------------------------------

df <- read_survey("feedback/data/feedback_2024-08-12.csv")

# Define the color palette
colors <- c("#FE6D73", "#FFCB77", "#FEF9EF", "#17C3B2", "#227C9D")

# Cleaning ---------------------------------------------------------------

df_long <- df |> 
  filter(!is.na(Q1_1)) |> 
  # Select only the correct questions
  select(starts_with("Q"), -Q_RecaptchaScore) |> 
  mutate(id = row_number()) |> 
  # Pivot the data
  pivot_longer(cols = -id, names_to = "Question", values_to = "Response") 

# List of patterns for subdatasets
question_patterns <- list(
  Q1 = "Q1_",
  Q2 = "Q2_",
  Q3 = "Q3"
)

# Create subdatasets and summarize them using purrr
summarized_data <- map(question_patterns, ~{
  df_long |> 
    filter(str_detect(Question, .x)) |> 
    group_by(Question, Response) |> 
    summarise(
      count = n(),
      .groups = "drop"
    ) |> 
    ungroup() |> 
    group_by(Question) |> 
    mutate(
      pct = count / sum(count)
    ) |> 
    ungroup()
})

# Count the total number of responses before filtering
total_responses <- df |> 
  filter(!is.na(Q1_1)) |> 
  nrow()

# Create the caption with glue
caption_text <- glue("**Notes:** A total of {total_responses} students completed the feedback survey.")

# Question 1 -------------------------------------------------------------

# Labels for the geom text
custom_label <- function(x) {
  p <- scales::percent(x, accuracy = 1)
  p[x < 0.05] <- ""
  p
}

# Factors
q1_fct <- c(
  "Extremely uncomfortable",
  "Somewhat uncomfortable",
  "Neither comfortable nor uncomfortable",
  "Somewhat comfortable",
  "Extremely comfortable"
)

# Question Labels
question_labels <- c(
  "Q1_1" = "R (before the summer tutorial)",
  "Q1_2" = "R (after the summer tutorial)",
  "Q1_3" = "Stata",
  "Q1_4" = "Python",
  "Q1_5" = "A programming language (C++, Java)",
  "Q1_6" = "Excel"
)

df_long |> 
  filter(str_detect(Question, "Q1_")) |> 
  # Order the Response factor levels
  mutate(
    Response = factor(Response, levels = q1_fct),
    Question = factor(Question, levels = names(question_labels), labels = question_labels)
  ) |> 
  group_by(Question) |> 
  ggplot(
    aes(
      y = Question, 
      fill = Response
    )
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
  geom_bar(position = "likert", stat = "prop", complete = "fill", color = "black", width = 0.6) +
  geom_text(
    aes(by = Question, label = custom_label(after_stat(prop))),
    stat = "prop",
    position = position_likert(vjust = .5), 
    complete = "fill",
    family = "Roboto Condensed"
  ) +
  scale_x_continuous(label = label_percent_abs()) +
  scale_fill_manual(values = colors) + 
  labs(
    x = "Percentage", 
    y = NULL, 
    fill = NULL,
    title = "How **comfortable** do you feel using the following software?",
    caption = caption_text
  ) +  
  hrbrthemes::theme_ipsum_rc() +
  theme(
    plot.title = element_markdown(hjust = 0, face = "plain"), 
    axis.text.y = element_text(size = rel(1.1), face = "bold"),  
    axis.text.x = element_text(size = rel(1.1)),  
    axis.title.x = element_text(size = rel(1.5), face = "bold", hjust = 0.5), 
    legend.text = element_text(size = rel(1.1)), 
    legend.position = "bottom",
    panel.grid.major.y = element_blank(), 
    panel.grid.major.x = element_blank(), 
    axis.ticks.y = element_blank(),       
    plot.caption = element_markdown(size = rel(1), hjust = 0)  
  )

ggsave(
  "figs/Fig-programming-likert.png", 
  dpi = 320, scale = 0.9, height = 22, width = 40, units = "cm", bg = "white"
)

## Separate the plots
question_labels <- c(
  "Q1_1" = "Before",
  "Q1_2" = "After"
)

df_long |> 
  filter(str_detect(Question, "Q1_")) |> 
  # Keep only R 
  filter(str_detect(Question, "Q1_1|Q1_2")) |> 
  # Order the Response factor levels
  mutate(
    Response = factor(Response, levels = q1_fct),
    Question = factor(Question, levels = names(question_labels), labels = question_labels)
  ) |> 
  group_by(Question) |> 
  ggplot(
    aes(
      y = Question, 
      fill = Response
    )
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
  geom_bar(position = "likert", stat = "prop", complete = "fill", color = "black", width = 0.5) +
  geom_text(
    aes(by = Question, label = custom_label(after_stat(prop))),
    stat = "prop",
    position = position_likert(vjust = .5), 
    complete = "fill",
    family = "JetBrains Mono"
  ) +
  scale_x_continuous(label = label_percent_abs()) +
  scale_fill_manual(values = colors) + 
  labs(
    x = "Percentage", 
    y = NULL, 
    fill = NULL,
    title = "How **comfortable** do you feel using **R**?",
    caption = caption_text
  ) +  
  hrbrthemes::theme_ipsum_rc() +
  theme(
    plot.title = element_markdown(hjust = 0.5, face = "plain"), 
    axis.text.y = element_text(size = rel(1.25), face = "bold"),  
    axis.text.x = element_text(size = rel(1.1), family = "JetBrains Mono"),  
    axis.title.x = element_text(size = rel(1.5), face = "bold", hjust = 0.5), 
    legend.text = element_text(size = rel(1.1)), 
    legend.position = "top",
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    axis.ticks.y = element_blank(),       
    plot.caption = element_markdown(size = rel(1), hjust = 0)  
  )

ggsave(
  "figs/Fig-programming-likert-R.png", 
  dpi = 320, scale = 0.9, height = 22, width = 40, units = "cm", bg = "white"
)

question_labels <- c(
  "Q1_1" = "R (before the summer tutorial)",
  "Q1_2" = "R (after the summer tutorial)",
  "Q1_3" = "Stata",
  "Q1_4" = "Python",
  "Q1_5" = "C++, Java, others",
  "Q1_6" = "Excel"
)

df_long |> 
  filter(str_detect(Question, "Q1_")) |> 
  # Keep only R 
  filter(!str_detect(Question, "Q1_1|Q1_2")) |> 
  # Order the Response factor levels
  mutate(
    Response = factor(Response, levels = q1_fct),
    Question = factor(Question, levels = names(question_labels), labels = question_labels)
  ) |> 
  group_by(Question) |> 
  ggplot(
    aes(
      y = Question, 
      fill = Response
    )
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
  geom_bar(position = "likert", stat = "prop", complete = "fill", color = "black", width = 0.5) +
  geom_text(
    aes(by = Question, label = custom_label(after_stat(prop))),
    stat = "prop",
    position = position_likert(vjust = .5), 
    complete = "fill",
    family = "JetBrains Mono"
  ) +
  scale_x_continuous(label = label_percent_abs()) +
  scale_fill_manual(values = colors) + 
  labs(
    x = "Percentage", 
    y = NULL, 
    fill = NULL,
    title = "How **comfortable** do you feel using the following software?",
    caption = caption_text
  ) +  
  hrbrthemes::theme_ipsum_rc() +
  theme(
    plot.title = element_markdown(size = rel(2), hjust = 0.5, face = "plain"), 
    axis.text.y = element_text(size = rel(1.25), face = "bold"),  
    axis.text.x = element_text(size = rel(1.1), family = "JetBrains Mono"),  
    axis.title.x = element_text(size = rel(1.75), face = "bold", hjust = 0.5), 
    legend.text = element_text(size = rel(1.1)), 
    legend.position = "top",
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    axis.ticks.y = element_blank(),       
    plot.caption = element_markdown(size = rel(1), hjust = 0)  
  )

ggsave(
  "figs/Fig-programming-likert-Others.png", 
  dpi = 320, scale = 0.9, height = 22, width = 40, units = "cm", bg = "white"
)

# Question 2 -------------------------------------------------------------

q2_fct <- c(
  "Not effective at all",
  "Slightly effective",  
  "Moderately effective",
  "Very effective",      
  "Extremely effective"
)

question_labels <- c(
  "Q2_1" = "R Videos (Primers + Tutorials)",
  "Q2_2" = "R Summer Assignment"
)

df_long |> 
  filter(str_detect(Question, "Q2_")) |> 
  # Order the Response factor levels
  mutate(
    Response = factor(Response, levels = q2_fct),
    Question = factor(Question, levels = names(question_labels), labels = question_labels)
  ) |> 
  group_by(Question) |> 
  ggplot(
    aes(
      y = Question, 
      fill = Response
    )
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
  geom_bar(position = "likert", stat = "prop", complete = "fill", color = "black", width = 0.6) +
  geom_text(
    aes(by = Question, label = custom_label(after_stat(prop))),
    stat = "prop",
    position = position_likert(vjust = .5), 
    complete = "fill",
    family = "Roboto Condensed"
  ) +
  scale_x_continuous(label = label_percent_abs()) +
  scale_fill_manual(values = colors) + 
  labs(
    x = "Percentage", 
    y = NULL, 
    fill = NULL,
    title = "How **effective** were the following in getting you to learn R?",
    caption = caption_text
  ) +  
  hrbrthemes::theme_ipsum_rc() +
  theme(
    plot.title = element_markdown(hjust = 0, face = "plain"), 
    axis.text.y = element_text(size = rel(1.1), face = "bold"),  
    axis.text.x = element_text(size = rel(1.1)),  
    axis.title.x = element_text(size = rel(1.5), face = "bold", hjust = 0.5), 
    legend.text = element_text(size = rel(1.1)), 
    legend.position = "bottom",
    panel.grid.major.y = element_blank(), 
    panel.grid.major.x = element_blank(), 
    axis.ticks.y = element_blank(),       
    plot.caption = element_markdown(size = rel(1), hjust = 0)  
  )

ggsave(
  "figs/Fig-effective-likert.png", 
  dpi = 320, scale = 0.9, height = 20, width = 40, units = "cm", bg = "white"
)


# Question 3 -------------------------------------------------------------

q3_fct <-  c(
  "Extremely difficult",
  "Somewhat difficult",
  "Neither easy nor difficult",
  "Somewhat easy",
  "Extremely easy"
)

q3 <- "How **difficult** was it to complete the R Summer assignment for you?"

df_long |> 
  filter(str_detect(Question, "Q3")) |> 
  # Order the Response factor levels
  mutate(
    Response = factor(Response, levels = q3_fct)
  ) |> 
  count(Response) |> 
  mutate(
    pct = n / sum(n)
  ) |> 
  ggplot(
    aes(
      x = Response, 
      y = pct,
      fill = Response
    )
  ) +
  geom_col(color = "black") +
  geom_text(
    aes(label = scales::percent(pct)),
    vjust = -.75,
    size = 5,
    family = "Roboto Condensed"
  ) + 
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = colors) +
    labs(
      y = "Percentage", 
      x = NULL, 
      title = q3, 
      caption = caption_text
    ) +    
  hrbrthemes::theme_ipsum_rc() +  
  theme(
    plot.title = element_markdown(hjust = 0, face = "plain"), 
    axis.text.y = element_text(size = rel(1.1), face = "bold"),  
    axis.text.x = element_text(size = rel(1.1)),  
    axis.title.x = element_text(size = rel(1.5), face = "bold", hjust = 0.5), 
    legend.text = element_text(size = rel(1.1)), 
    legend.position = "none",
    axis.ticks.y = element_blank(),       
    plot.caption = element_markdown(size = rel(1), hjust = 0)  
  )

ggsave(
  "figs/Fig-r-difficulty.png", 
  dpi = 320, scale = 0.9, height = 20, width = 40, units = "cm", bg = "white"
)
