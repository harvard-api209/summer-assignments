
# Packages ---------------------------------------------------------------

library(tidyverse)
library(openxlsx)

# Copy file from downloads -----------------------------------------------
# Define the paths
download_folder    <- "C:/Users/ifyou/Downloads"
destination_folder <- "grades/"

# Define the file pattern
file_pattern <- "_Grades-MPA_ID_Math_Camp_2024.csv"

# Get the list of files in the Downloads folder
files_in_download <- list.files(download_folder, pattern = file_pattern, full.names = TRUE)

# Check if the file exists and copy it
if (length(files_in_download) > 0) {
  file_to_copy <- files_in_download[1]  # Assuming there's only one matching file
  copy_success <- file.copy(from = file_to_copy, to = destination_folder)
  
  # If the copy was successful, delete the file from the Downloads folder
  if (copy_success) {
    file.remove(file_to_copy)
  }
}

# Get the list of files in the destination folder matching the pattern
files_in_destination <- list.files(destination_folder, pattern = file_pattern, full.names = TRUE)
most_recent_file <- files_in_destination[which.max(file.info(files_in_destination)$mtime)]

# Load data --------------------------------------------------------------

grades <- read_csv(most_recent_file) |>
  janitor::clean_names()

# Get notes --------------------------------------------------------------

tests <- c(
  "notebook_1_828256",
  "notebook_2_828257",
  "notebook_3_828258",
  "notebook_4_828259",
  "capstone_assignment_828254"
)

## Get the correct sum 
notes <- grades |> 
  filter(!student %in% c("Points Possible", "Finney, Carol", "Student, Test", "Olia, Sarah")) |> 
  filter(!is.na(student)) |> 
  select(student, id, sis_user_id, all_of(tests)) |>
  mutate(
    across(
      all_of(tests),
      ~ as.numeric(.x)
    )
  ) |> 
  rowwise(student, id, sis_user_id) |> 
  mutate(sum = sum(c_across(all_of(tests))), .keep = "used") |> 
  ungroup()

# Get the names of students with missing assignments
# Calculate the number of assignments missed by each student
students_missed_assignments <- notes %>%
  mutate(
    missed_count = 5 - (notebook_1_828256 + notebook_2_828257 + notebook_3_828258 + notebook_4_828259 + capstone_assignment_828254)
  )

# Create a list to store students who missed a specific number of assignments
missed_assignments_list <- map(0:5, ~ {
  students_missed_assignments %>%
    filter(missed_count == .x) %>%
    summarize(students = str_c(student, collapse = "; ")) %>%
    pull(students)
  }
)

names(missed_assignments_list) <- paste0("missed_", 0:5, "_assignments")

# Print the list to check the result
missed_assignments_list

# Export to excel
notes %>%
  mutate(
    status = case_when(
      sum %in% c(2,3,4,5) ~ "Completed two or more assignments",
      sum == 1 ~ "At least one assignment",
      sum == 0 ~ "No assignments completed"
    )
  ) %>%
  write.xlsx("grades/completion_status_math_camp.xlsx")

# Check people that didn't submit 1 but 2 --------------------------------

sub1 <- notes |>
  filter(notebook_1_828256 == 0) |> 
  select(student, id)

sub2 <- notes |> 
  filter(notebook_2_828257 == 0) |> 
  select(student, id)

anti_join(sub1, sub2)


# Completion table -------------------------------------------------------

completion_pct <- notes |> 
  summarize(
    across(
      starts_with("notebook") | starts_with("capstone"), 
      ~ sum(.x) / n() * 100, .names = "pct_{col}")
  )

completion_pct

# Figures ----------------------------------------------------------------

# Count the number of students with each number of missed assignments
missed_assignments_count <- students_missed_assignments |> 
  group_by(missed_count) |> 
  summarize(num_students = n()) 

# Plot the data using ggplot2
missed_assignments_count |> 
  filter(missed_count != 0) |> 
  ggplot(
    aes(
      x = missed_count, 
      y = num_students
    )
  ) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
    title = "Number of Students with Missing Assignments",
    x = "Number of Missing Assignments",
    y = "Number of Students"
  ) +
  scale_x_continuous(breaks = 0:5) +
  scale_y_continuous(breaks = seq(0,14,2)) + 
  hrbrthemes::theme_ipsum_rc() 

ggsave(
  "figs/Fig-missing-assignments.png", 
  dpi = 320, scale = 0.8, height = 20, width = 30, units = "cm", bg = "white"
)
