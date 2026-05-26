# API-209 Summer 2026 R Assignments

Welcome. This Posit Cloud project contains the five summer R assignments for
API-209. The assignments build R fluency before Math Camp using one cleaned
country-year development dataset across all five parts.

This is the student Posit Cloud branch: `posit-cloud-2026`.

## What to Open

Open the `assignments` folder and work through the files in order:

1. `R Summer Assignment 1.Rmd`
2. `R Summer Assignment 2.Rmd`
3. `R Summer Assignment 3.Rmd`
4. `R Summer Assignment 4.Rmd`
5. `R Summer Assignment 5.Rmd`

Write your code and short written answers directly in each `.Rmd` file.

## Project Structure

```text
summer-assignments/
  assignments/
    R Summer Assignment 1.Rmd
    R Summer Assignment 2.Rmd
    R Summer Assignment 3.Rmd
    R Summer Assignment 4.Rmd
    R Summer Assignment 5.Rmd
  data/
    development_indicators_2026.csv
    development_indicators_dictionary_2026.csv
  scripts/
    refresh_development_indicators.R
```

The assignments read the shared CSV snapshot from `../data/`. Do not move the
assignment files or the data folder; the file paths depend on this structure.

## Data

The data file is a pre-cleaned country-year snapshot created for this course.
Most indicators come from the World Bank World Development Indicators.
Governance indicators come from the Worldwide Governance Indicators. The data
dictionary explains each variable's source, unit, and interpretation notes.

## How to Work

- Save your own copy of this Posit Cloud project before editing.
- Knit early and often. Knitting runs the assignment from top to bottom in a
  fresh R session, which checks that your work is reproducible.
- Submit the completed `.Rmd` file for each part using the course submission
  instructions.
- If you use AI for debugging, run and check any suggested code yourself. You
  are responsible for every line of code and every interpretation you submit.
