#
# Merges the four csv files "blinded1.csv", ..., "blinded4.csv" that can be found in the
# study's supplemental material into a single csv file that contains sufficient
# information for analysis.R. If the file blinded.csv exists, you don't have to run this
# code.
#
# Anonymous Capybara, June 2018
#

rm(list=ls())
library(tidyverse)

# Read data from the 4 experiments and merge it into a single tibble.

data1 <- read_csv("helpers/blinded1.csv") %>%
  select(condition, effectiveness) %>%
  add_column(experiment = as.integer(1), .before = 1)

data2 <- read_csv("helpers/blinded2.csv") %>%
  select(condition, effectiveness = believe_in_effectiveness) %>%
  add_column(experiment = as.integer(2), .before = 1)

data3 <- read_csv("helpers/blinded3.csv") %>%
  select(condition, effectiveness = believe_in_effectiveness) %>%
  add_column(experiment = as.integer(3), .before = 1)

data4 <- read_csv("helpers/blinded4.csv") %>%
  select(condition, effectiveness = believe_in_effectiveness) %>%
  add_column(experiment = as.integer(4), .before = 1)

data <- bind_rows(data1, data2, data3, data4)

# Write

write_csv(data, "blinded.csv")
