#
# Read and analyze likert-type data from the "blinded with science" study.
#
# Anonymous Capybara, June 2018.
#

rm(list=ls())
library(tidyverse)
source("helpers/stat-helpers.R") # custom stat functions

# The study has 4 experiments. In each experiment, each participant is either shown
# a text about a hypothetical drug ("no_graph" condition) or the same text with a chart
# ("graph" condition). The participant is then asked to assess to what extent the
# drug is effective, on a scale from 1 to 7. We focus on this data here.
# The study has another dependent variable that's more important (error of the response
# to a comprehension question) but it's not Likert-type data.

# Read the Likert-type data from the 4 experiments.
data <- read_csv("blinded.csv")
print(data)

# For each experiment, compute the difference between the two means and its 95% bootstrap CI.
diff_means <- data %>%
  group_by(experiment) %>%
  do(diff_means_bootstrap(
    filter(., condition == "no_graph")$effectiveness,
    filter(., condition == "graph")$effectiveness))
print(diff_means)

# For each experiment, compute the difference between the two means, its 95% t-based CI and the p-value.
diff_means <- data %>%
  group_by(experiment) %>%
  do(diff_means_t(
    filter(., condition == "no_graph")$effectiveness,
    filter(., condition == "graph")$effectiveness))
print(diff_means)
