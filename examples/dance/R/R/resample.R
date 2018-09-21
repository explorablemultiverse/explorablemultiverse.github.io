rm(list=ls())
source("R/Figure_4.R")
source("R/Figure_5_Table_2.R")
source("R/Figure_6.R")
source("R/Figure_7.R")
source("R/generate-rankdata.R")

# make deterministic
set.seed(0)

# cleanup output dir
unlink("../figures/*")

# 1 -- re-create the original figures

createFigure4("data/master.csv", "../figures/Figure_4.jpg")
createFigure5("data/master.csv", "../figures/Figure_5.jpg")
createFigure6("data/master.csv", "../figures/Figure_6.jpg")
createFigure7("data/master.csv", "../figures/Figure_7.jpg")

# load the original dataset to modify it
data_original <- read.csv("data/master.csv", header = T)

# 2 -- verify that participant is not used in the model (and thus that shuffling participants has no effect on the result)

data <- data.frame(data_original)
data$participant <- sample(data$participant)
write.csv(data, "data/master-alt.csv", row.names = F)
createFigure4("data/master-alt.csv", "../figures/Figure_4-participants-shuffled.jpg")
createFigure5("data/master-alt.csv", "../figures/Figure_5-participants-shuffled.jpg")
createFigure6("data/master-alt.csv", "../figures/Figure_6-participants-shuffled.jpg")
createFigure7("data/master-alt.csv", "../figures/Figure_7-participants-shuffled.jpg")

# 3 -- resample with replacement (bootstrapping)

# list unique conditions
data_original$condition <- paste(data_original$visandsign, data_original$rbase, data_original$approach, sep="-")
conditions <- unique(data_original$condition)
cat("There is a total of ", length(conditions), " conditions in the dataset.\n", sep="")
cat("(according to the experiment design, there should be ", 9*2*6*2, " conditions)\n", sep="")

# compute and report sample size statistics
sample_sizes <- NULL
for (cond in conditions) {
  observations <- data_original[data_original$condition == cond,]$jnd
  sample_sizes = c(sample_sizes, length(observations))
}
cat("Sample size per condition ranges from ", min(sample_sizes), " to ", max(sample_sizes), " (median ", median(sample_sizes), ")\n", sep="")

# resample with replacement (bootstrap) R times
R <- 100 # number of replicates
for (r in 1:R) {
  cat("Generating figures for replication ", r, " out of ", R, "...\n", sep="")
  data <- data.frame(data_original)
  for (cond in conditions) {
    observations <- data[data$condition == cond,]$jnd
    observations <- sample(observations, replace = T)
    data[data$condition == cond,]$jnd <- observations
  }
  write.csv(data, "data/master-alt.csv", row.names = F)
  # Create figures
  jpgfilename <- paste("../figures/Figure_4-", r, ".jpg", sep="")
  createFigure4("data/master-alt.csv", jpgfilename)
  jpgfilename <- paste("../figures/Figure_5-", r, ".jpg", sep="")
  createFigure5("data/master-alt.csv", jpgfilename)
  jpgfilename <- paste("../figures/Figure_6-", r, ".jpg", sep="")
  createFigure6("data/master-alt.csv", jpgfilename)
  jpgfilename <- paste("../figures/Figure_7-", r, ".jpg", sep="")
  createFigure7("data/master-alt.csv", jpgfilename)
}
