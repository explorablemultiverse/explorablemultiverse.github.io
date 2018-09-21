#' # Analysis script for experiment 2 of the CHI 2018 article:
#' # How Relevant are Incidental Power Poses for HCI?
#' 
#' 2015-18 Anonymous Octopus
#' 
#' Created August 2016, cleaned up March 2018

set.seed(0)
setwd("analysis-exp2")

#' Our analysis of the experiment 2 data is based on R code provided by John K. Kruschke for his book "Doing Bayesian Data Analysis." We reused the code to do a robut analysis of metric data with one factorial predictor (page 573ff). 
source("Jags-Ymet-Xnom1fac-MrobustHet.R")

#' Loading our data...
d <- read.csv("../data/exp2.csv")




#' ## Analysing the standard BART measure (average adjusted number of pumps)
#' The standard BART measure is the average adjusted number of pumps which refers to the average number of pumps for balloons which did not explode. For these balloons the participant made the conscious decision to take no further risk and to secure the points for the pumps made.
#' 


#' We call the function that analyzes the data using a robust hierarchical model.
origMeasure <-  genMCMC(d, "orig", "condition", numSavedSteps = 50000, thinSteps = 10, saveName = "output/orig-")

#' We first check that our Markov chains converged and that they are healthy and reliable.
diagOrig <- diagMCMC(origMeasure, parName = "b0", saveName = "output/orig-diag-b0-")
diagOrig <- diagMCMC(origMeasure, parName = "b[1]", saveName = "output/orig-diag-b[1]-")
diagOrig <- diagMCMC(origMeasure, parName = "b[2]", saveName = "output/orig-diag-b[2]-")


#' The code from the book comes with an additional function that prints a summary table with the numerical results of the analysis. 
contrasts = list( 
  list( c("expansive") , c("constrictive") , compVal=0.0  ))
origMeasureSmry <- smryMCMC(origMeasure, d, "condition", contrasts = contrasts, saveName = "output/orig-")



#' To visualize the results of our analysis we first need to extract data from the coda samples object.


require(coda)
require(plyr)
require(ggplot2)
require(tidybayes)


#' We define a custom function that extracts samples for a list of parameters from a coda object and returns a list of data frames suitable for plotting with ggplot.

### for 2 groups

getParamOfInterest2group <- function(codaSamples, params, whichChain = 1){
  mydf  <- data.frame(parameter = character(), value = list())
  firstChain  <- as.matrix(codaSamples[[whichChain]])

  for (param in params) {
    mydf  <-  rbind(data.frame(parameter = param, value = firstChain[,param]), mydf)
  }
  
  # now the contrast
  
  mydf  <- rbind(data.frame(parameter = "contrast posture", value = firstChain[,"b[2]"] - firstChain[,"b[1]"]), mydf)
  
  # we rename the parameters to achieve proper labeling when plotting
  mydf$parameter  <- revalue(mydf$parameter, 
                             c("b0"="intercept", 
                               "b[1]"="constrictive - intercept",
                               "b[2]"="expansive - intercept",
                               "m[1]"="estimate constrictive",
                               "m[2]"="estimate expansive"))
  
  
  return(mydf)
}


#' Now we are almost ready to plot. We define a custom function that calls the geom_eyeh layer function from the tidybayes package with consistent drawing parameters across different visualizations.
require(extrafont)
loadfonts()

makeEyePlot <- function(data, measureName, ylimLow = -40, yLimHigh = 60, ticks=NULL, yscaleOrder = NULL, expandYLimits = c(0.1, 0.1)) {
  p  <- ggplot(data=data, mapping = aes(y=parameter, x=value)) + geom_eyeh(fill = "skyblue", .prob = 0.95, size = 1)
  p1  <-  p   + theme_bw() + ylab(measureName) + scale_x_continuous(breaks=ticks) + coord_cartesian(xlim = c(ylimLow, yLimHigh)) + scale_y_discrete(limits = yscaleOrder, expand = expandYLimits) +
    theme(text = element_text(size = 10, family = "Open Sans"),
          axis.title.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = unit(c(5.5, 5.5, 5.5, 0), "points"))

  
  return (p1) 
}

############################

paramsToExtract <- c("b0", "b[1]", "b[2]", "m[1]", "m[2]")

origMeasureParameters <- getParamOfInterest2group(origMeasure, params = paramsToExtract)
#'For figure 9 we need to divide the data frame to get 3 separate plots: (1) intercept, means for constrictive and expansive, (2) differences from the intercept for both body postures, and (3) the contrast, the difference between the two postures.

df.originalMeasurePlot1 <- dplyr::filter(origMeasureParameters, parameter %in% c("intercept", "estimate constrictive", "estimate expansive"))

df.originalMeasurePlot2 <- dplyr::filter(origMeasureParameters, parameter %in% c("constrictive - intercept", "expansive - intercept"))

df.originalMeasurePlot3 <- dplyr::filter(origMeasureParameters, parameter %in% c("contrast posture"))


origMeasurePlot1 <- makeEyePlot(df.originalMeasurePlot1, " ", ylimLow = 0, yLimHigh = 60, ticks = c(0, 20, 40, 60))
origMeasurePlot2 <- makeEyePlot(df.originalMeasurePlot2, " ", ylimLow = -14, yLimHigh = 14, ticks = c(-10, 0, 10))
origMeasurePlot3 <- makeEyePlot(df.originalMeasurePlot3, " ", ylimLow = -14, yLimHigh = 14, ticks = c(-10, 0, 10))

#' Now we compile the three plots into one. Doing this with directly  with the cowplot package enables us to minimize the required retouching in a vector drawing application.

library(cowplot)
origPlot <- ggdraw() +
  draw_plot(origMeasurePlot3, x = 0.763, y = 0.2, width = 0.24, height = 0.32) +
  draw_plot(origMeasurePlot2, x = 0.555, y = 0.04, width = 0.24, height = 0.63) +
  draw_plot(origMeasurePlot1, x = 0.13, y = 0.04, width = 0.455, height = 0.9) +
  draw_label("original BART measure (adjusted number of pumps)", x = 0.5, y = 0.99, vjust = 1, fontfamily = "Open Sans", fontface = "bold", size = 12) +
  draw_label("adjusted number of pumps", x = 0.35, y = 0.02, vjust = 0, fontfamily = "Open Sans", size = 9) +
  draw_label("intercept", x = 0.15, y = 0.78, hjust = 1, size = 9) +
  draw_label("constrictive\nbody posture", x = 0.15, y = 0.53, hjust = 1, size = 9) +
  draw_label("expansive\nbody posture", x = 0.15, y = 0.27, hjust = 1, size = 9) +
  draw_label("differences from\nintercept", x = 0.77, y = 0.82, hjust = 1, size = 9) +
  draw_label("difference\nbetween postures", x = 0.9, y = 0.6, hjust = 0.5, size = 9) +
  draw_line(x = c(0.575, 0.685, 0.685), y = c(0.795, 0.795, 0.645), size = 0.3)
  

combinedPlotMain <- ggsave("charts/exp2/Fig9-left.pdf", origPlot, width = 6, height = 3)




#' ## Analyzing the Change Measure

#' As with the original BART measure, we call the function 
changeMeasure <-  genMCMC(d, "change", "condition", numSavedSteps = 50000, thinSteps = 10, saveName = "change-")

#' Then we check that our Markov chains converged and that they are healthy and reliable.
diagMCMC(changeMeasure, parName = "b0", saveName = "analysis-exp2/output/change-diag-b0-")
diagMCMC(changeMeasure, parName = "b[1]", saveName = "analysis-exp2/output/change-diag-b[1]-")
diagMCMC(changeMeasure, parName = "b[2]", saveName = "analysis-exp2/output/change-diag-b[2]-")


changeMeasureSmry <- smryMCMC(changeMeasure, d, "condition", contrasts = contrasts, saveName = "output/change-")



#' Now we extract the parameters of interest from the Markov chain object into a dataframe
changeMeasureParameters <- getParamOfInterest2group(changeMeasure, params = paramsToExtract)

#' and create again three different data frames to create the visualization for Figure 9 right.

df.changeMeasurePlot1 <- dplyr::filter(changeMeasureParameters, parameter %in% c("intercept", "estimate constrictive", "estimate expansive"))

df.changeMeasurePlot2 <- dplyr::filter(changeMeasureParameters, parameter %in% c("constrictive - intercept", "expansive - intercept"))

df.changeMeasurePlot3 <- dplyr::filter(changeMeasureParameters, parameter %in% c("contrast posture"))


changeMeasurePlot1 <- makeEyePlot(df.changeMeasurePlot1, " ", ylimLow = -10, yLimHigh = 60, ticks = c(0, 20, 40, 60))
changeMeasurePlot2 <- makeEyePlot(df.changeMeasurePlot2, " ", ylimLow = -30, yLimHigh = 30, ticks = c(-20, 0, 20))
changeMeasurePlot3 <- makeEyePlot(df.changeMeasurePlot3, " ", ylimLow = -30, yLimHigh = 30, ticks = c(-20, 0, 20))

#' Now we compile the three plots into one. Doing this with directly  with the cowplot package enables us to minimize the required retouching in a vector drawing application.

changePlot <- ggdraw() +
  draw_plot(changeMeasurePlot3, x = 0.7, y = 0.2, width = 0.3, height = 0.32) +
  draw_plot(changeMeasurePlot2, x = 0.435, y = 0.04, width = 0.3, height = 0.63) +
  draw_plot(changeMeasurePlot1, x = 0.12, y = 0.04, width = 0.35, height = 0.9) +
  draw_label("percent change over the course of the task", x = 0.5, y = 0.99, vjust = 1, fontfamily = "Open Sans", fontface = "bold", size = 12) +
  draw_label("percent change", x = 0.32, y = 0.02, vjust = 0, fontfamily = "Open Sans", size = 9) +
  draw_label("intercept", x = 0.15, y = 0.78, hjust = 1, size = 9) +
  draw_label("constrictive\nbody posture", x = 0.15, y = 0.53, hjust = 1, size = 9) +
  draw_label("expansive\nbody posture", x = 0.15, y = 0.27, hjust = 1, size = 9) +
  draw_label("differences from\nintercept", x = 0.69, y = 0.82, hjust = 1, size = 9) +
  draw_label("difference\nbetween postures", x = 0.86, y = 0.6, hjust = 0.5, size = 9) +
  draw_line(x = c(0.457, 0.598, 0.598), y = c(0.795, 0.795, 0.645), size = 0.3)


#' Finally we save the plot as a pdf file.
combinedPlotMain <- ggsave("charts/exp2/Fig9-right.pdf", changePlot, width = 6, height = 3)













#' ## Part 2: Two-Factor Analysis with covariate "impulsiveness"
#' The final analysis of the article considers a covariate, "impulsiveness", measured through the BIS-11 scale. The original BART article reported a correlation with this measure, and we thus included it in our study and analyze 

source("Jags-Ymet-Xnom2fac-MrobustHet.R")

change2factor <-  genMCMC(d, "change", "condition", "BIS", numSavedSteps = 50000, thinSteps = 10,  saveName = "output/change-2factors-", nChains = 3)

#' We again check first that our Markov chains converged and that they are healthy and reliable.
diagMCMC(change2factor, parName = "b0", saveName = "output/orig-diag-b0-")
diagMCMC(change2factor, parName = "b1[1]", saveName = "output/orig-diag-b1[1]-")
diagMCMC(change2factor, parName = "b1[2]", saveName = "output/orig-diag-b1[2]-")
diagMCMC(change2factor, parName = "b2[1]", saveName = "output/orig-diag-b2[1]-")
diagMCMC(change2factor, parName = "b2[2]", saveName = "output/orig-diag-b2[2]-")
diagMCMC(change2factor, parName = "b1b2[1,1]", saveName = "output/orig-diag-b1b1[1,1]-")
diagMCMC(change2factor, parName = "b1b2[1,2]", saveName = "output/orig-diag-b1b1[1,2]-")
diagMCMC(change2factor, parName = "b1b2[2,1]", saveName = "output/orig-diag-b1b1[2,1]-")
diagMCMC(change2factor, parName = "b1b2[2,2]", saveName = "output/orig-diag-b1b1[2,2]-")

contrastsCondition <- list(
  list(c("expansive"), c("constrictive")))

contrastsBIS <- list(
  list(c("high"), c("low")))

contrastsConditionBIS <- list(
  list(
    list(c("expansive"), c("constrictive")),
    list(c("high"), c("low")))
)

change2factorSmry <- smryMCMC(change2factor, d, "condition", "BIS", contrastsCondition, contrastsBIS, contrastsConditionBIS, saveName = "output/change-2factor-")




#'Now we extract the samples for the parameters we want to visualize into data frame.
whichChain <- 1
change2factorParameters  <- data.frame(parameter = character(), value = list())
  firstChain  <- as.matrix(change2factor[[whichChain]])
  
  change2factorParameters  <-  rbind(data.frame(parameter = "intercept", value = firstChain[,"b0"]), change2factorParameters)

  change2factorParameters <- rbind(data.frame(parameter = "expansive", value = firstChain[,"b1[2]"]), change2factorParameters)
  
  change2factorParameters <- rbind(data.frame(parameter = "constrictive", value = firstChain[,"b1[1]"]), change2factorParameters)

  change2factorParameters <- rbind(data.frame(parameter = "high", value = firstChain[,"b2[1]"]), change2factorParameters)

  change2factorParameters <- rbind(data.frame(parameter = "low", value = firstChain[,"b2[2]"]), change2factorParameters)

  change2factorParameters <- rbind(data.frame(parameter = "expansive high", value = firstChain[,"b1b2[2,1]"]), change2factorParameters)

  change2factorParameters <- rbind(data.frame(parameter = "expansive low", value = firstChain[,"b1b2[2,2]"]), change2factorParameters)

  change2factorParameters <- rbind(data.frame(parameter = "constrictive high", value = firstChain[,"b1b2[1,1]"]), change2factorParameters)

  change2factorParameters <- rbind(data.frame(parameter = "constrictive low", value = firstChain[,"b1b2[1,2]"]), change2factorParameters)
  




#' and create again different data frames to create the visualization for Figure 11.

df.change2factorPlot1 <- dplyr::filter(change2factorParameters, parameter %in% c("intercept"))

df.change2factorPlot2 <- dplyr::filter(change2factorParameters, parameter %in% c("expansive", "constrictive"))

df.change2factorPlot3 <- dplyr::filter(change2factorParameters, parameter %in% c("high", "low"))

df.change2factorPlot4 <- dplyr::filter(change2factorParameters, parameter %in% c("constrictive high", "constrictive low", "expansive high", "expansive low"))

#' Figure 11 contains 4 separate plots, which we create first independently
change2factorPlot1 <- makeEyePlot(df.change2factorPlot1, " ", ylimLow = 0, yLimHigh = 55, ticks = c(0, 20, 40), expandYLimits = c(1.5, 0.1))
change2factorPlot2 <- makeEyePlot(df.change2factorPlot2, " ", ylimLow = -20, yLimHigh = 20, ticks = c(-20, 0, 20), yscaleOrder = c("constrictive", "expansive"), expandYLimits = c(0.1, 1.2))
change2factorPlot3 <- makeEyePlot(df.change2factorPlot3, " ", ylimLow = -20, yLimHigh = 20, ticks = c(-20, 0, 20), yscaleOrder = c("low", "high"), expandYLimits = c(0.1, 1.2))
change2factorPlot4 <- makeEyePlot(df.change2factorPlot4, " ", ylimLow = -25, yLimHigh = 25, ticks = c(-20, 0, 20) , yscaleOrder = c("constrictive high", "expansive high", "constrictive low", "expansive low"))

#' and then compile into one plot. Doing this with directly  with the cowplot package enables us to minimize the required retouching in a vector drawing application.

change2factorPlot <- ggdraw() +
  draw_plot(change2factorPlot4, x = 0.78, y = 0.3, width = 0.22, height = 0.6) +
  draw_plot(change2factorPlot3, x = 0.53, y = 0.3, width = 0.18, height = 0.6) +
  draw_plot(change2factorPlot2, x = 0.32, y = 0.3, width = 0.18, height = 0.6) +
  draw_plot(change2factorPlot1, x = 0, y = 0.3, width = 0.25, height = 0.6) +
  draw_label("intercept", x = 0.13, y = 0.99, vjust = 1, fontfamily = "Open Sans", size = 11) +
  draw_label("+", x = 0.3, y = 0.99, vjust = 1, fontfamily = "Open Sans", size = 11) +
  draw_label("body posture", x = 0.42, y = 0.99, vjust = 1, fontfamily = "Open Sans", size = 11) +
  draw_label("+", x = 0.525, y = 0.99, vjust = 1, fontfamily = "Open Sans", size = 11) +
  draw_label("impulsiveness", x = 0.63, y = 0.99, vjust = 1, fontfamily = "Open Sans", size = 11) +
  draw_label("+", x = 0.75, y = 0.99, vjust = 1, fontfamily = "Open Sans", size = 11) +
  draw_label("body posture x impulsiveness", x = 0.99, y = 0.99, vjust = 1, hjust = 1, fontfamily = "Open Sans", size = 11) +
  draw_label("percent change", x = 0.13, y = 0.29, fontfamily = "Open Sans", size = 9) +
  draw_label("percent change", x = 0.42, y = 0.29, fontfamily = "Open Sans", size = 9) +
  draw_label("percent change", x = 0.63, y = 0.29, fontfamily = "Open Sans", size = 9) +
  draw_label("percent change", x = 0.9, y = 0.29, fontfamily = "Open Sans", size = 9) +
  draw_label("expansive", x = 0.333, y = 0.72, hjust = 1, size = 9) +
  draw_label("constrictive", x = 0.333, y = 0.56, hjust = 1, size = 9) +
  draw_label("high", x = 0.542, y = 0.72, hjust = 1, size = 9) +
  draw_label("low", x = 0.542, y = 0.56, hjust = 1, size = 9) +
  draw_label("constrictive high", x = 0.795, y = 0.79, hjust = 1, size = 9) +
  draw_label("expansive high", x = 0.795, y = 0.69, hjust = 1, size = 9) +
  draw_label("constrictive low", x = 0.795, y = 0.59, hjust = 1, size = 9) +
  draw_label("expansive low", x = 0.795, y = 0.49, hjust = 1, size = 9) +
  draw_label("The intercept for the two factors\ncombined is 25.2% [15,2, 35.7].", x = 0.03, y = 0.2, vjust = 1, hjust = 0, size = 9) +
  draw_label("Body posture accounts for some of the\nuncertainty but similarly for both conditions.", x = 0.26, y = 0.2, hjust = 0, vjust = 1, size = 9) +
  draw_label("For high impulsiveness indices positive\nvalues are slightly more credible\nthan negative values and vice versa.", x = 0.5, y = 0.2, hjust = 0, vjust = 1, size = 9) +
  draw_label("It seems most credible that the interaction parameters\ncrossing body posture and impulsiveness account for\nmost of the observed differences.", x = 0.715, y = 0.2, hjust = 0, vjust = 1, size = 9) 



  change2factorPlot
#' Finally we save the plot as a pdf file.
combinedPlotMain <- ggsave("../charts/exp2/Fig11.pdf", change2factorPlot, width = 12, height = 3)


