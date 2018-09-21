# R code of Steegen, Tuerlinckx, Gelman & Vanpaemel (2016). Increasing Transparency through a Multiverse Analysis. Perspectives on Psychological Science, 11, 702-712

# This file contains code to perform a multiverse analysis on two datasets from Durante, K.M., Rae A., & Griskevicius, V. (2013). The fluctuating female vote: Politics, religion and the ovulatory cycle. Psychological Science, 24, 1007-1016. 
# As described in Steegen et al (2016), six different multiverse analyses are performed; one on the first dataset (Study 1), and five on the second dataset (Study 2).
# In each multiverse analysis, a data multiverse is created, and all associated p-values are shown in an histogram.
# For four analyses, the p-values are shown in a grid, allowing a closer inspection.

# The raw data are stored in durante_etal_2013_study1.txt and durante_etal_2013_study2.txt.  
# These two files are based on, but not identical to, two excel files Kristina Durante kindly shared with us. 
# The changes as compared to the original files include:
#   We changed the WorkerID, which consisted of a complicated string of letters and numbers in the original data file, to numbers in ascending order, for simplicity
#   We changed the variable name Cycle Length from the original file to Reported Cycle Length, for clarity
#   We made minor changes to some variable names, for clarity 
#   We fixed some coding errors (see supplemental material)
#   We converted data that were due to coding errors we could not fix to NA (see supplemental material)
#   The data files do not contain the raw variables that were included in the survey by Durante et al. (2013), but were not used in the analyses reported by Durante et al. (2013) or by Steegen et al. (2016) (see supplemental material)
#   The data files do not contain the processed data from the Durante et al. (2013) data files that we received 
# Kristina Durante has given us permission to publicly share these data files. 

# leuven, sept 30, 2016

#setwd("C:/Users/u0044384/Dropbox/RESEARCH/multiverse_code") #change to correct working directory

#install.packages("ggplot2")
#install.packages("Rmisc")
library(ggplot2)
library(Rmisc)

rm(list = ls())

annlist <- c(1, 2, 3, 4, 5, 6)  # which analyses? 1 = religiosity (study 1), 2 = religiosity (study 2), 3 = fiscal political attitudes,
                                # 4 = social political attitudes, 5 = voting preferences, 6 = donation preferences  
deplist <- c("RelComp","RelComp","FiscConsComp","SocConsComp","Vote","Donate")  # list of dependent variables
all.data.multiverses <- list()  # all data multiverses for all the analyses specified in annlist
all.p <- list()  # all p-values for all the analyses specified in annlist

##############################################################################################
######################## compute multiverse of statistical results ###########################
##############################################################################################

for (iii in 1:length(annlist)) {  # for each analysis
  
  rm(list = setdiff(ls(), c("annlist", "deplist", "all.p", "all.data.multiverses", 
                            "iii")))
  
  ann <- annlist[iii] #create analysis identifier
  
  ###### read in raw data
  
  if (ann == 1) {
    mydata.raw <- read.csv2("durante_etal_2013_study1.txt", sep = "") #read in raw data from Study 1
  }
  if (ann > 1) {
    mydata.raw <- read.csv2("durante_etal_2013_study2.txt", sep = "", na.strings = "NA") #read in raw data from Study 2
    mydata.raw$StartDateNext <- as.Date(mydata.raw$StartDateNext, format = "%m/%d/%y")
  }
  mydata.raw$DateTesting <- as.Date(mydata.raw$DateTesting, format = "%m/%d/%y")
  mydata.raw$StartDateofLastPeriod <- as.Date(mydata.raw$StartDateofLastPeriod, format = "%m/%d/%y")
  mydata.raw$StartDateofPeriodBeforeLast <- as.Date(mydata.raw$StartDateofPeriodBeforeLast, 
                                                    format = "%m/%d/%y")
    
  ###### process raw data to create multiverse of data sets
  
  no.nmo <- ifelse(ann == 1, 2, 3)  # number of next menstrual onset assessment (nmo) processing choices
  no.f <- 5  # number of fertility (f) assessment processing choices
  no.r <- 3  # number of relationship status(r)  assessment processing choices
  no.ecl <- 3  # number of exclusion based on cycle length (ecl) processing choices
  no.ec <- 2  # number of exclusion based on certainty ratings (ec) processing choices
  
  data.multiverse <- array(list(), dim = c(no.nmo, no.f, no.r, no.ecl, no.ec))  # multiverse of data sets
  p.multiverse <- array(0, dim = c(no.nmo, no.f, no.r, no.ecl, no.ec))  # multiverse of p values
  
    # data processing with a single option
  
  mydata.proc <- mydata.raw
  
      # create religiosity score
  mydata.proc$RelComp <- round(rowMeans(cbind(mydata.proc$Rel1, mydata.proc$Rel2, 
                                                   mydata.proc$Rel3), na.rm = TRUE), digits = 2)  
  
      # create fiscal and social political attitudes score
    if (ann > 1) {
    mydata.proc$Abortion <- abs(7 - mydata.proc$Abortion) + 1
    mydata.proc$StemCell <- abs(7 - mydata.proc$StemCell) + 1
    mydata.proc$Marijuana <- abs(7 - mydata.proc$Marijuana) + 1
    mydata.proc$RichTax <- abs(7 - mydata.proc$RichTax) + 1
    mydata.proc$StLiving <- abs(7 - mydata.proc$StLiving) + 1
    mydata.proc$Profit <- abs(7 - mydata.proc$Profit) + 1
    mydata.proc$FiscConsComp <- round(rowMeans(cbind(mydata.proc$FreeMarket, 
                                                          mydata.proc$PrivSocialSec, mydata.proc$RichTax, mydata.proc$StLiving, 
                                                          mydata.proc$Profit), na.rm = TRUE), digits = 2)  # create fiscal political attitudes score
    mydata.proc$SocConsComp <- round(rowMeans(cbind(mydata.proc$Marriage, 
                                                         mydata.proc$RestrictAbortion, mydata.proc$Abortion, mydata.proc$StemCell, 
                                                         mydata.proc$Marijuana), na.rm = TRUE), digits = 2)  # create social political attitudes score
  }
  
  mydata.proc.init <- mydata.proc
  
  # data processing with multiple options (see Table 1, p. 705)
  
  for (i in 1:no.nmo){  # for each nmo option
    for (j in 1:no.f){  # for each f option
      for (k in 1:no.r){  # for each r option
        for (l in 1:no.ecl){  # for each ecl option
          for (m in 1:no.ec){  # for each ec option
                                      
            mydata.proc <- mydata.proc.init #initialize processed data with variables with a single option only
            
            # next menstrual onset (nmo) assessment
            mydata.proc$ComputedCycleLength <- mydata.proc$StartDateofLastPeriod - 
              mydata.proc$StartDateofPeriodBeforeLast  # compute cycle length
            if (i == 1) {
              mydata.proc$NextMenstrualOnset <- mydata.proc$StartDateofLastPeriod + 
                mydata.proc$ComputedCycleLength  # first nmo option: based on computed cycle length
            } else if (i == 2) {
              mydata.proc$NextMenstrualOnset <- mydata.proc$StartDateofLastPeriod + 
                mydata.proc$ReportedCycleLength  # second nmo option: based on reported cycle length 
            } else if (i == 3) {
              mydata.proc$NextMenstrualOnset <- mydata.proc$StartDateNext  # third nmo option: based on reported estimate of next menstrual onset  
            }
            mydata.proc$DaysBeforeNextOnset <- mydata.proc$NextMenstrualOnset - mydata.proc$DateTesting  # compute days before next menstrual onset
            mydata.proc$CycleDay <- 28 - mydata.proc$DaysBeforeNextOnset  # compute cycle day
            mydata.proc$CycleDay <- ifelse(mydata.proc$CycleDay <1, 1, mydata.proc$CycleDay)
            mydata.proc$CycleDay <- ifelse(mydata.proc$CycleDay > 28, 28, mydata.proc$CycleDay)
            
            # as described in the Supplemental Material, for two participants, we did not manage to recover the value of Cycle Day. When Cycle Day is determined based on nmo1, we
            # adopt the Cycle Day value from the original data file to ensure that the results of our single data set analysis are identical 
            # to the single data set analysis in Durante et al. (2013)
                        if (ann > 1 & i == 1) {
              mydata.proc$CycleDay[mydata.proc$WorkerID == 15] <- 11
              mydata.proc$CycleDay[mydata.proc$WorkerID == 16] <- 18  
            }
            
            # fertility assessment
            high.lower <- c(7, 6, 9, 8, 9)  # lower boundaries of different options for 'high fertility' cycle days
            high.upper <- c(14, 14, 17, 14, 17)  # upper boundaries of different options for 'high fertility' cycle days
            low1.lower <- c(17, 17, 18, 1, 1)  # lower boundaries of different options for 'low fertility' cycle days (first range)
            low1.upper <- c(25, 27, 25, 7, 8)  # upper boundaries of different options for 'low fertility' cycle days (first range)
            low2.lower <- c(17, 17, 18, 15, 18)  # lower boundaries of different options for 'low fertility' cycle days (second range) 
            low2.upper <- c(25, 27, 25, 28, 28)  # upper boundaries of different options for 'low fertility' cycle days (second range)
            
            mydata.proc$Fertility <- rep(NA, dim(mydata.proc)[1])  # create fertility variable
            mydata.proc$Fertility[mydata.proc$CycleDay >= high.lower[j] & mydata.proc$CycleDay <= 
                                          high.upper[j]] <- "High"  # assign 'High' to fertility if cycle day is within the high fertility range 
            mydata.proc$Fertility[mydata.proc$CycleDay >= low1.lower[j] & mydata.proc$CycleDay <= 
                                          low1.upper[j]] <- "Low"  # assign 'Low' to fertility if cycle day is within the first low fertility range
            mydata.proc$Fertility[mydata.proc$CycleDay >= low2.lower[j] & mydata.proc$CycleDay <= 
                                          low2.upper[j]] <- "Low"  # assign 'Low' to fertility if cycle day is within the second low fertility range
                
            # relationship status assessment
            if (k == 1) {
              mydata.proc$RelationshipStatus <- ifelse(mydata.proc$Relationship <= 
                                                               2, "Single", "Relationship")  # first r option: single = response options 1 and 2; relationship = response options 3 and 4
            } else if (k == 2) {
              mydata.proc$RelationshipStatus <- ifelse(mydata.proc$Relationship == 
                                                               1, "Single", "Relationship")  # second r option: single = response option 1, relationship = response options 2, 3 and 4
            } else if (k == 3) {
              mydata.proc$RelationshipStatus[mydata.proc$Relationship == 1] <- "Single"
              mydata.proc$RelationshipStatus[mydata.proc$Relationship > 2 & mydata.proc$Relationship < 
                                                     5] <- "Relationship"  # third r option: single = response option 1, relationship = response options 3 and 4
            }
                      
            # exclusion based on cycle length
            if (l == 1) {
              mydata.proc <- mydata.proc  # first ecl option: no exclusion based on cycle length
            } else if (l == 2) {
              mydata.proc <- mydata.proc[!(mydata.proc$ComputedCycleLength < 
                                                         25 | mydata.proc$ComputedCycleLength > 35), ]  # second ecl option: exclusion based on computed cycle length
            } else if (l == 3) {
              mydata.proc <- mydata.proc[!(mydata.proc$ReportedCycleLength < 
                                                         25 | mydata.proc$ReportedCycleLength > 35), ]  # third ecl option: exclusion based on reported cycle length
            }
                  
            # exclusion based on certainty ratings
            if (m == 1) {
              mydata.proc <- mydata.proc  # first ec option: no exclusion based on certainty ratings
            } else if (m == 2) {
              mydata.proc <- mydata.proc[!(mydata.proc$Sure1 < 6 | mydata.proc$Sure2 < 
                                                         6), ]  # second ec option: exclusion based on variables Sure1 and Sure2
            }
            
            data.multiverse[[i, j, k, l, m]] = mydata.proc  # store processed data set in the data multiverse
          }
        }
      }
    }
  }                     
            
  ###### analyze multiverse of data sets to create multiverse of statistical results
  
  for (i in 1:no.nmo){  # for each nmo option
    for (j in 1:no.f){  # for each f option
      for (k in 1:no.r){  # for each r option
        for (l in 1:no.ecl){  # for each ecl option
          for (m in 1:no.ec){  # for each ec option
                        
            mydata.proc$Fertility <- factor(mydata.proc$Fertility)
            mydata.proc$RelationshipStatus <- factor(mydata.proc$RelationshipStatus)
            if (ann <= 4) {
              an = lm(paste(deplist[ann], "~Fertility*RelationshipStatus"), data.multiverse[[i, j, k, l, m]])  # for analyses 1 to 4, perform an ANOVA on the processed data set 
            }
            if (ann >= 5) {
              an = glm(paste(deplist[ann], "~Fertility*RelationshipStatus"), family = binomial(link = "logit"), 
                       data.multiverse[[i, j, k, l, m]])  # for analyses 5 and 6, perform a logistic regression on the processed data set
            }
            summar <- summary(an)
            p.multiverse[i, j, k, l, m] <- summar$coefficients[4, 4]  # store the p-value of the fertility x relationship interaction 
            
          }
        }
      }
    }
  }
  
  p.multiverse[1, , , 3, ] <- NA  # when participants are excluded based on reported cycle length, we do not consider cycle day assessment based on computed cycle length 
  p.multiverse[2, , , 2, ] <- NA  # when participants are excluded based on computed cycle length, we do not consider cycle day assessment based on reported cycle length
  
  all.data.multiverses[[iii]] <- data.multiverse
  all.p[[iii]] <- p.multiverse
  
}

# as a check, show the results of the single data set analyses by durante et. al (2013) 
sapply(all.p, "[[", 1)

# compute the proportion of data sets with a significant interaction effect
f <- function (ann) {length(which(all.p[[ann]]>.05))/length(which(!is.na(all.p[[ann]])))}
sapply(annlist,f) 

########################################################
######################## make graphs ###################
########################################################

graphnames <- c("Religiosity (Study 1)", "Religiosity (Study 2)", "Fiscal political attitudes", 
                "Social political attitudes", "Voting preferences", "Donation preferences")

# histograms of p-values
hists <- list()
pv <- list()
ylabs=c("Frequency","","Frequency","","Frequency","")
xlabs=c("","","","","p","p")
for (iii in 1:length(annlist)) local({
  ann <- annlist[iii]
  p <- all.p[[ann]]
  if (ann == 1) {
    cat1 <- rep(c(1:15), 8)
    cat2 <- rep(1:8, each = 15)
  } else {
    cat1 <- rep(c(1:15), 14)
    cat2 <- rep(1:14, each = 15)
  }
  df <- data.frame(category1 = cat1, category2 = cat2, value = (as.vector(p[!is.na(p)])))
  df[["sign"]] = ifelse(df[["value"]] <= 0.05, "significant", "nonsignificant")
  pv[[ann]]=df$value
  hists[[ann]] <<- qplot(pv[[ann]], geom = "histogram", binwidth = 0.01) + xlim(0,1) + geom_histogram(colour = "black", fill = "white", binwidth = 0.01) + 
    xlab(xlabs[[ann]]) + ylab(ylabs[[ann]]) + geom_vline(xintercept = 0.05, colour = "red", 
                                               linetype = "longdash") + ggtitle(graphnames[ann]) + theme(plot.title = element_text(lineheight = 0.8, 
                                                                                                                                   face = "bold")) + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 16))
  dev.new(8, 5)
  print(hists[[ann]])
  rm(p)
  rm(df)
})

# grids of p-values
grids <- list()
for (iii in c(2,4,5,6)){ #in the paper, we only show the grids for analyses 2,4,5, and 6
  ann <- annlist[iii]
  p <- all.p[[ann]]
  p.grid <- array(0,dim=c(no.f, no.r, no.ec, no.ecl, no.nmo))  # change the dimensions of the p multiverse for visualization purposes
    for (jj in 1:3){
    for (jjj in 1:3){
      p.grid[, , , jj, jjj] <- p[jjj, , , jj, ]
    }
  }
  cat1 <- rep(c(1:15), 14)
  cat2 <- rep(1:14, each = 15)
  df <- data.frame(category1 = cat1, category2 = cat2, value = (as.vector
                                                                (p.grid[!is.na(p.grid)])))
  df[["sign"]] = ifelse(df[["value"]] <= 0.05, "significant", "nonsignificant")
  grids[[ann]] <- ggplot(df, aes(x = category1, y = category2, fill = sign)) +
   geom_tile(colour = "black") +
  geom_text(label = round((df$value), 2), size = 3, colour = "black") + 
    # draw relationship branches vertical
    geom_segment(aes(x = 3, y = -1.7, xend = 3, yend = -0.3)) + 
    geom_segment(aes(x = 8, y = -1.7, xend = 8, yend = -0.3)) + 
    geom_segment(aes(x = 13, y = -1.7, xend = 13, yend = -0.3)) + 
    # draw relationship branches horizontal
    geom_segment(aes(x = 3, y = -1.7, xend = 13, yend = -1.7)) + 
    # draw fertility branches vertical
    geom_segment(aes(x = 1, y = -0.3, xend = 1, yend = 0.5)) + 
    geom_segment(aes(x = 2, y = -0.3, xend = 2, yend = 0.5)) + 
    geom_segment(aes(x = 3, y = -0.3, xend = 3, yend = 0.5)) + 
    geom_segment(aes(x = 4, y = -0.3, xend = 4, yend = 0.5)) + 
    geom_segment(aes(x = 5, y = -0.3, xend = 5, yend = 0.5)) + 
    geom_segment(aes(x = 6, y = -0.3, xend = 6, yend = 0.5)) + 
    geom_segment(aes(x = 7, y = -0.3, xend = 7, yend = 0.5)) + 
    geom_segment(aes(x = 8, y = -0.3, xend = 8, yend = 0.5)) + 
    geom_segment(aes(x = 9, y = -0.3, xend = 9, yend = 0.5)) + 
    geom_segment(aes(x = 10, y = -0.3, xend = 10, yend = 0.5)) + 
    geom_segment(aes(x = 11, y = -0.3, xend = 11, yend = 0.5)) + 
    geom_segment(aes(x = 12, y = -0.3, xend = 12, yend = 0.5)) + 
    geom_segment(aes(x = 13, y = -0.3, xend = 13, yend = 0.5)) + 
    geom_segment(aes(x = 14, y = -0.3, xend = 14, yend = 0.5)) + 
    geom_segment(aes(x = 15, y = -0.3, xend = 15, yend = 0.5)) + 
    # draw fertility branches horizontal
    geom_segment(aes(x = 1, y = -0.3, xend = 5, yend = -0.3)) + 
    geom_segment(aes(x = 6, y = -0.3, xend = 10, yend = -0.3)) + 
    geom_segment(aes(x = 11, y = -0.3, xend = 15, yend = -0.3)) + 
    # draw menstrual onset branches horizontal
    geom_segment(aes(x = 18.5, y = 2.5, xend = 20.5, yend = 2.5)) + 
    geom_segment(aes(x = 18.5, y = 6.5, xend = 20.5, yend = 6.5)) + 
    geom_segment(aes(x = 18.5, y = 11.5, xend = 20.5, yend = 11.5)) + 
    # draw menstrual onset branches vertical
    geom_segment(aes(x = 20.5, y = 2.5, xend = 20.5, yend = 11.5)) + 
    # draw exclusion cycle length branches horizontal
    geom_segment(aes(x = 16.5, y = 1.5, xend = 18.5, yend = 1.5)) + 
    geom_segment(aes(x = 16.5, y = 3.5, xend = 18.5, yend = 3.5)) + 
    geom_segment(aes(x = 16.5, y = 5.5, xend = 18.5, yend = 5.5)) + 
    geom_segment(aes(x = 16.5, y = 7.5, xend = 18.5, yend = 7.5)) + 
    geom_segment(aes(x = 16.5, y = 9.5, xend = 18.5, yend = 9.5)) + 
    geom_segment(aes(x = 16.5, y = 11.5, xend = 18.5, yend = 11.5)) + 
    geom_segment(aes(x = 16.5, y = 13.5, xend = 18.5, yend = 13.5)) + 
    # draw exclusion cycle length branches vertical
    geom_segment(aes(x = 18.5, y = 1.5, xend = 18.5, yend = 3.5)) + 
    geom_segment(aes(x = 18.5, y = 5.5, xend = 18.5, yend = 7.5)) + 
    geom_segment(aes(x = 18.5, y = 9.5, xend = 18.5, yend = 13.5)) + 
    # draw exclusion sure branches horizontal
    geom_segment(aes(x = 15.5, y = 1, xend = 16.5, yend = 1)) + 
    geom_segment(aes(x = 15.5, y = 2, xend = 16.5, yend = 2)) + 
    geom_segment(aes(x = 15.5, y = 3, xend = 16.5, yend = 3)) + 
    geom_segment(aes(x = 15.5, y = 4, xend = 16.5, yend = 4)) + 
    geom_segment(aes(x = 15.5, y = 5, xend = 16.5, yend = 5)) + 
    geom_segment(aes(x = 15.5, y = 6, xend = 16.5, yend = 6)) + 
    geom_segment(aes(x = 15.5, y = 7, xend = 16.5, yend = 7)) + 
    geom_segment(aes(x = 15.5, y = 8, xend = 16.5, yend = 8)) + 
    geom_segment(aes(x = 15.5, y = 9, xend = 16.5, yend = 9)) + 
    geom_segment(aes(x = 15.5, y = 10, xend = 16.5, yend = 10)) + 
    geom_segment(aes(x = 15.5, y = 11, xend = 16.5, yend = 11)) + 
    geom_segment(aes(x = 15.5, y = 12, xend = 16.5, yend = 12)) + 
    geom_segment(aes(x = 15.5, y = 13, xend = 16.5, yend = 13)) + 
    geom_segment(aes(x = 15.5, y = 14, xend = 16.5, yend = 14)) + 
    # draw exlusion sure branches vertical
    geom_segment(aes(x = 16.5, y = 1, xend = 16.5, yend = 2)) + 
    geom_segment(aes(x = 16.5, y = 3, xend = 16.5, yend = 4)) + 
    geom_segment(aes(x = 16.5, y = 5, xend = 16.5, yend = 6)) + 
    geom_segment(aes(x = 16.5, y = 7, xend = 16.5, yend = 8)) + 
    geom_segment(aes(x = 16.5, y = 9, xend = 16.5, yend = 10)) + 
    geom_segment(aes(x = 16.5, y = 11, xend = 16.5, yend = 12)) + 
    geom_segment(aes(x = 16.5, y = 13, xend = 16.5, yend = 14)) + 
    annotate("text", x = c(3, 8, 13), y = -2.2, label = c("R1", "R2", "R3")) + 
    annotate("text", x = 1:15, y = -0.8, label = rep(c("F1", "F2", "F3", 
                                                       "F4", "F5"), 3)) + 
    annotate("text", x = 20, y = c(2, 6, 11), label = c("NMO1", "NMO2", 
                                                        "NMO3")) + 
    annotate("text", x = 18, y = c(1, 3), label = c("ECL1", "ECL2")) + 
    annotate("text", x = 18, y = c(5, 7), label = c("ECL1", "ECL3")) + 
    annotate("text", x = 18, y = c(9, 11, 13), label = c("ECL1", "ECL2", "ECL3")) + 
    annotate("text", x = 16, y = c(0.7, 1.7, 2.7, 3.7, 4.7, 5.7, 6.7, 7.7, 8.7, 
                                   9.7, 10.7, 11.7, 12.7, 13.7), label = rep(c("EC1", "EC2"), 7)) + 
    scale_fill_manual(values = c(significant = "grey", nonsignificant = "white")) + 
    scale_x_discrete(expand = c(0, 0)) + scale_y_reverse() + ggtitle(graphnames[ann]) + 
    theme(plot.title = element_text(lineheight = 0.8, face = "bold")) + 
    theme(panel.grid.minor = element_blank()) + theme(panel.grid.major = element_blank()) + 
    theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
          axis.text.y = element_blank()) + theme(panel.background = element_rect(fill = "transparent")) + 
    theme(legend.position = "none") + theme() + xlab("") + ylab("")  
  # windows(30, 20)
  dev.new(10, 7)
  
  print(grids[ann])
    rm(df)
    rm(p)
    rm(p.grid)
}
