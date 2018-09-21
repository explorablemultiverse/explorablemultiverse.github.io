
##################################################################
# Recreates the file rankdata.csv based on master.csv.
# Anonymous Capybara
##################################################################

generateRankData <- function(csvfilename, outputfilename) {
  
  data <- read.csv(csvfilename, header = T)
  
  # define an array of vis names
  visLevels <- c("ordered_line","line","radar","stackedline","stackedarea","stackedbar","donut","scatterplot","parallelCoordinates")
  dirLevels <- levels(data$rdirection)
  abLevels <- levels(data$approach)
  
  # Names in figure 7
  visLevels_output <- c("ordered line","line","radar","stackedline","stackedarea","stackedbar","donut","scatterplot","pcp")
  dirLevels_output <- c("negative", "positive")
  
  # all coefficiences are treated as -k
  getY <- function(b, k, x) return (-1 * abs(k) * x + b)
  
  rlist <-  c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
    
  filter <- function(medians, mads, dataset){
    
    dataframe <- data.frame(col.names = c("jnd","rbase","sign","approach"))
    
    for(i in 1:length(mads$jnd)){
      medianv <- medians$jnd[i]
      madv <- mads$jnd[i]
      rbasev <- mads$rbase[i]
      approachv <- mads$approach[i]
      signv <- mads$sign[i]
      
      subdata <- subset(dataset, sign == signv & rbase == rbasev & approach == approachv & (abs(jnd - medianv) <= 3 * madv))	
      all_subdata <- subset(dataset, sign == signv & rbase == rbasev & approach == approachv)
      compl_subdata <- subset(dataset, sign == signv & rbase == rbasev & approach == approachv & (abs(jnd - medianv) > 3 * madv))
      
      if(length(dataframe$jnd) == 0){
        dataframe <- subdata
      } else {
        dataframe <- rbind(dataframe, subdata)
      }
    }	
    
    return (dataframe)
  }
  
  ######################run#######################
  
  correlations <- c(0.1, 0.3, 0.5, 0.7, 0.9, 0) # 0 is for average
  included_conditions <- c("scatterplot-negative", "scatterplot-positive", "pcp-negative", "pcp-positive", "donut-negative", "stackedbar-negative", "stackedarea-negative", "stackedline-negative", "radar-positive", "line-positive", "ordered line-negative", "ordered line-positive")
  JNDs <- NULL
  
  for(visid in 1:length(visLevels)){
  
    jnd <- subset(data, data$vis == visLevels[visid])$jnd
    rbase <- subset(data, data$vis == visLevels[visid])$rbase
    sign <- subset(data, data$vis == visLevels[visid])$sign
    approach <- subset(data, data$vis == visLevels[visid])$approach
    
    # get the sub dataset of a specific vis
    subdata <- data.frame(jnd, rbase, visLevels[visid], approach, sign)
    
    medians <-  aggregate(jnd ~ rbase*approach*sign, subdata, median)
    mads <-  aggregate(jnd ~ rbase*approach*sign, subdata, function(x){
      return (mad(x, constant = 1))
    })
    
    f_data <- filter(medians, mads, subdata)
    subdata <- aggregate(jnd ~ rbase * approach * sign, data =  f_data, mean)
    
    # get mean of this condition (junk line)
    subdata_mean <- aggregate(jnd ~ rbase*sign*approach, subdata, mean)
    
    # get adjusted r values for above approach
    adj_a <- aggregate(jnd ~ factor(rbase)*factor(sign), subdata, mean)
    adj_a_save <- subset(subdata_mean, approach == "above")
    adj_a_save$rbase <- (adj_a_save$rbase + 0.5 * adj_a$jnd) # adjust
    
    # get adjusted r values for below approach
    adj_b <- aggregate(jnd ~ rbase*sign, subdata, mean)
    adj_b_save <- subset(subdata_mean, approach == "below")
    adj_b_save$rbase <- (adj_b_save$rbase - 0.5 * adj_b$jnd) # adjust
    
    # merge above and below approach
    adj_ab <- rbind(adj_a_save , adj_b_save)
    
    #get positive
    adj_p <- subset(adj_ab, sign == 1)
    
    #get negative
    adj_n <- subset(adj_ab, sign == -1)
    
    # do regression on positive
    regression_p <- lm(jnd ~ rbase, adj_p)
    
    # compute correlation coefficient r
    regression_p_r <- cor(adj_p$jnd, adj_p$rbase)
    
    # do regression on negative
    regression_n <- lm(jnd ~ rbase, adj_n)
    
    # compute correlation coefficient r
    regression_n_r <- cor(adj_n$jnd, adj_n$rbase)
    
    # compute all negative correlations
    cond_neg <- paste(visLevels_output[visid], dirLevels_output[1], sep="-")
    if (cond_neg %in% included_conditions) {
      id <- which(included_conditions == cond_neg)[[1]]
      for (r in correlations) {
        if (r != 0) {
          jnd_neg <- getY(regression_n$coefficients[1], regression_n$coefficients[2], r)
        } else {
          jnd_neg <- (getY(regression_n$coefficients[1], regression_n$coefficients[2], 0) + getY(regression_n$coefficients[1], regression_n$coefficients[2], 1))/2
        }
        JNDs <- rbind(JNDs, data.frame(id = id, condition = cond_neg, r = r, jnd = jnd_neg))
      }
    }
    
    # compute all positive correlations
    cond_pos <- paste(visLevels_output[visid], dirLevels_output[2], sep="-")
    if (cond_pos %in% included_conditions) {
      id <- which(included_conditions == cond_pos)[[1]]
      for (r in correlations) {
        if (r != 0) {
          jnd_pos <- getY(regression_p$coefficients[1], regression_p$coefficients[2], r)
        } else {
          jnd_pos <- (getY(regression_p$coefficients[1], regression_p$coefficients[2], 0) + getY(regression_p$coefficients[1], regression_p$coefficients[2], 1))/2
        }
        JNDs <- rbind(JNDs, data.frame(id = id, condition = cond_pos, r = r, jnd = jnd_pos))
      }
    }
  }
  
  rankings <- NULL
    
  for (r in correlations) {
    d <- JNDs[JNDs$r == r,]
    d <- d[order(d$jnd),]
    dr <- data.frame(d$id, d$condition, d$jnd)
    if (is.null(rankings)) {
      rankings <- dr
    } else {
      rankings <- cbind(rankings, dr)
    }
  }
  
  names(rankings) <- c("id1", "vs1", "X0.1", "id3", "vs3", "X0.3", "id5", "vs5", "X0.5", "id7", "vs7", "X0.7", "id9", "vs9", "X0.9", "overallid", "visoverall", "overall")
  rankings$overall = NA
  #View(rankings)
  
  write.csv(rankings, outputfilename, row.names = F)

}
