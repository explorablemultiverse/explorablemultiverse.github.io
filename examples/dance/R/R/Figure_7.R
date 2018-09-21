library(tidyr)
library(dplyr)

############################################################
# This is an .R file to compute and plot the final ranking
############################################################

createFigure7 <- function(csvfilename, jpegfilename) {
  
  jpeg(jpegfilename, height = 300, width = 700, pointsize=14)
  
  ######################functions and parameters#########################
  
  generateRankData(csvfilename, "data/rankdata-alt.csv")
  raw <- read.csv("data/rankdata-alt.csv", stringsAsFactor = FALSE)

  # added by Capybara: shorten condition names
  raw$vs1 <- substr(raw$vs1, 1, nchar(raw$vs1)-5)
  raw$vs3 <- substr(raw$vs3, 1, nchar(raw$vs3)-5)
  raw$vs5 <- substr(raw$vs5, 1, nchar(raw$vs5)-5)
  raw$vs7 <- substr(raw$vs7, 1, nchar(raw$vs7)-5)
  raw$vs9 <- substr(raw$vs9, 1, nchar(raw$vs9)-5)
  raw$visoverall <- substr(raw$visoverall, 1, nchar(raw$visoverall)-5)
  
  # added by Capybara: reverse the row ordering to get the right order
  raw<-raw[nrow(raw):1,]
  
  names(raw) <- paste(rep(c(1, 3, 5, 7, 9, "overall"), each = 3), c("id", "vis", "x"), sep = "_")
  raw$rank <- 1:nrow(raw)
  
  ranks <- raw %>% 
    tbl_df() %>%
    gather(key, value, -rank) %>%
    separate(key, c("cor", "var"), "_") %>%
    spread(var, value, convert = TRUE) %>%
    select(rank, vis, cor, x) %>%
    arrange(cor, vis)
  
  # Use factors so integer values give position
  ranks <- mutate(ranks, 
    cor = factor(cor, 
      labels = c("r = 0.1", "r = 0.3", "r = 0.5", "r = 0.7", "r = 0.9", "Overall")
    ),
    vis = factor(vis)
  )
  
  # Set default colour palette for vis type
  palette(c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462",
    "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd", "#ccebc5", "#ffed6f"))
  
  # par(mar = c(0, 0, 0, 0))
  ncol <- length(levels(ranks$cor))
  nrow <- length(levels(ranks$vis)) + 1
  par(mar = c(0,0,0,0))
  plot(c(0, ncol + 0.5), c(0.5, nrow + 0.5), type = "n", 
    xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt='n', yaxt='n')
  
  # Comute path of each segment
  # (modified by Capybara to look more similar to the paper)
  draw_path <- function(df) {
    n <- nrow(df)
    
    x <- c(0.45, as.numeric(df$cor), ncol - 0.55)
    y <- as.numeric(df$rank[c(1, 1:n, n)])
    lines(x, y, col = df$vis, lwd = 20, lend = 1, ljoin = "mitre")  
  }
  draw_path_single <- function(df) {
    x <- c(as.numeric(df$cor[1])-0.45, as.numeric(df$cor[1])+0.45)
    y <- c(as.numeric(df$rank[1]), as.numeric(df$rank[1]))
    lines(x, y, col = df$vis[1], lwd = 20, lend = 1, ljoin = "mitre")  
  }
  ranks %>% 
    group_by(vis) %>%
    filter(cor != "Overall") %>%
    do(`_` = draw_path(.))
  ranks %>%
    group_by(vis) %>%
    filter(cor == "Overall") %>%
    do(`_` = draw_path_single(.))

  # Add text labels
  text(as.numeric(ranks$cor), as.numeric(ranks$rank), ranks$vis, cex = 0.9)
  
  # Draw ranking arrow
  arrows(x0 = 0.25, y0 = 0.5, x = 0.25, y = nrow, length = 0.15, angle = 25,
         code = 2)       
  text(x = 0.1, y = nrow / 2, srt= 90, labels = "better", cex = 1.1)  
  
  # Draw table grid lines and column headers
  abline(v = 2:ncol - 0.5)
  abline(h = 1:nrow - 0.5)
  text(1:ncol, nrow, levels(ranks$cor))
  
  dev.off()
}  

source("R/generate-rankdata.R")
createFigure7("data/master.csv", "../figures/Figure_7.jpg")
