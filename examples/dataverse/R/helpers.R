
generate_plot <- function(ann, i, j, k, l, m, data) {
  
  # For now, only works for analysis 1
  
  if (ann == 1) {
    # Generate summary data
    sum = summarySE(data, measurevar="RelComp", groupvars=c("Fertility","RelationshipStatus"), na.rm = T)
    
    # Clean up table
    sum <- sum[!is.na(sum$Fertility),]
    sum <- sum[!is.na(sum$RelationshipStatus),]
    sum$RelationshipStatus <- ifelse(sum$RelationshipStatus == "Relationship", "In Relationship", "Single")
    sum$Fertility <- factor(sum$Fertility, levels = c("Low", "High"))
    sum$RelationshipStatus <- factor(sum$RelationshipStatus, levels = c("Single", "In Relationship"))
    
    # Compute p-value of interaction
    an <- lm("RelComp ~ Fertility*RelationshipStatus", data)
    pvalue <- summary(an)$coefficients[4, 4]
    pvalue_label <- paste("Interaction: p =", format.pval(pvalue, digits = 2, eps = 0.00001), ifelse(pvalue < 0.05, "*", ""))

    # Plot
    limits = aes(ymax = RelComp + se, ymin= RelComp - se)
    dodge = position_dodge(width=0.6)
    p <- ggplot(sum, aes(x = RelationshipStatus, y = RelComp, fill = Fertility)) +
      geom_bar(stat='identity', position=dodge, color="black", width=0.6) +
      geom_errorbar(limits, position=dodge, width=0.05) +
      theme_bw()+
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank(),
            axis.line=element_line(),
            axis.text=element_text(size=12),
            axis.title=element_text(size=13)) + 
      scale_y_continuous(limits = c(0, 8), expand = c(0, 0), breaks = 0:7) +
      xlab('Relationship Status') +
      ylab('Religiosity Composite Score') +
      scale_fill_manual(values = c("#eeeeee", "#aaaaaa")) +
      annotate("text", 1.5, 7.5, label = pvalue_label, size = 5)
    
    ggsave(paste("figures/fig-", ann, "-", i, "-", j, "-", k, "-", l, "-", m, ".jpg", sep = ""), p, width = 4, height = 3.5, dpi = 100)
  }
}
