plot_PSCDobl = function(d, PSC_Match, syear, eyear, psc1D){
  
  if (!is.null(psc1D)){
    
    j = which(PSC_Match$psc.code == substr(psc1D, nchar(psc1D) - 1 , nchar(psc1D) - 1))
    
    dsub = subset(d, PSC1D == PSC_Match$psc.code[j])
    dsub = group_by(dsub, Fiscal.Year)
    dsub = dplyr::summarise(dsub, Amt = sum(Action.Obligation))
    
    MaxLimit = max(dsub$Amt)*1.10
    MinLimit = min(dsub$Amt)*1.10
    
    ggplot(data=dsub, aes(x=Fiscal.Year, y=Amt)) +
      geom_bar(stat="identity", width = 0.4, fill="#619cff", colour="black") +
      ggtitle(gsub(" \\([^\\)]*\\)","", x = psc1D)) +
      xlab("Fiscal Year") +
      ylab("Amount Obligated (millions)") +
      theme(plot.title = element_text(hjust = 0.5),
            text = element_text(size = 20),
            axis.text.x = element_text(size = 12)) +
      scale_x_continuous(breaks=seq(syear,eyear,1)) +
      scale_y_continuous(labels = function(x)x/1000000, limits = c(MinLimit,MaxLimit)) +
      geom_label(aes(label = Amt, vjust=-0.2))
  }
  
}