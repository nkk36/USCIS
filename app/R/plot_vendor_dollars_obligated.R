plot_vendor_dollars_obligated = function(d, syear, eyear, vendor){
  
  dsub = subset(d, Global.DUNS.Number == vendor)
  dsub = group_by(dsub, Fiscal.Year)
  dsub = dplyr::summarise(dsub, Amt = sum(Action.Obligation))
  MaxLimit = max(dsub$Amt)*1.10
  
  ggplot(data=dsub, aes(x = Fiscal.Year, y = Amt)) +
    geom_bar(stat="identity", width = 0.4, fill="#619cff", colour="black") +
    ggtitle("Total Dollars Obligated") +
    xlab("Fiscal Year") +
    ylab("Amount Obligated") +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = 20),
          axis.text.x = element_text(size = 12)) +
    scale_y_continuous(labels = scales::comma, limits = c(0,MaxLimit)) +
    scale_x_continuous(breaks=seq(syear,eyear,1)) +
    geom_label(aes(label=currency(Amt, digits = 0)), vjust=-0.2)
  
}