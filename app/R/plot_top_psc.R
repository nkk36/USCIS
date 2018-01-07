plot_top_psc = function(d, PSC_Match){
  
  dsub = group_by(d, Fiscal.Year, PSC1D)
  dsub = dplyr::summarise(dsub, Amt = sum(Action.Obligation))
  dsub = left_join(dsub, PSC_Match, by = c("PSC1D" = "psc.code"))
  dsub$Amt[which(is.na(dsub$Amt))] = 0
  #dsub = dsub[order(dsub$Amt, decreasing = TRUE),]
  #dsub = dsub[,c(3,1,2)]
  dsub$psc.desc[which(is.na(dsub$psc.desc))] = "Other"
  #colnames(dsub) = c("Product/Service Type", "Product/Service Code", "Funds Obligated")
  #dsub
  dsub$psc.desc <- reorder(dsub$psc.desc, -dsub$Amt)
  dsub$psc.desc <- factor(dsub$psc.desc, levels=rev(levels(dsub$psc.desc)))
  ggplot(dsub, aes(x=Fiscal.Year, y=Amt, fill=psc.desc)) + geom_bar(stat='identity')
  
}