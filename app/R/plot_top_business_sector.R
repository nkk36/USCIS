plot_top_business_sector = function(d, NCode_Match){
  
  dsub = group_by(d, NAICS.Code.2D)
  dsub = dplyr::summarise(dsub, Amt = sum(Action.Obligation))
  dsub$NAICS.Code.2D = as.integer(dsub$NAICS.Code.2D)
  dsub = right_join(dsub, NCode_Match, by = c("NAICS.Code.2D" = "Code"))
  dsub$Amt[which(is.na(dsub$Amt))] = 0
  dsub = dsub[order(dsub$Amt, decreasing = TRUE),]
  dsub = dsub[,c(3,1,2)]
  colnames(dsub) = c("Sector", "2D NAICS Code", "Funds Obligated")
  dsub
  
}