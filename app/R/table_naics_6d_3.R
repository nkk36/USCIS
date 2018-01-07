table_naics_6d_3 = function(d, naics6D){
  
  dsub = subset(d, NAICS.Code == naics6D)
  dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
  dsub = dplyr::summarise(dsub, AvgContVal = sum(Action.Obligation)/length(unique(PIID)))
  dsub = dsub[order(dsub$AvgContVal, decreasing = TRUE),]
  dsub = dsub[1:10,]
  colnames(dsub) = c("DUNS Number", "Contractor Name", "Average Contract Value")
  dsub
  

}