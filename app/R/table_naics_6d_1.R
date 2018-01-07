table_naics_6d_1 = function(d, naics6D){
  
  dsub = subset(d, NAICS.Code == naics6D)
  dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
  dsub = dplyr::summarise(dsub, Revenue = sum(Action.Obligation))
  dsub = dsub[order(dsub$Revenue, decreasing = TRUE),]
  dsub = dsub[1:10,]
  colnames(dsub) = c("DUNS Number", "Contractor Name", "Funds Obligated")
  dsub
  
}