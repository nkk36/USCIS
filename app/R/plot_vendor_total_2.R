plot_vendor_total_2 = function(d){
  
  dsub = group_by(d,Global.DUNS.Number, Global.Vendor.Name)
  dsub = dplyr::summarise(dsub, Revenue = sum(Action.Obligation), NContracts = length(unique(PIID)), AvgContVal = currency(Revenue/NContracts, digits = 0))
  dsub = dsub[order(dsub$AvgContVal, decreasing = TRUE),c(1,2,5)]
  colnames(dsub) = c("DUNS Number", "Contractor Name", "Average Contract Value")
  dsub = dsub[1:10,]
  dsub
  
}