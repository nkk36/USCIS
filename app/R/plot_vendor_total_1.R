plot_vendor_total_1 = function(d){
  
  dsub = group_by(d,Global.DUNS.Number, Global.Vendor.Name)
  dsub = dplyr::summarise(dsub, NContracts = length(unique(PIID)))
  dsub = dsub[order(dsub$NContracts, decreasing = TRUE),]
  colnames(dsub) = c("DUNS Number", "Contractor Name", "Number of Contracts")
  dsub = dsub[1:10,]
  dsub
  
}