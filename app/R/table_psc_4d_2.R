table_psc_4d_2 = function(d, psc4D){
  
  dsub = subset(d, Product.or.Service.Code == psc4D)
  dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
  dsub = dplyr::summarise(dsub, NContracts = length(unique(PIID)))
  dsub = dsub[order(dsub$NContracts, decreasing = TRUE),]
  dsub = dsub[1:10,]
  colnames(dsub) = c("DUNS Number", "Contractor Name", "Number of Contracts")
  dsub
  
}