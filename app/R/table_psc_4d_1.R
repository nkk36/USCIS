table_psc_4d_1 = function(d, psc4D){
  
  dsub = subset(d, Product.or.Service.Code == psc4D)
  dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
  dsub = dplyr::summarise(dsub, Revenue = sum(Action.Obligation))
  dsub = dsub[order(dsub$Revenue, decreasing = TRUE),]
  dsub = dsub[1:10,]
  colnames(dsub) = c("DUNS Number", "Contractor Name", "Funds Obligated")
  dsub
  
}