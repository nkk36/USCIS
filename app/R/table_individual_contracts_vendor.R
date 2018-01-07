table_individual_contracts_vendor = function(d, vendor){
  
  dsub = subset(d, Global.DUNS.Number == vendor)
  dsub = group_by(dsub, Global.Vendor.Name ,PIID)
  dsub = dplyr::summarise(dsub, Amt = sum(Action.Obligation))
  dsub = dsub[order(dsub$Amt, decreasing = TRUE),]
  colnames(dsub) = c("Contractor", "PIID", "Funds Obligated")
  dsub
  
}