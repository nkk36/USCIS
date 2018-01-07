table_contract_type_vendor = function(d, vendor){
  
  dsub = subset(d, Global.DUNS.Number == vendor)
  dsub = group_by(dsub, Global.Vendor.Name ,Type.of.Contract)
  dsub = dplyr::summarise(dsub, Number = length(unique(PIID)))
  dsub$Type.of.Contract[which(dsub$Type.of.Contract == "")] = "UNKNOWN"
  dsub = dsub[order(dsub$Number, decreasing = TRUE),]
  colnames(dsub) = c("Contractor", "Type of Contract", "Number of Contracts")
  dsub
  
}