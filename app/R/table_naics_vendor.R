table_naics_vendor = function(d, vendor){
  
  if (is.null(vendor)){
    
    validate(
      need(vendor != "", "Please select a contractor")
    )
    
  }else{
    
    dsub = subset(d, Global.DUNS.Number == vendor)
    dsub = group_by(dsub, Global.DUNS.Number, Global.Vendor.Name, NAICS.Code)
    dsub = dsub[order(dsub$NAICS.Code, decreasing = FALSE),]
    dsub = dplyr::summarise(dsub, Revenue = sum(Action.Obligation), NContract = length(unique(PIID)))
    colnames(dsub) = c("DUNS Number", "Contractor", "NAICS Code", "Revenue", "Number of Contracts")
    dsub
  }
  
}