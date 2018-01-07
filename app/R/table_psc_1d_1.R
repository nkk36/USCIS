table_psc_1d_1 = function(d, PSC_Match, psc1D){
  
  if (!is.null(psc1D)){
    
    psc1D = as.character(psc1D)
    PSC_Match$psc.desc.code = as.character(PSC_Match$psc.desc.code)
    
    j = which(PSC_Match$psc.desc.code == psc1D)
    
    dsub = subset(d, PSC1D == PSC_Match$psc.code[j])
    dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
    dsub = dplyr::summarise(dsub, Revenue = sum(Action.Obligation))
    dsub = dsub[order(dsub$Revenue, decreasing = TRUE),]
    dsub = dsub[1:10,]
    colnames(dsub) = c("DUNS Number", "Contractor Name", "Funds Obligated")
    dsub
  }
  
  
}