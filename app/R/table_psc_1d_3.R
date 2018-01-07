table_psc_1d_3 = function(d, PSC_Match, psc1D){
  
  if (!is.null(psc1D)){

    j = which(PSC_Match$psc.code == substr(psc1D, nchar(psc1D) - 1 , nchar(psc1D) - 1))

    dsub = subset(d, PSC1D == PSC_Match$psc.code[j])
    dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
    dsub = dplyr::summarise(dsub, AvgContVal = sum(Action.Obligation)/length(unique(PIID)))
    dsub = dsub[order(dsub$AvgContVal, decreasing = TRUE),]
    dsub = dsub[1:10,]
    colnames(dsub) = c("DUNS Number", "Contractor Name", "Average Contract Value")
    dsub
  }
  
}