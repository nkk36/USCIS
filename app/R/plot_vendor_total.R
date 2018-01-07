plot_vendor_total = function(d){
  
  dsub = group_by(d,Global.DUNS.Number, Global.Vendor.Name)
  dsub = dplyr::summarise(dsub, Revenue = sum(Action.Obligation))
  dsub = dsub[order(dsub$Revenue, decreasing = TRUE),]
  #colnames(dsub) = c("DUNS Number", "Contractor Name", "Funds Obligated")
  dsub = dsub[1:10,]
  field <- c("Global.Vendor.Name", "Revenue")
  dsub$labels <- do.call("paste", c(dsub[field], sep = " \n "))
  treemap(dsub, index = "labels", "Revenue")
  #dsub
  
}