table_naics_2d_1 = function(d, NCode_Match, naics2D){
  
  if (!is.null(naics2D)){
    
    j = which(naics2D == NCode_Match$Sector)
    
    LengthJ = length(j)
    
    if (LengthJ == 3){
      
      dsub = subset(d, NAICS.Code.2D == NCode_Match$Code[j[1]] | NAICS.Code.2D == NCode_Match$Code[j[2]] | NAICS.Code.2D == NCode_Match$Code[j[3]])
      dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
      dsub = dplyr::summarise(dsub, Revenue = sum(Action.Obligation))
      dsub = dsub[order(dsub$Revenue, decreasing = TRUE),]
      dsub = dsub[1:10,]
      colnames(dsub) = c("DUNS Number", "Contractor Name", "Funds Obligated")
      dsub
      
    }else if (LengthJ == 2){
      
      dsub = subset(d, NAICS.Code.2D == NCode_Match$Code[j[1]] | NAICS.Code.2D == NCode_Match$Code[j[2]])
      dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
      dsub = dplyr::summarise(dsub, Revenue = sum(Action.Obligation))
      dsub = dsub[order(dsub$Revenue, decreasing = TRUE),]
      dsub = dsub[1:10,]
      colnames(dsub) = c("DUNS Number", "Contractor Name", "Funds Obligated")
      dsub
      
    }else{
      
      dsub = subset(d, NAICS.Code.2D == NCode_Match$Code[j])
      dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
      dsub = dplyr::summarise(dsub, Revenue = sum(Action.Obligation))
      dsub = dsub[order(dsub$Revenue, decreasing = TRUE),]
      dsub = dsub[1:10,]
      colnames(dsub) = c("DUNS Number", "Contractor Name", "Funds Obligated")
      dsub
    }
  }
  
  
}