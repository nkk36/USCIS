plot_NCDobl = function(d, NCode_Match, syear, eyear, naics2D){
  
  if (!is.null(naics2D)){
    
#j = which(gsub(" \\([^\\)]*\\)","", x = naics2D) == NCode_Match$Sector)
    
    j = which(naics2D == NCode_Match$Sector)
    
    LengthJ = length(j)
    
    if (LengthJ == 3){
      
      dsub = subset(d, NAICS.Code.2D == NCode_Match$Code[j[1]] | NAICS.Code.2D == NCode_Match$Code[j[2]] | NAICS.Code.2D == NCode_Match$Code[j[3]])
      dsub = group_by(dsub, Fiscal.Year, NAICS.Code.2D)
      dsub = dplyr::summarise(dsub, Amt = sum(Action.Obligation))
      
      MaxLimit = max(dsub$Amt)*1.10
      
      ggplot(data=dsub, aes(x=Fiscal.Year, y=Amt)) +
        geom_bar(aes(fill = NAICS.Code.2D), position = "dodge", stat="identity", width = 0.4) +
        ggtitle(gsub(" \\([^\\)]*\\)","", x = naics2D)) +
        xlab("Fiscal Year") +
        ylab("Amount Obligated (millions)") +
        theme(plot.title = element_text(hjust = 0.5),
              text = element_text(size = 20),
              axis.text.x = element_text(size = 12)) +
        scale_x_continuous(breaks=seq(syear,eyear,1)) +
        scale_y_continuous(labels = function(x)x/1000000, limits = c(0,MaxLimit)) +
        geom_label(aes(label=currency(Amt, digits = 0)), vjust=-0.2)
      
    }else if (LengthJ == 2){
      
      dsub = subset(d, NAICS.Code.2D == NCode_Match$Code[j[1]] | NAICS.Code.2D == NCode_Match$Code[j[2]])
      dsub = group_by(dsub, Fiscal.Year, NAICS.Code.2D)
      dsub = dplyr::summarise(dsub, Amt = sum(Action.Obligation))
      
      MaxLimit = max(dsub$Amt)*1.10
      
      ggplot(data=dsub, aes(x = Fiscal.Year, y = Amt)) +
        geom_bar(aes(fill = NAICS.Code.2D), position = "dodge", stat="identity", width = 0.4) +
        ggtitle(gsub(" \\([^\\)]*\\)","", x = naics2D)) +
        xlab("Fiscal Year") +
        ylab("Amount Obligated (millions)") +
        theme(plot.title = element_text(hjust = 0.5),
              text = element_text(size = 20),
              axis.text.x = element_text(size = 12)) +
        scale_x_continuous(breaks=seq(syear,eyear,1)) +
        scale_y_continuous(labels = function(x)x/1000000, limits = c(0,MaxLimit)) +
        geom_label(aes(label=currency(Amt, digits = 0)), vjust=-0.2)
      
    }else{
      
      dsub = subset(d, NAICS.Code.2D == NCode_Match$Code[j])
      dsub = group_by(dsub, Fiscal.Year, NAICS.Code.2D)
      dsub = dplyr::summarise(dsub, Amt = sum(Action.Obligation))
      
      MaxLimit = max(dsub$Amt)*1.10
      
      ggplot(data=dsub, aes(x=Fiscal.Year, y=Amt)) +
        geom_bar(stat="identity", width = 0.4, fill="#619cff", colour="black") +
        ggtitle(gsub(" \\([^\\)]*\\)","", x = naics2D)) +
        xlab("Fiscal Year") +
        ylab("Amount Obligated (millions)") +
        theme(plot.title = element_text(hjust = 0.5),
              text = element_text(size = 20),
              axis.text.x = element_text(size = 12)) +
        scale_x_continuous(breaks=seq(syear,eyear,1)) +
        scale_y_continuous(labels = function(x)x/1000000, limits = c(0,MaxLimit)) +
        geom_label(aes(label=currency(Amt, digits = 0)), vjust=-0.2)
      
    }
  }
  
}