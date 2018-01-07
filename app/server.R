###########################################################################################################################
#                                                                                                                         #
#                                           LOAD PACKAGES/FUNCTIONS/DATA                                                  #
#                                                                                                                         #
###########################################################################################################################

# Load packages
library(shiny)
library(formattable)
library(ggplot2)
library(dplyr)
library(reshape2)
library(plotly)
library(data.table)
library(scales)
library(treemap)

# Load functions
source("R/load_data.R", chdir = TRUE)

# Load data
data = load_data(TRUE)

###########################################################################################################################
#                                                                                                                         #
#                                                         SERVER                                                          #
#                                                                                                                         #
###########################################################################################################################

# Define server 
shinyServer(function(input, output) {
  
  v <- reactiveValues(doPlot = FALSE)
  
  observeEvent(input$go, {
    v$doPlot <- input$go
  })
  
  d <- reactive({
    
    d = data[[1]]
    d = subset(d,Fiscal.Year >= input$fYear[1] & Fiscal.Year <= input$fYear[2])
    d$Action.Obligation = currency(d$Action.Obligation, digits = 0)
    d
    
  })
  
  NCode_Match <- reactive({
    
    d = data[[2]]
    
  })
  
  PSC_Match <- reactive({
    
    d = data[[3]]
    
  })
  
  #--------------------------------------------------------------------------------
  #                         OFFICE OVERVIEW
  #--------------------------------------------------------------------------------
  
  #-------------------------
  # TOTAL OBLIGATED PER FISCAL YEAR PLOT
  #-------------------------
  
  output$Dollars_Obligated_Plot <- renderPlot({
    if (v$doPlot == FALSE) return()
    
    isolate({
      
      dsub = group_by(d(),Fiscal.Year)
      dsub = dplyr::summarise(dsub, Amt = sum(Action.Obligation))
      MaxLimit = max(dsub$Amt)*1.10
      
      ggplot(data=dsub, aes(x=Fiscal.Year, y=Amt)) +
        geom_bar(stat="identity", width = 0.4, fill="#619cff", colour="black") +
        ggtitle("Total Funds Obligated") +
        xlab("Fiscal Year") +
        ylab("Amount Obligated (millions)") +
        theme(plot.title = element_text(hjust = 0.5),
              text = element_text(size = 20),
              axis.text.x = element_text(size = 12)) +
        scale_y_continuous(labels = function(x)x/1000000, limits = c(0,MaxLimit)) +
        scale_x_continuous(breaks=seq(input$fYear[1],input$fYear[2],1)) +
        geom_label(aes(label=currency(Amt, digits = 0)), vjust=-0.2)
      
    })
  })
  
  #-------------------------
  # VENDOR TABLE
  #-------------------------
  
  output$Vendor_Plot_Total <- renderPlot({
    if (v$doPlot == FALSE) return()
    
    isolate({
      
      dsub = group_by(d(),Global.DUNS.Number, Global.Vendor.Name)
      dsub = dplyr::summarise(dsub, Revenue = sum(Action.Obligation))
      dsub = dsub[order(dsub$Revenue, decreasing = TRUE),]
      #colnames(dsub) = c("DUNS Number", "Contractor Name", "Funds Obligated")
      dsub = dsub[1:10,]
      field <- c("Global.Vendor.Name", "Revenue")
      dsub$labels <- do.call("paste", c(dsub[field], sep = " \n "))
      treemap(dsub, index = "labels", "Revenue")
      #dsub
      
    })
  })
  
  output$Vendor_Plot_Total1 <- renderDataTable({
    if (v$doPlot == FALSE) return()
    
    isolate({
      
      dsub = group_by(d(),Global.DUNS.Number, Global.Vendor.Name)
      dsub = dplyr::summarise(dsub, NContracts = length(unique(PIID)))
      dsub = dsub[order(dsub$NContracts, decreasing = TRUE),]
      colnames(dsub) = c("DUNS Number", "Contractor Name", "Number of Contracts")
      dsub = dsub[1:10,]
      dsub
      
    })
  })
  
  output$Vendor_Plot_Total2 <- renderDataTable({
    if (v$doPlot == FALSE) return()
    
    isolate({
      
      dsub = group_by(d(),Global.DUNS.Number, Global.Vendor.Name)
      dsub = dplyr::summarise(dsub, Revenue = sum(Action.Obligation), NContracts = length(unique(PIID)), AvgContVal = currency(Revenue/NContracts, digits = 0))
      dsub = dsub[order(dsub$AvgContVal, decreasing = TRUE),c(1,2,5)]
      colnames(dsub) = c("DUNS Number", "Contractor Name", "Average Contract Value")
      dsub = dsub[1:10,]
      dsub
      
    })
  })
  
  #-------------------------
  # VENDOR TABLE DOWNLOAD
  #-------------------------
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste(input$dataset, '.csv', sep='')
    },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
  
  #-------------------------
  # PSC TABLE
  #-------------------------
  
  output$TopPSC <- renderPlot({
    if (v$doPlot == FALSE) return()
    isolate({
      
      dsub = group_by(d(), Fiscal.Year, PSC1D)
      dsub = dplyr::summarise(dsub, Amt = sum(Action.Obligation))
      dsub = left_join(dsub, PSC_Match(), by = c("PSC1D" = "psc.code"))
      dsub$Amt[which(is.na(dsub$Amt))] = 0
      #dsub = dsub[order(dsub$Amt, decreasing = TRUE),]
      #dsub = dsub[,c(3,1,2)]
      dsub$psc.desc[which(is.na(dsub$psc.desc))] = "Other"
      #colnames(dsub) = c("Product/Service Type", "Product/Service Code", "Funds Obligated")
      #dsub
      dsub$psc.desc <- reorder(dsub$psc.desc, -dsub$Amt)
      dsub$psc.desc <- factor(dsub$psc.desc, levels=rev(levels(dsub$psc.desc)))
      ggplot(dsub, aes(x=Fiscal.Year, y=Amt, fill=psc.desc)) + geom_bar(stat='identity')
      
    })
  })
  
  #-------------------------
  # BUSINESS SECTOR TABLE
  #-------------------------
  
  output$TopBSector <- renderDataTable({
    if (v$doPlot == FALSE) return()
    isolate({
      
      dsub = group_by(d(), NAICS.Code.2D)
      dsub = dplyr::summarise(dsub, Amt = sum(Action.Obligation))
      dsub = right_join(dsub, NCode_Match(), by = c("NAICS.Code.2D" = "Code"))
      dsub$Amt[which(is.na(dsub$Amt))] = 0
      dsub = dsub[order(dsub$Amt, decreasing = TRUE),]
      dsub = dsub[,c(3,1,2)]
      colnames(dsub) = c("Sector", "2D NAICS Code", "Funds Obligated")
      dsub
      
    })
  })
  
  #-------------------------
  # BUSINESS SECTOR TABLE DOWNLOAD
  #-------------------------
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste(input$dataset, '.csv', sep='')
    },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
  #--------------------------------------------------------------------------------
  #                         NAICS & PSC
  #--------------------------------------------------------------------------------
  
  #-------------------------
  # NAICS CODE DOLLARS OBLIGATED PER FISCAL YEAR PLOT
  #-------------------------
  
  output$NCDObl <- renderPlot({
    if (v$doPlot == FALSE) return()
    isolate({
      
      validate(
        need(input$NCode_2d != "", "Please select a 2-digit NAICS Code")
      )
      
      if (!is.null(input$NCode_2d)){
        
        j = which(gsub(" \\([^\\)]*\\)","", x = input$NCode_2d) == NCode_Match()$Sector)
        LengthJ = length(j)
        
        if (LengthJ == 3){
          
          dsub = subset(d(), NAICS.Code.2D == NCode_Match()$Code[j[1]] | NAICS.Code.2D == NCode_Match()$Code[j[2]] | NAICS.Code.2D == NCode_Match()$Code[j[3]])
          dsub = group_by(dsub, Fiscal.Year, NAICS.Code.2D)
          dsub = dplyr::summarise(dsub, Amt = sum(Action.Obligation))
          
          MaxLimit = max(dsub$Amt)*1.10
          
          ggplot(data=dsub, aes(x=Fiscal.Year, y=Amt)) +
            geom_bar(aes(fill = NAICS.Code.2D), position = "dodge", stat="identity", width = 0.4) +
            ggtitle(gsub(" \\([^\\)]*\\)","", x = input$NCode_2d)) +
            xlab("Fiscal Year") +
            ylab("Amount Obligated (millions)") +
            theme(plot.title = element_text(hjust = 0.5),
                  text = element_text(size = 20),
                  axis.text.x = element_text(size = 12)) +
            scale_x_continuous(breaks=seq(input$fYear[1],input$fYear[2],1)) +
            scale_y_continuous(labels = function(x)x/1000000, limits = c(0,MaxLimit)) +
            geom_label(aes(label=currency(Amt, digits = 0)), vjust=-0.2)
          
        }else if (LengthJ == 2){
          
          dsub = subset(d(), NAICS.Code.2D == NCode_Match()$Code[j[1]] | NAICS.Code.2D == NCode_Match()$Code[j[2]])
          dsub = group_by(dsub, Fiscal.Year, NAICS.Code.2D)
          dsub = dplyr::summarise(dsub, Amt = sum(Action.Obligation))
          
          MaxLimit = max(dsub$Amt)*1.10
          
          ggplot(data=dsub, aes(x = Fiscal.Year, y = Amt)) +
            geom_bar(aes(fill = NAICS.Code.2D), position = "dodge", stat="identity", width = 0.4) +
            ggtitle(gsub(" \\([^\\)]*\\)","", x = input$NCode_2d)) +
            xlab("Fiscal Year") +
            ylab("Amount Obligated (millions)") +
            theme(plot.title = element_text(hjust = 0.5),
                  text = element_text(size = 20),
                  axis.text.x = element_text(size = 12)) +
            scale_x_continuous(breaks=seq(input$fYear[1],input$fYear[2],1)) +
            scale_y_continuous(labels = function(x)x/1000000, limits = c(0,MaxLimit)) +
            geom_label(aes(label=currency(Amt, digits = 0)), vjust=-0.2)
          
        }else{
          
          dsub = subset(d(), NAICS.Code.2D == NCode_Match()$Code[j])
          dsub = group_by(dsub, Fiscal.Year, NAICS.Code.2D)
          dsub = dplyr::summarise(dsub, Amt = sum(Action.Obligation))
          
          MaxLimit = max(dsub$Amt)*1.10
          
          ggplot(data=dsub, aes(x=Fiscal.Year, y=Amt)) +
            geom_bar(stat="identity", width = 0.4, fill="#619cff", colour="black") +
            ggtitle(gsub(" \\([^\\)]*\\)","", x = input$NCode_2d)) +
            xlab("Fiscal Year") +
            ylab("Amount Obligated (millions)") +
            theme(plot.title = element_text(hjust = 0.5),
                  text = element_text(size = 20),
                  axis.text.x = element_text(size = 12)) +
            scale_x_continuous(breaks=seq(input$fYear[1],input$fYear[2],1)) +
            scale_y_continuous(labels = function(x)x/1000000, limits = c(0,MaxLimit)) +
            geom_label(aes(label=currency(Amt, digits = 0)), vjust=-0.2)
          
        }
      }
    })
  })
  
  #-------------------------
  # PRODUCT/SERVICE CODE DOLLARS OBLIGATED PER FISCAL YEAR PLOT
  #-------------------------
  
  output$PSCDObl <- renderPlot({
    if (v$doPlot == FALSE) return()
    isolate({
      
      validate(
        need(input$PSC_1d != "", "Please select a Product/Service Code")
      )
      
      if (!is.null(input$PSC_1d)){
        
        j = which(PSC_Match()$psc.code == substr(input$PSC_1d, nchar(input$PSC_1d) - 1 , nchar(input$PSC_1d) - 1))
        
        dsub = subset(d(), PSC1D == PSC_Match()$psc.code[j])
        dsub = group_by(dsub, Fiscal.Year)
        dsub = dplyr::summarise(dsub, Amt = sum(Action.Obligation))
        
        MaxLimit = max(dsub$Amt)*1.10
        MinLimit = min(dsub$Amt)*1.10
        
        ggplot(data=dsub, aes(x=Fiscal.Year, y=Amt)) +
          geom_bar(stat="identity", width = 0.4, fill="#619cff", colour="black") +
          ggtitle(gsub(" \\([^\\)]*\\)","", x = input$PSC_1d)) +
          xlab("Fiscal Year") +
          ylab("Amount Obligated (millions)") +
          theme(plot.title = element_text(hjust = 0.5),
                text = element_text(size = 20),
                axis.text.x = element_text(size = 12)) +
          scale_x_continuous(breaks=seq(input$fYear[1],input$fYear[2],1)) +
          scale_y_continuous(labels = function(x)x/1000000, limits = c(MinLimit,MaxLimit)) +
          geom_label(aes(label = Amt, vjust=-0.2))
      }
    })
  })
  
  #-------------------------
  # 2D-NAICS CODE VENDOR TABLE 1
  #-------------------------
  
  output$NCDobl_Table1 <- renderDataTable({
    if (v$doPlot == FALSE) return()
    isolate({
      
      validate(
        need(input$NCode_2d != "", "Please select a 2-digit NAICS Code")
      )
      
      if (!is.null(input$NCode_2d)){
        
        j = which(gsub(" \\([^\\)]*\\)","", x = input$NCode_2d) == NCode_Match()$Sector)
        LengthJ = length(j)
        
        if (LengthJ == 3){
          
          dsub = subset(d(), NAICS.Code.2D == NCode_Match()$Code[j[1]] | NAICS.Code.2D == NCode_Match()$Code[j[2]] | NAICS.Code.2D == NCode_Match()$Code[j[3]])
          dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
          dsub = dplyr::summarise(dsub, Revenue = sum(Action.Obligation))
          dsub = dsub[order(dsub$Revenue, decreasing = TRUE),]
          dsub = dsub[1:10,]
          colnames(dsub) = c("DUNS Number", "Contractor Name", "Funds Obligated")
          dsub
          
        }else if (LengthJ == 2){
          
          dsub = subset(d(), NAICS.Code.2D == NCode_Match()$Code[j[1]] | NAICS.Code.2D == NCode_Match()$Code[j[2]])
          dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
          dsub = dplyr::summarise(dsub, Revenue = sum(Action.Obligation))
          dsub = dsub[order(dsub$Revenue, decreasing = TRUE),]
          dsub = dsub[1:10,]
          colnames(dsub) = c("DUNS Number", "Contractor Name", "Funds Obligated")
          dsub
          
        }else{
          
          dsub = subset(d(), NAICS.Code.2D == NCode_Match()$Code[j])
          dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
          dsub = dplyr::summarise(dsub, Revenue = sum(Action.Obligation))
          dsub = dsub[order(dsub$Revenue, decreasing = TRUE),]
          dsub = dsub[1:10,]
          colnames(dsub) = c("DUNS Number", "Contractor Name", "Funds Obligated")
          dsub
        }
      }
    })
  })
  
  
  #-------------------------
  # 2D-NAICS CODE VENDOR TABLE 2
  #-------------------------
  
  output$NCDobl_Table2 <- renderDataTable({
    if (v$doPlot == FALSE) return()
    isolate({
      
      validate(
        need(input$NCode_2d != "", "Please select a 2-digit NAICS Code")
      )
      
      if (!is.null(input$NCode_2d)){
        
        j = which(gsub(" \\([^\\)]*\\)","", x = input$NCode_2d) == NCode_Match()$Sector)
        LengthJ = length(j)
        
        if (LengthJ == 3){
          
          dsub = subset(d(), NAICS.Code.2D == NCode_Match()$Code[j[1]] | NAICS.Code.2D == NCode_Match()$Code[j[2]] | NAICS.Code.2D == NCode_Match()$Code[j[3]])
          dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
          dsub = dplyr::summarise(dsub, NContracts = length(unique(PIID)))
          dsub = dsub[order(dsub$NContracts, decreasing = TRUE),]
          dsub = dsub[1:10,]
          colnames(dsub) = c("DUNS Number", "Contractor Name", "Number of Contracts")
          dsub
          
        }else if (LengthJ == 2){
          
          dsub = subset(d(), NAICS.Code.2D == NCode_Match()$Code[j[1]] | NAICS.Code.2D == NCode_Match()$Code[j[2]])
          dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
          dsub = dplyr::summarise(dsub, NContracts = length(unique(PIID)))
          dsub = dsub[order(dsub$NContracts, decreasing = TRUE),]
          dsub = dsub[1:10,]
          colnames(dsub) = c("DUNS Number", "Contractor Name", "Number of Contracts")
          dsub
          
        }else{
          
          dsub = subset(d(), NAICS.Code.2D == NCode_Match()$Code[j])
          dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
          dsub = dplyr::summarise(dsub, NContracts = length(unique(PIID)))
          dsub = dsub[order(dsub$NContracts, decreasing = TRUE),]
          dsub = dsub[1:10,]
          colnames(dsub) = c("DUNS Number", "Contractor Name", "Number of Contracts")
          dsub
        }
      }
    })
  })
  
  #-------------------------
  # 2D-NAICS CODE VENDOR TABLE 3
  #-------------------------
  
  output$NCDobl_Table3 <- renderDataTable({
    if (v$doPlot == FALSE) return()
    isolate({
      
      validate(
        need(input$NCode_2d != "", "Please select a 2-digit NAICS Code")
      )
      
      if (!is.null(input$NCode_2d)){
        
        j = which(gsub(" \\([^\\)]*\\)","", x = input$NCode_2d) == NCode_Match()$Sector)
        LengthJ = length(j)
        
        if (LengthJ == 3){
          
          dsub = subset(d(), NAICS.Code.2D == NCode_Match()$Code[j[1]] | NAICS.Code.2D == NCode_Match()$Code[j[2]] | NAICS.Code.2D == NCode_Match()$Code[j[3]])
          dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
          dsub = dplyr::summarise(dsub, AvgContVal = sum(Action.Obligation)/length(unique(PIID)))
          dsub = dsub[order(dsub$AvgContVal, decreasing = TRUE),]
          dsub = dsub[1:10,]
          colnames(dsub) = c("DUNS Number", "Contractor Name", "Average Contract Value")
          dsub
          
        }else if (LengthJ == 2){
          
          dsub = subset(d(), NAICS.Code.2D == NCode_Match()$Code[j[1]] | NAICS.Code.2D == NCode_Match()$Code[j[2]])
          dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
          dsub = dplyr::summarise(dsub, AvgContVal = sum(Action.Obligation)/length(unique(PIID)))
          dsub = dsub[order(dsub$AvgContVal, decreasing = TRUE),]
          dsub = dsub[1:10,]
          colnames(dsub) = c("DUNS Number", "Contractor Name", "Average Contract Value")
          dsub
          
        }else{
          
          dsub = subset(d(), NAICS.Code.2D == NCode_Match()$Code[j])
          dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
          dsub = dplyr::summarise(dsub, AvgContVal = sum(Action.Obligation)/length(unique(PIID)))
          dsub = dsub[order(dsub$AvgContVal, decreasing = TRUE),]
          dsub = dsub[1:10,]
          colnames(dsub) = c("DUNS Number", "Contractor Name", "Average Contract Value")
          dsub
        }
      }
    })
  })
  
  #-------------------------
  # 2D-NAICS CODE VENDOR TABLE DOWNLOAD
  #-------------------------
  
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste(input$dataset, '.csv', sep='')
    },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
  
  #-------------------------
  # 1D PRODUCT OR SERVICE CODE VENDOR TABLE 1
  #-------------------------
  
  output$PSCDobl1_Table1 <- renderDataTable({
    if (v$doPlot == FALSE) return()
    isolate({
      
      validate(
        need(input$PSC_1d != "", "Please select a 1D Product/Service Code")
      )
      
      if (!is.null(input$PSC_1d)){
        
        j = which(PSC_Match()$psc.code == substr(input$PSC_1d, nchar(input$PSC_1d) - 1 , nchar(input$PSC_1d) - 1))
        
        dsub = subset(d(), PSC1D == PSC_Match()$psc.code[j])
        dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
        dsub = dplyr::summarise(dsub, Revenue = sum(Action.Obligation))
        dsub = dsub[order(dsub$Revenue, decreasing = TRUE),]
        dsub = dsub[1:10,]
        colnames(dsub) = c("DUNS Number", "Contractor Name", "Funds Obligated")
        dsub
      }
    })
  })
  
  #-------------------------
  # 1D PRODUCT OR SERVICE CODE VENDOR TABLE 2
  #-------------------------
  
  output$PSCDobl1_Table2 <- renderDataTable({
    if (v$doPlot == FALSE) return()
    isolate({
      
      validate(
        need(input$PSC_1d != "", "Please select a 1D Product/Service Code")
      )
      
      if (!is.null(input$PSC_1d)){
        
        j = which(PSC_Match()$psc.code == substr(input$PSC_1d, nchar(input$PSC_1d) - 1 , nchar(input$PSC_1d) - 1))
        
        dsub = subset(d(), PSC1D == PSC_Match()$psc.code[j])
        dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
        dsub = dplyr::summarise(dsub, NContracts = length(unique(PIID)))
        dsub = dsub[order(dsub$NContracts, decreasing = TRUE),]
        dsub = dsub[1:10,]
        colnames(dsub) = c("DUNS Number", "Contractor Name", "Number of Contracts")
        dsub
      }
    })
  })
  
  #-------------------------
  # 1D PRODUCT OR SERVICE CODE VENDOR TABLE 3
  #-------------------------
  
  output$PSCDobl1_Table3 <- renderDataTable({
    if (v$doPlot == FALSE) return()
    isolate({
      
      validate(
        need(input$PSC_1d != "", "Please select a 1D Product/Service Code")
      )
      
      if (!is.null(input$PSC_1d)){
        
        j = which(PSC_Match()$psc.code == substr(input$PSC_1d, nchar(input$PSC_1d) - 1 , nchar(input$PSC_1d) - 1))
        
        dsub = subset(d(), PSC1D == PSC_Match()$psc.code[j])
        dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
        dsub = dplyr::summarise(dsub, AvgContVal = sum(Action.Obligation)/length(unique(PIID)))
        dsub = dsub[order(dsub$AvgContVal, decreasing = TRUE),]
        dsub = dsub[1:10,]
        colnames(dsub) = c("DUNS Number", "Contractor Name", "Average Contract Value")
        dsub
      }
    })
  })
  
  #-------------------------
  # 6D-NAICS CODE VENDOR TABLE 1
  #-------------------------
  
  output$NCDobl6_Table1 <- renderDataTable({
    if (v$doPlot == FALSE) return()
    isolate({
      
      validate(
        need(input$NCode_6d != "", "Please enter a 6-digit NAICS Code")
      )
      
      dsub = subset(d(), NAICS.Code == input$NCode_6d)
      dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
      dsub = dplyr::summarise(dsub, Revenue = sum(Action.Obligation))
      dsub = dsub[order(dsub$Revenue, decreasing = TRUE),]
      dsub = dsub[1:10,]
      colnames(dsub) = c("DUNS Number", "Contractor Name", "Funds Obligated")
      dsub
      
    })
  })
  
  #-------------------------
  # 6D-NAICS CODE VENDOR TABLE 2
  #-------------------------
  
  output$NCDobl6_Table2 <- renderDataTable({
    if (v$doPlot == FALSE) return()
    isolate({
      
      validate(
        need(input$NCode_6d != "", "Please enter a 6-digit NAICS Code")
      )
      
      dsub = subset(d(), NAICS.Code == input$NCode_6d)
      dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
      dsub = dplyr::summarise(dsub, NContracts = length(unique(PIID)))
      dsub = dsub[order(dsub$NContracts, decreasing = TRUE),]
      dsub = dsub[1:10,]
      colnames(dsub) = c("DUNS Number", "Contractor Name", "Number of Contracts")
      dsub
      
    })
  })
  
  #-------------------------
  # 6D-NAICS CODE VENDOR TABLE 3
  #-------------------------
  
  output$NCDobl6_Table3 <- renderDataTable({
    if (v$doPlot == FALSE) return()
    isolate({
      
      validate(
        need(input$NCode_6d != "", "Please enter a 6-digit NAICS Code")
      )
      
      dsub = subset(d(), NAICS.Code == input$NCode_6d)
      dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
      dsub = dplyr::summarise(dsub, AvgContVal = sum(Action.Obligation)/length(unique(PIID)))
      dsub = dsub[order(dsub$AvgContVal, decreasing = TRUE),]
      dsub = dsub[1:10,]
      colnames(dsub) = c("DUNS Number", "Contractor Name", "Average Contract Value")
      dsub
      
    })
  })
  
  #-------------------------
  # 4D PRODUCT OR SERVICE CODE VENDOR TABLE 1
  #-------------------------
  
  output$PSCDobl4_Table1 <- renderDataTable({
    if (v$doPlot == FALSE) return()
    isolate({
      
      validate(
        need(input$PSC_4d != "", "Please enter a 4-digit Product/Service Code")
      )
      
      dsub = subset(d(), Product.or.Service.Code == input$PSC_4d)
      dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
      dsub = dplyr::summarise(dsub, Revenue = sum(Action.Obligation))
      dsub = dsub[order(dsub$Revenue, decreasing = TRUE),]
      dsub = dsub[1:10,]
      colnames(dsub) = c("DUNS Number", "Contractor Name", "Funds Obligated")
      dsub
      
    })
  })
  
  #-------------------------
  # 4D PRODUCT OR SERVICE CODE VENDOR TABLE 2
  #-------------------------
  
  output$PSCDobl4_Table2 <- renderDataTable({
    if (v$doPlot == FALSE) return()
    isolate({
      
      validate(
        need(input$PSC_4d != "", "Please enter a 4-digit Product/Service Code")
      )
      
      dsub = subset(d(), Product.or.Service.Code == input$PSC_4d)
      dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
      dsub = dplyr::summarise(dsub, NContracts = length(unique(PIID)))
      dsub = dsub[order(dsub$NContracts, decreasing = TRUE),]
      dsub = dsub[1:10,]
      colnames(dsub) = c("DUNS Number", "Contractor Name", "Number of Contracts")
      dsub
      
    })
  })
  
  #-------------------------
  # 4D PRODUCT OR SERVICE CODE VENDOR TABLE 3
  #-------------------------
  
  output$PSCDobl4_Table3 <- renderDataTable({
    if (v$doPlot == FALSE) return()
    isolate({
      
      validate(
        need(input$PSC_4d != "", "Please enter a 4-digit Product/Service Code")
      )
      
      dsub = subset(d(), Product.or.Service.Code == input$PSC_4d)
      dsub = group_by(dsub,Global.DUNS.Number, Global.Vendor.Name)
      dsub = dplyr::summarise(dsub, AvgContVal = sum(Action.Obligation)/length(unique(PIID)))
      dsub = dsub[order(dsub$AvgContVal, decreasing = TRUE),]
      dsub = dsub[1:10,]
      colnames(dsub) = c("DUNS Number", "Contractor Name", "Average Contract Value")
      dsub
      
    })
  })
  
  #-------------------------
  # 6D-NAICS CODE VENDOR TABLE DOWNLOAD
  #-------------------------
  
  output$downloadData4 <- downloadHandler(
    filename = function() {
      paste(input$dataset, '.csv', sep='')
    },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
  
  
  #--------------------------------------------------------------------------------
  #                         CONTRACTOR PROFILE
  #--------------------------------------------------------------------------------
  
  #-------------------------
  # TOTAL OBLIGATED PER FISCAL YEAR VENDOR PLOT
  #-------------------------
  
  output$Dollars_Obligated_Vendor <- renderPlot({
    if (v$doPlot == FALSE) return()
    isolate({
      
      validate(
        need(input$Vendors != "", "Please select a contractor")
      )
      
      dsub = subset(d(), Global.DUNS.Number == input$Vendors)
      dsub = group_by(dsub, Fiscal.Year)
      dsub = dplyr::summarise(dsub, Amt = sum(Action.Obligation))
      MaxLimit = max(dsub$Amt)*1.10
      
      ggplot(data=dsub, aes(x = Fiscal.Year, y = Amt)) +
        geom_bar(stat="identity", width = 0.4, fill="#619cff", colour="black") +
        ggtitle("Total Dollars Obligated") +
        xlab("Fiscal Year") +
        ylab("Amount Obligated") +
        theme(plot.title = element_text(hjust = 0.5),
              text = element_text(size = 20),
              axis.text.x = element_text(size = 12)) +
        scale_y_continuous(labels = scales::comma, limits = c(0,MaxLimit)) +
        scale_x_continuous(breaks=seq(input$fYear[1],input$fYear[2],1)) +
        geom_label(aes(label=currency(Amt, digits = 0)), vjust=-0.2)
      
    })
  })
  
  #-------------------------
  # NAICS CODE PER VENDOR TABLE
  #-------------------------
  
  output$NAICS_Table_Vendor <- renderDataTable({
    if (v$doPlot == FALSE) return()
    isolate({
      
      if (is.null(input$Vendors)){
        
        validate(
          need(input$Vendors != "", "Please select a contractor")
        )
        
      }else{
        
        dsub = subset(d(), Global.DUNS.Number == input$Vendors)
        dsub = group_by(dsub, Global.DUNS.Number, Global.Vendor.Name, NAICS.Code)
        dsub = dsub[order(dsub$NAICS.Code, decreasing = FALSE),]
        dsub = dplyr::summarise(dsub, Revenue = sum(Action.Obligation), NContract = length(unique(PIID)))
        colnames(dsub) = c("DUNS Number", "Contractor", "NAICS Code", "Revenue", "Number of Contracts")
        dsub
      }
      
    })
  })
  
  #-------------------------
  # NAICS CODE PER VENDOR TABLE DOWNLOAD
  #-------------------------
  
  output$downloadData5 <- downloadHandler(
    filename = function() {
      paste(input$dataset, '.csv', sep='')
    },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
  
  #-------------------------
  # TYPE OF CONTRACTS PER VENDOR TABLE
  #-------------------------
  
  output$Contract_Type <- renderDataTable({
    if (v$doPlot == FALSE) return()
    isolate({
      
      validate(
        need(input$Vendors != "", "Please select a contractor")
      )
      
      dsub = subset(d(), Global.DUNS.Number == input$Vendors)
      dsub = group_by(dsub, Global.Vendor.Name ,Type.of.Contract)
      dsub = dplyr::summarise(dsub, Number = length(unique(PIID)))
      dsub$Type.of.Contract[which(dsub$Type.of.Contract == "")] = "UNKNOWN"
      dsub = dsub[order(dsub$Number, decreasing = TRUE),]
      colnames(dsub) = c("Contractor", "Type of Contract", "Number of Contracts")
      dsub
      
    })
  })
  
  #-------------------------
  # Individual Contracts
  #-------------------------
  
  output$Contracts <- renderDataTable({
    if (v$doPlot == FALSE) return()
    isolate({
      
      validate(
        need(input$Vendors != "", "Please select a contractor")
      )
      
      dsub = subset(d(), Global.DUNS.Number == input$Vendors)
      dsub = group_by(dsub, Global.Vendor.Name ,PIID)
      dsub = dplyr::summarise(dsub, Amt = sum(Action.Obligation))
      dsub = dsub[order(dsub$Amt, decreasing = TRUE),]
      colnames(dsub) = c("Contractor", "PIID", "Funds Obligated")
      dsub
      
    })
  })
  
  #-------------------------
  # TYPE OF CONTRACTS PER VENDOR TABLE DOWNLOAD
  #-------------------------
  
  output$downloadData6 <- downloadHandler(
    filename = function() {
      paste(input$dataset, '.csv', sep='')
    },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
  
})