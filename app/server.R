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
source("R/plot_total_obligated_per_fy.R", chdir = TRUE)
source("R/plot_vendor_total.R", chdir = TRUE)
source("R/plot_vendor_total_1.R", chdir = TRUE)
source("R/plot_vendor_total_2.R", chdir = TRUE)
source("R/plot_top_psc.R", chdir = TRUE)
source("R/plot_top_business_sector.R", chdir = TRUE)
source("R/plot_NCDobl.R", chdir = TRUE)
source("R/plot_PSCDobl.R", chdir = TRUE)
source("R/table_naics_2d_1.R", chdir = TRUE)
source("R/table_naics_2d_2.R", chdir = TRUE)
source("R/table_naics_2d_3.R", chdir = TRUE)
source("R/table_psc_1d_1.R", chdir = TRUE)
source("R/table_psc_1d_2.R", chdir = TRUE)
source("R/table_psc_1d_3.R", chdir = TRUE)
source("R/table_naics_6d_1.R", chdir = TRUE)
source("R/table_naics_6d_2.R", chdir = TRUE)
source("R/table_naics_6d_3.R", chdir = TRUE)
source("R/table_psc_4d_1.R", chdir = TRUE)
source("R/table_psc_4d_2.R", chdir = TRUE)
source("R/table_psc_4d_3.R", chdir = TRUE)
source("R/plot_vendor_dollars_obligated.R", chdir = TRUE)
source("R/table_naics_vendor.R", chdir = TRUE)
source("R/table_contract_type_vendor.R", chdir = TRUE)
source("R/table_individual_contracts_vendor.R", chdir = TRUE)



# Load data
data = load_data(TRUE)

###########################################################################################################################
#                                                                                                                         #
#                                                         SERVER                                                          #
#                                                                                                                         #
###########################################################################################################################

# Define server 
shinyServer(function(input, output) {
  
  #################################################################
  #                                                               #
  #                      REACTIVE VALUES                          #
  #                                                               #
  #################################################################
  
  v <- reactiveValues(doPlot = FALSE)
  
  observeEvent(input$go, {
    v$doPlot <- input$go
  })
  
  #################################################################
  #                                                               #
  #                       REACTIVE DATA                           #
  #                                                               #
  #################################################################
  
  d <- reactive({
    
    d = data[[1]]
    d = subset(d,Fiscal.Year >= input$fYear[1] & Fiscal.Year <= input$fYear[2])
    d$Action.Obligation = currency(d$Action.Obligation, digits = 0)
    d
    
  })
  
  NCode_Match <- reactive({

    d = data[[2]]
    colnames(d) = c("Code", "Description", "Sector")
    d
    
    
  })
  
  PSC_Match <- reactive({
    
    d = data[[3]]
    
  })
  
  #################################################################
  #                                                               #
  #                         OUTPUTS                               #
  #                                                               #
  #################################################################
  
  #--------------------------------------------------------------------------------
  #                         OFFICE OVERVIEW
  #--------------------------------------------------------------------------------
  
  #-------------------------
  # TOTAL OBLIGATED PER FISCAL YEAR PLOT
  #-------------------------
  
  output$Dollars_Obligated_Plot <- renderPlot({
    if (v$doPlot == FALSE) return()
    
    isolate({
      
      plot_total_obligated_per_fy(d(), input$fYear[1], input$fYear[2])
      
    })
  })
  
  #-------------------------
  # VENDOR TABLE
  #-------------------------
  
  output$Vendor_Plot_Total <- renderPlot({
    if (v$doPlot == FALSE) return()
    
    isolate({
      
      plot_vendor_total(d())
      
    })
  })
  
  output$Vendor_Plot_Total1 <- renderDataTable({
    if (v$doPlot == FALSE) return()
    
    isolate({
      
      plot_vendor_total_1(d())
      
    })
  })
  
  output$Vendor_Plot_Total2 <- renderDataTable({
    if (v$doPlot == FALSE) return()
    
    isolate({
      
      plot_vendor_total_2(d())
      
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
      
      plot_top_psc(d(), PSC_Match())
      
    })
  })
  
  #-------------------------
  # BUSINESS SECTOR TABLE
  #-------------------------
  
  output$TopBSector <- renderDataTable({
    if (v$doPlot == FALSE) return()
    isolate({
      
      plot_top_business_sector(d(), NCode_Match())
      
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
      
      plot_NCDobl(d(), NCode_Match(), input$fYear[1], input$fYear[2], input$NCode_2d)
      
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
      
      plot_PSCDobl(d(), PSC_Match(), input$fYear[1], input$fYear[2], input$PSC_1d)
      
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
      
      table_naics_2d_1(d(), NCode_Match(), input$NCode_2d)
      
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
      
      table_naics_2d_2(d(), NCode_Match(), input$NCode_2d)
      
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
      
      table_naics_2d_3(d(), NCode_Match(), input$NCode_2d)
      
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
      
      table_psc_1d_1(d(), PSC_Match(), input$PSC_1d)
      
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
      
      table_psc_1d_2(d(), PSC_Match(), input$PSC_1d)
      
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
      
      table_psc_1d_3(d(), PSC_Match(), input$PSC_1d)
    
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
      
      table_naics_6d_1(d(), input$NCode_6d)
      
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
      
      table_naics_6d_2(d(), input$NCode_6d)
      
      
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
      
      table_naics_6d_3(d(), input$NCode_6d)
      
      
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
      
      table_psc_4d_1(d(), input$PSC_4d)
      
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
      
      table_psc_4d_2(d(), input$PSC_4d)
      
      
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
      
      table_psc_4d_3(d(), input$PSC_4d)
      
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
      
      plot_vendor_dollars_obligated(d(), input$fYear[1], input$fYear[2], input$Vendors)
      
    })
  })
  
  #-------------------------
  # NAICS CODE PER VENDOR TABLE
  #-------------------------
  
  output$NAICS_Table_Vendor <- renderDataTable({
    if (v$doPlot == FALSE) return()
    isolate({
      
      table_naics_vendor(d(), input$Vendors)
      
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
      
      table_contract_type_vendor(d(), input$Vendors)
      
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
      
      table_individual_contracts_vendor(d(), input$Vendors)
      
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