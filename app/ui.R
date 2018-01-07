###########################################################################################################################
#                                                                                                                         #
#                                           LOAD PACKAGES/FUNCTIONS/DATA                                                  #
#                                                                                                                         #
###########################################################################################################################

# Load packages
library(shiny)
library(dplyr)
library(shinydashboard)
library(shinysky)

# Load functions
source("R/load_data.R", chdir = TRUE)

# Load data
data = load_data(TRUE)

psc = data[[3]]
psc = paste(psc$psc.desc, " (",psc$psc.code, ")", sep = "")
psc = append(psc, "", after = 0)
autocomplete_list = unique(data[[2]]$Description_Code)

###########################################################################################################################
#                                                                                                                         #
#                                                    USER INTERFACE                                                       #
#                                                                                                                         #
###########################################################################################################################

sidebar = dashboardSidebar(
  
  
  #################################################################
  #                                                               #
  #                         SIDEBAR                               #
  #                                                               #
  #################################################################
  
  sidebarMenu(
    menuItem("Office Overview", tabName = "dashboard", icon = icon("building")),
    menuItem("NAICS & PSC", tabName = "npsc", icon = icon("building")),
    menuItem("Contractor Profile",icon = icon("dashboard"), tabName = "contrprofile")),
  sliderInput(inputId = "fYear",
              label = "Fiscal Year",
              min = 2013,
              max = 2017,
              value = c(2013,2017), 
              sep = ""),
  selectInput(inputId = "NCode_2d", 
              label = "Business Sector (NAICS Code):", 
              choices = c("",autocomplete_list), 
              multiple = FALSE, 
              selectize = TRUE),
  textInput(inputId = "NCode_6d", 
            label = "6-Digit NAICS Code:",
            value = ""),
  selectInput(inputId = "PSC_1d", 
              label = "Product/Service Code (1D):", 
              choices = psc, 
              multiple = FALSE, 
              selectize = TRUE),
  textInput(inputId = "PSC_4d", 
            label = "Product/Service Code (4D):", 
            value = ""),
  textInput(inputId = "Vendors",
            label = "Vendor DUNS",
            value = ""),
  shinysky::actionButton(inputId = "go",
                         label = "Update",
                         styleclass = "primary")
  )

#################################################################
#                                                               #
#                             BODY                              #
#                                                               #
#################################################################

body = dashboardBody(
  tabItems(
    
    
    #################################################################
    #                                                               #
    #                         DASHBOARD                             #
    #                                                               #
    #################################################################
    
    tabItem(tabName = "dashboard",
            fluidRow(
              box(
                title = "Total Funds Obligated",
                plotOutput("Dollars_Obligated_Plot")
              ),
              tabBox(
                title = "Top 10 Vendors",
                id = "topvendortable",
                tabPanel("Total Amount", plotOutput("Vendor_Plot_Total")),
                tabPanel("Number of Contracts", dataTableOutput("Vendor_Plot_Total1")),
                tabPanel("Average Contract Value", dataTableOutput("Vendor_Plot_Total2"))
              )
            ),
            fluidRow(
              box(
                title = "Top Sectors (NAICS)",
                dataTableOutput("TopBSector")
              ),
              box(
                title = "Top Services (PSC)",
                plotOutput("TopPSC")
              )
            )
    ),
    
    
    #################################################################
    #                                                               #
    #                             NPSC                              #
    #                                                               #
    #################################################################
    
    tabItem(tabName = "npsc",
            fluidRow(
              box(
                title = "Total Funds Obligated: 2D NAICS",
                plotOutput("NCDObl")
              ),
              box(
                title = "Total Funds Obligated: 1D PSC",
                plotOutput("PSCDObl")
              )
            ),
            fluidRow(
              tabBox(
                title = "Top 10 Vendors: 2D NAICS",
                id = "naics2d",
                tabPanel("Total Amount", dataTableOutput("NCDobl_Table1")),
                tabPanel("Number of Contracts", dataTableOutput("NCDobl_Table2")),
                tabPanel("Average Contract Value", dataTableOutput("NCDobl_Table3"))
              ),
              tabBox(
                title = "Top 10 Vendors: 1D PSC",
                id = "psc1d",
                tabPanel("Total Amount", dataTableOutput("PSCDobl1_Table1")),
                tabPanel("Number of Contracts", dataTableOutput("PSCDobl1_Table2")),
                tabPanel("Average Contract Value", dataTableOutput("PSCDobl1_Table3"))
              )
            ),
            fluidRow(
              tabBox(
                title = "Top 10 Vendors: 6D NAICS",
                id = "naics6d",
                tabPanel("Total Amount", dataTableOutput("NCDobl6_Table1")),
                tabPanel("Number of Contracts", dataTableOutput("NCDobl6_Table2")),
                tabPanel("Average Contract Value", dataTableOutput("NCDobl6_Table3"))
              ),
              tabBox(
                title = "Top 10 Vendors: 4D PSC",
                id = "psc4d",
                tabPanel("Total Amount", dataTableOutput("PSCDobl4_Table1")),
                tabPanel("Number of Contracts", dataTableOutput("PSCDobl4_Table2")),
                tabPanel("Average Contract Value", dataTableOutput("PSCDobl4_Table3"))
              )
            )
    ),
    
    
    #################################################################
    #                                                               #
    #                   CONTRACTOR PROFILE                          #
    #                                                               #
    #################################################################
    
    tabItem(tabName = "contrprofile",
            fluidRow(
              box(
                title = "Total Funds Obligated: Contractor",
                plotOutput("Dollars_Obligated_Vendor")
              ),
              tabBox(
                title = "Contractor Breakdown",
                id = "vendorNAICS",
                tabPanel("Contracts", dataTableOutput("Contracts")),
                tabPanel("NAICS", dataTableOutput("NAICS_Table_Vendor")),
                tabPanel("Contract Types", dataTableOutput("Contract_Type"))
              )
            )
    )
    
    
  )
)



#################################################################
#                                                               #
#                 DEFINE DASHBOARD PAGE                         #
#                                                               #
#################################################################

dashboardPage(
  dashboardHeader(title = "USCIS Dashboard"),
  sidebar,
  body
)
