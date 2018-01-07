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

# conn = src_mysql(Sys.getenv("MYSQL_DB"),host = Sys.getenv("MYSQL_HOST"), user = Sys.getenv("MYSQL_USER"), password = Sys.getenv("MYSQL_PW"))
# tables = sort(src_tbls(conn), decreasing = FALSE)
# 
# psc = collect(tbl(conn, "psc"))
# psc = paste(psc$psc.desc, " (",psc$psc.code, ")", sep = "")
# psc = append(psc, "", after = 0)
# 
# autocomplete_list = c("","Agriculture; Forestry; Fishing; and Hunting (11)",
#                       "Mining; Quarrying; Oil; and Gas Extraction (21)",
#                       "Utilities (22)",
#                       "Construction (23)",
#                       "Manufacturing (31-33)",
#                       "Wholesale Trade (41/42)",
#                       "Retail Trade (44-45)",
#                       "Transportation and Warehousing (48-49)",
#                       "Information (51)",
#                       "Finance and Insurance (52)",
#                       "Real Estate; Rental; and Leasing (53)",
#                       "Professional; Scientific; and Technical Services (54)",
#                       "Management of Companies and Enterprises (55)",
#                       "Administrative; Support; Waste Management and Remidiation Services (56)",
#                       "Educational Services (61)",
#                       "Health Care and Social Assistance (62)",
#                       "Arts; Entertainment; and Recreation (71)",
#                       "Accomodation and Food Services (72)",
#                       "Other Services (81)",
#                       "Public Administration (92)")


sidebar = dashboardSidebar(
  
  sidebarMenu(
    menuItem("Office Overview", tabName = "dashboard", icon = icon("building")),
    menuItem("NAICS & PSC", tabName = "npsc", icon = icon("building")),
    menuItem("Contractor Profile",icon = icon("dashboard"), tabName = "contrprofile")),
  sliderInput("fYear",
              "Fiscal Year",
              min = 2013,
              max = 2017,
              value = c(2013,2017), 
              sep = ""
  ),
  selectInput("NCode_2d", "Business Sector (NAICS Code):", choices = c("",autocomplete_list), multiple = FALSE, selectize = TRUE),
  textInput("NCode_6d", "6-Digit NAICS Code:", ""),
  selectInput("PSC_1d", "Product/Service Code (1D):", choices = psc, multiple = FALSE, selectize = TRUE),
  textInput("PSC_4d", "Product/Service Code (4D):", ""),
  textInput("Vendors","Vendor DUNS",""),
  actionButton("go","Update",styleclass = "primary")
  
  
  
)

body = dashboardBody(
  tabItems(
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

dashboardPage(
  dashboardHeader(title = "USCIS Dashboard"),
  sidebar,
  body
)
