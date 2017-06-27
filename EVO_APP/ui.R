# ui.R
library(shiny)
library(shinydashboard)
library(highcharter)

dashboardPage(
  skin = "red",
  dashboardHeader(title = "Investimentos",
                  dropdownMenu(icon = icon("question-circle"),
                  messageItem(
                    from = "Developer - t684098",
                    message = "Tsy.Evo - v1.0",
                    icon = icon("connectdevelop")),
                  badgeStatus = NULL,
                  headerText = "About"
                  )
                  ),
  dashboardSidebar(
    sidebarMenu(id="sbmenu",
    menuItem("Evolução de Taxas", tabName = "taxevo",icon = icon("bar-chart-o"),
             menuSubItem("Compromissadas", tabName = "comp"),
             menuSubItem("CDB", tabName = "cdb"))
  )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel="stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      # taxevo itens Tab OC
      tabItem(tabName = "comp",
              fluidRow(box(title = p(strong("OC")," | Evolução - Share vs Peers (BRL MM)"),status = "danger",highchartOutput("graph"), width = 12)),
              fluidRow(
                box(title = "Seleção de data",status = "danger", width = 4,dateInput("date", label = "Date input", value = "2017-06-08"),solidHeader = TRUE,height = 120)),
              fluidRow(box(title = "Painel de Volumes",status = "danger", DT::dataTableOutput("tbl_vol")),
                       box(title = "Painel de Taxas",status = "danger", DT::dataTableOutput("tbl_tax")))
          ),
      # taxevo itens Tab CDB
      tabItem(tabName = "cdb",
              fluidRow(box(title = p(strong("CDB")," | Evolução - Share vs Peers (BRL MM)"),status = "danger", highchartOutput("graph_cdb"), width = 12)),
              fluidRow(box(status = "danger",
                           selectInput("var1",
                                       label = "Emissor:",
                                       choices = list("Mercado","Cliente"),
                                       selected = "Mercado"),
                           selectInput("var2",
                                       label = "Detentor:",
                                       choices = list("Mercado","Cliente"),
                                       selected = "Cliente")),
                       box(title = "Seleção de data",
                           status = "danger",
                           solidHeader = TRUE,
                           dateInput("data", label = "Date input", value = "2017-05-02"))),
              fluidRow(box(title = "Painel de Volumes",status = "danger", DT::dataTableOutput("tbl_vol_cdb")),
                       box(title = "Painel de Taxas",status = "danger", DT::dataTableOutput("tbl_tax_cdb")))
              )
      )
    )
)