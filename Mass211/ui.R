library(shiny)
library(plotly)
library(shinydashboard)
library(readr)
library(networkD3)
library(leaflet)

# == Sidebar ----------------------
sidebar <- dashboardSidebar(
  # disable = TRUE,
  sidebarMenu(
    menuItem(
      "Overview",
      icon = icon("globe"),
      tabName = "map"
    )
  )
)

# == overview tab --------
overview_tab <- fluidRow(
  column(
    width = 12,
    box(
      width = NULL,
      title = "The Timeline",
      plotlyOutput("main_timeline")
    )
  ),
  column(
    width = 12,
    box(
      width = NULL,
      title = "The Flow",
      sankeyNetworkOutput("callflow_sankey"),
      HTML(
        '
        <table class="table sankey-caption">
          <colgroup><col width="27%"><col width="21%"><col width="25%"><col></colgroup>
          <tr>
          <td>Have you used Mass211 before?</td>
          <td>How did you hear of Mass211?</td>
          <td align="right">AIRS I&R Need Category</td>
          <td align="right">Referrals Made</td>
          </tr>
        </table>
        '
      )
    )
  )
)

# == map tab -------
source("plot/map.R")

map_tab <- fluidRow(
  column(
    width = 12,
    box(
      width = NULL,
      title = "The map",
      absolutePanel(
        top = 10,
        right = 10,
        map_slider_control
      ),
      leafletOutput("main_map")
    )
  )
)

# === main body ----------
body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  shinyjs::useShinyjs(),
  tabItems(
    tabItem("map", map_tab),
    tabItem("overview", overview_tab)
  ),
  tags$script(read_file("www/app.js"))
)

# === whole page ---------
shiny_ui <- dashboardPage(
  title = "Mass 211 Data Explorer",
  dashboardHeader(title = div("Mass 211")),
  sidebar, body
)