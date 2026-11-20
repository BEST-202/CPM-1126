# =============================================================================
#  Credit Portfolio Management — Tableau de bord risque de crédit (R Shiny)
#  Lancer :  shiny::runApp()   (depuis la racine du projet)
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(plotly)
  library(DT)
})

source("src/config.R",  local = TRUE, encoding = "UTF-8")
source("src/data.R",    local = TRUE, encoding = "UTF-8")
source("src/helpers.R", local = TRUE, encoding = "UTF-8")
source("src/arrears.R", local = TRUE, encoding = "UTF-8")
source("src/ui.R",      local = TRUE, encoding = "UTF-8")

dt_x <- function(df, page = FALSE, page_len = 10, order = NULL, search = page, ...) {
  datatable(
    df, rownames = FALSE, escape = FALSE, selection = "none",
    class = "compact hover", ...,
    options = list(
      dom = paste0(if (search) "f" else "", "t", if (page) "ip" else ""),
      paging = page, pageLength = page_len, scrollX = TRUE, autoWidth = FALSE,
      order = order %||% list(),
      language = list(search = "Rechercher :", info = "_START_–_END_ sur _TOTAL_",
                      infoEmpty = "0 ligne", zeroRecords = "Aucun résultat",
                      paginate = list(previous = "‹", `next` = "›"))
    )
  )
}

server <- function(input, output, session) {
  # navigation
  observeEvent(input$nav, {
    nav_select("tabs", input$nav)
    session$sendCustomMessage("resize", list())
  })

  cur <- reactive(input$devise %||% "USD")
  M   <- function(x, d = 1) fmt_m(x, cur(), d)
  div_ <- reactive(cur_div(cur()))
  unit <- reactive(cur_unit(cur()))

  # portefeuille filtré (incl. passages en perte) et stock actif
  loans_all <- reactive({
    d <- PORTFOLIO
    if (!is.null(input$f_region) && input$f_region != "ALL") d <- d[d$region == input$f_region, ]
    if (!is.null(input$f_segment) && input$f_segment != "ALL") d <- d[d$segment == input$f_segment, ]
    validate(need(nrow(d) > 0, "Aucune exposition pour ce périmètre (région × segment)."))
    d
  })
  loans <- reactive({
    d <- loans_all()[loans_all()$statut == "Actif", ]
    validate(need(nrow(d) > 0, "Aucune donnée pour ce périmètre."))
    d
  })

  source("src/server/server_overview.R",      local = TRUE, encoding = "UTF-8")
  source("src/server/server_regions.R",       local = TRUE, encoding = "UTF-8")
  source("src/server/server_vintage.R",       local = TRUE, encoding = "UTF-8")
  source("src/server/server_arrears.R",      local = TRUE, encoding = "UTF-8")
  source("src/server/server_concentration.R", local = TRUE, encoding = "UTF-8")
  source("src/server/server_limits.R",       local = TRUE, encoding = "UTF-8")
  source("src/server/server_quality.R",      local = TRUE, encoding = "UTF-8")
  source("src/server/server_performance.R",   local = TRUE, encoding = "UTF-8")
  source("src/server/server_stress.R",        local = TRUE, encoding = "UTF-8")
  source("src/server/server_report.R",        local = TRUE, encoding = "UTF-8")

  output$dl_portfolio <- downloadHandler(
    filename = function() paste0("portefeuille_", format(ARRETE, "%Y%m%d"), ".csv"),
    content = function(file) {
      d <- loans_all()
      cols <- c("loan_id", "emprunteur", "groupe", "segment", "secteur", "region", "agence", "rm",
                "devise", "date_octroi", "cohorte", "encours", "autorisation", "stage", "stage_q2",
                "dpd", "pd12", "pd_lt", "lgd", "ecl", "collateral", "rating", "taux", "raroc_num", "statut")
      d$raroc_num <- round(safe_div(d$revenu_net, d$capital), 4)
      write.csv(d[, cols], file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
