# =============================================================================
#  Page 2 — Segmentation & Régions
# =============================================================================

agency_risk <- reactive({
  d <- loans()
  g <- d %>% group_by(agence) %>% summarise_risk()
  a <- AGENCES[match(g$agence, AGENCES$agence), ]
  cbind(g, a[, c("code", "region_code", "region", "type", "lat", "lon")])
})

output$rg_kpis <- renderUI({
  d <- loans(); s <- summarise_risk(d)
  siege <- d$agence %in% AGENCES$agence[AGENCES$type == "Siège"]
  reseau <- sum(d$encours[!siege]); cent <- sum(d$encours[siege])
  n1_res <- sum(loans_all()$encours_n1[!(loans_all()$agence %in% AGENCES$agence[AGENCES$type == "Siège"])])
  div(class = "kpi-row",
      kpi_card("Encours déployé réseau régional", M(reseau), icon_name = "map",
               badge_txt = paste(fmt_pct(reseau / s$encours, 1), "du total"), badge_cls = "neutral",
               sub = HTML(sprintf('<span class="up">%s%s</span> vs N-1 (expansion Kivu & Katanga) · 48 agences hors sièges',
                                  ifelse(reseau >= n1_res, "+", ""), fmt_pct(safe_div(reseau, n1_res) - 1, 1)))),
      kpi_card("Centralisé sièges (Kinshasa & Lubumbashi)", M(cent), icon_name = "landmark",
               badge_txt = paste(fmt_pct(cent / s$encours, 1), "du total"), badge_cls = "neutral",
               sub = "Syndications minières, grandes entreprises & secteur public"),
      kpi_card("Indice de risque territorial", HTML(sprintf('<span class="%s">%s</span> <small style="font-size:14px">PAR30</small> · <span>%s</span> <small style="font-size:14px">NPL</small>',
                                                            if (s$par30 > SEUIL_PAR30) "warn-t" else "up", fmt_pct(s$par30), fmt_pct(s$npl))),
               icon_name = "gauge", badge_txt = if (s$par30 > SEUIL_PAR30) "Sous contrôle renforcé" else "Normal",
               badge_cls = if (s$par30 > SEUIL_PAR30) "warn" else "ok",
               sub = "Pression accrue : corridor Est & agroforesterie"),
      kpi_card("Couverture collatéraux régions", fmt_pct(s$collat, 1), icon_name = "shield",
               value_cls = if (s$collat > 1) "green" else "orange",
               badge_txt = if (s$collat > 1) "Surcollatéralisé" else "Sous-collatéralisé",
               badge_cls = if (s$collat > 1) "ok" else "warn",
               sub = sprintf("Valeur des garanties : %s", M(sum(d$collateral))))
  )
})

output$rg_map <- renderPlotly({
  a <- agency_risk()
  metric <- input$rg_map_metric %||% "enc"
  val <- switch(metric, enc = a$encours / div_(), par30 = a$par30, cdf = a$cdf, collat = a$collat)
  lab <- switch(metric, enc = paste0("Encours (", unit(), ")"), par30 = "PAR30", cdf = "Part CDF", collat = "Couverture collat.")
  fmtv <- if (metric == "enc") sprintf("%.1f", val) else fmt_pct(val, 1)
  scale <- if (metric == "collat") list(c(0, COL$red), c(.5, COL$orange), c(1, COL$s1))
           else if (metric == "enc") list(c(0, "#60a5fa"), c(1, "#1e3a8a"))
           else list(c(0, COL$s1), c(.5, COL$orange), c(1, COL$red))
  a$txt <- sprintf("<b>%s</b><br>%s<br>Encours : %s<br>PAR30 : %s · NPL : %s<br>%s : %s",
                   a$agence, a$region, sapply(a$encours, M), fmt_pct(a$par30), fmt_pct(a$npl), lab, fmtv)
  plot_ly(a, type = "scattergeo", mode = "markers", lat = ~lat, lon = ~lon, text = ~txt, hoverinfo = "text",
          marker = list(size = 7 + 34 * sqrt(a$encours / max(a$encours)), color = val, colorscale = scale,
                        cmin = if (metric == "par30") 0 else NULL, cmax = if (metric == "par30") .12 else NULL,
                        line = list(color = "#fff", width = .8), opacity = .9,
                        colorbar = list(title = list(text = lab, font = list(color = "#cbd5e1", size = 10)),
                                        tickfont = list(color = "#cbd5e1", size = 9), len = .6, thickness = 10,
                                        tickformat = if (metric == "enc") "" else ".0%"))) %>%
    plotly::layout(
      paper_bgcolor = "#0f1b2d", margin = list(l = 0, r = 0, t = 0, b = 0),
      font = list(family = "Source Serif 4, Georgia, serif"),
      geo = list(scope = "africa", projection = list(type = "mercator"), resolution = 50,
                 lonaxis = list(range = c(11.5, 31.5)), lataxis = list(range = c(-13.8, 5.5)),
                 showland = TRUE, landcolor = "#1b2a41", showocean = TRUE, oceancolor = "#0b1424",
                 showcountries = TRUE, countrycolor = "#4b6286", countrywidth = .8,
                 showlakes = TRUE, lakecolor = "#0b1424", showrivers = TRUE, rivercolor = "#21406b",
                 coastlinecolor = "#4b6286", bgcolor = "#0f1b2d")) %>%
    plotly::config(displaylogo = FALSE)
})

output$rg_region_tiles <- renderUI({
  g <- loans() %>% group_by(region) %>% summarise_risk()
  g <- g[match(intersect(REGIONS$region, g$region), g$region), ]
  div(class = "tiles", lapply(seq_len(nrow(g)), function(i) {
    r <- g[i, ]; nag <- sum(AGENCES$region == r$region)
    div(class = "tile", div(class = "tile-h", sub(" — .*", "", r$region), sprintf(" (%d ag.)", nag)),
        div(class = "tile-v", M(r$encours)),
        div(class = "tile-s", style = paste0("color:", if (r$par30 > .06) COL$red else if (r$par30 > SEUIL_PAR30) COL$orange else COL$green),
            "PAR ", fmt_pct(r$par30)))
  }))
})

output$rg_region_bars <- renderUI({
  g <- loans() %>% group_by(region) %>% summarise_risk()
  g <- g[match(intersect(REGIONS$region, g$region), g$region), ]
  lapply(seq_len(nrow(g)), function(i) {
    r <- g[i, ]
    div(class = "seg-line",
        div(class = "seg-line-top",
            span(tags$b(r$region), span(class = "seg-meta", paste0(" · ", REGIONS$zone[REGIONS$region == r$region]))),
            span(badge(M(r$encours), "dark"), span(class = "seg-meta", paste(" Ticket :", M(r$encours / r$n, 2))))),
        stage_bar(r$s1, r$s2, r$s3),
        div(class = "seg-line-top seg-meta",
            span(sprintf("St.1 : %s", fmt_pct(r$s1 / r$encours, 0))), span(sprintf("St.2 : %s", fmt_pct(r$s2 / r$encours, 0))),
            span(sprintf("St.3 (NPL) : %s", fmt_pct(r$npl, 1)))))
  })
})

output$rg_fx <- renderUI({
  d <- loans()
  f <- d %>% group_by(devise) %>% summarise_risk()
  u <- f[f$devise == "USD", ]; c <- f[f$devise == "CDF", ]
  tot <- sum(f$encours)
  dr_u <- if (nrow(u)) u$npl else NA; dr_c <- if (nrow(c)) c$npl else NA
  tagList(
    div(class = "mini-grid",
        div(class = "mini", div(class = "mini-h", "Portefeuille USD", span(fmt_pct(sum(u$encours) / tot, 1))),
            div(class = "mini-v", fmt_full(sum(u$encours), "USD")),
            div(class = "mini-s", "Taux de défaut : ", tags$b(class = "up", fmt_pct(dr_u)))),
        div(class = "mini", div(class = "mini-h", "Portefeuille CDF", span(fmt_pct(sum(c$encours) / tot, 1))),
            div(class = "mini-v", paste(formatC(sum(c$encours) * FX_RATE / 1e9, format = "f", digits = 1, big.mark = " "), "Mrd FC")),
            div(class = "mini-s", "Taux de défaut : ", tags$b(class = "down", fmt_pct(dr_c))))),
    div(class = "info-box", icon("circle-info"), tags$b(" Directive risque : "),
        sprintf("écart d'impayés de %s sur les créances en CDF en raison du glissement du taux de change officiel (%s CDF/USD). Obligation de constitution de provision additionnelle pour les débiteurs de détail sans revenus en devises dures.",
                if (is.finite(dr_c - dr_u)) fmt_bps(dr_c - dr_u) else "—", format(FX_RATE, big.mark = " ")))
  )
})

output$rg_agency_table <- renderDT({
  a <- agency_risk()
  sel <- input$rg_tab_region %||% "ALL"
  if (sel != "ALL") a <- a[a$region_code == sel, ]
  validate(need(nrow(a) > 0, "Aucune agence dans ce périmètre."))
  tot <- sum(loans()$encours)
  deleg <- ifelse(a$par30 > .08, status_html("Gel > $100k", "bad"),
           ifelse(a$par30 > .05, status_html("Sous surveillance", "warn"), status_html("Normal", "ok")))
  df <- data.frame(
    Agence = paste0("<b>", a$agence, "</b>", '<span class="sub-cell">', a$code, " · ", a$type, "</span>"),
    `Région` = sub(" — ", "<br>", a$region),
    `Directeur régional` = REGIONS$directeur[match(a$region_code, REGIONS$region_code)],
    Encours = round(a$encours / div_(), 2), Part = a$encours / tot,
    PAR30 = a$par30, NPL = a$npl, `Collatéral.` = a$collat, `Risque FX (% CDF)` = a$cdf,
    `Statut délégation` = deleg, check.names = FALSE)
  names(df)[4] <- paste0("Encours (", unit(), ")")
  dt_x(df, page = TRUE, page_len = 12, order = list(list(3, "desc"))) %>%
    formatPercentage(c("Part", "PAR30", "NPL", "Collatéral.", "Risque FX (% CDF)"), 1) %>%
    formatStyle("PAR30", fontWeight = "bold", color = styleInterval(c(SEUIL_PAR30, .06), c(COL$green, COL$orange, COL$red))) %>%
    formatStyle("NPL", color = styleInterval(c(.03, SEUIL_NPL), c(COL$green, COL$orange, COL$red))) %>%
    formatStyle("Collatéral.", color = styleInterval(1, c(COL$orange, COL$green)))
})
