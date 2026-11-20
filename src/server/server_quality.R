# =============================================================================
#  Page — Performance des agences : qualité des actifs, délégations,
#  concentration du NPL (top 10 agences, top 5 clients), top 20 clients, qualité par RM
# =============================================================================

quality_note <- function(npl, par30) {
  ifelse(npl < .03 & par30 < SEUIL_PAR30, "A", ifelse(npl < SEUIL_NPL & par30 < .07, "B", ifelse(npl < .10, "C", "D")))
}
note_cls <- c(A = "ok", B = "neutral", C = "warn", D = "bad")

aq_agencies <- reactive({
  d <- loans()
  d %>% group_by(agence, region) %>%
    summarise(n_cli = n_distinct(emprunteur), s1 = sum(encours[stage == 1]), s2 = sum(encours[stage == 2]),
              s3 = sum(encours[stage == 3]), par30v = sum(encours[dpd >= 30]), ecl3 = sum(ecl[stage == 3]), ecl = sum(ecl),
              n_npl = n_distinct(emprunteur[stage == 3]), encours = sum(encours), .groups = "drop") %>%
    mutate(npl = s3 / encours, par30 = par30v / encours, note = quality_note(npl, par30)) %>%
    arrange(desc(s3))
})

aq_top10 <- reactive(head(aq_agencies()[aq_agencies()$s3 > 0, ], 10))

aq_top5 <- reactive({
  d <- loans(); a <- aq_top10()
  d3 <- d[d$stage == 3 & d$agence %in% a$agence, ]
  validate(need(nrow(d3) > 0, "Aucun encours NPL dans ce périmètre."))
  c <- d3 %>% group_by(emprunteur) %>%
    summarise(groupe = first(groupe), segment = first(segment), secteur = first(secteur), agence = first(agence),
              region = first(region), rm = first(rm), dpd = max(dpd), ecl = sum(ecl), collat = sum(collateral),
              autor = sum(autorisation), approbateur = first(approbateur), octroi = min(date_octroi),
              rating = first(rating), n_fac = n(), encours = sum(encours), .groups = "drop") %>%
    arrange(desc(encours))
  list(all = c, top = head(c, 5), npl_top10 = sum(d3$encours), npl_total = sum(d$encours[d$stage == 3]))
})

output$aq_kpis <- renderUI({
  d <- loans(); a <- aq_agencies(); t10 <- aq_top10(); t5 <- aq_top5()
  npl_tot <- sum(a$s3)
  over <- sum(d$autorisation > DELEGATION_AGENCE); brk <- d[d$approbateur == "Agence (hors délégation)", ]
  div(class = "kpi-row",
      kpi_card("Qualité des actifs du réseau", fmt_pct(npl_tot / sum(a$encours)), icon_name = "shield-halved",
               value_cls = if (npl_tot / sum(a$encours) > SEUIL_NPL) "red" else "",
               badge_txt = sprintf("%d agences NPL > 5 %%", sum(a$npl > SEUIL_NPL)), badge_cls = if (any(a$npl > SEUIL_NPL)) "warn" else "ok",
               sub = sprintf("NPL %s · PAR30 %s", M(npl_tot), fmt_pct(sum(a$par30v) / sum(a$encours))),
               foot = tagList(lapply(c("A", "B", "C", "D"), function(n) span(badge(n, note_cls[[n]]), tags$b(sum(a$note == n)))))),
      kpi_card("Concentration NPL : top 10 agences", fmt_pct(safe_div(sum(t10$s3), npl_tot), 1), icon_name = "chart-column", value_cls = "red",
               badge_txt = "du NPL total", badge_cls = "bad",
               sub = sprintf("%s de NPL sur 10 des %d agences", M(sum(t10$s3)), nrow(a)),
               foot = tagList(span("1ʳᵉ agence : ", tags$b(sprintf("%s (%s)", t10$agence[1], fmt_pct(t10$s3[1] / npl_tot, 1)))))),
      kpi_card("Top 5 clients NPL dans le top 10", fmt_pct(safe_div(sum(t5$top$encours), t5$npl_top10), 1), icon_name = "user-xmark", value_cls = "red",
               badge_txt = "du NPL du top 10 agences", badge_cls = "bad",
               sub = sprintf("%s · soit %s du NPL total", M(sum(t5$top$encours)), fmt_pct(safe_div(sum(t5$top$encours), t5$npl_total), 1)),
               foot = tagList(span("Plus gros : ", tags$b(t5$top$emprunteur[1])))),
      kpi_card(sprintf("Pouvoir de décaissement (≤ %s)", M(DELEGATION_AGENCE, 0)), nrow(brk), icon_name = "stamp",
               value_cls = if (nrow(brk)) "red" else "green",
               badge_txt = if (nrow(brk)) "Dépassements" else "Conforme", badge_cls = if (nrow(brk)) "bad" else "ok",
               sub = sprintf("dossiers approuvés en agence au-delà de la délégation (%s)", M(sum(brk$autorisation))),
               foot = tagList(span("Dossiers > 1 M USD : ", tags$b(over)),
                              span("Taux de conformité : ", tags$b(fmt_pct(1 - safe_div(nrow(brk), over), 1)))))
  )
})

output$aq_table <- renderDT({
  a <- aq_agencies()
  df <- data.frame(Rang = seq_len(nrow(a)),
    Agence = sprintf('<b>%s</b><span class="sub-cell">%s</span>', a$agence, a$region),
    Encours = round(a$encours / div_(), 2), Clients = a$n_cli,
    `Répartition S1 / S2 / S3` = mapply(function(x, y, z) paste0('<div style="min-width:120px">', as.character(stage_bar(x, y, z)),
                                        sprintf('<span class="sub-cell">%s · %s · %s</span>', fmt_pct(x / (x + y + z), 0), fmt_pct(y / (x + y + z), 0), fmt_pct(z / (x + y + z), 1)), "</div>"),
                                        a$s1, a$s2, a$s3),
    PAR30 = a$par30, NPL = a$npl, `Encours NPL` = round(a$s3 / div_(), 2), `Clients NPL` = a$n_npl,
    `Couv. ECL du NPL` = safe_div(a$ecl3, a$s3), `ECL / encours` = a$ecl / a$encours,
    Note = mapply(function(n) status_html(paste("Note", n), note_cls[[n]]), a$note), check.names = FALSE)
  names(df)[3] <- paste0("Encours (", unit(), ")"); names(df)[8] <- paste0("NPL (", unit(), ")")
  dt_x(df, page = TRUE, page_len = 10) %>%
    formatPercentage(c("PAR30", "NPL", "Couv. ECL du NPL", "ECL / encours"), 1) %>%
    formatStyle("NPL", fontWeight = "bold", color = styleInterval(c(.03, SEUIL_NPL), c(COL$green, COL$orange, COL$red))) %>%
    formatStyle("PAR30", color = styleInterval(c(SEUIL_PAR30, .07), c(COL$green, COL$orange, COL$red))) %>%
    formatStyle("Couv. ECL du NPL", color = styleInterval(.4, c(COL$red, COL$green)))
})

output$aq_pareto <- renderPlotly({
  a <- aq_agencies(); a <- a[a$s3 > 0, ]
  validate(need(nrow(a) > 0, "Aucun encours NPL."))
  cum <- cumsum(a$s3) / sum(a$s3)
  a <- head(a, 15); cum <- head(cum, 15)
  plot_ly(x = factor(a$agence, a$agence)) %>%
    add_bars(y = a$s3 / div_(), name = paste0("Encours NPL (", unit(), ")"),
             marker = list(color = ifelse(seq_len(nrow(a)) <= 10, COL$red, "#d1d5db")),
             hovertemplate = paste0("%{x}<br>%{y:,.2f} ", unit(), "<extra></extra>")) %>%
    add_trace(y = cum, type = "scatter", mode = "lines+markers", yaxis = "y2", name = "Part cumulée du NPL",
              line = list(color = "#111827", width = 2), marker = list(size = 6, color = "#111827"),
              hovertemplate = "%{x}<br>Cumul : %{y:.1%}<extra></extra>") %>%
    pl_layout(yaxis = list(title = unit(), gridcolor = "#f0f1f3"),
              yaxis2 = list(overlaying = "y", side = "right", tickformat = ".0%", range = c(0, 1.05), showgrid = FALSE),
              xaxis = list(tickangle = -35, tickfont = list(size = 10)), margin = list(l = 50, r = 45, t = 30, b = 90),
              annotations = list(list(x = min(9, nrow(a) - 1), y = cum[min(10, nrow(a))], yref = "y2",
                                      text = sprintf("<b>Top 10 = %s du NPL</b>", fmt_pct(cum[min(10, nrow(a))], 0)),
                                      showarrow = TRUE, ax = 30, ay = 30, bgcolor = "#fff", bordercolor = COL$red)))
})

output$aq_top5_donut <- renderPlotly({
  t <- aq_top5()
  lab <- c(t$top$emprunteur, "Autres clients NPL du top 10")
  val <- c(t$top$encours, t$npl_top10 - sum(t$top$encours))
  plot_ly(labels = lab, values = val, type = "pie", hole = .6, sort = FALSE, textinfo = "none",
          marker = list(colors = c("#7f1d1d", "#b91c1c", "#dc2626", "#f87171", "#fca5a5", "#e5e7eb"), line = list(color = "#fff", width = 2)),
          hovertemplate = "%{label}<br>%{percent}<extra></extra>") %>%
    pl_layout(showlegend = FALSE, margin = list(l = 0, r = 0, t = 5, b = 5),
              annotations = list(list(text = sprintf("<b style='font-size:20px'>%s</b><br><span style='font-size:10px'>TOP 5 CLIENTS</span>",
                                                     fmt_pct(sum(t$top$encours) / t$npl_top10, 0)), showarrow = FALSE, x = .5, y = .5)))
})

output$aq_top5_note <- renderUI({
  t <- aq_top5()
  tagList(
    lapply(seq_len(nrow(t$top)), function(i) div(class = "kv", span(sprintf("%d. %s", i, t$top$emprunteur[i]), span(class = "seg-meta", paste0(" · ", t$top$agence[i]))),
                                                tags$b(class = "down", M(t$top$encours[i])))),
    div(class = "alert-box", sprintf("Ces 5 clients représentent %s du NPL des 10 agences les plus exposées et %s du NPL total. Leur résolution est prioritaire (comité de recouvrement, réalisation des sûretés).",
                                     fmt_pct(sum(t$top$encours) / t$npl_top10, 1), fmt_pct(sum(t$top$encours) / t$npl_total, 1)))
  )
})

output$aq_top5 <- renderDT({
  t <- aq_top5(); c <- t$top
  df <- data.frame(Rang = seq_len(nrow(c)),
    Client = sprintf('<b>%s</b><span class="sub-cell">Groupe : %s</span>', c$emprunteur, c$groupe),
    `Segment / secteur` = sprintf('%s<span class="sub-cell">%s</span>', c$segment, c$secteur),
    `Agence / RM` = sprintf('%s<span class="sub-cell">%s</span>', c$agence, c$rm),
    `Encours NPL` = round(c$encours / div_(), 3), `Jours de retard` = c$dpd,
    `ECL constituée` = round(c$ecl / div_(), 3), `Couv. ECL` = c$ecl / c$encours, `Couv. collat.` = c$collat / c$encours,
    `Montant approuvé` = round(c$autor / div_(), 3), Approbation = c$approbateur, Octroi = format(c$octroi, "%d/%m/%Y"),
    `% NPL top 10` = c$encours / t$npl_top10, `% NPL total` = c$encours / t$npl_total, check.names = FALSE)
  names(df)[5] <- paste0("Encours NPL (", unit(), ")"); names(df)[7] <- paste0("ECL (", unit(), ")"); names(df)[10] <- paste0("Approuvé (", unit(), ")")
  dt_x(df, search = FALSE) %>% formatPercentage(c("Couv. ECL", "Couv. collat.", "% NPL top 10", "% NPL total"), 1) %>%
    formatStyle("% NPL top 10", fontWeight = "bold", color = COL$red) %>%
    formatStyle("Approbation", color = styleEqual("Agence (hors délégation)", COL$red), fontWeight = styleEqual("Agence (hors délégation)", "bold"))
})

aq_deleg <- reactive({
  d <- loans()
  d %>% group_by(agence, region) %>%
    summarise(n = n(), approuve = sum(autorisation), dans = sum(autorisation[autorisation <= DELEGATION_AGENCE]),
              n_sup = sum(autorisation > DELEGATION_AGENCE), n_siege = sum(approbateur %in% c("Comité de Crédit Siège", "Conseil d'Administration")),
              n_brk = sum(approbateur == "Agence (hors délégation)"), m_brk = sum(autorisation[approbateur == "Agence (hors délégation)"]),
              max_app = max(autorisation), .groups = "drop") %>% arrange(desc(n_brk), desc(n_sup))
})

output$aq_deleg_chart <- renderPlotly({
  g <- aq_deleg(); g <- g[g$n_sup > 0, ]
  validate(need(nrow(g) > 0, "Aucun dossier au-delà de 1 M USD."))
  g <- head(g[order(-g$n_sup), ], 15)
  plot_ly(x = factor(g$agence, g$agence)) %>%
    add_bars(y = g$n_siege, name = "Validés comité / conseil", marker = list(color = "#1f3a93")) %>%
    add_bars(y = g$n_brk, name = "Approuvés en agence (hors délégation)", marker = list(color = COL$red)) %>%
    pl_layout(barmode = "stack", yaxis = list(title = "Dossiers > 1 M USD", gridcolor = "#f0f1f3"),
              xaxis = list(tickangle = -35, tickfont = list(size = 10)), margin = list(l = 50, r = 10, t = 30, b = 90))
})

output$aq_deleg_note <- renderUI({
  g <- aq_deleg(); b <- g[g$n_brk > 0, ]
  if (!nrow(b)) return(div(class = "info-box", icon("circle-check"), " Aucun dépassement du pouvoir de décaissement des agences."))
  div(class = "alert-box", icon("ban"), tags$b(sprintf(" %d agences en dépassement de délégation : ", nrow(b))),
      paste(head(b$agence, 6), collapse = ", "), if (nrow(b) > 6) "…",
      ". Action : régularisation a posteriori par le Comité de Crédit, suspension temporaire de la délégation et audit des dossiers.")
})

output$aq_breaches <- renderDT({
  d <- loans(); b <- d[d$approbateur == "Agence (hors délégation)", ]
  validate(need(nrow(b) > 0, "Aucun dépassement de délégation dans ce périmètre."))
  b <- b[order(-b$autorisation), ]
  df <- data.frame(Dossier = sprintf('<b>%s</b><span class="sub-cell">%s · %s</span>', b$emprunteur, b$loan_id, b$segment),
                   `Agence / RM` = sprintf('%s<span class="sub-cell">%s</span>', b$agence, b$rm),
                   `Montant approuvé` = round(b$autorisation / div_(), 3), `Dépassement` = round((b$autorisation - DELEGATION_AGENCE) / div_(), 3),
                   Encours = round(b$encours / div_(), 3), Octroi = format(b$date_octroi, "%d/%m/%Y"),
                   IFRS9 = ifelse(b$stage == 3, status_html("Stage 3", "bad"), ifelse(b$stage == 2, status_html("Stage 2", "warn"), status_html("Stage 1", "ok"))),
                   check.names = FALSE)
  names(df)[3:5] <- paste0(c("Approuvé", "Dépassement", "Encours"), " (", unit(), ")")
  dt_x(df, page = TRUE, page_len = 8) %>% formatStyle(names(df)[4], color = COL$red, fontWeight = "bold")
})

output$aq_deleg_table <- renderDT({
  g <- aq_deleg()
  st <- ifelse(g$n_brk > 0, status_html("Délégation à suspendre", "bad"),
        ifelse(g$max_app > .9 * DELEGATION_AGENCE & g$n_sup == 0, status_html("Proche du plafond", "warn"), status_html("Conforme", "ok")))
  df <- data.frame(Agence = sprintf('<b>%s</b><span class="sub-cell">%s</span>', g$agence, g$region), Dossiers = g$n,
                   `Montant approuvé` = round(g$approuve / div_(), 2), `Dans la délégation` = g$dans / g$approuve,
                   `Dossiers > 1 M` = g$n_sup, `Dont validés comité` = g$n_siege, `Dont hors délégation` = g$n_brk,
                   `Montant hors délégation` = round(g$m_brk / div_(), 2), `Plus gros montant approuvé` = round(g$max_app / div_(), 3),
                   Statut = st, check.names = FALSE)
  names(df)[3] <- paste0("Approuvé (", unit(), ")"); names(df)[8] <- paste0("Hors délégation (", unit(), ")"); names(df)[9] <- paste0("Max approuvé (", unit(), ")")
  dt_x(df, page = TRUE, page_len = 10) %>% formatPercentage("Dans la délégation", 1) %>%
    formatStyle("Dont hors délégation", fontWeight = "bold", color = styleInterval(0, c(COL$green, COL$red)))
})

output$aq_top20 <- renderDT({
  d <- loans()
  mode <- input$aq_top20_mode %||% "ALL"
  c <- d %>% group_by(emprunteur) %>%
    summarise(groupe = first(groupe), segment = first(segment), secteur = first(secteur), agence = first(agence), rm = first(rm),
              autor = sum(autorisation), stage = max(stage), dpd = max(dpd), rating = first(rating), ecl = sum(ecl), collat = sum(collateral),
              approbateur = paste(unique(approbateur), collapse = " / "), encours = sum(encours), .groups = "drop") %>%
    arrange(desc(encours))
  tot <- sum(d$encours)
  if (mode == "NPL") c <- c[c$stage == 3, ] else if (mode == "S2") c <- c[c$stage == 2, ]
  c <- head(c, 20)
  validate(need(nrow(c) > 0, "Aucun client pour ce filtre."))
  df <- data.frame(Rang = seq_len(nrow(c)),
    Client = sprintf('<b>%s</b>%s', c$emprunteur, ifelse(c$groupe != c$emprunteur, sprintf('<span class="sub-cell">Groupe : %s</span>', c$groupe), "")),
    `Segment / secteur` = sprintf('%s<span class="sub-cell">%s</span>', c$segment, c$secteur),
    `Agence / RM` = sprintf('%s<span class="sub-cell">%s</span>', c$agence, c$rm),
    Encours = round(c$encours / div_(), 2), `Part` = c$encours / tot, `% FP` = c$encours / FONDS_PROPRES,
    Approuvé = round(c$autor / div_(), 2), Rating = c$rating,
    IFRS9 = ifelse(c$stage == 3, status_html("Stage 3", "bad"), ifelse(c$stage == 2, status_html("Stage 2", "warn"), status_html("Stage 1", "ok"))),
    `Jours retard` = c$dpd, `Couv. collat.` = c$collat / c$encours, Approbation = c$approbateur, check.names = FALSE)
  names(df)[5] <- paste0("Encours (", unit(), ")"); names(df)[8] <- paste0("Approuvé (", unit(), ")")
  dt_x(df, search = FALSE) %>% formatPercentage(c("Part", "% FP", "Couv. collat."), 1) %>%
    formatStyle("% FP", fontWeight = "bold", color = styleInterval(c(SEUIL_GRAND_RISQUE, LIMITE_INDIV), c(COL$ink, COL$orange, COL$red))) %>%
    formatStyle("Couv. collat.", color = styleInterval(c(.9, 1.2), c(COL$red, COL$orange, COL$green)))
})

aq_rm <- reactive({
  d <- loans()
  d %>% group_by(rm) %>%
    summarise(agence = names(which.max(tapply(encours, agence, sum))), n_cli = n_distinct(emprunteur),
              s2 = sum(encours[stage == 2]), s3 = sum(encours[stage == 3]), par30v = sum(encours[dpd >= 30]),
              ecl3 = sum(ecl[stage == 3]), ecl = sum(ecl), n_npl = n_distinct(emprunteur[stage == 3]),
              prod = sum(production_ytd), npl26 = sum(encours[stage == 3 & cohorte == 2026]),
              encours = sum(encours), .groups = "drop") %>%
    mutate(npl = s3 / encours, par30 = par30v / encours, s2p = s2 / encours, note = quality_note(npl, par30))
})

output$aq_rm_chart <- renderPlotly({
  r <- aq_rm(); r <- r[r$encours > 1e6, ]
  r <- head(r[order(-r$npl), ], 15); r <- r[order(r$npl), ]
  validate(need(nrow(r) > 0, "Pas assez de données."))
  y <- factor(r$rm, r$rm)
  plot_ly(y = y, x = r$npl, type = "bar", orientation = "h", name = "NPL", marker = list(color = COL$red),
          customdata = r$agence, hovertemplate = "%{y} (%{customdata})<br>NPL : %{x:.1%}<extra></extra>") %>%
    add_bars(x = r$par30, name = "PAR30", marker = list(color = COL$orange), hovertemplate = "%{y}<br>PAR30 : %{x:.1%}<extra></extra>") %>%
    pl_layout(barmode = "group", xaxis = list(tickformat = ".0%", gridcolor = "#f0f1f3"), yaxis = list(automargin = TRUE, tickfont = list(size = 10)),
              shapes = list(list(type = "line", x0 = SEUIL_NPL, x1 = SEUIL_NPL, yref = "paper", y0 = 0, y1 = 1, line = list(color = COL$red, dash = "dash"))),
              margin = list(l = 10, r = 10, t = 30, b = 30))
})

output$aq_rm_table <- renderDT({
  r <- aq_rm(); r <- r[order(-r$s3), ]
  df <- data.frame(RM = sprintf('<b>%s</b><span class="sub-cell">%s</span>', r$rm, r$agence), Clients = r$n_cli,
                   Encours = round(r$encours / div_(), 2), `Stage 2` = r$s2p, PAR30 = r$par30, NPL = r$npl,
                   `Clients NPL` = r$n_npl, `NPL cohorte 2026` = round(r$npl26 / div_(), 3), `Couv. ECL du NPL` = safe_div(r$ecl3, r$s3),
                   Note = mapply(function(n) status_html(paste("Note", n), note_cls[[n]]), r$note), check.names = FALSE)
  names(df)[3] <- paste0("Encours (", unit(), ")"); names(df)[8] <- paste0("NPL 2026 (", unit(), ")")
  dt_x(df, page = TRUE, page_len = 10) %>% formatPercentage(c("Stage 2", "PAR30", "NPL", "Couv. ECL du NPL"), 1) %>%
    formatStyle("NPL", fontWeight = "bold", color = styleInterval(c(.03, SEUIL_NPL), c(COL$green, COL$orange, COL$red))) %>%
    formatStyle("PAR30", color = styleInterval(c(SEUIL_PAR30, .07), c(COL$green, COL$orange, COL$red)))
})

output$dl_quality <- downloadHandler(
  filename = function() paste0("qualite_agences_", format(ARRETE, "%Y%m%d"), ".csv"),
  content = function(file) {
    a <- aq_agencies(); g <- aq_deleg()[, c("agence", "n_sup", "n_brk", "m_brk", "max_app")]
    a <- merge(a, g, by = "agence", all.x = TRUE)
    write.csv(data.frame(agence = a$agence, region = a$region, encours_usd = round(a$encours), clients = a$n_cli,
                         stage1 = round(a$s1), stage2 = round(a$s2), stage3 = round(a$s3), par30 = round(a$par30, 4),
                         npl = round(a$npl, 4), ecl = round(a$ecl), note = a$note, dossiers_sup_1M = a$n_sup,
                         hors_delegation = a$n_brk, montant_hors_delegation = round(a$m_brk), max_approuve = round(a$max_app)),
              file, row.names = FALSE, fileEncoding = "UTF-8")
  }
)
