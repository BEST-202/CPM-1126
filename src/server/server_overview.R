# =============================================================================
#  Page 1 — Vue d'ensemble portefeuille (cockpit exécutif)
# =============================================================================

ov_sum <- reactive(summarise_risk(loans()))

output$ov_perimetre <- renderUI({
  lbl <- paste(c(if (input$f_region != "ALL") input$f_region else "Consolidé global",
                 if (input$f_segment != "ALL") input$f_segment), collapse = " · ")
  span(class = "perim", "Périmètre : ", tags$b(lbl), paste0(" (", M(ov_sum()$encours), ")"))
})

output$ov_kpis <- renderUI({
  d <- loans(); s <- ov_sum()
  n1 <- sum(loans_all()$encours_n1)
  yoy <- safe_div(s$encours, n1) - 1
  cible <- n1 * 1.12
  corp_sh <- safe_div(sum(d$encours[d$segment %in% c("Corporate Grands Comptes", "Secteur Public & Souverain")]), s$encours)
  npl_cov <- safe_div(sum(d$ecl[d$stage == 3]), s$s3)
  # principal contributeur PAR30 (région x segment)
  drv <- d %>% filter(dpd >= 30) %>% group_by(region, segment) %>% summarise(v = sum(encours), .groups = "drop") %>%
    arrange(desc(v)) %>% slice(1)
  drv_lbl <- if (nrow(drv)) paste(sub(" &.*| Grands.*", "", drv$segment), "—", sub(".*— ", "", drv$region)) else "—"
  ecl <- sum(d$ecl)
  div(class = "kpi-row",
    kpi_card("Encours total consolidé", fmt_full(s$encours, cur()), icon_name = "building-columns",
             badge_txt = paste0(ifelse(yoy >= 0, "+", ""), fmt_pct(yoy, 1), " YoY"), badge_cls = if (yoy >= 0) "ok" else "bad",
             sub = HTML(sprintf('<span class="%s">%s</span> vs cible annuelle %s (%s)', if (yoy >= 0) "up" else "down",
                                M(s$encours - n1), M(cible, 2), fmt_pct(s$encours / cible, 1))),
             foot = tagList(span("Corporate & Inst. : ", tags$b(fmt_pct(corp_sh, 1))),
                            span("PME & Retail : ", tags$b(fmt_pct(1 - corp_sh, 1))))),
    kpi_card("Ratio NPL (IFRS 9 Stage 3)", fmt_pct(s$npl), icon_name = "shield-halved",
             value_cls = if (s$npl > SEUIL_NPL) "red" else "",
             badge_txt = if (s$npl < SEUIL_NPL) "Sain < 5,0%" else "Alerte > 5,0%",
             badge_cls = if (s$npl < SEUIL_NPL) "ok" else "bad",
             sub = HTML(sprintf('Marge : <span class="%s">%s</span> · Volume NPL brut : <b class="down">%s</b>',
                                if (s$npl < SEUIL_NPL) "up" else "down", fmt_bps(SEUIL_NPL - s$npl), M(s$s3))),
             foot = tagList(span("Norme : < ", tags$b("5,0%")), span("Couverture NPL : ", tags$b(class = "up", fmt_pct(npl_cov, 1))))),
    kpi_card("Ratio PAR30 (> 30 jours)", fmt_pct(s$par30), icon_name = "triangle-exclamation",
             value_cls = if (s$par30 > SEUIL_PAR30) "orange" else "green",
             badge_txt = if (s$par30 > SEUIL_PAR30) paste("Vigilance", fmt_bps(s$par30 - SEUIL_PAR30)) else "Sous seuil",
             badge_cls = if (s$par30 > SEUIL_PAR30) "warn" else "ok",
             sub = HTML(sprintf('Seuil : %s · Encours à risque : <b class="down">%s</b>', fmt_pct(SEUIL_PAR30),
                                M(s$par30 * s$encours))),
             foot = tagList(span("Driver principal :"), tags$b(class = "down", drv_lbl))),
    kpi_card("Couverture ECL IFRS 9", M(ecl), icon_name = "umbrella",
             badge_txt = paste(fmt_pct(safe_div(ecl, s$s3), 1), "du NPL"), badge_cls = "ok",
             sub = HTML(sprintf("Stock total ECL · Coût du risque (CoR) : <b>%s</b>", fmt_pct(s$cor))),
             foot = tagList(span("St.1 : ", tags$b(M(sum(d$ecl[d$stage == 1])))),
                            span("St.2 : ", tags$b(M(sum(d$ecl[d$stage == 2])))),
                            span("St.3 : ", tags$b(class = "down", M(sum(d$ecl[d$stage == 3]))))))
  )
})

output$ov_staging <- renderUI({
  d <- loans(); tot <- sum(d$encours)
  g <- d %>% group_by(segment) %>% summarise_risk() %>% arrange(desc(encours))
  mx <- max(g$encours)
  rows <- lapply(seq_len(nrow(g)), function(i) {
    r <- g[i, ]
    warn <- r$s3 / r$encours > SEUIL_NPL
    div(class = "seg-line",
        div(class = "seg-line-top",
            span(tags$b(r$segment), span(class = "seg-share", paste(fmt_pct(r$encours / tot, 1), "du portefeuille")),
                 if (warn) span(class = "badge-x bad", style = "margin-left:6px", "Stage 3 > 5%")),
            span(tags$b(M(r$encours)), span(class = "seg-meta",
                 sprintf(" · St.1 %s · St.2 %s · St.3 %s", fmt_pct(r$s1 / r$encours, 0),
                         fmt_pct(r$s2 / r$encours, 0), fmt_pct(r$s3 / r$encours, 1))))),
        div(style = if (input$ov_stage_mode == "val") sprintf("width:%.1f%%", 100 * r$encours / mx) else "width:100%",
            stage_bar(r$s1, r$s2, r$s3)))
  })
  tagList(
    div(class = "legend-row",
        span(span(class = "sq", style = paste0("background:", COL$s1)), "Stage 1 : Performant (< 30j)"),
        span(span(class = "sq", style = paste0("background:", COL$s2)), "Stage 2 : Dégradation significative (SICR > 30j)"),
        span(span(class = "sq", style = paste0("background:", COL$s3)), "Stage 3 : NPL / Défaillant (> 90j)")),
    rows,
    div(class = "total-line",
        span("TOTAL CONSOLIDÉ : ", M(tot)),
        span(style = paste0("color:", COL$s1), sprintf("St.1 : %s (%s)", fmt_pct(sum(g$s1) / tot, 1), M(sum(g$s1)))),
        span(style = paste0("color:", COL$s2), sprintf("St.2 : %s (%s)", fmt_pct(sum(g$s2) / tot, 1), M(sum(g$s2)))),
        span(style = paste0("color:", COL$s3), sprintf("St.3 : %s (%s)", fmt_pct(sum(g$s3) / tot, 1), M(sum(g$s3)))))
  )
})

sector_shares <- reactive({
  d <- loans()
  d %>% group_by(secteur) %>% summarise(encours = sum(encours), .groups = "drop") %>%
    mutate(part = encours / sum(encours), limite = SECTEURS$limite[match(secteur, SECTEURS$secteur)]) %>%
    arrange(desc(encours))
})

output$ov_hhi_donut <- renderPlotly({
  s <- sector_shares()
  top <- head(s, 6)
  pie <- rbind(top[, c("secteur", "encours")],
               data.frame(secteur = "Autres secteurs", encours = sum(s$encours) - sum(top$encours)))
  h <- hhi(s$encours)
  cols <- c("#111827", COL$red, "#1f3a93", "#0b8a4f", COL$orange, "#7c3aed", "#cbd5e1")
  plot_ly(pie, labels = ~secteur, values = ~encours, type = "pie", hole = .68, sort = FALSE,
          marker = list(colors = cols, line = list(color = "#fff", width = 2)), textinfo = "none",
          hovertemplate = "%{label}<br>%{percent}<extra></extra>") %>%
    pl_layout(showlegend = FALSE, margin = list(l = 0, r = 0, t = 5, b = 5),
              annotations = list(list(text = sprintf("<b style='font-size:22px'>%s</b><br><span style='font-size:10px'>INDICE HHI</span><br><span style='font-size:10px;color:#b91c1c'>%s</span>",
                                                     format(round(h), big.mark = " "),
                                                     if (h < 1500) "Concentration faible" else if (h < 2000) "Concentration modérée" else "Concentration élevée"),
                                      showarrow = FALSE, x = .5, y = .5)))
})

output$ov_hhi_list <- renderUI({
  s <- sector_shares()
  cols <- c("#111827", COL$red, "#1f3a93", "#0b8a4f", COL$orange, "#7c3aed", rep("#cbd5e1", 6))
  over <- s[s$part > s$limite, ]
  tagList(
    tags$ul(class = "hhi-list", lapply(seq_len(min(8, nrow(s))), function(i) {
      r <- s[i, ]; al <- r$part > r$limite
      tags$li(class = if (al) "alert-row",
              span(span(class = "sq", style = paste0("background:", cols[i])), r$secteur,
                   if (al) span(class = "badge-x bad", style = "margin-left:6px", paste(fmt_bps(r$part - r$limite), "ALERTE"))),
              span(sprintf("%s (%s)", fmt_pct(r$part, 1), M(r$encours, 0))))
    })),
    if (nrow(over)) div(class = "alert-box", icon("bell"), tags$b(sprintf(" Plafond prudentiel %s dépassé (> %s)", over$secteur[1], fmt_pct(over$limite[1], 1))),
                        tags$br(), sprintf("Exposition actuelle %s. Activation de la règle de double signature CRO & Directeur Général pour tout nouvel engagement > $5,0M.",
                                           fmt_pct(over$part[1], 1)))
  )
})

vint <- reactive(vintage_curves(loans_all()))

output$ov_vintage_badge <- renderUI({
  v <- vint(); r <- v$taux[v$cohorte == "2026" & v$mob == 6]
  if (length(r) && !is.na(r) && r > SEUIL_EPD) badge("Alerte EPD M6 Cohorte 2026", "warn") else badge("EPD sous contrôle", "ok")
})

vintage_plot <- function(v, height_ann = TRUE) {
  cols <- c("2023" = "#9ca3af", "2024" = "#475569", "2025" = "#111827", "2026" = COL$red)
  p <- plot_ly()
  for (c in names(cols)) {
    vv <- v[v$cohorte == c & !is.na(v$taux), ]
    if (!nrow(vv)) next
    p <- p %>% add_trace(data = vv, x = ~mob, y = ~taux, type = "scatter", mode = "lines+markers",
                         name = paste("Cohorte", c),
                         line = list(color = cols[[c]], width = if (c == "2026") 3 else 2,
                                     dash = if (c == "2023") "dot" else if (c == "2026") "dash" else "solid"),
                         marker = list(size = if (c == "2026") 7 else 4, color = cols[[c]]),
                         hovertemplate = paste0("Cohorte ", c, " · M%{x}<br>%{y:.2%}<extra></extra>"))
  }
  p %>% pl_layout(
    xaxis = list(title = "", tickvals = c(0, 3, 6, 9, 12, 18, 24, 30, 36), ticktext = paste0("M", c(0, 3, 6, 9, 12, 18, 24, 30, 36)),
                 gridcolor = "#f0f1f3", zeroline = FALSE),
    yaxis = list(title = "", tickformat = ".1%", gridcolor = "#f0f1f3", zeroline = FALSE,
                 range = c(0, max(0.06, max(v$taux, na.rm = TRUE) * 1.15))),
    shapes = list(list(type = "line", x0 = 0, x1 = 36, y0 = SEUIL_NPL, y1 = SEUIL_NPL,
                       line = list(color = COL$red, dash = "dash", width = 1.2))),
    annotations = list(list(x = 36, y = SEUIL_NPL, text = "Seuil de tolérance prudentiel (5,0%)", showarrow = FALSE,
                            xanchor = "right", yanchor = "bottom", font = list(color = COL$red, size = 10))))
}

output$ov_vintage <- renderPlotly(vintage_plot(vint()))

output$ov_vintage_note <- renderUI({
  v <- vint()
  r26 <- v$taux[v$cohorte == "2026" & v$mob == 6]; r25 <- v$taux[v$cohorte == "2025" & v$mob == 6]
  if (!length(r26) || is.na(r26)) return(NULL)
  div(class = "info-box", tags$b("Diagnostic EPD (Early Payment Default) : "),
      sprintf("la cohorte 2026 atteint %s à M6 contre %s pour la cohorte 2025 au même âge (%s).",
              fmt_pct(r26), fmt_pct(r25), fmt_bps(r26 - r25)),
      " Décrochage attribuable aux retards de liquidation des marchés publics provinciaux et aux tensions logistiques à l'Est.")
})

region_perf <- reactive({
  d <- loans()
  g <- d %>% group_by(region) %>% summarise_risk()
  tot <- summarise_risk(d); tot$region <- "PONDÉRATION GLOBALE"
  rbind(g[match(intersect(REGIONS$region, g$region), g$region), ], tot[, names(g)])
})

perf_status <- function(par30, raroc) {
  ifelse(par30 > .06 | raroc < HURDLE * .8, status_html("Action requise", "bad"),
  ifelse(par30 > SEUIL_PAR30 | raroc < HURDLE, status_html("Vigilance", "warn"), status_html("Excellence", "ok")))
}

output$ov_region_table <- renderDT({
  g <- region_perf()
  df <- data.frame(
    `Région / Desk` = ifelse(g$region == "PONDÉRATION GLOBALE", "<b>PONDÉRATION GLOBALE</b>",
                             paste0(g$region, '<span class="sub-cell">', REGIONS$zone[match(g$region, REGIONS$region)], "</span>")),
    Encours = round(g$encours / div_(), 1),
    PAR30 = g$par30, NPL = g$npl, `CoR (bps)` = round(g$cor * 1e4), RAROC = g$raroc,
    Statut = ifelse(g$region == "PONDÉRATION GLOBALE", status_html("Sous contrôle", "neutral"), perf_status(g$par30, g$raroc)),
    check.names = FALSE)
  names(df)[2] <- paste0("Encours (", unit(), ")")
  dt_x(df) %>% formatPercentage(c("PAR30", "NPL", "RAROC"), 2) %>%
    formatRound(2, 1) %>%
    formatStyle("PAR30", color = styleInterval(c(SEUIL_PAR30 - .0001, .06), c(COL$green, COL$orange, COL$red))) %>%
    formatStyle("NPL", color = styleInterval(c(.03, SEUIL_NPL), c(COL$green, COL$orange, COL$red))) %>%
    formatStyle("RAROC", fontWeight = "bold", color = styleInterval(HURDLE, c(COL$red, COL$green)))
})

# ---------------------------------------------------------------- EWS
output$ov_ews <- renderUI({
  d <- loans(); tot <- sum(d$encours)
  s <- sector_shares()
  over <- s %>% mutate(gap = part - limite) %>% arrange(desc(gap)) %>% slice(1)
  a1 <- if (over$gap > 0) {
    list(tag = "Alerte plafond sectoriel", title = paste("Dépassement limite sectorielle :", over$secteur),
         body = sprintf("L'encours global s'élève à <b>%s (%s)</b> contre un plafond interne fixé à %s. Risque de corrélation forte en cas de retournement des cours LME.",
                        M(over$encours), fmt_pct(over$part, 1), fmt_pct(over$limite, 1)), cls = "bad")
  } else {
    list(tag = "Proximité plafond", title = paste("Secteur proche de sa limite :", over$secteur),
         body = sprintf("Exposition %s (%s) pour une limite de %s. Marge disponible : %s.",
                        M(over$encours), fmt_pct(over$part, 1), fmt_pct(over$limite, 1), M((over$limite - over$part) * tot)), cls = "warn")
  }
  ag <- d %>% group_by(agence, region) %>% summarise_risk() %>% filter(encours > 2e6) %>% arrange(desc(par30)) %>% slice(1)
  avg <- sum(d$encours[d$dpd >= 30]) / tot
  mig <- loans_all() %>% filter(!is.na(stage_q2), stage_q2 == 1, stage == 2) %>% group_by(region) %>%
    summarise(v = sum(encours_q2), n = n(), dE = sum(ecl - ecl_q2), .groups = "drop") %>% arrange(desc(v)) %>% slice(1)
  div(class = "ews-grid",
      ews_card(a1$tag, a1$cls, "Délai : 72 heures", a1$title, a1$body,
               "Action : Convoquer Comité Risques", "ews_b1", "Geler tirages"),
      if (nrow(ag)) ews_card("Early Payment Default", "warn", "Détecté J-2",
               paste("Défaillances précoces : Agence", ag$agence),
               sprintf("PAR30 de <b>%s</b> sur un encours de %s (moyenne périmètre : %s). NPL agence : %s ; %d dossiers à surveiller.",
                       fmt_pct(ag$par30), M(ag$encours), fmt_pct(avg), fmt_pct(ag$npl), ag$n),
               "Action : Audit portefeuille agence", "ews_b2", "Mandater auditeur", "btn-ghost"),
      if (nrow(mig)) ews_card("Migration ECL IFRS 9", "dark", "Impact Q3",
               paste("Glissement Stage 1 → Stage 2 :", sub(".*— ", "", mig$region)),
               sprintf("Transfert de <b>%s</b> de créances saines vers Stage 2 (%d dossiers) suite à la dégradation des retards et des signaux qualitatifs (watchlist).",
                       M(mig$v), mig$n),
               sprintf("Provision additive : %s", M(mig$dE)), "ews_b3", "Valider provision", "btn-dark-sm")
  )
})
observeEvent(input$ews_b1, showNotification("Demande de gel des tirages transmise au Comité des Risques.", type = "warning"))
observeEvent(input$ews_b2, showNotification("Mandat d'audit portefeuille agence créé.", type = "message"))
observeEvent(input$ews_b3, showNotification("Provision additive soumise pour validation CRO.", type = "message"))

output$ov_sector_stage <- renderPlotly({
  d <- loans()
  g <- d %>% group_by(secteur) %>% summarise(s1 = sum(encours[stage == 1]), s2 = sum(encours[stage == 2]),
                                             s3 = sum(encours[stage == 3]), tot = sum(encours), .groups = "drop") %>%
    arrange(tot)
  pct <- identical(input$ov_stage_mode, "pct")
  y <- factor(g$secteur, g$secteur)
  val <- function(x) if (pct) x / g$tot else x / div_()
  hov <- if (pct) "%{y}<br>%{x:.1%}<extra></extra>" else paste0("%{y}<br>%{x:,.1f} ", unit(), "<extra></extra>")
  plot_ly(y = y, x = val(g$s1), type = "bar", orientation = "h", name = "Stage 1", marker = list(color = COL$s1), hovertemplate = hov) %>%
    add_trace(x = val(g$s2), name = "Stage 2", marker = list(color = COL$s2)) %>%
    add_trace(x = val(g$s3), name = "Stage 3", marker = list(color = COL$s3)) %>%
    pl_layout(barmode = "stack", xaxis = list(title = "", tickformat = if (pct) ".0%" else ",.0f", gridcolor = "#f0f1f3"),
              yaxis = list(title = "", automargin = TRUE, tickfont = list(size = 10.5)),
              margin = list(l = 10, r = 10, t = 10, b = 25), showlegend = FALSE, bargap = .35)
})
