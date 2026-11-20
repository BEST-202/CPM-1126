# =============================================================================
#  Page — Modèle de gestion des arriérés précoces
# =============================================================================

ar_all <- reactive({
  d <- loans()
  d$bucket <- bucket_of(d$dpd)
  d$bucket_m1 <- bucket_of(d$dpd_m1)
  d$p_bascule <- arrears_score(d)
  d
})

# population en arriérés précoces : 1 à 89 jours de retard
ar_pop <- reactive({
  d <- ar_all()
  e <- d[d$dpd >= 1 & d$dpd < 90, ]
  validate(need(nrow(e) > 0, "Aucun dossier en arriérés précoces dans ce périmètre."))
  e$classe <- as.character(score_class(e$p_bascule))
  e$perte_evitable <- e$encours * e$p_bascule * e$lgd
  e$strategie <- assign_strategy(e, e$p_bascule, median(e$encours))
  e$sla <- STRATEGIES$sla[match(e$strategie, STRATEGIES$strategie)]
  e[order(-e$perte_evitable), ]
})

flow_rate <- function(d, from, to_fun) {
  base <- d$encours[from]
  if (sum(base) == 0) return(NA_real_)
  sum(d$encours[from & to_fun]) / sum(base)
}

output$ar_kpis <- renderUI({
  d <- ar_all(); e <- ar_pop(); tot <- sum(d$encours)
  early <- sum(e$encours)
  entry <- flow_rate(d, d$dpd_m1 == 0, d$dpd > 0)
  cure  <- flow_rate(d, d$dpd_m1 >= 1 & d$dpd_m1 < 30, d$dpd == 0)
  r1    <- flow_rate(d, d$dpd_m1 >= 1 & d$dpd_m1 < 30, d$dpd >= 30)
  r2    <- flow_rate(d, d$dpd_m1 >= 30 & d$dpd_m1 < 60, d$dpd >= 60)
  r3    <- flow_rate(d, d$dpd_m1 >= 60 & d$dpd_m1 < 90, d$dpd >= 90)
  div(class = "kpi-row",
      kpi_card("Encours en arriérés précoces (1-89 j)", M(early), icon_name = "hourglass-half", value_cls = "orange",
               badge_txt = paste(fmt_pct(early / tot, 1), "du portefeuille"), badge_cls = "warn",
               sub = sprintf("%s dossiers · perte évitable estimée %s", format(nrow(e), big.mark = " "), M(sum(e$perte_evitable))),
               foot = tagList(span("1-29 j : ", tags$b(M(sum(e$encours[e$dpd < 30])))),
                              span("30-89 j : ", tags$b(class = "down", M(sum(e$encours[e$dpd >= 30])))))),
      kpi_card("Taux d'entrée en impayés", fmt_pct(entry), icon_name = "door-open",
               badge_txt = "Courant → 1 j+", badge_cls = if (entry > .03) "bad" else "warn",
               sub = sprintf("%s d'encours sain tombé en retard ce mois", M(sum(d$encours[d$dpd_m1 == 0 & d$dpd > 0]))),
               foot = tagList(span("Dont crédits < 12 mois : ",
                                   tags$b(fmt_pct(safe_div(sum(d$encours[d$dpd_m1 == 0 & d$dpd > 0 & d$mob < 12]),
                                                           sum(d$encours[d$dpd_m1 == 0 & d$dpd > 0])), 0))))),
      kpi_card("Taux d'auto-régularisation", fmt_pct(cure, 1), icon_name = "rotate-left", value_cls = "green",
               badge_txt = "Self-cure 1-29 j", badge_cls = "ok",
               sub = sprintf("%s régularisés sans action lourde", M(sum(d$encours[d$dpd_m1 >= 1 & d$dpd_m1 < 30 & d$dpd == 0]))),
               foot = tagList(span("Clients injoignables : ", tags$b(class = "down", fmt_pct(mean(!e$joignable), 1))))),
      kpi_card("Taux de bascule (roll-forward)", fmt_pct(r1, 1), icon_name = "arrow-trend-up", value_cls = "red",
               badge_txt = "1-29 j → 30 j+", badge_cls = "bad",
               sub = "Part de l'encours en retard précoce qui aggrave son retard au-delà de 30 jours",
               foot = tagList(span("30-59 → 60+ : ", tags$b(fmt_pct(r2, 1))), span("60-89 → NPL : ", tags$b(class = "down", fmt_pct(r3, 1)))))
  )
})

output$ar_buckets <- renderPlotly({
  d <- ar_all()
  b <- BUCKETS[-1]
  enc <- sapply(b, function(x) sum(d$encours[d$bucket == x]))
  idx <- match(as.character(d$bucket), BUCKETS)
  roll <- sapply(BUCKETS[-length(BUCKETS)], function(x) {
    from <- d$bucket_m1 == x
    if (!any(from)) return(NA)
    sum(d$encours[from & idx > match(x, BUCKETS)]) / sum(d$encours[from])
  })
  plot_ly() %>%
    add_bars(x = factor(b, b), y = enc / div_(), name = paste0("Encours (", unit(), ")"),
             marker = list(color = BUCKET_COLS[-1]), hovertemplate = paste0("%{x}<br>%{y:,.1f} ", unit(), "<extra></extra>")) %>%
    add_trace(x = factor(b[-length(b)], b), y = roll[-1], type = "scatter", mode = "lines+markers+text", yaxis = "y2",
              name = "Taux de bascule vers la tranche suivante", line = list(color = "#111827", width = 2.5),
              marker = list(size = 8, color = "#111827"), text = fmt_pct(roll[-1], 0), textposition = "top center",
              textfont = list(size = 10), hovertemplate = "%{x} → tranche supérieure : %{y:.1%}<extra></extra>") %>%
    pl_layout(yaxis = list(title = unit(), gridcolor = "#f0f1f3"),
              yaxis2 = list(overlaying = "y", side = "right", tickformat = ".0%", range = c(0, 1.05), showgrid = FALSE),
              xaxis = list(title = ""), margin = list(l = 50, r = 45, t = 30, b = 40))
})

output$ar_rollrates <- renderDT({
  d <- ar_all()
  m <- tapply(d$encours, list(d$bucket_m1, d$bucket), sum)
  m[is.na(m)] <- 0
  m <- m[rowSums(m) > 0, , drop = FALSE]
  mode <- input$ar_rr_mode %||% "pct"
  cells <- sapply(colnames(m), function(cn) sapply(rownames(m), function(rn) {
    v <- m[rn, cn]; p <- v / sum(m[rn, ])
    if (v == 0) return('<span style="color:#d1d5db">—</span>')
    i <- match(rn, BUCKETS); j <- match(cn, BUCKETS)
    col <- if (i == j) "#111827" else if (j < i) COL$green else COL$red
    bg <- if (j > i) sprintf("rgba(185,28,28,%.2f)", min(.35, p * .6)) else if (j < i) sprintf("rgba(11,138,79,%.2f)", min(.3, p * .5)) else "transparent"
    sprintf('<div style="background:%s;padding:2px 4px;border-radius:3px"><b style="color:%s">%s</b></div>', bg, col,
            if (mode == "pct") fmt_pct(p, 1) else M(v))
  }))
  if (is.null(dim(cells))) cells <- matrix(cells, nrow = 1, dimnames = list(rownames(m), colnames(m)))
  df <- data.frame(`M-1 \\ M` = paste0("<b>", rownames(m), "</b>"), cells, `Encours M-1` = sapply(rowSums(m), M), check.names = FALSE)
  names(df)[2:(ncol(df) - 1)] <- colnames(m)
  dt_x(df, search = FALSE) %>% formatStyle(seq_len(ncol(df)), `text-align` = "center", fontSize = "11.5px")
})

output$ar_coef <- renderDT({
  z <- SCORE_COEF / SCORE_MODEL$se
  df <- data.frame(Variable = names(SCORE_COEF), `Coefficient (β)` = round(SCORE_COEF, 3),
                   `Signif.` = ifelse(is.finite(z) & abs(z) > 2.58, "***", ifelse(is.finite(z) & abs(z) > 1.96, "**", ifelse(is.finite(z) & abs(z) > 1.64, "*", ""))),
                   `Odds ratio` = ifelse(names(SCORE_COEF) == "Constante", NA, round(exp(SCORE_COEF), 2)),
                   Effet = ifelse(names(SCORE_COEF) == "Constante", "",
                                  ifelse(SCORE_COEF > 0, status_html("↑ risque", "bad"), status_html("↓ risque", "ok"))),
                   check.names = FALSE)
  dt_x(df, search = FALSE)
})

bt <- reactive(backtest(ar_all()))

output$ar_backtest <- renderUI({
  b <- bt()
  if (is.null(b)) return(div(class = "info-box", "Échantillon insuffisant pour le backtest."))
  q <- if (b$gini > .5) c("Bon pouvoir discriminant", "ok") else if (b$gini > .3) c("Pouvoir discriminant acceptable", "warn") else c("Pouvoir discriminant faible", "bad")
  tagList(
    div(class = "mini-grid", style = "grid-template-columns:repeat(4,1fr);margin-top:10px",
        div(class = "mini", div(class = "mini-h", "AUC"), div(class = "mini-v", formatC(b$auc, format = "f", digits = 2))),
        div(class = "mini", div(class = "mini-h", "Gini"), div(class = "mini-v", formatC(b$gini, format = "f", digits = 2))),
        div(class = "mini", div(class = "mini-h", "KS"), div(class = "mini-v", formatC(b$ks, format = "f", digits = 2))),
        div(class = "mini", div(class = "mini-h", "Taux observé"), div(class = "mini-v", fmt_pct(b$taux, 1)))),
    div(class = "info-box", badge(q[1], q[2]),
        sprintf(" Backtest hors échantillon (30 %% de test) sur %s dossiers en retard ou sous surveillance à M-1. Modèle estimé par régression logistique sur les 70 %% restants. Seuils de décision : classe A < 10 %%, B 10-25 %%, C 25-50 %%, D ≥ 50 %%.",
                format(b$n, big.mark = " ")))
  )
})

output$ar_calib <- renderPlotly({
  b <- bt()
  validate(need(!is.null(b), "Échantillon insuffisant pour le backtest."))
  c <- b$cal
  plot_ly(c, x = ~factor(decile)) %>%
    add_bars(y = ~predit, name = "Probabilité prédite (score)", marker = list(color = "#1f3a93"),
             hovertemplate = "Décile %{x}<br>Prédit : %{y:.1%}<extra></extra>") %>%
    add_bars(y = ~observe, name = "Bascule observée", marker = list(color = COL$red),
             hovertemplate = "Décile %{x}<br>Observé : %{y:.1%}<extra></extra>") %>%
    pl_layout(barmode = "group", xaxis = list(title = "Décile de score (1 = moins risqué, 10 = plus risqué)"),
              yaxis = list(title = "", tickformat = ".0%", gridcolor = "#f0f1f3"))
})

output$ar_classes <- renderUI({
  e <- ar_pop()
  g <- e %>% group_by(classe) %>% summarise(n = n(), p = mean(p_bascule), pe = sum(perte_evitable), encours = sum(encours), .groups = "drop")
  cls <- c("A — Faible" = "ok", "B — Modéré" = "neutral", "C — Élevé" = "warn", "D — Critique" = "bad")
  div(class = "mini-grid", style = "grid-template-columns:repeat(4,1fr);margin-top:8px",
      lapply(seq_len(nrow(g)), function(i) div(class = "mini",
        div(class = "mini-h", badge(g$classe[i], cls[[g$classe[i]]])),
        div(class = "mini-v", format(g$n[i], big.mark = " "), tags$small(style = "font-size:11px", " dossiers")),
        div(class = "mini-s", sprintf("%s · P moy. %s", M(g$encours[i]), fmt_pct(g$p[i], 0))))))
})

output$ar_strategy <- renderUI({
  e <- ar_pop()
  div(class = "strat-grid", lapply(seq_len(nrow(STRATEGIES)), function(i) {
    s <- STRATEGIES[i, ]; x <- e[e$strategie == s$strategie, ]
    div(class = paste("strat", s$cls),
        div(class = "strat-h", icon(s$icone), span(s$quadrant)),
        div(class = "strat-t", s$strategie),
        div(class = "strat-n", format(nrow(x), big.mark = " "), tags$small(" dossiers")),
        div(class = "kv", span("Encours"), tags$b(M(sum(x$encours)))),
        div(class = "kv", span("Perte évitable"), tags$b(class = "down", M(sum(x$perte_evitable)))),
        div(class = "kv", span("SLA"), tags$b(s$sla)),
        p(class = "strat-a", s$action))
  }))
})

capacity <- reactive((input$ar_agents %||% 3) * (input$ar_prod %||% 6) * 22)

output$ar_gains <- renderPlotly({
  e <- ar_pop(); N <- nrow(e)
  cum <- c(0, cumsum(e$perte_evitable)) / sum(e$perte_evitable)
  x <- 0:N
  cap <- min(capacity(), N)
  plot_ly() %>%
    add_trace(x = x, y = cum, type = "scatter", mode = "lines", name = "Priorisation par score", fill = "tozeroy",
              fillcolor = "rgba(31,58,147,.08)", line = list(color = "#1f3a93", width = 3),
              hovertemplate = "%{x} dossiers<br>%{y:.1%} de la perte évitable<extra></extra>") %>%
    add_trace(x = c(0, N), y = c(0, 1), type = "scatter", mode = "lines", name = "Traitement au hasard",
              line = list(color = "#9ca3af", dash = "dot")) %>%
    pl_layout(xaxis = list(title = "Nombre de dossiers traités sur le mois", gridcolor = "#f0f1f3"),
              yaxis = list(title = "", tickformat = ".0%", gridcolor = "#f0f1f3"),
              shapes = list(list(type = "line", x0 = cap, x1 = cap, y0 = 0, y1 = 1, line = list(color = COL$red, dash = "dash"))),
              annotations = list(list(x = cap, y = cum[cap + 1], text = sprintf("<b>Capacité : %s dossiers → %s</b>", format(cap, big.mark = " "), fmt_pct(cum[cap + 1], 0)),
                                      showarrow = TRUE, ax = 60, ay = 30, bgcolor = "#fff", bordercolor = COL$red, font = list(size = 11))))
})

output$ar_impact <- renderUI({
  e <- ar_pop(); N <- nrow(e); cap <- min(capacity(), N); eff <- (input$ar_eff %||% 35) / 100
  top <- head(e, cap)
  avoided <- sum(top$perte_evitable) * eff
  random <- sum(e$perte_evitable) * cap / N * eff
  npl_av <- sum(top$encours * top$p_bascule) * eff
  days <- N / ((input$ar_agents %||% 3) * (input$ar_prod %||% 6))
  d <- loans()
  tagList(
    div(class = "kv", span("Dossiers en arriérés précoces"), tags$b(format(N, big.mark = " "))),
    div(class = "kv", span("Capacité mensuelle (22 jours ouvrés)"), tags$b(format(capacity(), big.mark = " "))),
    div(class = "kv", span("Taux de couverture du stock"), tags$b(fmt_pct(cap / N, 0))),
    div(class = "kv", span("Délai pour traiter tout le stock"), tags$b(sprintf("%.1f jours ouvrés", days))),
    tags$hr(),
    div(class = "mini-grid",
        div(class = "mini", div(class = "mini-h", "Perte évitée / mois"), div(class = "mini-v up", M(avoided, 2)),
            div(class = "mini-s", sprintf("vs %s au hasard (×%.1f)", M(random, 2), safe_div(avoided, random)))),
        div(class = "mini", div(class = "mini-h", "Encours sauvé du NPL"), div(class = "mini-v up", M(npl_av)),
            div(class = "mini-s", sprintf("NPL : %s → %s", fmt_pct(sum(d$encours[d$stage == 3]) / sum(d$encours)),
                                          fmt_pct((sum(d$encours[d$stage == 3]) - npl_av * .35) / sum(d$encours)))))),
    div(class = if (cap < N) "warn-box" else "info-box",
        if (cap < N) sprintf("Capacité insuffisante : %s dossiers non traités ce mois. Les classes A/B basculent vers la relance digitale automatique.", format(N - cap, big.mark = " "))
        else "Capacité suffisante pour traiter l'ensemble du stock d'arriérés précoces ce mois.")
  )
})

output$ar_region <- renderPlotly({
  d <- ar_all()
  d <- d[d$dpd >= 1 & d$dpd < 90, ]
  validate(need(nrow(d) > 0, "Aucun arriéré précoce."))
  regs <- intersect(REGIONS$region, unique(d$region))
  b <- BUCKETS[2:6]
  p <- plot_ly()
  for (i in seq_along(b)) {
    v <- sapply(regs, function(r) sum(d$encours[d$region == r & d$bucket == b[i]]))
    p <- p %>% add_bars(y = factor(regs, rev(regs)), x = v / div_(), orientation = "h", name = b[i],
                        marker = list(color = BUCKET_COLS[i + 1]), hovertemplate = paste0("%{y}<br>", b[i], " : %{x:,.1f} ", unit(), "<extra></extra>"))
  }
  p %>% pl_layout(barmode = "stack", xaxis = list(title = unit(), gridcolor = "#f0f1f3"), yaxis = list(title = "", automargin = TRUE),
                  margin = list(l = 10, r = 10, t = 10, b = 40)) %>%
    plotly::layout(legend = list(orientation = "h", x = 0, y = -0.18))
})

output$ar_agencies <- renderDT({
  d <- ar_all()
  g <- d %>% group_by(agence, region) %>%
    summarise(early = sum(encours[dpd >= 1 & dpd < 30]) / sum(encours),
              entree = safe_div(sum(encours[dpd_m1 == 0 & dpd > 0]), sum(encours[dpd_m1 == 0])),
              cure = safe_div(sum(encours[dpd_m1 >= 1 & dpd_m1 < 30 & dpd == 0]), sum(encours[dpd_m1 >= 1 & dpd_m1 < 30])),
              roll = safe_div(sum(encours[dpd_m1 >= 1 & dpd_m1 < 30 & dpd >= 30]), sum(encours[dpd_m1 >= 1 & dpd_m1 < 30])),
              score = weighted.mean(p_bascule[dpd >= 1 & dpd < 90], encours[dpd >= 1 & dpd < 90]),
              encours = sum(encours), .groups = "drop") %>%
    arrange(desc(early))
  statut <- ifelse(g$early > .10 | (!is.na(g$roll) & g$roll > .5), status_html("Cellule de crise", "bad"),
            ifelse(g$early > .05, status_html("Renforcer relances", "warn"), status_html("Sous contrôle", "ok")))
  df <- data.frame(Agence = sprintf('<b>%s</b><span class="sub-cell">%s</span>', g$agence, g$region),
                   Encours = round(g$encours / div_(), 1), `Arriérés 1-29 j` = g$early, `Taux d'entrée` = g$entree,
                   `Self-cure` = g$cure, `Bascule 30 j+` = g$roll, `Score moyen` = g$score, Statut = statut, check.names = FALSE)
  names(df)[2] <- paste0("Encours (", unit(), ")")
  dt_x(df, page = TRUE, page_len = 8) %>%
    formatPercentage(c("Arriérés 1-29 j", "Taux d'entrée", "Self-cure", "Bascule 30 j+", "Score moyen"), 1) %>%
    formatStyle("Arriérés 1-29 j", fontWeight = "bold", color = styleInterval(c(.05, .10), c(COL$green, COL$orange, COL$red))) %>%
    formatStyle("Self-cure", color = styleInterval(.5, c(COL$orange, COL$green))) %>%
    formatStyle("Bascule 30 j+", color = styleInterval(c(.3, .5), c(COL$ink, COL$orange, COL$red)))
})

ar_queue_data <- reactive({
  e <- ar_pop()
  f <- input$ar_queue_filter %||% "ALL"
  if (f == "HIGH") e <- e[e$p_bascule >= .25, ]
  else if (f == "UNREACH") e <- e[!e$joignable, ]
  else if (f != "ALL") e <- e[e$strategie == f, ]
  e
})

output$ar_queue <- renderDT({
  e <- head(ar_queue_data(), 400)
  validate(need(nrow(e) > 0, "Aucun dossier pour ce filtre."))
  cls_map <- c("A — Faible" = "ok", "B — Modéré" = "neutral", "C — Élevé" = "warn", "D — Critique" = "bad")
  st_cls <- STRATEGIES$cls[match(e$strategie, STRATEGIES$strategie)]
  df <- data.frame(
    Rang = seq_len(nrow(e)),
    Dossier = sprintf('<b>%s</b><span class="sub-cell">%s · %s</span>', e$emprunteur, e$loan_id, e$segment),
    `Agence / RM` = sprintf('%s<span class="sub-cell">%s</span>', e$agence, e$rm),
    Encours = round(e$encours / div_(), 3), `Jours de retard` = e$dpd,
    `P(bascule)` = e$p_bascule, Classe = mapply(status_html, e$classe, cls_map[e$classe]),
    `Perte évitable` = round(e$perte_evitable / div_(), 3),
    `Stratégie` = mapply(status_html, e$strategie, st_cls), SLA = e$sla,
    Contact = ifelse(e$joignable, '<span class="up">Joignable</span>', '<span class="down">Injoignable — visite</span>'),
    check.names = FALSE)
  names(df)[4] <- paste0("Encours (", unit(), ")"); names(df)[8] <- paste0("Perte évitable (", unit(), ")")
  dt_x(df, page = TRUE, page_len = 12) %>% formatPercentage("P(bascule)", 1) %>%
    formatStyle("P(bascule)", fontWeight = "bold", color = styleInterval(c(.25, .5), c(COL$ink, COL$orange, COL$red))) %>%
    formatStyle("Jours de retard", color = styleInterval(c(29, 59), c(COL$orange, COL$red, COL$red)))
})

output$dl_arrears_queue <- downloadHandler(
  filename = function() paste0("file_actions_recouvrement_", format(ARRETE, "%Y%m%d"), ".csv"),
  content = function(file) {
    e <- ar_queue_data()
    write.csv(data.frame(rang = seq_len(nrow(e)), loan_id = e$loan_id, client = e$emprunteur, segment = e$segment,
                         region = e$region, agence = e$agence, rm = e$rm, encours_usd = round(e$encours),
                         jours_retard = e$dpd, retard_m1 = e$dpd_m1, p_bascule = round(e$p_bascule, 4), classe = e$classe,
                         perte_evitable_usd = round(e$perte_evitable), strategie = e$strategie, sla = e$sla,
                         joignable = e$joignable), file, row.names = FALSE, fileEncoding = "UTF-8")
  }
)
