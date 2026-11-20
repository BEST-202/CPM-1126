# =============================================================================
#  Page 4 — Risque de concentration & HHI
# =============================================================================

groups <- reactive({
  d <- loans()
  d %>% group_by(groupe) %>%
    summarise(n_ent = n_distinct(emprunteur),
              secteur = names(which.max(tapply(encours, secteur, sum))),
              segment = names(which.max(tapply(encours, segment, sum))),
              region = names(which.max(tapply(encours, region, sum))),
              agence = names(which.max(tapply(encours, agence, sum))),
              pd = weighted.mean(pd12, encours), stage = max(stage), collat = sum(collateral),
              garantie = names(which.max(tapply(collateral, type_garantie, sum))),
              ecl = sum(ecl), lgd = weighted.mean(lgd, encours), dpd = max(dpd),
              encours = sum(encours), .groups = "drop") %>%
    arrange(desc(encours)) %>%
    mutate(rang = row_number(), part = encours / sum(encours), ratio_t1 = encours / TIER1,
           souverain = secteur == "Administration & Souverain",   # exempté de la limite grands risques
           rating = as.character(.rate_grade(pd)))
})

output$cc_kpis <- renderUI({
  d <- loans(); g <- groups(); s <- sector_shares()
  h <- hhi(s$encours); hg <- hhi(g$encours)
  top20 <- head(g, 20); tot <- sum(d$encours)
  mx <- max(c(0, g$ratio_t1[!g$souverain]))
  mines <- s[s$secteur == "Mines & Métaux", ]
  mines_v <- if (nrow(mines)) mines$encours else 0; mines_p <- if (nrow(mines)) mines$part else 0
  sub_m <- sum(d$encours[d$secteur == "Mines & Métaux" & d$segment == "PME"])
  div(class = "kpi-row",
      kpi_card("Indice HHI sectoriel & concentration", format(round(h), big.mark = " "), icon_name = "chart-pie",
               badge_txt = if (h < 1500) "Faible" else if (h < 2000) "Modéré" else "Élevé",
               badge_cls = if (h < 1500) "ok" else if (h < 2000) "warn" else "bad",
               sub = sprintf("/ %s max · HHI débiteurs uniques : %s", format(SEUIL_HHI, big.mark = " "), format(round(hg), big.mark = " ")),
               foot = if (mines_p > .2) span(class = "down", icon("triangle-exclamation"), sprintf(" Mines (%s > seuil 20%%)", fmt_pct(mines_p, 1)))),
      kpi_card("Ratio division risques (Top 20)", fmt_pct(sum(top20$encours) / tot, 1), icon_name = "users-rectangle",
               badge_txt = if (mx < LIMITE_INDIV) "Conforme" else "Dépassement", badge_cls = if (mx < LIMITE_INDIV) "ok" else "bad",
               sub = sprintf("Volume Top 20 : %s", M(sum(top20$encours))),
               foot = tagList(span("Max individuel (hors souverain) : ", tags$b(fmt_pct(mx, 1))), span("Plafond : ", tags$b(fmt_pct(LIMITE_INDIV, 0))))),
      kpi_card("Exposition mines & sous-traitance", M(mines_v), icon_name = "gem",
               value_cls = if (mines_p > .2) "red" else "", badge_txt = if (mines_p > .2) "Alerte sectorielle" else "Sous limite",
               badge_cls = if (mines_p > .2) "bad" else "ok",
               sub = sprintf("%s du total · dépassement : %s", fmt_pct(mines_p, 1), if (mines_p > .2) fmt_bps(mines_p - .2) else "aucun"),
               foot = tagList(span("PME sous-traitance : ", tags$b(M(sub_m))),
                              span("PAR30 : ", tags$b(class = "down", fmt_pct(safe_div(sum(d$encours[d$secteur == "Mines & Métaux" & d$dpd >= 30]), mines_v)))))),
      kpi_card("Couverture collatéraux Top 20", fmt_pct(sum(top20$collat) / sum(top20$encours), 1), icon_name = "shield-halved",
               value_cls = "green", badge_txt = "Sur-collatéralisé", badge_cls = "ok",
               sub = sprintf("%s de garanties totales", M(sum(top20$collat))))
  )
})

output$cc_sector_table <- renderDT({
  d <- loans(); s <- sector_shares()
  npl <- tapply(d$encours * (d$stage == 3), d$secteur, sum) / tapply(d$encours, d$secteur, sum)
  st <- ifelse(s$part > s$limite, status_html(paste0("Dépassé (", fmt_pp(s$part - s$limite, 1), ")"), "bad"),
        ifelse(s$part > .85 * s$limite, status_html("Sous contrôle", "warn"), status_html("Normal", "ok")))
  ic <- SECTEURS$icone[match(s$secteur, SECTEURS$secteur)]
  df <- data.frame(
    `Secteur économique` = sprintf('<i class="fa fa-%s" style="color:#9ca3af;width:18px"></i> %s', ic, s$secteur),
    Encours = round(s$encours / div_(), 1), `Part (%)` = s$part,
    `Seuil & jauge limite` = mapply(function(p, l) paste0(gauge_bar(p, l, .35), sprintf('<span class="sub-cell">%s · Max %s</span>', fmt_pct(p, 1), fmt_pct(l, 0))), s$part, s$limite),
    NPL = as.numeric(npl[s$secteur]), `Statut prudentiel` = st, check.names = FALSE)
  names(df)[2] <- paste0("Encours (", unit(), ")")
  dt_x(df, search = FALSE) %>% formatPercentage(c("Part (%)", "NPL"), 1) %>%
    formatStyle("NPL", color = styleInterval(c(.03, SEUIL_NPL), c(COL$green, COL$orange, COL$red)))
})

output$cc_gauge <- renderPlotly({
  h <- hhi(sector_shares()$encours)
  plot_ly(type = "indicator", mode = "gauge+number", value = h,
          number = list(font = list(size = 30, family = "Source Serif 4, Georgia, serif"), valueformat = ",.0f"),
          gauge = list(axis = list(range = list(0, 3000), tickwidth = 1, tickfont = list(size = 9)),
                       bar = list(color = "#111827", thickness = .25),
                       steps = list(list(range = c(0, 1500), color = "#c7ecd6"), list(range = c(1500, 2000), color = "#fde3b5"),
                                    list(range = c(2000, 3000), color = "#f8c4c4")),
                       threshold = list(line = list(color = COL$red, width = 3), thickness = .8, value = SEUIL_HHI))) %>%
    pl_layout(margin = list(l = 25, r = 25, t = 25, b = 5))
})

lorenz <- reactive({
  g <- groups(); x <- sort(g$encours)
  data.frame(p = c(0, seq_along(x) / length(x)), l = c(0, cumsum(x) / sum(x)))
})

output$cc_lorenz <- renderPlotly({
  lz <- lorenz()
  plot_ly(lz, x = ~p, y = ~l, type = "scatter", mode = "lines", name = "Courbe de Lorenz", fill = "tozeroy",
          fillcolor = "rgba(185,28,28,.08)", line = list(color = COL$red, width = 2.5),
          hovertemplate = "%{x:.1%} des débiteurs<br>%{y:.1%} de l'encours<extra></extra>") %>%
    add_trace(x = c(0, 1), y = c(0, 1), mode = "lines", name = "Égalité parfaite", fill = "none",
              line = list(color = "#9ca3af", dash = "dot", width = 1), inherit = FALSE, type = "scatter") %>%
    pl_layout(xaxis = list(title = "Part cumulée des débiteurs", tickformat = ".0%", gridcolor = "#f0f1f3"),
              yaxis = list(title = "Part cumulée de l'encours", tickformat = ".0%", gridcolor = "#f0f1f3"),
              margin = list(l = 55, r = 10, t = 10, b = 45))
})

output$cc_lorenz_note <- renderUI({
  g <- groups(); lz <- lorenz()
  gini <- 1 - sum((lz$p[-1] - lz$p[-nrow(lz)]) * (lz$l[-1] + lz$l[-nrow(lz)]))
  n <- nrow(g)
  top1 <- sum(head(g$encours, max(1, round(n * .01)))) / sum(g$encours)
  top10 <- sum(head(g$encours, max(1, round(n * .10)))) / sum(g$encours)
  tagList(
    div(class = "kv", span(sprintf("Top 1 %% débiteurs (%d groupes)", max(1, round(n * .01)))), tags$b(fmt_pct(top1, 1))),
    div(class = "kv", span(sprintf("Top 10 %% débiteurs (%d groupes)", max(1, round(n * .1)))), tags$b(fmt_pct(top10, 1))),
    div(class = "kv", span("Coefficient de Gini"), tags$b(formatC(gini, format = "f", digits = 2))),
    div(class = "info-box", icon("gavel"), tags$b(" Décision Risk Committee : "),
        "moratoire sur tout nouvel engagement bilatéral supérieur à $25M sans collatéral métallique LME ou hedging de premier rang.")
  )
})

output$cc_top20 <- renderDT({
  g <- head(groups(), 20)
  f <- input$cc_filter %||% "ALL"
  if (f == "Mines & Métaux") g <- g[g$secteur == f, ]
  else if (f == "Secteur Public & Souverain") g <- g[g$segment == f, ]
  else if (f == "ALERT") g <- g[g$ratio_t1 > .15 & !g$souverain, ]
  validate(need(nrow(g) > 0, "Aucun débiteur pour ce filtre."))
  stage_lbl <- ifelse(g$stage == 3, status_html("Stage 3 (NPL)", "bad"),
               ifelse(g$stage == 2, status_html("Stage 2 (SICR)", "warn"), status_html("Stage 1", "ok")))
  cov <- g$collat / g$encours
  statut <- ifelse(g$stage == 3, status_html(paste("Provision", fmt_pct(g$ecl / g$encours, 0)), "bad"),
            ifelse(g$souverain, status_html("Exemption souveraine", "neutral"),
            ifelse(g$ratio_t1 > LIMITE_INDIV, status_html("Dépassement", "bad"),
            ifelse(g$stage == 2, status_html(if_else(g$dpd >= 30, "Alerte EPD", "Watchlist"), "warn"),
            ifelse(g$ratio_t1 > .15, status_html("Surveillance réglem.", "warn"),
                   status_html(sprintf("Conforme (marge %s)", M(LIMITE_INDIV * TIER1 - g$encours)), "ok"))))))
  df <- data.frame(
    `Rang / Débiteur / Groupe` = sprintf("<b>%02d.</b> %s%s", g$rang, g$groupe,
                                         ifelse(g$n_ent > 1, sprintf('<span class="sub-cell">%d entités liées</span>', g$n_ent), "")),
    `Secteur & région` = sprintf('%s<span class="sub-cell">%s · %s</span>', g$secteur, sub(".*— ", "", g$region), g$agence),
    `Encours brut EAD` = round(g$encours / div_(), 2), `Part portef.` = g$part, `Ratio / Tier 1` = g$ratio_t1,
    `Rating` = g$rating, `IFRS 9` = stage_lbl,
    `Garanties & collatéraux` = sprintf("%s (%s)", M(g$collat), g$garantie),
    `Couv. %` = cov, `Statut prudentiel (max 25 %)` = statut, check.names = FALSE)
  names(df)[3] <- paste0("EAD (", unit(), ")")
  dt_x(df, search = FALSE) %>% formatPercentage(c("Part portef.", "Ratio / Tier 1", "Couv. %"), 1) %>%
    formatStyle("Ratio / Tier 1", fontWeight = "bold", color = styleInterval(c(.15, LIMITE_INDIV), c(COL$ink, COL$orange, COL$red))) %>%
    formatStyle("Couv. %", color = styleInterval(c(.9, 1.2), c(COL$red, COL$orange, COL$green))) %>%
    formatStyle("Rating", fontWeight = "bold", color = styleEqual(c("B", "B+", "CCC", "BB"), c(COL$red, COL$orange, COL$red, COL$orange), default = COL$green))
})

output$cc_heat <- renderPlotly({
  d <- loans()
  m <- tapply(d$encours, list(factor(d$segment, SEGMENTS), factor(d$region, REGIONS$region)), sum)
  m[is.na(m)] <- 0; m <- m / sum(m)
  plot_ly(x = sub(" — ", "<br>", colnames(m)), y = rownames(m), z = m, type = "heatmap",
          colorscale = list(c(0, "#f8fafc"), c(.15, "#fde3b5"), c(.5, "#f59e0b"), c(1, "#b91c1c")),
          text = matrix(fmt_pct(m, 1), nrow(m)), texttemplate = "%{text}", hovertemplate = "%{y}<br>%{x}<br>%{z:.1%}<extra></extra>",
          showscale = FALSE) %>%
    pl_layout(xaxis = list(tickfont = list(size = 10)), yaxis = list(tickfont = list(size = 10), automargin = TRUE),
              margin = list(l = 10, r = 10, t = 10, b = 50))
})

output$cc_single_name <- renderUI({
  g <- groups(); top <- g[!g$souverain, ][1, ]
  loss <- max(0, top$encours * .45 - top$ecl)
  car0 <- FONDS_PROPRES / RWA_TOTAL
  car1 <- (FONDS_PROPRES - loss) / (RWA_TOTAL - top$encours * 1)
  t1_0 <- TIER1 / RWA_TOTAL; t1_1 <- (TIER1 - loss) / (RWA_TOTAL - top$encours)
  res <- sum(PORTFOLIO$revenu_net[PORTFOLIO$statut == "Actif"])
  tagList(
    p(style = "font-size:12.5px", sprintf("Hypothèse : défaut immédiat du plus grand groupe débiteur (%s — %s EAD) avec une LGD de 45 %% après liquidation des garanties.",
                                         top$groupe, M(top$encours))),
    div(class = "mini-grid", style = "grid-template-columns:repeat(3,1fr)",
        div(class = "mini", div(class = "mini-h", "Perte brute nette"), div(class = "mini-v down", M(loss)),
            div(class = "mini-s", "45 % EAD – ECL existante")),
        div(class = "mini", div(class = "mini-h", "Impact ratio Tier 1"), div(class = "mini-v down", fmt_bps(t1_1 - t1_0)),
            div(class = "mini-s", sprintf("%s → %s", fmt_pct(t1_0), fmt_pct(t1_1)))),
        div(class = "mini", div(class = "mini-h", "Résultat annuel"), div(class = "mini-v down", fmt_pct(-loss / res, 1)),
            div(class = "mini-s", "du résultat net de risque"))),
    div(class = if (car1 > CAR_MIN) "info-box" else "alert-box", icon(if (car1 > CAR_MIN) "circle-check" else "triangle-exclamation"),
        sprintf(" Résilience %s : CAR post-choc %s (vs %s avant choc, plancher réglementaire %s).",
                if (car1 > CAR_MIN) "confirmée" else "insuffisante", fmt_pct(car1), fmt_pct(car0), fmt_pct(CAR_MIN, 1)))
  )
})
