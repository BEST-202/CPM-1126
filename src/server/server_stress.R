# =============================================================================
#  Page 6 — Stress testing & RAROC
# =============================================================================

scenario <- reactive({
  s <- input$st_scenario %||% "3"
  if (s == "custom") {
    data.frame(scenario = "Personnalisé", poids = NA, cuivre = input$st_cu, cobalt = input$st_co, fx = input$st_fx,
               pib = input$st_pib / 100, taux_bcc = input$st_rate / 100, inflation = input$st_inf / 100)
  } else SCENARIOS[as.integer(s), ]
})

stress_res <- reactive(run_stress(loans(), scenario()))
bridge <- reactive(car_bridge(loans(), scenario()))

output$st_kpis <- renderUI({
  d <- loans(); r <- stress_res(); t <- which.min(r$car)
  car0 <- FONDS_PROPRES / RWA_TOTAL; car1 <- r$car[t]
  t1_0 <- TIER1 / RWA_TOTAL; t1_1 <- (TIER1 - (FONDS_PROPRES - r$cap[t])) / r$rwa_path[t]
  ecl0 <- sum(d$ecl); ecl1 <- sum(r$ecl_s)
  div(class = "kpi-row",
      kpi_card("CAR (fonds propres / RWA)", fmt_pct(car1), icon_name = "building-shield",
               value_cls = if (car1 < CAR_MIN) "red" else if (car1 < CAR_ALERT) "orange" else "",
               badge_txt = if (car1 >= CAR_MIN) "Conforme" else "Sous plancher", badge_cls = if (car1 >= CAR_MIN) "ok" else "bad",
               sub = HTML(sprintf("Après choc (point bas %s) · actuel <s>%s</s>", r$labels[t], fmt_pct(car0))),
               foot = tagList(span(class = "down", fmt_bps(car1 - car0)),
                              span("Marge vs plancher : ", tags$b(fmt_bps(car1 - CAR_MIN))))),
      kpi_card("Ratio Tier 1 (CET1 / RWA)", fmt_pct(t1_1), icon_name = "layer-group",
               value_cls = if (t1_1 < .085) "red" else "",
               badge_txt = if (t1_1 >= .085) "Sous vigilance" else "Insuffisant", badge_cls = if (t1_1 >= .085) "warn" else "bad",
               sub = HTML(sprintf("Après choc · actuel <s>%s</s>", fmt_pct(t1_0))),
               foot = tagList(span(class = "down", fmt_bps(t1_1 - t1_0)), span("Seuil légal : ", tags$b("8,50%")))),
      kpi_card("Stock pertes attendues (IFRS 9)", M(ecl1), icon_name = "fire", value_cls = "red",
               badge_txt = paste0("+", fmt_pct(ecl1 / ecl0 - 1, 1), " ECL"), badge_cls = "bad",
               sub = sprintf("Provision totale · stock actuel %s", M(ecl0)),
               foot = tagList(span(class = "down", paste0("+", M(ecl1 - ecl0), " choc net")),
                              span("Coût risque : ", tags$b(paste(round(ecl1 / sum(d$encours) * 1e4), "bps"))))),
      kpi_card("RWA (actifs pondérés)", M(r$rwa_s, 0), icon_name = "scale-balanced",
               badge_txt = "Inflation FX & PD", badge_cls = "warn",
               sub = sprintf("Projeté choc · actuel %s", M(RWA_TOTAL, 0)),
               foot = tagList(span(class = "down", sprintf("+%s (%s)", M(r$rwa_s - RWA_TOTAL, 0), fmt_pct(r$rwa_s / RWA_TOTAL - 1, 1))),
                              span("Multiplicateur PD moyen : ", tags$b(sprintf("×%.2f", weighted.mean(r$mult, d$encours))))))
  )
})

output$st_macro <- renderDT({
  sc <- scenario(); S <- SCENARIOS
  vars <- data.frame(
    v = c("Cours du cuivre LME (USD/t)", "Cours du cobalt (USD/t)", "Taux de change USD / CDF", "Croissance réelle PIB RDC",
          "Taux directeur Banque Centrale", "Inflation annuelle (IPC)"),
    sens = c("Exposition directe mines & sous-traitants", "Grands extracteurs Katanga & Lualaba",
             "Mismatch devises retail & PME locales", "Demande globale crédits & trésoreries",
             "Capacité de remboursement emprunteurs CDF", "Érosion pouvoir d'achat & marges PME"),
    key = c("cuivre", "cobalt", "fx", "pib", "taux_bcc", "inflation"), stringsAsFactors = FALSE)
  fm <- function(k, x) if (k %in% c("pib", "taux_bcc", "inflation")) fmt_pct(x, 1) else
    paste(format(round(x), big.mark = " "), if (k == "fx") "CDF" else "")
  delta <- function(k) {
    b <- S[[k]][1]; x <- sc[[k]]
    if (k %in% c("pib", "taux_bcc", "inflation")) fmt_bps(x - b)
    else sprintf("%s%s", ifelse(x >= b, "+", ""), fmt_pct(x / b - 1, 1))
  }
  df <- data.frame(`Vecteur macro-financier` = paste0("<b>", vars$v, "</b>"), `Sensibilité portefeuille` = vars$sens,
                   `Baseline (60%)` = mapply(fm, vars$key, S[1, vars$key]),
                   `Adverse modéré (25%)` = mapply(fm, vars$key, S[2, vars$key]),
                   `Sévère systémique (15%)` = mapply(fm, vars$key, S[3, vars$key]),
                   `Scénario actif` = sprintf("<b style='color:%s'>%s</b>", COL$red, mapply(fm, vars$key, sc[1, vars$key])),
                   `Variation / choc net` = sprintf("<b style='color:%s'>%s</b>", COL$red, sapply(vars$key, delta)),
                   check.names = FALSE)
  dt_x(df, search = FALSE)
})

output$st_car <- renderPlotly({
  r <- stress_res(); b <- run_stress(loans(), SCENARIOS[1, ])
  t <- which.min(r$car)
  plot_ly(x = factor(r$labels, r$labels)) %>%
    add_trace(y = b$car, type = "scatter", mode = "lines", name = "Trajectoire baseline", line = list(color = "#9ca3af", dash = "dot")) %>%
    add_trace(y = r$car, type = "scatter", mode = "lines+markers", name = "Trajectoire CAR stressée", fill = "tozeroy",
              fillcolor = "rgba(127,29,29,.08)", line = list(color = "#7f1d1d", width = 3), marker = list(size = 7, color = "#7f1d1d"),
              hovertemplate = "%{x}<br>CAR : %{y:.2%}<extra></extra>") %>%
    pl_layout(
      yaxis = list(title = "", tickformat = ".1%", range = c(min(.08, min(r$car) - .01), max(b$car) + .01), gridcolor = "#f0f1f3"),
      xaxis = list(title = ""),
      shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = CAR_MIN, y1 = CAR_MIN, line = list(color = COL$red, dash = "dash")),
                    list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = CAR_ALERT, y1 = CAR_ALERT, line = list(color = COL$orange, dash = "dash"))),
      annotations = list(
        list(x = r$labels[t], y = r$car[t], text = sprintf("<b>Creux %s</b>", fmt_pct(r$car[t])), showarrow = TRUE, ay = 30,
             bgcolor = COL$red, font = list(color = "#fff", size = 11)),
        list(xref = "paper", x = 1, y = CAR_MIN, text = "Seuil réglementaire (10,0 %)", showarrow = FALSE, xanchor = "right", yanchor = "bottom", font = list(size = 10, color = COL$red)),
        list(xref = "paper", x = 1, y = CAR_ALERT, text = "Alerte COMEX (12,5 %)", showarrow = FALSE, xanchor = "right", yanchor = "bottom", font = list(size = 10, color = COL$orange))))
})

output$st_car_note <- renderUI({
  r <- stress_res(); t <- which.min(r$car)
  coussin <- r$car[t] - CAR_MIN
  div(class = if (coussin > 0) "info-box" else "alert-box", icon("clipboard-check"), tags$b(" Résultat clé : "),
      if (coussin > 0) sprintf("le capital reste en permanence supérieur au seuil prudentiel de 10 %% avec un coussin minimal de %s (%s) au plus fort du choc (%s).",
                              fmt_bps(coussin), M(coussin * r$rwa_path[t]), r$labels[t])
      else sprintf("le CAR passe sous le plancher réglementaire au %s (%s). Activation immédiate du plan de contingence du capital.", r$labels[t], fmt_pct(r$car[t])))
})

output$st_bridge <- renderPlotly({
  b <- bridge()
  lbl <- c("CAR initial (Q3 2026)", names(b$impacts), "CAR résiduel sous stress")
  vals <- c(b$car0, b$impacts, b$car_final)
  meas <- c("absolute", rep("relative", length(b$impacts)), "total")
  plot_ly(type = "waterfall", orientation = "h", y = factor(lbl, rev(lbl)), x = vals, measure = meas,
          text = c(fmt_pct(b$car0), sprintf("%+d bps", round(b$impacts * 1e4)), fmt_pct(b$car_final)),
          textposition = "outside", cliponaxis = FALSE,
          connector = list(line = list(color = "#d1d5db")),
          decreasing = list(marker = list(color = COL$red)), increasing = list(marker = list(color = COL$green)),
          totals = list(marker = list(color = "#111827")), hovertemplate = "%{y}<br>%{text}<extra></extra>") %>%
    pl_layout(xaxis = list(tickformat = ".0%", range = c(0, b$car0 * 1.15), gridcolor = "#f0f1f3"),
              yaxis = list(automargin = TRUE, tickfont = list(size = 10.5)), margin = list(l = 10, r = 40, t = 10, b = 30))
})

output$st_sector <- renderPlotly({
  d <- loans(); r <- stress_res()
  g <- data.frame(secteur = d$secteur, de = r$ecl_s - d$ecl, m = r$mult * d$encours, e = d$encours) %>%
    group_by(secteur) %>% summarise(de = sum(de), mult = sum(m) / sum(e), .groups = "drop") %>% arrange(de)
  plot_ly(g, y = ~factor(secteur, secteur), x = ~de / div_(), type = "bar", orientation = "h", name = paste0("ΔECL (", unit(), ")"),
          marker = list(color = COL$red), hovertemplate = "%{y}<br>ΔECL : %{x:.1f}<extra></extra>") %>%
    add_trace(x = ~mult, type = "scatter", mode = "markers", name = "Multiplicateur PD (axe haut)", xaxis = "x2",
              marker = list(color = "#111827", size = 9, symbol = "diamond"), hovertemplate = "%{y}<br>×%{x:.2f}<extra></extra>") %>%
    pl_layout(xaxis = list(title = paste0("Hausse des provisions (", unit(), ")"), gridcolor = "#f0f1f3"),
              xaxis2 = list(overlaying = "x", side = "top", title = "", showgrid = FALSE, tickprefix = "×"),
              yaxis = list(title = "", automargin = TRUE), margin = list(l = 10, r = 10, t = 40, b = 40),
              legend = list(orientation = "h", y = -0.15))
})

output$st_targeted <- renderUI({
  d <- loans(); sc <- scenario()
  # reverse stress : prix du cuivre provoquant un CAR < 10 % (autres facteurs au scénario actif)
  rev_p <- NA
  for (p in seq(9400, 1000, by = -400)) {
    s <- sc; s$cuivre <- p; s$cobalt <- sc$cobalt * p / max(sc$cuivre, 1)
    if (min(run_stress(d, s)$car) < CAR_MIN) { rev_p <- p; break }
  }
  sov <- sum(d$encours[d$secteur == "Administration & Souverain"])
  sov_loss <- .20 * sov
  mines_dep <- .25 * sum(d$encours[d$secteur == "Mines & Métaux" & d$segment == "Corporate Grands Comptes"]) * .6
  lcr0 <- 1.42; lcr1 <- lcr0 - mines_dep / 900e6
  tagList(
    div(class = "target-card",
        div(class = "target-h", span("Choc 1 : liquidité & dépôts"), badge("Tension 30 jours", "warn")),
        div(class = "target-t", "Sorties massives secteur minier"),
        div(class = "seg-meta", "Retrait simulé de 25 % des dépôts corporate miniers sous 30 jours."),
        div(class = "kv", span("Ratio LCR actuel"), tags$b(fmt_pct(lcr0, 0))),
        div(class = "kv", span("LCR après choc 30j"), tags$b(class = if (lcr1 < 1) "down" else "warn-t", fmt_pct(lcr1, 0))),
        div(class = "kv", span("Seuil prudentiel"), tags$b("100%"))),
    div(class = "target-card",
        div(class = "target-h", span("Choc 2 : reverse stress test"), badge("Point de rupture", "bad")),
        div(class = "target-t", "Conditions de violation du seuil de 10 %"),
        div(class = "kv", span("Chute du cuivre requise"),
            tags$b(class = "down", if (is.na(rev_p)) "Non atteint (> -90 %)" else sprintf("< $%s /t (%s)", format(rev_p, big.mark = " "), fmt_pct(rev_p / 9400 - 1, 0)))),
        div(class = "kv", span("Autres facteurs"), tags$b(sc$scenario)),
        div(class = "kv", span("Probabilité de réalisation"), tags$b(if (is.na(rev_p) || rev_p < 4500) "< 1,2 %" else "≈ 3-5 %"))),
    div(class = "target-card",
        div(class = "target-h", span("Choc 3 : risque souverain RDC"), badge("Titres d'État", "neutral")),
        div(class = "target-t", "Haircut 20 % sur l'exposition souveraine"),
        div(class = "kv", span("Exposition titres & provinces"), tags$b(M(sov))),
        div(class = "kv", span("Perte nette constatée"), tags$b(class = "down", paste0("-", M(sov_loss)))),
        div(class = "kv", span("Impact CAR"), tags$b(class = "down", fmt_bps(-sov_loss / RWA_TOTAL)))),
    div(class = "warn-box", tags$b("Plan de contingence (seuil déclenchement CAR < 13,5 %) : "),
        "gel des dividendes, suspension des bonus variables, plafonnement des maturités > 3 ans dans l'extractif non couvert.")
  )
})

output$st_raroc <- renderDT({
  d <- loans(); r <- stress_res()
  d$de <- r$ecl_s - d$ecl
  d$cap_s <- d$capital * r$mult^.35
  g <- d %>% group_by(segment) %>%
    summarise(raroc = sum(revenu_net) / sum(capital),
              raroc_s = (sum(revenu_net) - sum(de) / 2) / sum(cap_s),
              eva = sum(revenu_net) - HURDLE * sum(capital),
              eva_s = sum(revenu_net) - sum(de) / 2 - HURDLE * sum(cap_s), encours = sum(encours), .groups = "drop")
  df <- data.frame(Segment = paste0("<b>", g$segment, "</b>"), Encours = round(g$encours / div_(), 1),
                   `RAROC baseline` = g$raroc, `RAROC stressé` = g$raroc_s, `Δ RAROC` = g$raroc_s - g$raroc,
                   `EVA baseline` = round(g$eva / div_(), 2), `EVA stressée` = round(g$eva_s / div_(), 2),
                   Arbitrage = ifelse(g$raroc_s >= HURDLE, status_html("Surpondérer", "ok"),
                               ifelse(g$raroc_s >= .08, status_html("Neutre / repricing", "warn"), status_html("Sous-traiter / réduire", "bad"))),
                   check.names = FALSE)
  names(df)[2] <- paste0("Encours (", unit(), ")")
  dt_x(df, search = FALSE) %>% formatPercentage(c("RAROC baseline", "RAROC stressé", "Δ RAROC"), 1) %>%
    formatStyle(c("RAROC baseline", "RAROC stressé"), fontWeight = "bold", color = styleInterval(HURDLE, c(COL$red, COL$green))) %>%
    formatStyle("Δ RAROC", color = COL$red) %>%
    formatStyle(c("EVA baseline", "EVA stressée"), color = styleInterval(0, c(COL$red, COL$green)))
})
