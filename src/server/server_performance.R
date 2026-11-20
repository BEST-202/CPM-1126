# =============================================================================
#  Page 5 — Performance RMs & Agences (scorecards & RAROC)
# =============================================================================

with_obj <- function(d) {
  r <- OBJECTIFS$objectif_prod / pmax(1, OBJECTIFS$production_ytd)
  d$objectif <- d$production_ytd * r[match(d$agence_code, OBJECTIFS$agence_code)]
  d
}

rm_perf <- reactive({
  d <- with_obj(loans())
  d %>% group_by(rm_id, rm) %>%
    summarise(agence = names(which.max(tapply(encours, agence, sum))),
              segment = names(which.max(tapply(encours, segment, sum))),
              secteur = names(which.max(tapply(encours, secteur, sum))),
              autor = sum(autorisation), prod = sum(production_ytd),
              par30 = sum(encours[dpd >= 30]) / sum(encours), s2 = sum(encours[stage == 2]),
              cor = sum(el) / sum(encours), raroc = sum(revenu_net) / sum(capital),
              eva = sum(revenu_net) - HURDLE * sum(capital),
              derog = sum(encours[!is.na(derogation)]) / sum(encours), n = n(),
              encours = sum(encours), .groups = "drop") %>%
    mutate(region = AGENCES$region[match(agence, AGENCES$agence)],
           statut = case_when(
             raroc < .10 & par30 > .07 ~ "Gel délégation / Malus",
             raroc < HURDLE | par30 > .06 ~ "Alerte / Bonus réduit (-50%)",
             par30 > SEUIL_PAR30 | derog > .05 ~ "Conforme / Surveillance",
             raroc > .25 & par30 < .02 ~ "Élite / Bonus 125%",
             TRUE ~ "Performant / Bonus 100%"))
})

output$pf_kpis <- renderUI({
  d <- with_obj(loans()); r <- rm_perf()
  raroc <- sum(d$revenu_net) / sum(d$capital)
  corp <- d$segment %in% c("Corporate Grands Comptes", "Secteur Public & Souverain")
  prod <- sum(d$production_ytd); obj <- sum(d$objectif, na.rm = TRUE)
  cor <- sum(d$el) / sum(d$encours)
  n_alert <- sum(r$cor > SEUIL_COR_ALERT)
  eva <- sum(d$revenu_net) - HURDLE * sum(d$capital)
  div(class = "kpi-row",
      kpi_card("RAROC moyen portefeuille", fmt_pct(raroc, 1), icon_name = "chart-simple",
               value_cls = if (raroc >= HURDLE) "" else "red",
               badge_txt = fmt_bps(raroc - HURDLE), badge_cls = if (raroc >= HURDLE) "ok" else "bad",
               sub = sprintf("vs hurdle %s", fmt_pct(HURDLE, 1)),
               foot = tagList(span("Corporate & Public : ", tags$b(class = "up", fmt_pct(sum(d$revenu_net[corp]) / sum(d$capital[corp]), 1))),
                              span("PME & Retail : ", tags$b(class = "warn-t", fmt_pct(sum(d$revenu_net[!corp]) / sum(d$capital[!corp]), 1))))),
      kpi_card("Production nouvelle YTD", M(prod), icon_name = "seedling",
               badge_txt = paste(fmt_pct(safe_div(prod, obj), 1), "cible"), badge_cls = if (prod >= obj) "ok" else "warn",
               sub = HTML(sprintf('<span class="%s">%s%s</span> vs objectif', if (prod >= obj) "up" else "down",
                                  ifelse(prod >= obj, "+", ""), M(prod - obj))),
               foot = tagList(span("Objectif annuel 2026 : ", tags$b(M(obj))),
                              span("Taux de tirage : ", tags$b(fmt_pct(sum(d$encours) / sum(d$autorisation), 1))))),
      kpi_card("Coût du risque imputé (RMs)", paste(round(cor * 1e4), "bps"), icon_name = "scale-unbalanced",
               badge_txt = sprintf(paste0("%d RMs > ", round(SEUIL_COR_ALERT*1e4), " bps"), n_alert), badge_cls = if (n_alert) "bad" else "ok",
               sub = sprintf("(%s de pertes attendues)", M(sum(d$el))),
               foot = tagList(span("Seuil tolérance : ", tags$b(paste(round(SEUIL_COR*1e4), "bps"))),
                              span("RMs en alerte : ", tags$b(class = "down", sprintf("%s (%d / %d)", fmt_pct(n_alert / nrow(r), 1), n_alert, nrow(r)))))),
      kpi_card("EVA globale générée", paste0(ifelse(eva >= 0, "+", ""), M(eva)), icon_name = "sack-dollar",
               value_cls = if (eva >= 0) "green" else "red",
               badge_txt = "Valeur économique nette", badge_cls = "neutral",
               sub = sprintf("Résultat net de risque %s – coût du capital %s", M(sum(d$revenu_net)), M(HURDLE * sum(d$capital))),
               foot = tagList(span("RMs créateurs de valeur : ", tags$b(class = "up", sprintf("%d / %d", sum(r$raroc > HURDLE), nrow(r))))))
  )
})

output$pf_scatter <- renderPlotly({
  r <- rm_perf()
  r$cor_bps <- r$cor * 1e4
  r$grp <- ifelse(r$raroc >= HURDLE, "Créateurs", "Destructeurs")
  mx <- max(220, max(r$cor_bps) * 1.05); my <- max(.35, max(r$raroc) * 1.08); mn <- min(0, min(r$raroc) * 1.1)
  r$txt <- sprintf("<b>%s</b><br>%s · %s<br>Encours : %s<br>RAROC : %s · CoR : %d bps<br>PAR30 : %s",
                   r$rm, r$agence, r$segment, sapply(r$encours, M), fmt_pct(r$raroc, 1), round(r$cor_bps), fmt_pct(r$par30, 1))
  top <- head(r[order(-r$encours), ], 3); worst <- head(r[order(r$raroc), ], 2)
  lab <- rbind(top, worst)
  plot_ly(r, x = ~cor_bps, y = ~raroc, type = "scatter", mode = "markers", color = ~grp,
          colors = c("Créateurs" = "#10b981", "Destructeurs" = COL$red),
          marker = list(size = 8 + 30 * sqrt(r$encours / max(r$encours)), opacity = .8, line = list(color = "#fff", width = 1)),
          text = ~txt, hoverinfo = "text") %>%
    pl_layout(
      xaxis = list(title = "Coût du risque (bps)", range = c(0, mx), gridcolor = "#f0f1f3", zeroline = FALSE),
      yaxis = list(title = "RAROC", tickformat = ".0%", range = c(mn, my), gridcolor = "#f0f1f3", zeroline = FALSE),
      shapes = list(
        list(type = "rect", x0 = 0, x1 = SEUIL_COR*1e4, y0 = .20, y1 = my, fillcolor = "rgba(16,185,129,.07)", line = list(width = 0), layer = "below"),
        list(type = "rect", x0 = SEUIL_COR*1e4, x1 = mx, y0 = mn, y1 = HURDLE, fillcolor = "rgba(185,28,28,.07)", line = list(width = 0), layer = "below"),
        list(type = "line", x0 = 0, x1 = mx, y0 = HURDLE, y1 = HURDLE, line = list(color = COL$red, dash = "dash", width = 1)),
        list(type = "line", x0 = SEUIL_COR*1e4, x1 = SEUIL_COR*1e4, y0 = mn, y1 = my, line = list(color = COL$red, dash = "dash", width = 1))),
      annotations = list(
        list(x = 3, y = my, text = "Q1 : ÉTOILES CRÉATRICES (RAROC > 20 %)", showarrow = FALSE, xanchor = "left", yanchor = "top", font = list(size = 9, color = COL$green)),
        list(x = mx, y = mn, text = "Q4 : ZONE CRITIQUE (DESTRUCTEURS)", showarrow = FALSE, xanchor = "right", yanchor = "bottom", font = list(size = 9, color = COL$red)),
        list(x = mx, y = HURDLE, text = sprintf("HURDLE RATE (%s)", fmt_pct(HURDLE, 1)), showarrow = FALSE, xanchor = "right", yanchor = "bottom", font = list(size = 9, color = COL$red)),
        list(x = SEUIL_COR*1e4, y = my, text = sprintf("SEUIL COÛT RISQUE (%d bps)", round(SEUIL_COR*1e4)), showarrow = FALSE, xanchor = "left", yanchor = "top", font = list(size = 9, color = COL$red))))
})

output$pf_gov <- renderUI({
  r <- rm_perf(); q4 <- sum(r$cor > SEUIL_COR_ALERT & r$raroc < .12)
  div(class = "alert-box", icon("gavel"), tags$b(" Règle de gouvernance Q4 2026 : "),
      sprintf("gel strict des primes d'octroi pour les %d portefeuilles du quadrant 4 (coût du risque > %d bps & RAROC < 12 %%). Majoration de +25 %% de bonus pour les flux générant un RAROC > 22 %% sans dégradation IFRS 9 Stage 2.", q4, round(SEUIL_COR_ALERT*1e4)))
})

output$pf_regions <- renderUI({
  d <- loans()
  g <- d %>% group_by(region) %>% summarise_risk() %>% arrange(desc(raroc))
  tagList(lapply(seq_len(nrow(g)), function(i) {
    r <- g[i, ]
    alert <- r$raroc < HURDLE * .8 || r$par30 > .06
    vig <- r$par30 > SEUIL_PAR30 || r$raroc < HURDLE
    score <- c("A+", "A", "B+", "B-", "C")[i]
    div(class = paste("reg-card", if (alert) "alert"),
        div(class = "reg-card-h", span(r$region, span(class = "sub-cell", REGIONS$zone[REGIONS$region == r$region])),
            badge(sprintf("Rang %d · Score %s", i, score), if (alert) "bad" else if (vig) "warn" else "ok")),
        div(class = "reg-card-g",
            div(div(class = "l", "Encours"), div(class = "v", M(r$encours))),
            div(div(class = "l", "PAR 30+"), div(class = "v", style = paste0("color:", if (r$par30 > .06) COL$red else if (r$par30 > SEUIL_PAR30) COL$orange else COL$green), fmt_pct(r$par30))),
            div(div(class = "l", "Coût risque"), div(class = "v", paste(round(r$cor * 1e4), "bps"))),
            div(div(class = "l", "RAROC"), div(class = "v", style = paste0("color:", if (r$raroc < HURDLE) COL$red else COL$green), fmt_pct(r$raroc, 1)))))
  }),
  div(class = "kv", span(sprintf("Cible nationale PAR 30+ : < %s", fmt_pct(SEUIL_PAR30))),
      tags$b(sprintf("Moyenne banque : %s", fmt_pct(sum(d$encours[d$dpd >= 30]) / sum(d$encours))))))
})

output$pf_rm_table <- renderDT({
  r <- rm_perf()
  f <- input$pf_rm_filter %||% "ALL"
  if (f == "TOP") r <- head(r[order(-r$raroc), ], 10)
  if (f == "COR") r <- r[r$cor > SEUIL_COR, ]
  if (f == "DER") r <- r[r$derog > .05, ]
  validate(need(nrow(r) > 0, "Aucun RM pour ce filtre."))
  cls <- ifelse(grepl("Gel", r$statut), "bad", ifelse(grepl("Alerte", r$statut), "warn",
         ifelse(grepl("Surveillance", r$statut), "warn", "ok")))
  dotc <- ifelse(cls == "bad", COL$red, ifelse(cls == "warn", COL$orange, COL$green))
  df <- data.frame(
    `Chargé d'affaires & pôle` = sprintf('<b><span style="color:%s">●</span> %s</b><span class="sub-cell">%s · %s</span>', dotc, r$rm, r$agence, r$secteur),
    Encours = round(r$encours / div_(), 2), `Octrois YTD` = round(r$prod / div_(), 2),
    `Tirage (%)` = r$encours / r$autor, `PAR 30+` = r$par30, `Stage 2 (SICR)` = round(r$s2 / div_(), 2),
    `Coût risque (bps)` = round(r$cor * 1e4), RAROC = r$raroc, `Dérogations` = r$derog,
    `Statut bonus risque` = status_html(r$statut, cls), check.names = FALSE)
  names(df)[2] <- paste0("EAD (", unit(), ")"); names(df)[3] <- paste0("Octrois YTD (", unit(), ")")
  names(df)[6] <- paste0("Stage 2 (", unit(), ")")
  dt_x(df, page = TRUE, page_len = 10, order = list(list(1, "desc"))) %>%
    formatPercentage(c("Tirage (%)", "PAR 30+", "RAROC", "Dérogations"), 1) %>%
    formatStyle("PAR 30+", color = styleInterval(c(.02, SEUIL_PAR30), c(COL$green, COL$ink, COL$red)), fontWeight = "bold") %>%
    formatStyle("RAROC", fontWeight = "bold", fontSize = "14px", color = styleInterval(HURDLE, c(COL$red, COL$green))) %>%
    formatStyle("Coût risque (bps)", color = styleInterval(SEUIL_COR*1e4, c(COL$ink, COL$red))) %>%
    formatStyle("Dérogations", color = styleInterval(.05, c(COL$ink, COL$orange)))
})

output$pf_agency_table <- renderDT({
  d <- with_obj(loans())
  g <- d %>% group_by(agence, region) %>%
    summarise(prod = sum(production_ytd), obj = sum(objectif, na.rm = TRUE),
              par30 = sum(encours[dpd >= 30]) / sum(encours), npl = sum(encours[stage == 3]) / sum(encours),
              cor = sum(el) / sum(encours), raroc = sum(revenu_net) / sum(capital),
              eva = sum(revenu_net) - HURDLE * sum(capital), encours = sum(encours), .groups = "drop")
  df <- data.frame(Agence = sprintf('<b>%s</b><span class="sub-cell">%s</span>', g$agence, g$region),
                   Encours = round(g$encours / div_(), 2), `Prod. / Objectif` = safe_div(g$prod, g$obj),
                   PAR30 = g$par30, NPL = g$npl, `CoR (bps)` = round(g$cor * 1e4), RAROC = g$raroc,
                   EVA = round(g$eva / div_(), 2), check.names = FALSE)
  names(df)[2] <- paste0("Encours (", unit(), ")"); names(df)[8] <- paste0("EVA (", unit(), ")")
  dt_x(df, page = TRUE, page_len = 10, order = list(list(1, "desc"))) %>%
    formatPercentage(c("Prod. / Objectif", "PAR30", "NPL", "RAROC"), 1) %>%
    formatStyle("RAROC", fontWeight = "bold", color = styleInterval(HURDLE, c(COL$red, COL$green))) %>%
    formatStyle("PAR30", color = styleInterval(c(SEUIL_PAR30, .06), c(COL$green, COL$orange, COL$red))) %>%
    formatStyle(names(df)[8], color = styleInterval(0, c(COL$red, COL$green)))
})

output$pf_derog <- renderUI({
  d <- loans(); tot <- sum(d$encours)
  g <- d %>% filter(!is.na(derogation)) %>% group_by(derogation) %>% summarise(n = n(), v = sum(encours), .groups = "drop")
  get <- function(k, f) { x <- g[[f]][g$derogation == k]; if (length(x)) x else 0 }
  tx <- sum(g$v) / tot
  tagList(
    div(class = "seg-meta", sprintf("%d dossiers actifs (%s) — surveillance des écarts aux grilles tarifaires et exigences de sûretés réelles.", sum(g$n), M(sum(g$v)))),
    div(class = "mini-grid", style = "grid-template-columns:repeat(3,1fr);margin-top:10px",
        lapply(c("Taux & Marge", "Collatéral < 120%", "Maturité"), function(k)
          div(class = "mini", div(class = "mini-h", k), div(class = "mini-v", paste(get(k, "n"), "dossiers")),
              div(class = "mini-s", M(get(k, "v")))))),
    div(class = if (tx < .08) "info-box" else "alert-box", icon("shield-halved"),
        sprintf(" Plafond dérogations pôle : 8,0 %% max de l'EAD — taux consolidé : %s (%s)", fmt_pct(tx, 1), if (tx < .08) "Conforme" else "Dépassement")),
    tags$hr(),
    div(class = "card-sub", tags$b("Plan d'assainissement commercial (Q4 2026)")),
    tags$ol(style = "font-size:12.5px;padding-left:18px;margin-top:6px",
            tags$li(tags$b("Recalibration pondération bonus 2027 : "), "RAROC porté de 30 % à 50 % dans l'évaluation des RMs."),
            tags$li(tags$b("Clause de clawback : "), "récupération des primes si un dossier migre en Stage 3 dans les 12 mois suivant l'octroi."),
            tags$li(tags$b("Audit spécial Région 4 — Est : "), "réévaluation des collatéraux agricoles et des expositions M2/M3."))
  )
})
