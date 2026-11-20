# =============================================================================
#  Page — Analyse des limites d'exposition & de concentration
#  (calculée sur le périmètre consolidé : les limites s'appliquent à la banque)
# =============================================================================

lm_data <- reactive(PORTFOLIO[PORTFOLIO$statut == "Actif", ])

lm_groups <- reactive({
  d <- lm_data()
  d %>% group_by(groupe) %>%
    summarise(secteur = names(which.max(tapply(encours, secteur, sum))),
              segment = names(which.max(tapply(encours, segment, sum))),
              agence = names(which.max(tapply(encours, agence, sum))),
              n_ent = n_distinct(emprunteur), stage = max(stage), collat = sum(collateral),
              encours = sum(encours), .groups = "drop") %>%
    mutate(souverain = secteur == "Administration & Souverain", ratio_fp = encours / FONDS_PROPRES) %>%
    arrange(desc(encours))
})

limits_table <- reactive({
  d <- lm_data(); g <- lm_groups(); tot <- sum(d$encours)
  share <- function(x) x / tot
  ns <- g[!g$souverain, ]
  big <- ns[ns$ratio_fp > SEUIL_GRAND_RISQUE, ]
  sec <- tapply(d$encours, d$secteur, sum); reg <- tapply(d$encours, d$region_code, sum); seg <- tapply(d$encours, d$segment, sum)
  breaches <- sum(d$approbateur == "Agence (hors délégation)")
  row <- function(cat, lbl, val, lim, kind, base) data.frame(categorie = cat, limite = lbl, valeur = val, plafond = lim,
                                                             type = kind, base = base, stringsAsFactors = FALSE)
  out <- rbind(
    row("Contrepartie", "Plus grande exposition individuelle (hors souverain) / fonds propres", max(ns$ratio_fp), LIMITE_INDIV, "pct", FONDS_PROPRES),
    row("Contrepartie", "Somme des grands risques (> 10 % FP) / fonds propres", sum(big$ratio_fp), LIMITE_GRANDS_RISQUES, "pct", FONDS_PROPRES),
    row("Contrepartie", "Top 20 clients / encours total", sum(head(g$encours, 20)) / tot, LIMITE_TOP20, "pct", tot),
    do.call(rbind, lapply(seq_len(nrow(SECTEURS)), function(i) {
      s <- SECTEURS$secteur[i]; row("Secteur", s, share(if (is.na(sec[s])) 0 else sec[[s]]), SECTEURS$limite[i], "pct", tot) })),
    do.call(rbind, lapply(seq_len(nrow(REGIONS)), function(i) {
      r <- REGIONS$region_code[i]; row("Région", REGIONS$region[i], share(if (is.na(reg[r])) 0 else reg[[r]]), LIMITES_REGION[[r]], "pct", tot) })),
    do.call(rbind, lapply(SEGMENTS, function(s) row("Segment", s, share(if (is.na(seg[s])) 0 else seg[[s]]), LIMITES_SEGMENT[[s]], "pct", tot))),
    row("Autres", "Encours en francs congolais (CDF) / encours total", share(sum(d$encours[d$devise == "CDF"])), LIMITE_CDF, "pct", tot),
    row("Autres", "Indice HHI sectoriel", hhi(sec), SEUIL_HHI, "num", NA),
    row("Autres", "Ratio NPL (Stage 3)", share(sum(d$encours[d$stage == 3])), SEUIL_NPL, "pct", tot),
    row("Autres", "Ratio PAR30", share(sum(d$encours[d$dpd >= 30])), SEUIL_PAR30, "pct", tot),
    row("Autres", sprintf("Dossiers approuvés en agence au-delà de %s", fmt_full(DELEGATION_AGENCE)), breaches, 0, "count", NA)
  )
  out$utilisation <- ifelse(out$plafond > 0, out$valeur / out$plafond, ifelse(out$valeur > 0, Inf, 0))
  out$marge <- ifelse(out$type == "pct", (out$plafond - out$valeur) * out$base, NA)
  out$statut <- ifelse(out$utilisation > 1, "Dépassé", ifelse(out$utilisation > .85, "Alerte", "Conforme"))
  out
})

fmt_lim <- function(v, type) ifelse(type == "pct", fmt_pct(v, 1), ifelse(type == "num", format(round(v), big.mark = " "), as.character(round(v))))

output$lm_kpis <- renderUI({
  t <- limits_table()
  n_dep <- sum(t$statut == "Dépassé"); n_al <- sum(t$statut == "Alerte")
  g <- lm_groups(); big <- g[!g$souverain & g$ratio_fp > SEUIL_GRAND_RISQUE, ]
  div(class = "kpi-row",
      kpi_card("Limites suivies", nrow(t), icon_name = "list-check", badge_txt = "Périmètre consolidé", badge_cls = "neutral",
               sub = "Contrepartie, secteurs, régions, segments, devise, qualité, délégations",
               foot = tagList(span("Fonds propres : ", tags$b(M(FONDS_PROPRES))))),
      kpi_card("Limites dépassées", n_dep, icon_name = "circle-xmark", value_cls = if (n_dep) "red" else "green",
               badge_txt = if (n_dep) "Escalade requise" else "Aucun dépassement", badge_cls = if (n_dep) "bad" else "ok",
               sub = paste(head(t$limite[t$statut == "Dépassé"], 3), collapse = " · ")),
      kpi_card("Limites en alerte (> 85 %)", n_al, icon_name = "triangle-exclamation", value_cls = if (n_al) "orange" else "green",
               badge_txt = "Pré-alerte", badge_cls = if (n_al) "warn" else "ok",
               sub = paste(head(t$limite[t$statut == "Alerte"], 3), collapse = " · ")),
      kpi_card("Grands risques (> 10 % FP)", nrow(big), icon_name = "users-rectangle",
               badge_txt = paste(fmt_pct(sum(big$ratio_fp), 0), "des FP"), badge_cls = if (sum(big$ratio_fp) > LIMITE_GRANDS_RISQUES) "bad" else "ok",
               sub = sprintf("Plus grande exposition (hors souverain) : %s des fonds propres", fmt_pct(max(g$ratio_fp[!g$souverain]), 1)),
               foot = tagList(span("Limite individuelle : ", tags$b(fmt_pct(LIMITE_INDIV, 0))),
                              span("Cumul max : ", tags$b(paste0(LIMITE_GRANDS_RISQUES * 100, " %")))))
  )
})

output$lm_util <- renderPlotly({
  t <- limits_table(); t <- t[is.finite(t$utilisation), ]
  t <- t[order(t$utilisation), ]
  col <- ifelse(t$statut == "Dépassé", COL$red, ifelse(t$statut == "Alerte", COL$orange, COL$s1))
  lbl <- ifelse(nchar(t$limite) > 42, paste0(substr(t$limite, 1, 40), "…"), t$limite)
  plot_ly(y = factor(lbl, unique(lbl)), x = t$utilisation, type = "bar", orientation = "h", marker = list(color = col),
          text = fmt_pct(t$utilisation, 0), textposition = "outside", cliponaxis = FALSE,
          customdata = paste(t$categorie, "—", t$limite), hovertemplate = "%{customdata}<br>Utilisation : %{x:.0%}<extra></extra>") %>%
    pl_layout(xaxis = list(tickformat = ".0%", range = c(0, max(1.25, max(t$utilisation) * 1.15)), gridcolor = "#f0f1f3"),
              yaxis = list(tickfont = list(size = 10), automargin = TRUE),
              shapes = list(list(type = "line", x0 = 1, x1 = 1, yref = "paper", y0 = 0, y1 = 1, line = list(color = COL$red, dash = "dash")),
                            list(type = "line", x0 = .85, x1 = .85, yref = "paper", y0 = 0, y1 = 1, line = list(color = COL$orange, dash = "dot"))),
              margin = list(l = 10, r = 40, t = 10, b = 30))
})

output$lm_table <- renderDT({
  t <- limits_table()
  if (!identical(input$lm_cat, "ALL") && !is.null(input$lm_cat)) t <- t[t$categorie == input$lm_cat, ]
  cls <- ifelse(t$statut == "Dépassé", "bad", ifelse(t$statut == "Alerte", "warn", "ok"))
  df <- data.frame(
    `Catégorie` = t$categorie, Limite = paste0("<b>", t$limite, "</b>"),
    `Valeur actuelle` = fmt_lim(t$valeur, t$type), Plafond = fmt_lim(t$plafond, t$type),
    Utilisation = ifelse(is.finite(t$utilisation),
                         mapply(function(v) paste0(gauge_bar(min(v, 1.5), 1, 1.5), sprintf('<span class="sub-cell">%s</span>', fmt_pct(v, 0))), t$utilisation),
                         '<span class="down"><b>Tolérance zéro</b></span>'),
    `Marge disponible` = ifelse(is.na(t$marge), "—", ifelse(t$marge >= 0, sapply(t$marge, M), paste0('<b class="down">', sapply(t$marge, M), "</b>"))),
    Statut = mapply(status_html, t$statut, cls), check.names = FALSE)
  dt_x(df, search = FALSE, page = TRUE, page_len = 15)
})

output$lm_large <- renderDT({
  g <- lm_groups(); g <- g[g$ratio_fp > SEUIL_GRAND_RISQUE, ]
  validate(need(nrow(g) > 0, "Aucun grand risque."))
  st <- ifelse(g$souverain, status_html("Exempté (souverain)", "neutral"),
        ifelse(g$ratio_fp > LIMITE_INDIV, status_html("Dépassement", "bad"),
        ifelse(g$ratio_fp > .85 * LIMITE_INDIV, status_html("Alerte", "warn"), status_html("Conforme", "ok"))))
  df <- data.frame(Groupe = sprintf("<b>%s</b>%s", g$groupe, ifelse(g$n_ent > 1, sprintf('<span class="sub-cell">%d entités liées</span>', g$n_ent), "")),
                   Secteur = g$secteur, Agence = g$agence, Encours = round(g$encours / div_(), 2),
                   `% fonds propres` = g$ratio_fp, `Marge vs limite 25 %` = ifelse(g$souverain, "—", sapply((LIMITE_INDIV - g$ratio_fp) * FONDS_PROPRES, M)),
                   `Couv. collat.` = g$collat / g$encours, Statut = st, check.names = FALSE)
  names(df)[4] <- paste0("Encours (", unit(), ")")
  dt_x(df, search = FALSE) %>% formatPercentage(c("% fonds propres", "Couv. collat."), 1) %>%
    formatStyle("% fonds propres", fontWeight = "bold", color = styleInterval(c(.85 * LIMITE_INDIV, LIMITE_INDIV), c(COL$ink, COL$orange, COL$red)))
})

output$lm_heat <- renderPlotly({
  d <- lm_data()
  m <- tapply(d$encours, list(factor(d$secteur, SECTEURS$secteur), factor(d$region, REGIONS$region)), sum)
  m[is.na(m)] <- 0; m <- m / sum(m)
  plot_ly(x = sub(" — ", "<br>", colnames(m)), y = rownames(m), z = m, type = "heatmap",
          colorscale = list(c(0, "#f8fafc"), c(.15, "#fde3b5"), c(.4, "#f59e0b"), c(1, "#b91c1c")),
          text = matrix(ifelse(m > 0, fmt_pct(m, 1), ""), nrow(m)), texttemplate = "%{text}", showscale = FALSE,
          hovertemplate = "%{y}<br>%{x}<br>%{z:.2%}<extra></extra>") %>%
    pl_layout(xaxis = list(tickfont = list(size = 10)), yaxis = list(tickfont = list(size = 10), automargin = TRUE, autorange = "reversed"),
              margin = list(l = 10, r = 10, t = 10, b = 50))
})

output$lm_headroom <- renderPlotly({
  t <- limits_table(); t <- t[t$categorie %in% c("Secteur", "Région", "Segment"), ]
  t <- t[order(t$marge), ]
  lbl <- paste0(t$limite, " (", t$categorie, ")")
  plot_ly(x = factor(lbl, lbl), y = t$marge / div_(), type = "bar",
          marker = list(color = ifelse(t$marge < 0, COL$red, ifelse(t$statut == "Alerte", COL$orange, "#1f3a93"))),
          hovertemplate = paste0("%{x}<br>%{y:,.1f} ", unit(), "<extra></extra>")) %>%
    pl_layout(yaxis = list(title = paste("Marge disponible", unit()), gridcolor = "#f0f1f3"),
              xaxis = list(tickangle = -40, tickfont = list(size = 9.5)), margin = list(l = 60, r = 10, t = 10, b = 140))
})

output$dl_limits <- downloadHandler(
  filename = function() paste0("limites_exposition_", format(ARRETE, "%Y%m%d"), ".csv"),
  content = function(file) {
    t <- limits_table()
    write.csv(data.frame(categorie = t$categorie, limite = t$limite, valeur = round(t$valeur, 4), plafond = t$plafond,
                         utilisation = round(t$utilisation, 4), marge_usd = round(t$marge), statut = t$statut),
              file, row.names = FALSE, fileEncoding = "UTF-8")
  }
)
