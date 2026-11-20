# =============================================================================
#  Page 3 — Vintage curves & staging IFRS 9
# =============================================================================

tm <- reactive(transition_matrix(loans_all()))

output$vt_kpis <- renderUI({
  d <- loans(); da <- loans_all(); m <- tm()
  ecl <- sum(d$ecl); ecl_q2 <- sum(da$ecl_q2)
  v <- vint(); r26 <- v$taux[v$cohorte == "2026" & v$mob == 6]; r26 <- if (length(r26)) r26 else NA
  s1q2 <- sum(m["Stage 1", ]); mig12 <- if ("Stage 1" %in% rownames(m)) m["Stage 1", "Stage 2"] else 0
  s2q2 <- if ("Stage 2" %in% rownames(m)) sum(m["Stage 2", ]) else 0
  cure <- if (s2q2 > 0) m["Stage 2", "Stage 1"] / s2q2 else NA
  mot <- d %>% filter(stage == 2) %>% count(sicr_motif, wt = encours) %>% mutate(p = n / sum(n))
  getm <- function(k) { x <- mot$p[mot$sicr_motif == k]; if (length(x)) fmt_pct(x, 1) else "0%" }
  div(class = "kpi-row",
      kpi_card("Total stock provisions ECL", M(ecl), icon_name = "vault",
               badge_txt = paste("Couverture NPL :", fmt_pct(safe_div(ecl, sum(d$encours[d$stage == 3])), 1)), badge_cls = "ok",
               sub = HTML(sprintf('<span class="%s">%s</span> vs Q2 · Portefeuille brut : %s',
                                  if (ecl > ecl_q2) "down" else "up", paste0(ifelse(ecl > ecl_q2, "+", ""), M(ecl - ecl_q2)), M(sum(d$encours)))),
               foot = tagList(
                 span(sprintf("St.1 (PD 12m %s) : ", fmt_pct(weighted.mean(d$pd12[d$stage == 1], d$encours[d$stage == 1]))), tags$b(M(sum(d$ecl[d$stage == 1])))),
                 span(sprintf("St.2 (lifetime %s) : ", fmt_pct(weighted.mean(d$pd_lt[d$stage == 2], d$encours[d$stage == 2]))), tags$b(class = "warn-t", M(sum(d$ecl[d$stage == 2])))),
                 span(sprintf("St.3 (LGD %s) : ", fmt_pct(weighted.mean(d$lgd[d$stage == 3], d$encours[d$stage == 3]), 1)), tags$b(class = "down", M(sum(d$ecl[d$stage == 3])))))),
      kpi_card("Vitesse dégradation (EPD M6)", fmt_pct(r26), icon_name = "gauge-high",
               value_cls = if (isTRUE(r26 > SEUIL_EPD)) "red" else "green",
               badge_txt = paste("Seuil >", fmt_pct(SEUIL_EPD)), badge_cls = if (isTRUE(r26 > SEUIL_EPD)) "bad" else "ok",
               sub = HTML(sprintf("<b>%s</b> vs seuil · sur-défaut précoce concentré sur les cohortes 2026",
                                  if (is.na(r26)) "—" else fmt_bps(r26 - SEUIL_EPD))),
               foot = tagList(span("Seuil de tolérance : ", tags$b(fmt_pct(SEUIL_EPD))), span("Actuel : ", tags$b(class = "down", fmt_pct(r26))))),
      kpi_card("Migration nette S1 → S2 (SICR)", fmt_pct(safe_div(mig12, s1q2)), icon_name = "arrow-trend-down",
               badge_txt = "Trimestriel Q3", badge_cls = "warn",
               sub = HTML(sprintf("du livre S1 · <b class='down'>+%s</b> transférés", M(mig12))),
               foot = tagList(span("Arriérés 30-89j : ", tags$b(getm("Arriérés 30-89 jours"))),
                              span("Watchlist : ", tags$b(getm("Watchlist qualitative"))),
                              span("Forbearance : ", tags$b(getm("Forbearance / Restructuré"))))),
      kpi_card("Taux de guérison (cure rate)", fmt_pct(cure, 1), icon_name = "heart-pulse", value_cls = "green",
               badge_txt = "S2 → S1", badge_cls = "ok",
               sub = HTML(sprintf("Reprise de provisions : <b class='up'>%s</b>",
                                  M(sum(da$ecl_q2[!is.na(da$stage_q2) & da$stage_q2 == 2 & da$stage == 1])))),
               foot = tagList(span("Probation : ", tags$b("12 mois")),
                              span("S3 → S2 : ", tags$b(M(if ("Stage 3" %in% rownames(m)) m["Stage 3", "Stage 2"] else 0)))))
  )
})

output$vt_vintage <- renderPlotly({
  d <- loans_all()
  sel <- input$vt_seg %||% "ALL"
  if (sel == "MINES") d <- d[d$secteur == "Mines & Métaux", ]
  else if (sel != "ALL") d <- d[d$segment == sel, ]
  validate(need(nrow(d) > 50, "Échantillon insuffisant pour ce filtre."))
  v <- vintage_curves(d)
  p <- vintage_plot(v)
  r26 <- v[v$cohorte == "2026" & v$mob == 6, "taux"]
  if (length(r26) && !is.na(r26))
    p <- p %>% add_annotations(x = 6, y = r26, text = sprintf("<b>%s à M6</b>", fmt_pct(r26)), showarrow = TRUE, arrowhead = 0,
                               ax = 0, ay = -35, bgcolor = COL$red, font = list(color = "#fff", size = 11), bordercolor = COL$red)
  p
})

output$vt_diag <- renderUI({
  d <- loans_all()
  v <- vint()
  r26 <- v$taux[v$cohorte == "2026" & v$mob == 6]; r25 <- v$taux[v$cohorte == "2025" & v$mob == 6]
  c26 <- d %>% filter(cohorte == 2026, !is.na(mob_defaut)) %>%
    group_by(region, secteur) %>% summarise(v = sum(montant), n = n(), .groups = "drop") %>% arrange(desc(v)) %>% head(3)
  tagList(
    p(sprintf("Le sur-défaut précoce à M6 (%s) sur la cohorte 2026 %s de %s par rapport à la cohorte 2025 au même âge. Principaux foyers :",
              fmt_pct(r26), if (isTRUE(r26 > r25)) "dévie" else "reste aligné", if (length(r25)) fmt_bps(r26 - r25) else "—"),
      style = "font-size:12.5px"),
    lapply(seq_len(nrow(c26)), function(i) {
      div(class = "info-box", icon("location-crosshairs"),
          tags$b(sprintf(" %s — %s : ", c26$secteur[i], sub(".*— ", "", c26$region[i]))),
          sprintf("%s en défaut (%d dossiers).", M(c26$v[i]), c26$n[i]))
    }),
    div(class = "warn-box", tags$b("Décisions du Comité de Crédit : "), tags$br(),
        "• Resserrement DSTI max : abaissé de 60 % à 45 %", tags$br(),
        "• Garanties minières exigées : nantissement direct des créances", tags$br(),
        "• Revue mensuelle des cohortes 2026 jusqu'à M12")
  )
})

output$vt_transition <- renderDT({
  m <- tm()
  validate(need(nrow(m) > 0, "Pas de données de transition."))
  tot <- rowSums(m)
  mode <- input$vt_tm_mode %||% "pct"
  cells <- sapply(colnames(m), function(cn) sapply(rownames(m), function(rn) {
    v <- m[rn, cn]; p <- v / tot[rn]
    diag <- sub("Stage ", "", rn) == sub("Stage ", "", cn)
    worse <- cn == "Write-off" || (cn != "Write-off" && as.integer(sub("Stage ", "", cn)) > as.integer(sub("Stage ", "", rn)))
    col <- if (diag) COL$green else if (worse) COL$red else COL$orange
    main <- if (mode == "pct") fmt_pct(p) else M(v)
    second <- if (mode == "pct") M(v) else fmt_pct(p)
    sprintf('<b style="color:%s">%s</b><span class="sub-cell">%s</span>', col, main, second)
  }))
  if (is.null(dim(cells))) cells <- matrix(cells, nrow = 1)
  df <- data.frame(`Origine (Q2) \\ Destination (Q3)` = sprintf('<b>De %s</b><span class="sub-cell">%s</span>', rownames(m), sapply(tot, M)),
                   cells, `Encours initial` = sapply(tot, M), check.names = FALSE)
  names(df)[2:5] <- c("Stage 1 (Performant)", "Stage 2 (SICR)", "Stage 3 (NPL)", "Write-off (pertes)")
  dt_x(df, search = FALSE) %>% formatStyle(1:6, `text-align` = "left")
})

output$vt_flux <- renderUI({
  d <- loans_all()
  q2 <- function(s) sum(d$encours_q2[!is.na(d$stage_q2) & d$stage_q2 == s])
  q3 <- function(s) sum(d$encours[d$statut == "Actif" & d$stage == s])
  newp <- function(s) sum(d$encours[is.na(d$stage_q2) & d$stage == s])
  inn <- function(s) sum(d$encours_q2[!is.na(d$stage_q2) & d$stage_q2 != s & d$stage == s & d$statut == "Actif"])
  out <- function(s) sum(d$encours_q2[!is.na(d$stage_q2) & d$stage_q2 == s & (d$stage != s | d$statut != "Actif")])
  wo <- sum(d$encours_q2[d$statut != "Actif"])
  blk <- function(s, cls, extra) {
    net <- q3(s) - q2(s)
    div(class = paste("flux", cls),
        div(class = "flux-h", span(sprintf("Encours Stage %d : %s", s, M(q3(s)))),
            span(style = paste0("color:", if (net >= 0) COL$red else COL$green), sprintf("%s%s net", ifelse(net >= 0, "+", ""), M(net)))),
        div(class = "flux-g", extra))
  }
  am <- function(s) q3(s) - q2(s) - newp(s) - inn(s) + out(s) + if (s == 3) wo else 0
  tagList(
    blk(1, "", tagList(span("Production nouvelle : ", tags$b(class = "up", paste0("+", M(newp(1))))),
                       span("Amortissements : ", tags$b(M(am(1)))),
                       span("Transferts vers S2/S3 : ", tags$b(class = "down", paste0("-", M(out(1))))))),
    blk(2, "s2", tagList(span("Entrées nettes : ", tags$b(class = "down", paste0("+", M(inn(2) + newp(2))))),
                         span("Guérisons vers S1 : ", tags$b(class = "up", paste0("-", M(sum(d$encours_q2[!is.na(d$stage_q2) & d$stage_q2 == 2 & d$stage == 1]))))),
                         span("Chutes en S3 : ", tags$b(class = "down", paste0("-", M(sum(d$encours_q2[!is.na(d$stage_q2) & d$stage_q2 == 2 & d$stage == 3]))))))),
    blk(3, "s3", tagList(span("Nouveaux défauts : ", tags$b(class = "down", paste0("+", M(inn(3) + newp(3))))),
                         span("Recouvrements / cures : ", tags$b(M(am(3) - sum(d$encours_q2[!is.na(d$stage_q2) & d$stage_q2 == 3 & d$stage == 2])))),
                         span("Passages en perte : ", tags$b(paste0("-", M(wo)))))),
    div(class = "alert-box", tags$b("Ratio NPL consolidé (Stage 3 / Total EAD) : "),
        tags$b(style = "float:right", fmt_pct(q3(3) / sum(q3(1), q3(2), q3(3)))))
  )
})

clusters <- reactive({
  d <- loans()
  d$cluster <- with(d, ifelse(secteur == "Mines & Métaux", "Mines & sous-traitance minière",
                       ifelse(segment == "Corporate Grands Comptes", "Corporate Grands Comptes (hors mines)",
                       ifelse(segment == "Secteur Public & Souverain", "Souverain & entreprises publiques",
                       ifelse(segment == "Retail & Particuliers", "Retail & Particuliers",
                       ifelse(region_code == "R1", "PME Réseau Ouest (Kinshasa / Kongo Central)",
                       ifelse(region_code == "R4", "PME Est & Grands Lacs", "PME Sud, Centre & Nord")))))))
  d
})

output$vt_clusters <- renderDT({
  d <- clusters()
  g <- d %>% group_by(cluster) %>% summarise(ead = sum(encours), pd12 = weighted.mean(pd12, encours),
                                              pdlt = weighted.mean(pd_lt, encours), lgd = weighted.mean(lgd, encours),
                                              cov = sum(collateral) / sum(encours), ecl = sum(ecl), .groups = "drop") %>%
    arrange(desc(ead))
  tot <- d %>% summarise(cluster = "<b>Total consolidé portefeuille</b>", ead = sum(encours), pd12 = weighted.mean(pd12, encours),
                         pdlt = weighted.mean(pd_lt, encours), lgd = weighted.mean(lgd, encours),
                         cov = sum(collateral) / sum(encours), ecl = sum(ecl))
  g <- rbind(g, tot)
  alert <- ifelse(g$pd12 > .05, ' <span class="st bad">Stress</span>', ifelse(g$pd12 > .03, ' <span class="st warn">Alerte SICR</span>', ""))
  df <- data.frame(`Cluster de risque` = paste0(g$cluster, alert), EAD = round(g$ead / div_(), 1),
                   `PD 12m` = g$pd12, `PD lifetime` = g$pdlt, `LGD moyenne` = g$lgd,
                   `Couverture collat.` = g$cov, `Stock ECL` = round(g$ecl / div_(), 2), `ECL / EAD` = g$ecl / g$ead,
                   check.names = FALSE)
  names(df)[2] <- paste0("EAD (", unit(), ")"); names(df)[7] <- paste0("ECL (", unit(), ")")
  dt_x(df) %>% formatPercentage(c("PD 12m", "PD lifetime", "LGD moyenne", "Couverture collat.", "ECL / EAD"), 2) %>%
    formatStyle("PD 12m", color = styleInterval(c(.015, .035), c(COL$green, COL$orange, COL$red)), fontWeight = "bold") %>%
    formatStyle("Couverture collat.", color = styleInterval(.9, c(COL$orange, COL$green)))
})

fli <- reactive({
  d <- loans()
  e <- sapply(1:3, function(i) sum(run_stress(d, SCENARIOS[i, ])$ecl_s))
  data.frame(SCENARIOS[, c("scenario", "poids")], ecl = e)
})

output$vt_fli_box <- renderUI({
  f <- fli(); sel <- input$vt_fli %||% "Baseline"
  w <- sum(f$poids * f$ecl)
  sc <- SCENARIOS
  row <- function(lbl, b, a, s, fm) div(class = "kv", span(lbl), span(tags$b(fm(b)), " · ", span(class = "warn-t", fm(a)), " · ", span(class = "down", fm(s))))
  tagList(
    div(class = "seg-meta", "Base · Adverse · Sévère"),
    row("Croissance PIB réel RDC", sc$pib[1], sc$pib[2], sc$pib[3], function(x) fmt_pct(x, 1)),
    row("Change USD/CDF", sc$fx[1], sc$fx[2], sc$fx[3], function(x) format(x, big.mark = " ")),
    row("Cuivre LME (USD/t)", sc$cuivre[1], sc$cuivre[2], sc$cuivre[3], function(x) format(x, big.mark = " ")),
    row("Taux directeur BCC", sc$taux_bcc[1], sc$taux_bcc[2], sc$taux_bcc[3], function(x) fmt_pct(x, 1)),
    tags$hr(),
    lapply(seq_len(nrow(f)), function(i) div(class = "kv", style = if (f$scenario[i] == sel) "background:#fff4e0;font-weight:700" else "",
                                             span(sprintf("%s (%s)", f$scenario[i], fmt_pct(f$poids[i], 0))), span(M(f$ecl[i])))),
    div(class = "kv", style = "font-weight:700", span("ECL pondérée (FLI)"), span(M(w))),
    div(class = "alert-box", sprintf("Impact bascule 100 %% scénario « %s » : ", sel),
        tags$b(sprintf("%s%s ECL", ifelse(f$ecl[f$scenario == sel] - w >= 0, "+", ""), M(f$ecl[f$scenario == sel] - w))))
  )
})
