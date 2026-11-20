# =============================================================================
#  Page 7 — Reporting réglementaire (synthèse, benchmark, export XML)
# =============================================================================

report_ref <- sprintf("%s/CRD/REP-%s-Q3-0001", BANK_SHORT, format(ARRETE, "%Y"))

output$rp_header <- renderUI({
  div(class = "card-x",
      div(class = "doc-head",
          div(div(style = "display:flex;gap:12px;align-items:center",
                  div(class = "doc-logo", BANK_SHORT),
                  div(h3(class = "card-title", toupper(BANK_NAME)),
                      div(class = "crumb-act", "Direction Centrale de la Gestion des Risques • Credit Portfolio Management"))),
              div(class = "card-sub", style = "margin-top:10px", tags$b("Destinataires : "),
                  "Direction de la Surveillance des Intermédiaires Financiers (Banque Centrale) • Comité des Risques du Conseil d'Administration • COMEX • Commissaires aux comptes"),
              div(class = "card-sub", tags$b("Périmètre : "),
                  paste(c(if (input$f_region != "ALL") input$f_region else "Consolidé (5 régions, 50 agences)",
                          if (input$f_segment != "ALL") input$f_segment else "4 segments"), collapse = " · "))),
          div(class = "doc-ref",
              div(class = "kv", span("Référence dépôt"), span(class = "num", report_ref)),
              div(class = "kv", span("Horodatage"), span(class = "num", format(ARRETE, "%d/%m/%Y 18:00 (UTC+1)"))),
              div(class = "kv", span("Cycle comptable"), span(class = "num up", "T3 2026 / YTD validé")),
              div(class = "sign-grid",
                  div(class = "sign", div(class = "sign-h up", "Signé"), "Chef Unité Reporting"),
                  div(class = "sign", div(class = "sign-h warn-t", "En attente"), "Chief Risk Officer"),
                  div(class = "sign", div(class = "sign-h", "Visa"), "Directeur Général")))))
})

output$rp_kpis <- renderUI({
  m <- tm(); da <- loans_all()
  g <- function(r, c) if (r %in% rownames(m)) m[r, c] else 0
  tot <- function(r) if (r %in% rownames(m)) sum(m[r, ]) else 0
  div(class = "kpi-row",
      kpi_card("Migration Stage 1 → Stage 2", M(g("Stage 1", "Stage 2")), value_cls = "orange",
               badge_txt = fmt_pct(safe_div(g("Stage 1", "Stage 2"), tot("Stage 1"))), badge_cls = "warn",
               sub = "SICR déclenché (arriérés, watchlist, forbearance)"),
      kpi_card("Migration Stage 2 → Stage 3 (NPL)", M(g("Stage 2", "Stage 3")), value_cls = "red",
               badge_txt = fmt_pct(safe_div(g("Stage 2", "Stage 3"), tot("Stage 2"))), badge_cls = "bad",
               sub = "Défauts constatés (> 90 jours de retard)"),
      kpi_card("Taux de guérison (cure S2 → S1)", M(g("Stage 2", "Stage 1")), value_cls = "green",
               badge_txt = fmt_pct(safe_div(g("Stage 2", "Stage 1"), tot("Stage 2"))), badge_cls = "ok",
               sub = "Apurements d'arriérés & restructurations saines"),
      kpi_card("Sorties sèches & pertes nettoyées", M(sum(da$encours_q2[da$statut != "Actif"])),
               badge_txt = "Write-offs T3", badge_cls = "neutral",
               sub = "Créances totalement provisionnées passées en perte"))
})

report_segments <- reactive({
  d <- loans()
  g <- d %>% group_by(segment) %>% summarise_risk()
  tot <- summarise_risk(d); tot$segment <- "TOTAL PORTEFEUILLE AUDITÉ"
  rbind(g, tot[, names(g)])
})

output$rp_segment_table <- renderDT({
  g <- report_segments()
  df <- data.frame(`Segment de portefeuille` = ifelse(grepl("TOTAL", g$segment), paste0("<b>", g$segment, "</b>"), g$segment),
                   `EAD total` = round(g$encours / div_(), 1), `Stage 1` = round(g$s1 / div_(), 1),
                   `Stage 2` = round(g$s2 / div_(), 1), `Stage 3` = round(g$s3 / div_(), 1),
                   `Ratio NPL` = g$npl, `Stock ECL` = round(g$ecl / div_(), 2), `Couverture NPL` = safe_div(g$ecl, g$s3),
                   `Statut conformité` = ifelse(grepl("TOTAL", g$segment), status_html("Validé", "dark"),
                                         ifelse(g$npl > SEUIL_NPL, status_html("Sous surveillance", "bad"), status_html("Conforme", "ok"))),
                   check.names = FALSE)
  names(df)[2:5] <- paste0(names(df)[2:5], " (", unit(), ")"); names(df)[7] <- paste0("Stock ECL (", unit(), ")")
  dt_x(df, search = FALSE) %>% formatPercentage(c("Ratio NPL", "Couverture NPL"), 2) %>%
    formatStyle(names(df)[4], color = COL$s2) %>% formatStyle(names(df)[5], color = COL$s3) %>%
    formatStyle("Ratio NPL", fontWeight = "bold", color = styleInterval(c(.03, SEUIL_NPL), c(COL$green, COL$orange, COL$red)))
})

output$rp_agency_table <- renderDT({
  a <- agency_risk()
  classe <- ifelse(a$raroc >= .20 & a$par30 < .035, status_html("Tier 1 (Alpha)", "ok"),
            ifelse(a$par30 > .07 | a$raroc < .10, status_html("Tier 3 (Critique / Moratoire)", "bad"),
            ifelse(a$par30 > SEUIL_PAR30, status_html("Tier 2 (Surveillance)", "warn"), status_html("Tier 2 (Standard)", "neutral"))))
  df <- data.frame(`Agence / hub opérationnel` = sprintf('<b>%s</b><span class="sub-cell">%s · Code %s</span>', a$agence, a$region, a$code),
                   `Encours brut (EAD)` = round(a$encours / div_(), 2), `Production YTD` = round(a$prod / div_(), 2),
                   `Ratio PAR30+` = a$par30, `Ratio NPL (S3)` = a$npl, `Coût du risque (bps)` = round(a$cor * 1e4),
                   `RAROC effectif` = a$raroc, `Couv. collatéral` = a$collat, `Classe prudentielle` = classe, check.names = FALSE)
  names(df)[2] <- paste0("EAD (", unit(), ")"); names(df)[3] <- paste0("Production YTD (", unit(), ")")
  dt_x(df, page = TRUE, page_len = 10, order = list(list(1, "desc"))) %>%
    formatPercentage(c("Ratio PAR30+", "Ratio NPL (S3)", "RAROC effectif", "Couv. collatéral"), 2) %>%
    formatStyle("RAROC effectif", fontWeight = "bold", color = styleInterval(HURDLE, c(COL$red, COL$green))) %>%
    formatStyle("Ratio PAR30+", color = styleInterval(c(SEUIL_PAR30, .06), c(COL$green, COL$orange, COL$red)))
})

xml_instance <- reactive({
  g <- report_segments(); m <- tm()
  fact <- function(name, v, unit = "USD", dec = 0, ctx = "Q3_2026")
    sprintf('  <bdr:%s contextRef="%s" unitRef="%s" decimals="%d">%s</bdr:%s>', name, ctx, unit, dec,
            formatC(v, format = "f", digits = max(dec, 0)), name)
  seg_facts <- unlist(lapply(seq_len(nrow(g) - 1), function(i) {
    ctx <- paste0("Q3_2026_", gsub("[^A-Za-z]", "", g$segment[i]))
    c(fact("GrossCarryingAmount", g$encours[i], ctx = ctx), fact("Stage3Exposure", g$s3[i], ctx = ctx),
      fact("ExpectedCreditLoss", g$ecl[i], ctx = ctx), fact("NonPerformingRatio", g$npl[i], "pure", 4, ctx))
  }))
  t <- g[nrow(g), ]
  paste(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    sprintf("<!-- %s — Déclaration trimestrielle IFRS 9 (données simulées) — %s -->", BANK_NAME, report_ref),
    '<xbrli:xbrl xmlns:xbrli="http://www.xbrl.org/2003/instance" xmlns:bdr="http://example.org/taxonomy/2026/credit-risk"',
    '            xmlns:iso4217="http://www.xbrl.org/2003/iso4217">',
    '  <xbrli:context id="Q3_2026"><xbrli:period><xbrli:instant>2026-09-30</xbrli:instant></xbrli:period></xbrli:context>',
    '  <xbrli:unit id="USD"><xbrli:measure>iso4217:USD</xbrli:measure></xbrli:unit>',
    fact("GrossCarryingAmount", t$encours), fact("Stage1Exposure", t$s1), fact("Stage2Exposure", t$s2),
    fact("Stage3Exposure", t$s3), fact("ExpectedCreditLoss", t$ecl), fact("NonPerformingRatio", t$npl, "pure", 4),
    fact("PAR30Ratio", t$par30, "pure", 4), fact("SectoralHHI", hhi(sector_shares()$encours), "pure", 0),
    fact("CapitalAdequacyRatio", FONDS_PROPRES / RWA_TOTAL, "pure", 4),
    fact("TransferStage1ToStage2", if ("Stage 1" %in% rownames(m)) m["Stage 1", "Stage 2"] else 0),
    fact("TransferStage2ToStage3", if ("Stage 2" %in% rownames(m)) m["Stage 2", "Stage 3"] else 0),
    seg_facts,
    "</xbrli:xbrl>"), collapse = "\n")
})

output$rp_xml <- renderText(xml_instance())

output$dl_xbrl <- downloadHandler(
  filename = function() paste0("declaration_ifrs9_", format(ARRETE, "%Y%m%d"), ".xml"),
  content = function(file) writeLines(xml_instance(), file, useBytes = TRUE)
)

output$dl_report_csv <- downloadHandler(
  filename = function() paste0("synthese_segments_", format(ARRETE, "%Y%m%d"), ".csv"),
  content = function(file) {
    g <- report_segments()
    write.csv(data.frame(segment = g$segment, encours_usd = round(g$encours), stage1 = round(g$s1), stage2 = round(g$s2),
                         stage3 = round(g$s3), npl = round(g$npl, 4), par30 = round(g$par30, 4), ecl = round(g$ecl),
                         raroc = round(g$raroc, 4)), file, row.names = FALSE, fileEncoding = "UTF-8")
  }
)
