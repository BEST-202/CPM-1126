# =============================================================================
#  Interface utilisateur
# =============================================================================

NAV_ITEMS <- list(
  list(id = "overview",      label = "Vue d'ensemble Portefeuille",      icon = "table-cells-large"),
  list(id = "regions",       label = "Segmentation & Régions",           icon = "earth-africa"),
  list(id = "vintage",       label = "Vintage Curves & Staging IFRS 9",  icon = "chart-line"),
  list(id = "concentration", label = "Risque de Concentration & HHI",    icon = "circle-nodes"),
  list(id = "performance",   label = "Performance RMs & Agences",        icon = "briefcase"),
  list(id = "stress",        label = "Stress Testing & RAROC",           icon = "wave-square"),
  list(id = "report",        label = "Reporting Réglementaire",          icon = "file-shield")
)

page_header <- function(crumb, title, tag = NULL, right = NULL) {
  div(class = "page-head",
      div(div(class = "crumb", "Portefeuille institutionnel", icon("chevron-right"), span(class = "crumb-act", crumb)),
          h1(class = "page-title", title),
          if (!is.null(tag)) div(class = "page-tag", icon("circle-check"), tag)),
      if (!is.null(right)) div(class = "page-actions", right))
}

seg_toggle <- function(id, choices, selected = choices[[1]]) {
  div(class = "seg-toggle", radioButtons(id, NULL, choices, selected = selected, inline = TRUE))
}

sidebar_ui <- div(
  class = "sidebar",
  div(class = "brand",
      div(class = "brand-logo", BANK_SHORT),
      div(div(class = "brand-name", BANK_NAME), div(class = "brand-sub", DIVISION))),
  div(class = "nav-section", "Portefeuille institutionnel"),
  tags$nav(class = "side-nav",
           lapply(seq_along(NAV_ITEMS), function(i) {
             it <- NAV_ITEMS[[i]]
             tags$a(href = "#", class = paste("nav-link-x", if (i == 1) "active"),
                    `data-tab` = it$id, icon(it$icon), span(it$label))
           })),
  div(class = "side-status",
      div(class = "ss-title", icon("rotate"), "Status système"),
      div("Dernière synchro Core Banking : ", tags$b("Aujourd'hui 06:00")),
      div("Kinshasa | Données validées ", span(class = "txt-green", "IFRS 9")),
      div(class = "ss-note", "Données simulées à des fins de démonstration"))
)

topbar_ui <- div(
  class = "topbar",
  tags$button(class = "burger", type = "button", onclick = "document.body.classList.toggle('nav-open')", icon("bars")),
  span(class = "live", span(class = "dot"), "Live Market Data"),
  div(class = "seg-toggle cur-toggle", radioButtons("devise", NULL, c("USD", "CDF"), inline = TRUE)),
  span(class = "tb-chip", icon("calendar"), "Q3 2026 / YTD"),
  div(class = "tb-select", icon("location-dot"),
      selectInput("f_region", NULL, c("Toutes les régions" = "ALL", setNames(REGIONS$region, REGIONS$region)),
                  width = "175px", selectize = FALSE)),
  div(class = "tb-select", icon("layer-group"),
      selectInput("f_segment", NULL, c("Tous les segments" = "ALL", setNames(SEGMENTS, SEGMENTS)),
                  width = "185px", selectize = FALSE)),
  downloadButton("dl_portfolio", "Exporter CSV", class = "tb-btn", icon = icon("download")),
  div(class = "user",
      div(div(class = "user-name", USER_NAME), div(class = "user-role", USER_ROLE)),
      div(class = "avatar", icon("user-tie")))
)

# ----------------------------------------------------------------- pages
page_overview <- tagList(
  div(class = "subhead",
      div(class = "crumb", "Portefeuille institutionnel", icon("chevron-right"),
          span(class = "crumb-act", "Vue d'ensemble & Cockpit exécutif")),
      div(class = "subhead-right",
          span(class = "audit", icon("circle-check"), "Audit IFRS 9 validé"),
          uiOutput("ov_perimetre", inline = TRUE))),
  uiOutput("ov_kpis"),
  layout_columns(
    col_widths = c(8, 4),
    card_x("Distribution Portefeuille & Staging IFRS 9", icon_name = "chart-bar",
           subtitle = "Exposition brute par segment opérationnel & migration de risque de crédit",
           right = seg_toggle("ov_stage_mode", c("Valeur" = "val", "Ratio %" = "pct")),
           uiOutput("ov_staging"),
           div(class = "card-sub", style = "margin-top:6px;font-weight:700;color:#111827", "Staging IFRS 9 par secteur d'activité (12 secteurs)"),
           plotlyOutput("ov_sector_stage", height = "300px")),
    card_x("Concentration & Indice HHI", icon_name = "circle-half-stroke",
           subtitle = "Herfindahl-Hirschman sectoriel & limites de souscription",
           right = badge(paste0("Norme < ", format(SEUIL_HHI, big.mark = " ")), "neutral"),
           plotlyOutput("ov_hhi_donut", height = "230px"),
           uiOutput("ov_hhi_list"))
  ),
  layout_columns(
    col_widths = c(5, 7),
    card_x("Courbes de Vintage (Cohortes d'origination)", icon_name = "chart-line",
           subtitle = "Taux cumulé de défaut (PAR90+) de M0 à M36",
           right = uiOutput("ov_vintage_badge"),
           plotlyOutput("ov_vintage", height = "290px"),
           uiOutput("ov_vintage_note")),
    card_x("Performance & Profil de Risque par Région", icon_name = "table",
           subtitle = "Rentabilité ajustée du risque (RAROC) vs coût du risque (CoR)",
           right = badge(paste0("RAROC cible > ", fmt_pct(HURDLE, 0)), "ok"),
           DTOutput("ov_region_table"))
  ),
  div(class = "section-title", icon("bell"), "Early Warning System (EWS) — Alertes prudentielles",
      span(class = "pulse-tag", span(class = "dot red"), "Protocole d'escalade automatique actif")),
  uiOutput("ov_ews")
)

page_regions <- tagList(
  page_header("Segmentation & cartographie régionale", "Exposition Territoriale & Risque Géographique",
              tag = "Arrêté Q3 2026 — RDC"),
  uiOutput("rg_kpis"),
  layout_columns(
    col_widths = c(7, 5),
    card_x("Cartographie de l'Exposition & Stress Territorial", icon_name = "map-location-dot",
           subtitle = "Visualisation géospatiale des 50 agences et corridors économiques",
           right = seg_toggle("rg_map_metric", c("Exposition" = "enc", "PAR30" = "par30",
                                                 "Sensibilité FX" = "cdf", "Collatéraux" = "collat")),
           plotlyOutput("rg_map", height = "430px"),
           uiOutput("rg_region_tiles")),
    div(
      card_x("Ventilation : Régions & Staging IFRS 9", icon_name = "chart-pie",
             subtitle = "Répartition des encours par région et par stage",
             uiOutput("rg_region_bars")),
      card_x("Sensibilité FX & Clivage USD / CDF", icon_name = "money-bill-transfer",
             uiOutput("rg_fx"))
    )
  ),
  card_x("Matrice de Risque des Agences & Délégations", icon_name = "building-columns",
         subtitle = "Surveillance opérationnelle des 50 agences : plafonds de crédit et délégation de signature locale",
         right = seg_toggle("rg_tab_region", c("Toutes" = "ALL", setNames(REGIONS$region, REGIONS$region_code))),
         DTOutput("rg_agency_table"))
)

page_vintage <- tagList(
  page_header("Modélisation du risque & provisions IFRS 9", "Courbes de Vintage & Dynamique de Staging IFRS 9",
              tag = "Modèle d'impairment IFRS 9 — ECL = PD × LGD × EAD",
              right = div(class = "flex-gap",
                          seg_toggle("vt_fli", c("Baseline (60%)" = "Baseline", "Adverse (25%)" = "Adverse modéré",
                                                 "Sévère (15%)" = "Sévère systémique")))),
  uiOutput("vt_kpis"),
  layout_columns(
    col_widths = c(8, 4),
    card_x("Analyse Vintage : Taux Cumulé de Défaut (PAR 90+ / Stage 3) par Millésime", icon_name = "chart-line",
           subtitle = "Suivi de la courbe de maturité des cohortes d'octroi depuis le mois d'origination (M0 à M36)",
           right = seg_toggle("vt_seg", c("Consolidé" = "ALL", "Corporate" = "Corporate Grands Comptes",
                                          "PME" = "PME", "Retail" = "Retail & Particuliers",
                                          "Mines" = "MINES")),
           plotlyOutput("vt_vintage", height = "380px")),
    card_x("Diagnostic d'Origination 2026", icon_name = "stethoscope", uiOutput("vt_diag"))
  ),
  layout_columns(
    col_widths = c(7, 5),
    card_x("Matrice de Transition Trimestrielle (S1 → S2 → S3)", icon_name = "arrows-split-up-and-left",
           subtitle = "Probabilités de migration empiriques Q2 → Q3 2026 (en % de l'encours d'origine)",
           right = seg_toggle("vt_tm_mode", c("Probabilités" = "pct", "Montants" = "val")),
           DTOutput("vt_transition")),
    card_x("Flux de Réconciliation d'Encours (Gross EAD)", icon_name = "right-left",
           subtitle = "Décomposition des transferts nets d'exposition sur le trimestre",
           uiOutput("vt_flux"))
  ),
  layout_columns(
    col_widths = c(8, 4),
    card_x("Moteur IFRS 9 Forward-Looking : Décomposition PD × LGD × EAD", icon_name = "calculator",
           subtitle = "Valorisation des provisions par cluster de risque homogène",
           DTOutput("vt_clusters")),
    card_x("Pondération Scénarios (FLI)", icon_name = "sliders",
           subtitle = "Scénarios macro-économiques RDC et ECL pondérée",
           uiOutput("vt_fli_box"))
  )
)

page_concentration <- tagList(
  page_header("Concentration, grands risques & limites prudentielles",
              "Risque de Concentration & Suivi des Top 20 Débiteurs",
              tag = sprintf("Plafond individuel %s des fonds propres & Bâle II/III", fmt_pct(LIMITE_INDIV, 0))),
  uiOutput("cc_kpis"),
  layout_columns(
    col_widths = c(7, 5),
    card_x("Ventilation Sectorielle vs Plafonds Prudentiels & Limites d'Appétit", icon_name = "industry",
           subtitle = "12 secteurs d'activité — part de l'encours et limite interne",
           DTOutput("cc_sector_table")),
    card_x("Indice HHI & Courbe de Lorenz", icon_name = "gauge-high",
           plotlyOutput("cc_gauge", height = "210px"),
           plotlyOutput("cc_lorenz", height = "220px"),
           uiOutput("cc_lorenz_note"))
  ),
  card_x("Registre Prudentiel des TOP 20 Débiteurs & Groupes Économiques Liés", icon_name = "list-ol",
         subtitle = "Surveillance des grands risques — plafond individuel 25 % des fonds propres Tier 1",
         right = seg_toggle("cc_filter", c("Tous les Top 20" = "ALL", "Mines" = "Mines & Métaux",
                                           "Souverain / Para-étatique" = "Secteur Public & Souverain",
                                           "Alertes > 15 %" = "ALERT")),
         DTOutput("cc_top20")),
  layout_columns(
    col_widths = c(6, 6),
    card_x("Concentration par Région & Segment", icon_name = "table-cells",
           subtitle = "Carte de chaleur des encours (part du total)",
           plotlyOutput("cc_heat", height = "300px")),
    card_x("Stress Test Réglementaire : Single Name Default", icon_name = "bolt",
           subtitle = "Défaut immédiat du plus grand groupe débiteur (LGD 45 % post-liquidation des garanties)",
           uiOutput("cc_single_name"))
  )
)

page_performance <- tagList(
  page_header("Pilotage commercial & performance ajustée au risque",
              "Performance des Chargés d'Affaires & Agences (Scorecards & RAROC)",
              tag = sprintf("Méthodologie RAROC Bâle II/III & EVA — Ke : %s", fmt_pct(HURDLE, 1))),
  uiOutput("pf_kpis"),
  layout_columns(
    col_widths = c(7, 5),
    card_x("Arbitrage RAROC vs Coût du Risque (Portefeuilles RMs)", icon_name = "braille",
           subtitle = "Chaque bulle = un chargé d'affaires (taille = encours)",
           plotlyOutput("pf_scatter", height = "400px"),
           uiOutput("pf_gov")),
    card_x("Benchmark Régional : Performance des 5 Pôles Territoriaux", icon_name = "ranking-star",
           uiOutput("pf_regions"))
  ),
  card_x("Scorecard Détaillée des Relationship Managers (RMs)", icon_name = "id-card",
         subtitle = "Pilotage individuel de l'exposition (EAD), des impayés et de la conformité au modèle RAROC",
         right = seg_toggle("pf_rm_filter", c("Tous" = "ALL", "Top 10 RAROC" = "TOP",
                                              "Alerte coût risque > 250 bps" = "COR",
                                              "Dérogations > 5 %" = "DER")),
         DTOutput("pf_rm_table")),
  layout_columns(
    col_widths = c(7, 5),
    card_x("Scorecard des 50 Agences", icon_name = "building",
           DTOutput("pf_agency_table")),
    card_x("Registre des Dérogations de Crédit (Exceptions Policy)", icon_name = "file-circle-exclamation",
           uiOutput("pf_derog"))
  )
)

page_stress <- tagList(
  page_header("Modélisation du risque & pilotage du capital",
              "Stress Testing Réglementaire & Simulation Macro-prudentielle",
              tag = "Pilier 2 ICAAP — Moteur économétrique sectoriel"),
  card_x("Scénarios macro-prudentiels", icon_name = "sliders",
         seg_toggle("st_scenario", c("Baseline (60%)" = "1", "Adverse modéré (25%)" = "2",
                                     "Sévère Macro-RDC (15%)" = "3", "Choc systémique personnalisé" = "custom"),
                    selected = "3"),
         conditionalPanel(
           "input.st_scenario == 'custom'",
           div(class = "custom-grid",
               sliderInput("st_cu", "Cours du cuivre (USD/t)", 3000, 11000, 6500, step = 100),
               sliderInput("st_co", "Cours du cobalt (USD/t)", 8000, 35000, 16000, step = 500),
               sliderInput("st_fx", "Taux de change (CDF/USD)", 2500, 5000, 3600, step = 50),
               sliderInput("st_pib", "Croissance PIB réel (%)", -6, 8, -1, step = .5),
               sliderInput("st_rate", "Taux directeur BCC (%)", 8, 35, 22, step = .5),
               sliderInput("st_inf", "Inflation annuelle (%)", 3, 50, 25, step = 1)))),
  uiOutput("st_kpis"),
  card_x("Hypothèses Macroéconomiques des Scénarios de Stress", icon_name = "globe",
         subtitle = "Paramètres macro-financiers calibrés sur l'écosystème RDC (cours LME, change, taux BCC)",
         DTOutput("st_macro")),
  layout_columns(
    col_widths = c(7, 5),
    card_x("Trajectoire Dynamique du Ratio de Solvabilité (CAR)", icon_name = "chart-area",
           subtitle = "Projection trimestrielle Q3 2026 → Q3 2028",
           plotlyOutput("st_car", height = "330px"), uiOutput("st_car_note")),
    card_x("Cascade de Sensibilité du CAR (Bridge Bâle II/III)", icon_name = "stairs",
           subtitle = "Décomposition additive des impacts factoriels au point bas",
           plotlyOutput("st_bridge", height = "360px"))
  ),
  layout_columns(
    col_widths = c(7, 5),
    card_x("Impact du Stress par Secteur (multiplicateur de PD & ΔECL)", icon_name = "industry",
           plotlyOutput("st_sector", height = "340px")),
    card_x("Stress Tests Ciblés & Reverse Stress Testing", icon_name = "triangle-exclamation",
           uiOutput("st_targeted"))
  ),
  card_x("RAROC & Création de Valeur par Segment (Baseline vs Stress)", icon_name = "scale-balanced",
         DTOutput("st_raroc"))
)

page_report <- tagList(
  page_header("Centre d'édition & reporting réglementaire", "Rapport Officiel d'Audit & Déclaration Réglementaire (T3 2026)",
              tag = "Synthèse trimestrielle IFRS 9 — données simulées",
              right = div(class = "flex-gap",
                          downloadButton("dl_report_csv", "Télécharger synthèse (CSV)", class = "btn-red"),
                          downloadButton("dl_xbrl", "Instance XML (XBRL-like)", class = "btn-dark-x"))),
  uiOutput("rp_header"),
  card_x("01 · Analyse Trimestrielle des Migrations de Risque & Transitions IFRS 9", icon_name = "arrows-rotate",
         uiOutput("rp_kpis"),
         DTOutput("rp_segment_table")),
  card_x("02 · Benchmark Réglementaire des Régions & Agences", icon_name = "building-columns",
         subtitle = "Évaluation prudentielle de rentabilité ajustée au risque (RAROC), qualité du collatéral et classification",
         DTOutput("rp_agency_table")),
  card_x("03 · Console XML — Faits déclarés", icon_name = "code", cls = "dark-card",
         verbatimTextOutput("rp_xml"))
)

ui <- page(
  theme = bs_theme(version = 5, bg = "#f4f5f7", fg = "#111827", primary = "#0f1b2d",
                   base_font = "Source Serif 4, Georgia, serif", "font-size-base" = "0.88rem"),
  title = paste(BANK_NAME, "— Credit Portfolio Management"),
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Source+Serif+4:opsz,wght@8..60,400;8..60,600;8..60,700&family=JetBrains+Mono:wght@400;600&display=swap"),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(src = "app.js")
  ),
  div(class = "app-shell",
      sidebar_ui,
      div(class = "main",
          topbar_ui,
          div(class = "content",
              navset_hidden(
                id = "tabs",
                nav_panel_hidden("overview", page_overview),
                nav_panel_hidden("regions", page_regions),
                nav_panel_hidden("vintage", page_vintage),
                nav_panel_hidden("concentration", page_concentration),
                nav_panel_hidden("performance", page_performance),
                nav_panel_hidden("stress", page_stress),
                nav_panel_hidden("report", page_report)
              ),
              div(class = "footer-x",
                  span(icon("shield-halved"), paste(BANK_NAME, "• Diffusion restreinte COMEX • Données entièrement simulées")),
                  span("Version système : CPM-IFRS9-v4.8"))))),
  div(class = "nav-backdrop", onclick = "document.body.classList.remove('nav-open')")
)
