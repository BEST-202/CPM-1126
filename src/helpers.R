# =============================================================================
#  Formatage, composants UI et moteurs de calcul
# =============================================================================

# ---------------------------------------------------------------- formatage
fmt_num <- function(x, d = 1) formatC(x, format = "f", digits = d, big.mark = ",")
fmt_pct <- function(x, d = 2) ifelse(is.finite(x), paste0(formatC(100 * x, format = "f", digits = d), "%"), "—")
fmt_bps <- function(x) paste0(ifelse(x > 0, "+", ""), round(x * 1e4), " bps")
fmt_pp  <- function(x, d = 2) paste0(ifelse(x >= 0, "+", ""), formatC(100 * x, format = "f", digits = d), " pp")

# montants : USD en millions, CDF en milliards de francs
fmt_m <- function(x, cur = "USD", d = 1) {
  if (identical(cur, "CDF")) paste0(formatC(x * FX_RATE / 1e9, format = "f", digits = d, big.mark = " "), " Mrd FC")
  else paste0(ifelse(x < 0, "-", ""), "$", formatC(abs(x) / 1e6, format = "f", digits = d, big.mark = ","), "M")
}
fmt_full <- function(x, cur = "USD") {
  if (identical(cur, "CDF")) paste0(formatC(x * FX_RATE, format = "f", digits = 0, big.mark = " "), " FC")
  else paste0("$", formatC(x, format = "f", digits = 0, big.mark = ","))
}
cur_mult <- function(cur) if (identical(cur, "CDF")) FX_RATE else 1
cur_unit <- function(cur) if (identical(cur, "CDF")) "Mrd FC" else "M USD"
cur_div  <- function(cur) if (identical(cur, "CDF")) 1e9 / FX_RATE else 1e6

safe_div <- function(a, b) ifelse(b == 0 | is.na(b), NA_real_, a / b)

# ---------------------------------------------------------------- composants UI
badge <- function(txt, cls = "neutral") span(class = paste("badge-x", cls), txt)

kpi_card <- function(title, value, sub = NULL, badge_txt = NULL, badge_cls = "ok",
                     foot = NULL, icon_name = NULL, value_cls = "") {
  div(class = "card-x kpi",
      div(class = "kpi-head",
          div(class = "kpi-title", if (!is.null(icon_name)) icon(icon_name), title),
          if (!is.null(badge_txt)) badge(badge_txt, badge_cls)),
      div(class = paste("kpi-value", value_cls), value),
      if (!is.null(sub)) div(class = "kpi-sub", sub),
      if (!is.null(foot)) div(class = "kpi-foot", foot))
}

card_x <- function(title, ..., subtitle = NULL, right = NULL, icon_name = NULL, cls = "") {
  div(class = paste("card-x", cls),
      div(class = "card-head",
          div(h3(class = "card-title", if (!is.null(icon_name)) icon(icon_name), title),
              if (!is.null(subtitle)) div(class = "card-sub", subtitle)),
          if (!is.null(right)) div(class = "card-right", right)),
      div(class = "card-body-x", ...))
}

stage_bar <- function(s1, s2, s3) {
  tot <- s1 + s2 + s3
  if (tot <= 0) return(div(class = "stagebar"))
  div(class = "stagebar",
      div(class = "sb s1", style = sprintf("width:%.2f%%", 100 * s1 / tot)),
      div(class = "sb s2", style = sprintf("width:%.2f%%", 100 * s2 / tot)),
      div(class = "sb s3", style = sprintf("width:%.2f%%", 100 * s3 / tot)))
}

gauge_bar <- function(val, lim, max_scale = NULL) {
  max_scale <- max_scale %||% max(lim * 1.5, val * 1.1)
  cls <- if (val > lim) "over" else if (val > .85 * lim) "warn" else "okb"
  sprintf('<div class="gauge"><div class="g-fill %s" style="width:%.1f%%"></div><div class="g-lim" style="left:%.1f%%"></div></div>',
          cls, min(100, 100 * val / max_scale), min(100, 100 * lim / max_scale))
}

status_html <- function(txt, cls) sprintf('<span class="st %s">%s</span>', cls, txt)

ews_card <- function(tag, tag_cls, meta, title, body, action, btn_id, btn_lbl, btn_cls = "btn-danger-x") {
  div(class = paste("card-x ews", tag_cls),
      div(class = "ews-top", badge(tag, tag_cls), span(class = "ews-meta", meta)),
      h4(class = "ews-title", title),
      p(class = "ews-body", HTML(body)),
      div(class = "ews-foot", span(class = "ews-action", icon("bolt"), action),
          actionButton(btn_id, btn_lbl, class = paste("btn-sm", btn_cls))))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------- plotly
pl_layout <- function(p, ..., legend_top = TRUE) {
  p %>% plotly::layout(
    font = list(family = "Source Serif 4, Georgia, serif", size = 12, color = "#111827"),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    margin = list(l = 50, r = 20, t = 20, b = 40),
    legend = if (legend_top) list(orientation = "h", x = 0, y = 1.12, font = list(size = 11)) else list(),
    hoverlabel = list(font = list(family = "Source Serif 4, Georgia, serif")),
    ...) %>%
    plotly::config(displaylogo = FALSE, modeBarButtonsToRemove =
                     c("lasso2d", "select2d", "autoScale2d", "toggleSpikelines"))
}

# ---------------------------------------------------------------- agrégations
summarise_risk <- function(d) {
  d %>% dplyr::summarise(
    s1 = sum(encours[stage == 1]), s2 = sum(encours[stage == 2]), s3 = sum(encours[stage == 3]),
    par30 = safe_div(sum(encours[dpd >= 30]), sum(encours)),
    npl = safe_div(sum(encours[stage == 3]), sum(encours)),
    ecl = sum(ecl),
    cor = safe_div(sum(el), sum(encours)),
    raroc = safe_div(sum(revenu_net), sum(capital)),
    capital = sum(capital),
    collat = safe_div(sum(collateral), sum(encours)),
    cdf = safe_div(sum(encours[devise == "CDF"]), sum(encours)),
    prod = sum(production_ytd),
    autor = sum(autorisation),
    n = dplyr::n(),
    encours = sum(encours), .groups = "drop")
}

hhi <- function(x) { s <- x / sum(x); sum((100 * s)^2) }

# courbes de vintage : taux cumulé de défaut (en nombre de dossiers) par mois depuis l'octroi
vintage_curves <- function(d, months = c(0, 3, 6, 9, 12, 15, 18, 21, 24, 30, 36)) {
  out <- lapply(sort(unique(d$cohorte)), function(c) {
    dc <- d[d$cohorte == c, ]
    r <- sapply(months, function(m) {
      elig <- dc$mob >= m
      if (sum(elig) < 25) return(NA_real_)
      # en nombre de dossiers (insensible aux tickets corporate isolés)
      sum(elig & !is.na(dc$mob_defaut) & dc$mob_defaut <= m) / sum(elig)
    })
    ok <- !is.na(r)
    r[ok] <- cummax(r[ok])
    data.frame(cohorte = as.character(c), mob = months, taux = r)
  })
  do.call(rbind, out)
}

# matrice de transition Q2 -> Q3 (en montants Q2)
transition_matrix <- function(d) {
  e <- d[!is.na(d$stage_q2), ]
  dest <- ifelse(e$statut == "Passé en perte", "Write-off", paste("Stage", e$stage))
  m <- tapply(e$encours_q2, list(paste("Stage", e$stage_q2), factor(dest, c("Stage 1", "Stage 2", "Stage 3", "Write-off"))), sum)
  m[is.na(m)] <- 0
  m
}

# ---------------------------------------------------------------- stress testing
SCENARIOS <- data.frame(
  scenario = c("Baseline", "Adverse modéré", "Sévère systémique"),
  poids    = c(.60, .25, .15),
  cuivre   = c(9400, 7800, 5800),
  cobalt   = c(28500, 21000, 14000),
  fx       = c(2840, 3250, 3900),
  pib      = c(.048, .015, -.018),
  taux_bcc = c(.1425, .18, .25),
  inflation = c(.082, .154, .285),
  stringsAsFactors = FALSE
)

# multiplicateurs de PD par crédit selon le scénario macro
stress_pd_mult <- function(d, sc) {
  base <- SCENARIOS[1, ]
  sx <- match(d$secteur, SECTEURS$secteur)
  d_cu   <- pmax(0, (base$cuivre - sc$cuivre) / base$cuivre)
  d_fx   <- pmax(0, (sc$fx - base$fx) / base$fx)
  d_gdp  <- pmax(0, base$pib - sc$pib)
  d_rate <- pmax(0, sc$taux_bcc - base$taux_bcc)
  fx_exp <- ifelse(d$devise == "CDF", 1.4, ifelse(d$segment %in% c("PME", "Retail & Particuliers"), 1, .4))
  cbind(
    cuivre = SECTEURS$b_cu[sx] * d_cu * 1.7,
    fx     = SECTEURS$b_fx[sx] * d_fx * fx_exp * 1.3,
    taux   = SECTEURS$b_rate[sx] * d_rate * ifelse(d$devise == "CDF", 3, 1),
    pib    = SECTEURS$b_gdp[sx] * d_gdp * 4.5
  )
}

run_stress <- function(d, sc, quarters = 9) {
  comp <- stress_pd_mult(d, sc)
  mult <- 1 + rowSums(comp)
  lgd_s <- pmin(.95, d$lgd * (1 + .15 * (mult - 1) / pmax(1, max(mult) - 1)))
  pd_s <- pmin(.95, d$pd12 * mult)
  # ECL stressé : migrations S1->S2 accrues + nouveaux défauts (écart vs scénario neutre)
  ecl_f <- function(m, lgd_x) {
    pd_x <- pmin(.95, d$pd12 * m)
    mig <- pmin(.6, (pd_x - d$pd12) * 2.5)
    e <- ifelse(d$stage == 3, d$encours * lgd_x,
         ifelse(d$stage == 2, d$encours * pmin(1, d$pd_lt * m * (1 + (m - 1) * .5)) * lgd_x,
                d$encours * ((1 - mig) * pd_x + mig * pmin(1, d$pd_lt * m)) * lgd_x))
    pmin(e, d$encours * lgd_x)
  }
  ecl_s <- pmax(d$ecl, d$ecl + ecl_f(mult, lgd_s) - ecl_f(1, d$lgd))
  # RWA : inflation par migration de ratings + effet change sur créances USD détenues en CDF
  rwa_s <- d$rwa * (1 + .35 * pmin(1, (mult - 1)))
  rwa_tot_s <- sum(rwa_s) * (RWA_TOTAL / sum(d$rwa))
  # trajectoire trimestrielle (perte concentrée au T3/T4, puis rétention organique)
  dloss <- sum(ecl_s) - sum(d$ecl)
  shape <- c(0, .18, .42, .70, .88, .96, 1, 1, 1)[seq_len(quarters)]
  pnb_q <- sum(d$revenu_net + d$el) / 4 * .35          # résultat trimestriel retenu
  stress_drag <- 1 - .6 * min(1, dloss / (FONDS_PROPRES * .4))
  earn <- cumsum(c(0, rep(pnb_q * stress_drag, quarters - 1)))
  rwa_path <- RWA_TOTAL + (rwa_tot_s - RWA_TOTAL) * pmin(1, shape * 1.2)
  cap_path <- FONDS_PROPRES - dloss * shape + earn * .55
  car <- cap_path / rwa_path
  list(mult = mult, comp = comp, ecl_s = ecl_s, dloss = dloss, rwa_s = rwa_tot_s,
       car = car, cap = cap_path, rwa_path = rwa_path,
       labels = c("Q3 2026", "Q4 2026", "Q1 2027", "Q2 2027", "Q3 2027", "Q4 2027",
                  "Q1 2028", "Q2 2028", "Q3 2028")[seq_len(quarters)])
}

# décomposition du choc de CAR par facteur (chaque facteur isolé)
car_bridge <- function(d, sc) {
  base <- SCENARIOS[1, ]
  car0 <- FONDS_PROPRES / RWA_TOTAL
  full <- run_stress(d, sc)
  t <- which.min(full$car)
  car_base <- run_stress(d, base)$car[t]
  one <- function(fields) {
    s <- base; s[fields] <- sc[fields]
    run_stress(d, s)$car[t] - car_base
  }
  imp <- c(
    "Chute cuivre & cobalt (migration ratings)" = one(c("cuivre", "cobalt")),
    "Dépréciation CDF (inflation RWA devises)"  = one("fx"),
    "Hausse taux directeur (pression DSR)"      = one("taux_bcc"),
    "Récession / contagion PIB"                = one("pib")
  )
  inter <- (full$car[t] - car_base) - sum(imp)
  imp <- c(imp, "Effets croisés (non-linéarités)" = inter,
           "Absorption par le PNB résiduel" = car_base - car0)
  list(car0 = car0, impacts = imp, car_final = full$car[t], t = t, label = full$labels[t])
}
