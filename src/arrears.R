# =============================================================================
#  Modèle de gestion des arriérés précoces (early collections)
#   1. Tranches de retard & roll rates (M-1 -> M)
#   2. Score de bascule : probabilité qu'un dossier en arriérés précoces
#      (1-89 jours) ou à signaux d'alerte passe en PAR30+ / aggrave sa tranche le mois suivant
#   3. Segmentation risque x montant -> stratégie de recouvrement & SLA
#   4. Priorisation sous contrainte de capacité (file d'actions)
# =============================================================================

BUCKETS <- c("Courant", "1-7 j", "8-15 j", "16-29 j", "30-59 j", "60-89 j", "90+ j")
bucket_of <- function(dpd) {
  factor(as.character(cut(dpd, c(-Inf, 0, 7, 15, 29, 59, 89, Inf), labels = BUCKETS)), BUCKETS)
}
BUCKET_COLS <- c("#0f9d58", "#a3d977", "#f6c343", "#f59e0b", "#e07b00", "#d9480f", "#b91c1c")

SCORE_VARS <- c("Constante", "Jours de retard (par jour)", "log(PD 12 mois / 2 %)",
                "Antécédent SICR (Stage 2/3 au trimestre précédent)", "Crédit en CDF",
                "Cohorte récente (< 12 mois)", "Segment PME", "Segment Retail",
                "Tirage > 90 % de l'autorisation", "Collatéral > 120 %",
                "log(multiplicateur risque région)", "Client injoignable")

score_features <- function(dpd, d) {
  rx <- REGIONS$risk_mult[match(d$region_code, REGIONS$region_code)]
  X <- cbind(1, pmin(dpd, 89), log(pmax(d$pd12, 1e-4) / .02), as.numeric(!is.na(d$stage_q2) & d$stage_q2 >= 2),
             as.numeric(d$devise == "CDF"), as.numeric(d$mob < 12),
             as.numeric(d$segment == "PME"), as.numeric(d$segment == "Retail & Particuliers"),
             as.numeric(d$encours / pmax(d$autorisation, 1) > .9),
             as.numeric(d$collateral / pmax(d$encours, 1) > 1.2), log(rx), as.numeric(!d$joignable))
  colnames(X) <- SCORE_VARS
  X
}

# Échantillon de modélisation : dossiers en retard (1-89 j) ou sous surveillance à M-1,
# cible = aggravation du retard au-delà de 30 jours à M
score_sample <- function(d) {
  e <- d[(d$dpd_m1 >= 1 & d$dpd_m1 < 90) | (d$dpd_m1 == 0 & d$stage == 2), ]
  list(X = score_features(e$dpd_m1, e), y = as.numeric(e$dpd >= 30 & e$dpd > e$dpd_m1), d = e)
}

# Estimation par régression logistique (70 % apprentissage / 30 % test, graine fixe)
fit_score <- function(d, seed = 2026) {
  s <- score_sample(d)
  set.seed(seed)
  train <- runif(length(s$y)) < .7
  fit <- suppressWarnings(glm.fit(s$X[train, ], s$y[train], family = binomial()))
  beta <- setNames(fit$coefficients, SCORE_VARS)
  beta[is.na(beta)] <- 0
  list(coef = beta, se = sqrt(pmax(0, diag(tryCatch(solve(crossprod(s$X[train, ] * sqrt(fit$weights))),
                                                     error = function(e) diag(NA, ncol(s$X)))))),
       train = train, sample = s)
}

SCORE_MODEL <- fit_score(PORTFOLIO[PORTFOLIO$statut == "Actif", ])
SCORE_COEF <- SCORE_MODEL$coef

arrears_score <- function(d) plogis(as.vector(score_features(d$dpd, d) %*% SCORE_COEF))

score_class <- function(p) cut(p, c(-Inf, .10, .25, .50, Inf), labels = c("A — Faible", "B — Modéré", "C — Élevé", "D — Critique"))

# Backtest hors échantillon : score calculé à M-1 vs bascule observée à M
backtest <- function(d) {
  s <- score_sample(d)
  if (length(s$y) < 30 || length(unique(s$y)) < 2) return(NULL)
  test <- !(s$d$loan_id %in% SCORE_MODEL$sample$d$loan_id[SCORE_MODEL$train])
  if (sum(test) < 30 || length(unique(s$y[test])) < 2) test <- rep(TRUE, length(s$y))
  p <- plogis(as.vector(s$X[test, , drop = FALSE] %*% SCORE_COEF)); y <- s$y[test]
  r <- rank(p); n1 <- sum(y); n0 <- sum(1 - y)
  auc <- (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  dec <- cut(rank(p, ties.method = "first"), 10, labels = FALSE)
  cal <- data.frame(decile = 1:10, predit = as.vector(tapply(p, dec, mean)), observe = as.vector(tapply(y, dec, mean)))
  o <- order(p, decreasing = TRUE)
  ks <- max(abs(cumsum(y[o]) / n1 - cumsum(1 - y[o]) / n0))
  list(auc = auc, gini = 2 * auc - 1, ks = ks, n = length(y), taux = mean(y), cal = cal)
}

# Stratégie de recouvrement (matrice risque x montant)
assign_strategy <- function(d, p, seuil_montant) {
  high_risk <- p >= .25
  high_amt <- d$encours >= seuil_montant
  ifelse(d$dpd >= 60, "Pré-contentieux",
  ifelse(high_risk & high_amt, "Visite terrain & restructuration",
  ifelse(high_risk, "Centre d'appels intensif",
  ifelse(high_amt, "Relance personnalisée RM", "Relance digitale (self-cure)"))))
}

STRATEGIES <- data.frame(
  strategie = c("Visite terrain & restructuration", "Centre d'appels intensif", "Relance personnalisée RM",
                "Relance digitale (self-cure)", "Pré-contentieux"),
  quadrant  = c("Risque élevé · montant élevé", "Risque élevé · montant faible", "Risque faible · montant élevé",
                "Risque faible · montant faible", "Retard ≥ 60 jours"),
  action    = c("Visite du RM + analyste sous 48 h, revue du plan de trésorerie, proposition de rééchelonnement ou de garantie additionnelle au comité.",
                "Appel J+1 par l'agent de recouvrement, promesse de paiement tracée, visite à J+7 si promesse non tenue.",
                "Appel et courrier du RM sous 72 h, rappel des échéances, vérification des flux domiciliés.",
                "SMS / notification Mobile Money J+0, prélèvement automatique J+3, relance automatique J+7.",
                "Mise en demeure, inscription au fichier, transfert à l'unité de recouvrement spécialisé et revue des sûretés."),
  sla       = c("48 h", "24 h", "72 h", "Automatique", "5 jours"),
  icone     = c("person-walking-arrow-right", "headset", "user-tie", "mobile-screen", "gavel"),
  cls       = c("bad", "warn", "neutral", "ok", "dark"),
  efficacite = c(.45, .35, .30, .20, .15),   # réduction attendue de la probabilité de bascule
  stringsAsFactors = FALSE
)
