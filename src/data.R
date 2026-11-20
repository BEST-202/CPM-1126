# =============================================================================
#  Génération d'un portefeuille de crédit simulé (reproductible)
#  ~5 600 lignes de crédit, encours ~3,25 Md USD
# =============================================================================

.prenoms_h <- c("Patrick", "Eric", "Dieudonné", "Aimé", "Serge", "Jean-Marie", "Fiston",
                "Junior", "Blaise", "Olivier", "Héritier", "Cédric", "Trésor", "Bienvenu",
                "Josué", "Christian", "Guy", "Didier", "Papy", "Franck")
.prenoms_f <- c("Grâce", "Sylvie", "Clarisse", "Nadine", "Christelle", "Arlette", "Rachel",
                "Gloria", "Esther", "Divine", "Prisca", "Ruth", "Merveille", "Laetitia",
                "Nathalie", "Sarah", "Joëlle", "Carine")
.noms <- c("Ilunga", "Kalonji", "Mutombo", "Tshimanga", "Mwamba", "Kabwe", "Banza",
           "Kasongo", "Lukusa", "Mbuyi", "Ngoy", "Nzuzi", "Makiese", "Lusamba", "Kanku",
           "Mpiana", "Bokele", "Lokwa", "Matondo", "Nsimba", "Kiala", "Muhindo", "Kambale",
           "Bahati", "Mumbere", "Kahindo", "Mapendo", "Amani", "Kalala", "Mulumba",
           "Tshibangu", "Mbala", "Kibambe", "Lumbala", "Ndaya", "Mavungu")

.tags <- c("Alpha", "Atlas", "Aurore", "Boréal", "Cèdre", "Delta", "Équinoxe", "Horizon",
           "Orion", "Sirius", "Nova", "Vega", "Lyra", "Phénix", "Titan", "Zénith", "Meridian",
           "Sahel", "Baobab", "Okapi", "Lualaba", "Kwango", "Ubangi", "Lomami", "Tanganyika",
           "Kivu", "Mayumbe", "Ruwenzori", "Upemba", "Kundelungu", "Mitumba", "Itombwe",
           "Sankuru", "Kasaï", "Maniema", "Ituri", "Lukuga", "Luapula", "Kalemie", "Inga",
           "Kwilu", "Mongala", "Lulua", "Uele", "Tshopo", "Bandundu", "Kibali", "Salonga",
           "Garamba", "Virunga", "Kahuzi", "Lufira", "Kafubu", "Dilolo", "Musonoi", "Kamoto",
           "Etoile", "Pangea", "Equator", "Congo River")

.prefixes <- list(
  "Mines & Métaux"               = c("Société Minière", "Copper & Cobalt", "Mining Services", "Métaux"),
  "Commerce & Import-Export"     = c("Groupe Commercial", "Distribution", "Import-Export", "Négoce"),
  "Télécoms & Fintech"           = c("Télécom", "Digital Infrastructure", "Fintech", "Mobile Money"),
  "Hydrocarbures"                = c("Pétroles", "Terminal Pétrolier", "Énergies Fossiles"),
  "Industrie Manufacturière"     = c("Brasseries", "Cimenterie", "Industries", "Plastiques"),
  "Énergie & Utilities"          = c("Hydro Power", "Énergie Solaire", "Électricité"),
  "BTP & Travaux Publics"        = c("Constructions", "Génie Civil", "Travaux Publics"),
  "Agriculture & Agro-industrie" = c("Agro", "Plantations", "Minoterie", "Huileries")
)

.pick_names <- function(n) {
  pool <- rbind(expand.grid(p = .prenoms_h, n = .noms, t = "M.", stringsAsFactors = FALSE),
                expand.grid(p = .prenoms_f, n = .noms, t = "Mme", stringsAsFactors = FALSE))
  pool <- pool[sample(nrow(pool), n), ]
  paste(pool$t, pool$p, pool$n)
}

.rate_grade <- function(pd) {
  cut(pd, c(-Inf, .003, .006, .010, .016, .025, .040, .065, .10, .18, Inf),
      labels = c("AAA", "AA", "A", "BBB+", "BBB", "BB+", "BB", "B+", "B", "CCC"))
}

gen_portfolio <- function(seed = 1126) {
  set.seed(seed)
  ag <- AGENCES
  code_of <- function(nm) ag$code[match(nm, ag$agence)]

  # ---------------------------------------------------------------- RMs
  corp_agences <- c(rep("Gombe Siège", 6), rep("Lubumbashi Siège", 2), "Kolwezi Mining Hub")
  n_rm_ag <- ifelse(ag$type == "Siège", 3, ifelse(ag$type == "Hub", 2,
                    ifelse(ag$region_code == "R1", 1, 2)))
  rms <- data.frame(
    agence_code = c(code_of(corp_agences), rep(ag$code, n_rm_ag)),
    portefeuille = c(rep("Corporate", length(corp_agences)), rep("Réseau", sum(n_rm_ag))),
    stringsAsFactors = FALSE
  )
  rms$rm <- .pick_names(nrow(rms))
  rms$rm_id <- sprintf("RM%03d", seq_len(nrow(rms)))
  rms$derog_bias <- rlnorm(nrow(rms), 0, 0.6)

  # ---------------------------------------------------------------- segments
  build <- function(segment, sector, amount_m, n, sdlog, agence_code, borrower, groupe) {
    a <- rlnorm(n, 0, sdlog)
    data.frame(segment = segment, secteur = sector, montant = a / sum(a) * amount_m * 1e6,
               agence_code = agence_code, emprunteur = borrower, groupe = groupe,
               stringsAsFactors = FALSE)
  }

  # Corporate : 1 400 M USD
  corp_t <- c("Mines & Métaux" = 600, "Commerce & Import-Export" = 280,
              "Télécoms & Fintech" = 210, "Hydrocarbures" = 100,
              "Industrie Manufacturière" = 90, "Énergie & Utilities" = 50,
              "BTP & Travaux Publics" = 40, "Agriculture & Agro-industrie" = 30)
  corp_n <- c(26, 24, 10, 8, 10, 5, 7, 6)
  corp <- do.call(rbind, lapply(seq_along(corp_t), function(i) {
    s <- names(corp_t)[i]; n <- corp_n[i]
    t <- sample(.tags, n)
    nm <- paste(sample(.prefixes[[s]], n, TRUE), t)
    grp <- ifelse(runif(n) < 0.3, paste("Groupe", sample(t[1:max(2, n %/% 4)], n, TRUE)), nm)
    agc <- if (s == "Mines & Métaux") {
      sample(code_of(c("Lubumbashi Siège", "Kolwezi Mining Hub")), n, TRUE, c(.45, .55))
    } else {
      sample(code_of(c("Gombe Siège", "Lubumbashi Siège", "Goma", "Matadi Port")), n, TRUE,
             c(.80, .10, .05, .05))
    }
    build("Corporate Grands Comptes", s, corp_t[[i]], n, 0.9, agc, nm, grp)
  }))

  # Secteur public : 600 M USD
  pub <- data.frame(
    emprunteur = c("Trésor Public — Bons du Trésor", "Province de Kinshasa",
                   "Province du Haut-Katanga", "Province du Lualaba",
                   "Entreprise Publique d'Électricité", "Régie Publique des Eaux",
                   "Office Public des Ports", "Société Publique des Chemins de Fer",
                   "Régie Aéroportuaire", "Office Public des Routes", "Fonds National d'Entretien Routier"),
    secteur = c(rep("Administration & Souverain", 4), rep("Énergie & Utilities", 2),
                rep("Transport & Logistique", 3), rep("BTP & Travaux Publics", 2)),
    montant = c(195, 60, 35, 25, 110, 50, 40, 30, 15, 25, 15) * 1e6,
    agence = c("Gombe Siège", "Gombe Siège", "Lubumbashi Siège", "Kolwezi Mining Hub",
               "Gombe Siège", "Gombe Siège", "Matadi Port", "Lubumbashi Siège", "Gombe Siège",
               "Gombe Siège", "Gombe Siège"),
    stringsAsFactors = FALSE
  )
  # plusieurs facilités par emprunteur public
  pub <- pub[rep(seq_len(nrow(pub)), c(4, 2, 2, 2, 3, 2, 2, 2, 1, 2, 1)), ]
  pub$montant <- ave(pub$montant, pub$emprunteur, FUN = function(x) x * prop.table(rlnorm(length(x), 0, .4)))
  pub <- data.frame(segment = "Secteur Public & Souverain", secteur = pub$secteur,
                    montant = pub$montant, agence_code = code_of(pub$agence),
                    emprunteur = pub$emprunteur,
                    groupe = ifelse(grepl("^Province|Trésor", pub$emprunteur),
                                    "État & Provinces", pub$emprunteur),
                    stringsAsFactors = FALSE)
  pub$groupe[pub$emprunteur %in% c("Office Public des Routes", "Fonds National d'Entretien Routier")] <-
    "Pôle Infrastructures Publiques"

  # Poids des agences pour le réseau (PME & Retail)
  w <- with(ag, ifelse(region_code == "R1", 1.0, ifelse(region_code == "R2", 1.6,
           ifelse(region_code == "RC", 2.4, ifelse(region_code == "R3", 2.4, 2.8)))))
  w[ag$agence %in% c("Gombe Siège", "Lubumbashi Siège")] <- 4
  w[ag$agence %in% c("Goma", "Bukavu")] <- 5
  w[ag$agence %in% c("Matadi Port", "Kolwezi Mining Hub", "Mbuji-Mayi")] <- 3

  # PME : 820 M USD
  n_pme <- 1500
  pme_sp <- c("Commerce & Import-Export" = .34, "BTP & Travaux Publics" = .13,
              "Agriculture & Agro-industrie" = .14, "Transport & Logistique" = .10,
              "Mines & Métaux" = .11, "Industrie Manufacturière" = .07,
              "Immobilier & Habitat" = .05, "Télécoms & Fintech" = .03, "Hydrocarbures" = .03)
  pme_sec <- sample(names(pme_sp), n_pme, TRUE, pme_sp)
  r3 <- ag$code[ag$region_code == "R3" & ag$agence %in%
                  c("Lubumbashi Siège", "Lubumbashi Golf", "Kolwezi Mining Hub", "Likasi", "Kipushi", "Kasumbalesa")]
  pme_ag <- ifelse(pme_sec == "Mines & Métaux", sample(r3, n_pme, TRUE),
                   sample(ag$code, n_pme, TRUE, w))
  # l'agriculture est sur-représentée à l'Est
  agri_est <- pme_sec == "Agriculture & Agro-industrie" & runif(n_pme) < .45
  pme_ag[agri_est] <- sample(ag$code[ag$region_code == "R4"], sum(agri_est), TRUE)
  pme_id <- sprintf("PME-%05d", seq_len(n_pme))
  pme <- build("PME", pme_sec, 820, n_pme, 1.0, pme_ag, pme_id, pme_id)

  # Retail : 430 M USD
  n_ret <- 4000
  ret_sec <- sample(c("Ménages & Particuliers", "Immobilier & Habitat"), n_ret, TRUE, c(.84, .16))
  ret_id <- sprintf("CLI-%06d", seq_len(n_ret))
  ret <- build("Retail & Particuliers", ret_sec, 430, n_ret, 0.8,
               sample(ag$code, n_ret, TRUE, w), ret_id, ret_id)

  df <- rbind(corp, pub, pme, ret)
  n <- nrow(df)
  df$loan_id <- sprintf("LN%06d", seq_len(n))
  df$agence <- ag$agence[match(df$agence_code, ag$code)]
  df$region_code <- ag$region_code[match(df$agence_code, ag$code)]
  df$region <- REGIONS$region[match(df$region_code, REGIONS$region_code)]

  # RM : Corporate -> RMs corporate de l'agence, sinon RMs réseau
  df$rm_id <- NA_character_
  for (i in seq_len(n)) {
    pool <- rms$rm_id[rms$agence_code == df$agence_code[i] &
                        rms$portefeuille == ifelse(df$segment[i] == "Corporate Grands Comptes" &
                                                     df$agence_code[i] %in% rms$agence_code[rms$portefeuille == "Corporate"],
                                                   "Corporate", "Réseau")]
    df$rm_id[i] <- if (length(pool) == 1) pool else sample(pool, 1)
  }
  df$rm <- rms$rm[match(df$rm_id, rms$rm_id)]

  # ------------------------------------------------------------ dates & devise
  seg <- df$segment
  is_net <- seg %in% c("PME", "Retail & Particuliers")
  coh <- ifelse(is_net, sample(2023:2026, n, TRUE, c(.12, .22, .33, .33)),
                        sample(2023:2026, n, TRUE, c(.25, .30, .25, .20)))
  start <- as.Date(paste0(coh, "-01-01"))
  end <- pmin(as.Date(paste0(coh, "-12-31")), ARRETE - 5)
  df$date_octroi <- start + floor(runif(n) * as.numeric(end - start))
  df$cohorte <- coh
  df$mob <- pmax(0, as.numeric(ARRETE - df$date_octroi) / 30.44)
  p_cdf <- c("Corporate Grands Comptes" = .03, "PME" = .18, "Retail & Particuliers" = .38,
             "Secteur Public & Souverain" = .10)[seg]
  df$devise <- ifelse(runif(n) < p_cdf, "CDF", "USD")
  df$devise[df$emprunteur == "Trésor Public — Bons du Trésor"] <- "USD"

  # ------------------------------------------------------------ risque
  sx <- match(df$secteur, SECTEURS$secteur)
  rx <- match(df$region_code, REGIONS$region_code)
  base_pd <- c("Corporate Grands Comptes" = .012, "PME" = .038, "Retail & Particuliers" = .028,
               "Secteur Public & Souverain" = .006)[seg]
  coh_mult <- c(`2023` = 1, `2024` = 0.95, `2025` = 1.10, `2026` = 1.25)[as.character(coh)]
  sub_mines <- df$segment == "PME" & df$secteur == "Mines & Métaux"
  pd <- base_pd * SECTEURS$pd_mult[sx] * REGIONS$risk_mult[rx] * coh_mult *
    ifelse(sub_mines, 1.5, 1) * exp(rnorm(n, 0, .35))
  pd[df$emprunteur == "Trésor Public — Bons du Trésor"] <- .004
  df$pd12 <- pmin(.45, pd)
  mat <- c("Corporate Grands Comptes" = 5, "PME" = 3, "Retail & Particuliers" = 4,
           "Secteur Public & Souverain" = 7)[seg]
  df$maturite_ans <- mat
  df$pd_lt <- 1 - (1 - df$pd12)^pmax(1, mat - df$mob / 12)
  lgd0 <- c("Corporate Grands Comptes" = .38, "PME" = .52, "Retail & Particuliers" = .58,
            "Secteur Public & Souverain" = .20)[seg]
  df$lgd <- pmin(.9, pmax(.05, lgd0 + rnorm(n, 0, .05)))
  df$lgd[df$secteur == "Administration & Souverain"] <- .10
  df$rating <- as.character(.rate_grade(df$pd12))

  # ------------------------------------------------------------ staging Q2 -> Q3
  existed_q2 <- df$date_octroi <= ARRETE_Q2
  mfac <- pmin(1, pmax(0, df$mob - 3) / 12)^0.6
  u <- runif(n)
  p3 <- pmin(.8, df$pd12 * 1.1 * mfac); p2 <- pmin(.6, df$pd12 * 1.0 * mfac)
  prev <- ifelse(u < p3, 3L, ifelse(u < p3 + p2, 2L, 1L))
  prev[!existed_q2] <- NA_integer_

  u <- runif(n)
  rm_ <- REGIONS$risk_mult[rx] * SECTEURS$pd_mult[sx]
  cur <- integer(n)
  # depuis stage 1
  t12 <- pmin(.4, df$pd12 * 0.40 + .003); t13 <- df$pd12 * 0.12
  # depuis stage 2
  t21 <- rep(.13, n); t23 <- pmin(.5, .06 * rm_)
  # depuis stage 3
  t32 <- rep(.04, n); t3w <- rep(.035, n)
  wo <- rep(FALSE, n)
  i1 <- which(prev == 1); i2 <- which(prev == 2); i3 <- which(prev == 3); i0 <- which(is.na(prev))
  cur[i1] <- ifelse(u[i1] < t13[i1], 3L, ifelse(u[i1] < t13[i1] + t12[i1], 2L, 1L))
  cur[i2] <- ifelse(u[i2] < t21[i2], 1L, ifelse(u[i2] < t21[i2] + t23[i2], 3L, 2L))
  cur[i3] <- ifelse(u[i3] < t3w[i3], 3L, ifelse(u[i3] < t3w[i3] + t32[i3], 2L, 3L))
  wo[i3] <- u[i3] < t3w[i3]
  cur[i0] <- ifelse(u[i0] < df$pd12[i0] * .12, 3L, ifelse(u[i0] < df$pd12[i0] * .8, 2L, 1L))
  # Quelques grands comptes en défaut / sous surveillance (réalisme)
  big <- order(-df$montant)
  cor_idx <- which(df$segment == "Corporate Grands Comptes" & df$cohorte <= 2025)
  pick <- function(sec, k) head(intersect(big, cor_idx[df$secteur[cor_idx] == sec]), 12)[k]
  for (j in c(pick("BTP & Travaux Publics", 1), pick("Agriculture & Agro-industrie", 2))) {
    prev[j] <- 2L; cur[j] <- 3L }
  for (j in c(pick("Mines & Métaux", 5), pick("Mines & Métaux", 9), pick("Commerce & Import-Export", 4),
              pick("Industrie Manufacturière", 3))) { prev[j] <- 1L; cur[j] <- 2L }
  j <- which(df$emprunteur == "Office Public des Routes")[1]; prev[j] <- 1L; cur[j] <- 2L
  df$stage_q2 <- prev
  df$stage <- cur
  df$statut <- ifelse(wo, "Passé en perte", "Actif")

  # jours de retard
  df$dpd <- ifelse(cur == 1, ifelse(runif(n) < .86, 0L, sample(1:29, n, TRUE)),
            ifelse(cur == 2, ifelse(runif(n) < .40, sample(30:89, n, TRUE), sample(0:29, n, TRUE)),
                   sample(90:540, n, TRUE)))
  corp_s2 <- cur == 2 & df$segment != "PME" & df$segment != "Retail & Particuliers"
  df$dpd[corp_s2] <- ifelse(runif(sum(corp_s2)) < .1, sample(30:60, sum(corp_s2), TRUE), sample(0:29, sum(corp_s2), TRUE))
  df$sicr_motif <- ifelse(cur != 2, NA_character_,
                   ifelse(df$dpd >= 30, "Arriérés 30-89 jours",
                          sample(c("Watchlist qualitative", "Forbearance / Restructuré"), n, TRUE, c(.7, .3))))
  # mois de défaut (vintage)
  df$mob_defaut <- NA_real_
  d3 <- which(cur == 3 | wo)
  # mois de défaut : loi gamma tronquée à l'âge du crédit (EPD plus précoce sur 2026)
  sc <- ifelse(df$cohorte[d3] == 2026, 3.2, 6)
  df$mob_defaut[d3] <- pmax(1, qgamma(runif(length(d3)) * pgamma(pmax(1, df$mob[d3]), 2.3, scale = sc),
                                      2.3, scale = sc))

  # ------------------------------------------------------------ encours & historiques
  df$encours <- ifelse(wo, 0, df$montant)
  df$encours_q2 <- ifelse(existed_q2, df$montant * runif(n, 1.01, 1.07), 0)
  df$encours_n1 <- ifelse(df$date_octroi <= ARRETE - 365, df$montant * runif(n, 1.08, 1.35), 0)
  df$autorisation <- df$montant / runif(n, .62, .97)
  df$collateral <- df$montant * rlnorm(n, log(c("Corporate Grands Comptes" = 1.35, "PME" = 1.15,
                                                 "Retail & Particuliers" = .85,
                                                 "Secteur Public & Souverain" = .60)[seg]), .25)
  df$type_garantie <- ifelse(seg == "Corporate Grands Comptes",
                             ifelse(df$secteur == "Mines & Métaux", "Warrants & Offtake",
                                    sample(c("Hypothèques", "Nantissement stocks", "Cession créances"), n, TRUE)),
                      ifelse(seg == "Secteur Public & Souverain", "Garantie État / Escrow",
                      ifelse(seg == "PME", sample(c("Hypothèques", "Gages équipements", "Caution"), n, TRUE),
                             sample(c("Domiciliation salaire", "Hypothèque", "Caution"), n, TRUE))))

  # ------------------------------------------------------------ ECL IFRS 9
  df$ecl <- with(df, ifelse(stage == 1, encours * pd12 * lgd,
                     ifelse(stage == 2, encours * pd_lt * lgd, encours * lgd)))
  df$ecl_q2 <- with(df, ifelse(is.na(stage_q2), 0,
                        ifelse(stage_q2 == 1, encours_q2 * pd12 * lgd,
                        ifelse(stage_q2 == 2, encours_q2 * pd_lt * lgd, encours_q2 * lgd))))

  # ------------------------------------------------------------ rentabilité (RAROC)
  taux0 <- c("Corporate Grands Comptes" = .077, "PME" = .116, "Retail & Particuliers" = .135,
             "Secteur Public & Souverain" = .062)[seg]
  df$taux <- taux0 + df$pd12 * .25 + ifelse(df$devise == "CDF", .08, 0)
  opex <- c("Corporate Grands Comptes" = .008, "PME" = .030, "Retail & Particuliers" = .050,
            "Secteur Public & Souverain" = .004)[seg]
  rw <- c("Corporate Grands Comptes" = 1, "PME" = 1, "Retail & Particuliers" = .75,
          "Secteur Public & Souverain" = 1)[seg]
  rw[df$secteur == "Administration & Souverain"] <- .2
  rw[df$stage == 3] <- 1.5
  df$rw <- rw
  df$rwa <- df$encours * rw
  df$capital <- df$rwa * CAP_ALLOC
  df$el <- df$encours * df$pd12 * df$lgd
  # surcoût opérationnel du réseau en province (logistique, sécurité, transport de fonds)
  opex <- opex * c(R1 = 1.0, R2 = 1.2, RC = 1.2, R3 = 1.05, R4 = 1.3)[df$region_code] * exp(rnorm(n, 0, .15))
  df$revenu_net <- df$encours * (df$taux - COST_FUNDS[df$devise]) - df$encours * opex - df$el
  df$production_ytd <- ifelse(format(df$date_octroi, "%Y") == "2026", df$montant, 0)
  df$derogation <- ifelse(runif(n) < pmin(.5, .045 * rms$derog_bias[match(df$rm_id, rms$rm_id)]),
                          sample(c("Taux & Marge", "Collatéral < 120%", "Maturité"), n, TRUE, c(.43, .36, .21)),
                          NA_character_)
  df$rm_id <- df$rm_id
  df
}

PORTFOLIO <- gen_portfolio()

# Fonds propres calibrés pour un CAR initial de 16,4 % (RWA crédit + RWA opérationnel/marché)
RWA_CREDIT   <- sum(PORTFOLIO$rwa[PORTFOLIO$statut == "Actif"])
RWA_AUTRES   <- 0.16 * RWA_CREDIT
RWA_TOTAL    <- RWA_CREDIT + RWA_AUTRES
FONDS_PROPRES <- CAR_INITIAL * RWA_TOTAL
TIER1        <- 0.87 * FONDS_PROPRES

# Objectifs commerciaux simulés par agence
set.seed(42)
OBJECTIFS <- {
  p <- aggregate(cbind(production_ytd, encours_n1) ~ agence_code, PORTFOLIO, sum)
  p$objectif_prod <- p$production_ytd / runif(nrow(p), .85, 1.25)
  p$budget_encours <- p$encours_n1 * runif(nrow(p), 1.05, 1.20)
  p
}
