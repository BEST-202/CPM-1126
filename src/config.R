# =============================================================================
#  Paramètres globaux du tableau de bord (banque fictive, données simulées)
# =============================================================================

BANK_NAME   <- "Banque Démo RDC"
BANK_SHORT  <- "BDR"
DIVISION    <- "Credit Risk Division"
USER_NAME   <- "Analyste CPM"
USER_ROLE   <- "Chef d'Unité Reporting — Direction des Risques"

ARRETE      <- as.Date("2026-09-30")   # date d'arrêté (Q3 2026)
ARRETE_Q2   <- as.Date("2026-06-30")
FX_RATE     <- 2840                    # CDF par USD (taux officiel)

# Normes prudentielles & seuils internes
SEUIL_NPL     <- 0.050   # norme NPL
SEUIL_PAR30   <- 0.045   # seuil interne PAR30
SEUIL_EPD     <- 0.020   # tolérance Early Payment Default à M6
SEUIL_HHI     <- 2000
SEUIL_COR     <- 0.025   # tolérance coût du risque RM (250 bps)
SEUIL_COR_ALERT <- 0.030 # alerte coût du risque (300 bps)    # norme HHI
LIMITE_INDIV  <- 0.25    # limite grand risque individuel (% fonds propres)
CAR_MIN       <- 0.10    # ratio de solvabilité minimum réglementaire
CAR_ALERT     <- 0.125   # seuil d'alerte COMEX
HURDLE        <- 0.16    # coût des fonds propres (Ke)
COST_FUNDS    <- c(USD = 0.045, CDF = 0.12)
CAP_ALLOC     <- 0.105   # fonds propres alloués / RWA pour le RAROC
CAR_INITIAL   <- 0.164   # CAR de départ (calibre les fonds propres)

# ---------------------------------------------------------------------------
# 5 régions géographiques
# ---------------------------------------------------------------------------
REGIONS <- data.frame(
  region_code = c("R1", "R2", "RC", "R3", "R4"),
  region      = c("Région 1 — Ouest", "Région 2 — Nord", "Région Centre",
                  "Région 3 — Sud", "Région 4 — Est"),
  zone        = c("Kinshasa & Kongo Central", "Équateur",
                  "Kasaï & Kasaï-Oriental", "Haut-Katanga, Lualaba, Tanganyika",
                  "Nord & Sud-Kivu, Ituri, Tshopo, Maniema"),
  risk_mult   = c(1.00, 1.25, 1.35, 0.90, 1.60),
  directeur   = c("J. Lusamba", "P. Bokele", "C. Kanku", "M. Ilunga", "A. Muhindo"),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# 4 segments
# ---------------------------------------------------------------------------
SEGMENTS <- c("Corporate Grands Comptes", "PME", "Retail & Particuliers",
              "Secteur Public & Souverain")

# ---------------------------------------------------------------------------
# 12 secteurs d'activité (limite = plafond interne en % de l'encours total)
#   b_cu : sensibilité au cours du cuivre/cobalt, b_fx : au change USD/CDF,
#   b_gdp : à la croissance, b_rate : au taux directeur
# ---------------------------------------------------------------------------
SECTEURS <- data.frame(
  secteur = c("Commerce & Import-Export", "Mines & Métaux", "BTP & Travaux Publics",
              "Télécoms & Fintech", "Agriculture & Agro-industrie", "Énergie & Utilities",
              "Hydrocarbures", "Transport & Logistique", "Industrie Manufacturière",
              "Immobilier & Habitat", "Administration & Souverain", "Ménages & Particuliers"),
  icone   = c("store", "gem", "helmet-safety", "tower-cell", "wheat-awn", "bolt",
              "oil-well", "truck", "industry", "house", "landmark", "users"),
  limite  = c(0.30, 0.20, 0.20, 0.18, 0.15, 0.12, 0.08, 0.10, 0.10, 0.08, 0.12, 0.15),
  pd_mult = c(1.10, 1.30, 1.30, 0.70, 1.40, 0.80, 0.90, 1.20, 1.00, 1.00, 0.50, 1.00),
  b_cu    = c(0.20, 1.60, 0.40, 0.10, 0.05, 0.30, 0.20, 0.50, 0.30, 0.20, 0.30, 0.15),
  b_fx    = c(0.90, 0.10, 0.40, 0.30, 0.50, 0.40, 0.20, 0.50, 0.60, 0.50, 0.30, 0.80),
  b_gdp   = c(0.80, 0.50, 0.90, 0.50, 0.70, 0.40, 0.40, 0.80, 0.70, 0.90, 0.30, 0.90),
  b_rate  = c(0.40, 0.20, 0.50, 0.30, 0.40, 0.20, 0.20, 0.40, 0.40, 0.70, 0.10, 0.80),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# 50 agences : 30 Région 1, 1 Région 2, 2 Région Centre, 8 Région 3, 9 Région 4
# ---------------------------------------------------------------------------
.kin <- c("Gombe Siège", "Gombe Boulevard", "Limete", "Kintambo", "Ngaliema", "Binza",
          "Matete", "Masina", "Kalamu", "Bandalungwa", "Lingwala", "Barumbu",
          "Kasa-Vubu", "Lemba", "Ndjili", "Kimbanseke", "Selembao", "Mont-Ngafula",
          "Marché Central", "Victoire", "Kingabwa", "Righini", "Kinkole", "Maluku")
.kin_lat <- -4.32 + c(0.02, 0.01, -0.03, 0.00, -0.02, -0.04, -0.06, -0.05, -0.03, -0.02,
                      0.00, 0.01, -0.01, -0.07, -0.08, -0.09, -0.06, -0.11, 0.00, -0.02,
                      -0.01, -0.08, -0.03, 0.12)
.kin_lon <- 15.30 + c(0.01, 0.02, 0.05, -0.03, -0.06, -0.08, 0.08, 0.15, 0.02, -0.01,
                      0.00, 0.01, 0.00, 0.03, 0.12, 0.18, -0.03, -0.02, 0.01, 0.01,
                      0.06, 0.04, 0.32, 0.45)

AGENCES <- data.frame(
  agence = c(.kin, "Matadi Port", "Matadi Ville", "Boma", "Muanda", "Mbanza-Ngungu", "Kisantu",
             "Mbandaka",
             "Kananga", "Mbuji-Mayi",
             "Lubumbashi Siège", "Lubumbashi Golf", "Kolwezi Mining Hub", "Likasi",
             "Kipushi", "Kasumbalesa", "Kalemie", "Kamina",
             "Goma", "Bukavu", "Uvira", "Butembo", "Beni", "Bunia", "Kisangani",
             "Kindu", "Kasindi"),
  region_code = c(rep("R1", 30), "R2", rep("RC", 2), rep("R3", 8), rep("R4", 9)),
  lat = c(.kin_lat, -5.82, -5.83, -5.85, -5.93, -5.25, -5.13, 0.05, -5.90, -6.13,
          -11.66, -11.68, -10.71, -10.98, -11.76, -12.25, -5.95, -8.74,
          -1.68, -2.51, -3.39, 0.14, 0.49, 1.56, 0.52, -2.95, 0.03),
  lon = c(.kin_lon, 13.45, 13.47, 13.05, 12.35, 14.86, 15.10, 18.26, 22.42, 23.59,
          27.48, 27.50, 25.47, 26.73, 27.25, 27.80, 29.19, 24.99,
          29.22, 28.86, 29.14, 29.29, 29.47, 30.25, 25.19, 25.92, 29.71),
  stringsAsFactors = FALSE
)
AGENCES$code <- sprintf("AG%02d", seq_len(nrow(AGENCES)))
AGENCES$type <- ifelse(AGENCES$agence %in% c("Gombe Siège", "Lubumbashi Siège"), "Siège",
                ifelse(AGENCES$agence %in% c("Kolwezi Mining Hub", "Matadi Port", "Goma",
                                             "Mbuji-Mayi", "Mbandaka"), "Hub", "Agence"))
AGENCES$region <- REGIONS$region[match(AGENCES$region_code, REGIONS$region_code)]
stopifnot(nrow(AGENCES) == 50,
          identical(as.vector(table(factor(AGENCES$region_code, REGIONS$region_code))),
                    c(30L, 1L, 2L, 8L, 9L)))

# Palette
COL <- list(
  navy = "#0f1b2d", ink = "#111827", muted = "#6b7280", line = "#e5e7eb",
  s1 = "#0f9d58", s2 = "#e07b00", s3 = "#d32f2f", red = "#b91c1c",
  green = "#0b8a4f", orange = "#d97706", blue = "#2563eb", grey = "#9ca3af"
)
STAGE_COLS <- c("Stage 1" = COL$s1, "Stage 2" = COL$s2, "Stage 3" = COL$s3)
REGION_COLS <- setNames(c("#1f3a93", "#7c3aed", "#d97706", "#0b8a4f", "#d32f2f"),
                        REGIONS$region)
