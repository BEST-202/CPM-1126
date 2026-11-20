# Credit Portfolio Management — Tableau de bord R Shiny

Cockpit de pilotage du risque de crédit (IFRS 9, concentration, RAROC, stress testing) pour une
banque fictive, **« Banque Démo RDC »**. Toutes les données sont **simulées** de façon
reproductible (`set.seed(1126)`).

## Portefeuille simulé

| Dimension | Contenu |
|---|---|
| **5 régions** | Région 1 — Ouest (Kinshasa & Kongo Central), Région 2 — Nord (Équateur), Région Centre (Kasaï), Région 3 — Sud (Katanga), Région 4 — Est (Kivu, Ituri, Tshopo, Maniema) |
| **50 agences** | 30 en Région 1 · 1 en Région 2 · 2 en Région Centre · 8 en Région 3 · 9 en Région 4 |
| **4 segments** | Corporate Grands Comptes · PME · Retail & Particuliers · Secteur Public & Souverain |
| **12 secteurs** | Commerce & Import-Export, Mines & Métaux, BTP, Télécoms & Fintech, Agriculture, Énergie & Utilities, Hydrocarbures, Transport & Logistique, Industrie, Immobilier & Habitat, Administration & Souverain, Ménages & Particuliers |

≈ 5 600 crédits, ≈ 3,25 Md USD d'encours, 83 chargés d'affaires, cohortes d'octroi 2023–2026,
staging IFRS 9 Q2 → Q3 2026 (matrice de Markov), PD / LGD / ECL, collatéraux, devises USD/CDF,
dérogations et rentabilité (RAROC, EVA).

## Pages (10)

1. **Vue d'ensemble** : KPIs (encours, NPL, PAR30, ECL), staging par segment et par secteur, HHI, vintage, performance par région, alertes EWS générées à partir des données.
2. **Segmentation & Régions** : carte des 50 agences (exposition / PAR30 / part CDF / collatéraux), staging régional, sensibilité FX, matrice des agences et délégations.
3. **Vintage & Staging IFRS 9** : courbes de vintage par cohorte et segment, matrice de transition, flux de réconciliation, décomposition PD × LGD × EAD, pondération des scénarios forward-looking.
4. **Arriérés précoces & recouvrement** (early collections) :
   - tranches de retard (1-7, 8-15, 16-29, 30-59, 60-89, 90+ jours) et matrice des *roll rates* M-1 → M ;
   - taux d'entrée en impayés, d'auto-régularisation (self-cure) et de bascule vers le NPL ;
   - **score de bascule** : régression logistique estimée sur 70 % de l'historique (retard, PD, antécédent SICR, devise, ancienneté, segment, tirage, collatéral, région, joignabilité), backtest hors échantillon sur 30 % (AUC, Gini, KS, calibration par décile), classes A à D ;
   - matrice stratégique risque × montant → canal de recouvrement et SLA (visite terrain, centre d'appels, relance RM, relance digitale, pré-contentieux) ;
   - simulateur de capacité (agents × dossiers/jour × efficacité) avec courbe de gains « priorisation vs hasard » ;
   - suivi par région et par agence, file d'actions priorisée par perte évitable (EAD × P(bascule) × LGD), export CSV.
5. **Concentration & HHI** : limites sectorielles, jauge HHI, courbe de Lorenz, Top 20 des groupes (ratio / Tier 1, exemption souveraine), carte de chaleur, stress du plus grand débiteur.
6. **Limites d'exposition** (périmètre consolidé) : 29 limites suivies — plus grande exposition individuelle (≤ 25 % des fonds propres, souverain exempté), somme des grands risques (> 10 % FP, ≤ 800 %), top 20 clients (≤ 40 %), 12 secteurs, 5 régions, 4 segments, part CDF, HHI, NPL, PAR30 et dépassements de délégation ; taux d'utilisation, marge disponible, statut (conforme / alerte > 85 % / dépassé), liste des grands risques, carte région × secteur, capacité d'octroi résiduelle, export CSV.
7. **Qualité des actifs des agences** :
   - qualité des actifs par agence (S1/S2/S3, PAR30, NPL, couverture ECL, note A à D) ;
   - concentration du NPL sur le top 10 des agences (Pareto) ;
   - poids des 5 plus gros clients NPL dans le NPL de ces 10 agences, et identification de ces clients (agence, RM, retard, ECL, garanties, niveau d'approbation) ;
   - pouvoir de décaissement : plafond de 1 000 000 USD par dossier en agence, dossiers au-delà validés en comité ou approuvés hors délégation, délégations par agence ;
   - top 20 clients (tous / NPL / Stage 2) ;
   - qualité du portefeuille par RM (graphique et scorecard).
8. **Performance RMs & Agences** : matrice RAROC × coût du risque, benchmark des 5 régions, scorecards des RMs et des agences, dérogations.
9. **Stress Testing & RAROC** : scénarios Baseline / Adverse / Sévère ou personnalisés (curseurs), trajectoire du CAR, cascade des impacts, impact par secteur, reverse stress test, RAROC stressé.
10. **Reporting réglementaire** : synthèse trimestrielle, benchmark des agences, export CSV et instance XML (format type XBRL, simplifié).

Filtres globaux (région, segment) et bascule de devise **USD / CDF** (2 840 CDF/USD) dans la barre supérieure.

## Lancer l'application

```r
install.packages(c("shiny", "bslib", "dplyr", "plotly", "DT"))
shiny::runApp()          # depuis la racine du projet
```

Versions testées : R 4.3, shiny 1.8, bslib 0.6, plotly 4.10, DT 0.31.
La carte (`scattergeo`) et les polices sont chargées depuis Internet (cdn.plot.ly, Google Fonts).

## Structure

```
app.R                     # point d'entrée : filtres, réactifs, export
src/config.R              # paramètres, régions, 50 agences, 12 secteurs, seuils
src/data.R                # générateur du portefeuille simulé
src/helpers.R             # formatage, composants UI, vintage, transitions, moteur de stress
src/arrears.R             # modèle d'arriérés précoces : tranches, score logistique, backtest, stratégies
src/ui.R                  # mise en page et 10 pages
src/server/server_*.R     # logique serveur de chaque page
www/styles.css, app.js    # thème et navigation
```

Les paramètres (nom de la banque, seuils prudentiels, taux de change, scénarios) se modifient dans
`src/config.R` et `src/helpers.R` (`SCENARIOS`).
