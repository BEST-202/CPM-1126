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

## Pages

1. **Vue d'ensemble** : KPIs (encours, NPL, PAR30, ECL), staging par segment et par secteur, HHI, vintage, performance par région, alertes EWS générées à partir des données.
2. **Segmentation & Régions** : carte des 50 agences (exposition / PAR30 / part CDF / collatéraux), staging régional, sensibilité FX, matrice des agences et délégations.
3. **Vintage & Staging IFRS 9** : courbes de vintage par cohorte et segment, matrice de transition, flux de réconciliation, décomposition PD × LGD × EAD, pondération des scénarios forward-looking.
4. **Concentration & HHI** : limites sectorielles, jauge HHI, courbe de Lorenz, Top 20 des groupes (ratio / Tier 1, exemption souveraine), carte de chaleur, stress du plus grand débiteur.
5. **Performance RMs & Agences** : matrice RAROC × coût du risque, benchmark des 5 régions, scorecards des RMs et des agences, dérogations.
6. **Stress Testing & RAROC** : scénarios Baseline / Adverse / Sévère ou personnalisés (curseurs), trajectoire du CAR, cascade des impacts, impact par secteur, reverse stress test, RAROC stressé.
7. **Reporting réglementaire** : synthèse trimestrielle, benchmark des agences, export CSV et instance XML (format type XBRL, simplifié).

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
src/ui.R                  # mise en page et 7 pages
src/server/server_*.R     # logique serveur de chaque page
www/styles.css, app.js    # thème et navigation
```

Les paramètres (nom de la banque, seuils prudentiels, taux de change, scénarios) se modifient dans
`src/config.R` et `src/helpers.R` (`SCENARIOS`).
