# Projet-personnel-Mod-lisation-Financi-re---Pricing-d-Options-Black-Scholes-Monte-Carlo
📌 Valorisation d’Options Européennes — Modèle de Black-Scholes & Simulation de Monte-Carlo
🎓 Master 1 MBFA – Ingénierie Économique & Financière
Université de Rennes — 2025-2026
Auteur : Nguyen Hoang Phuc PHAN
1. Présentation du projet
Ce projet développe un cadre complet de valorisation et de gestion du risque des options européennes (Call & Put) reposant sur :
✔ la formule fermée de Black-Scholes-Merton
✔ la simulation de Monte-Carlo en mesure risque-neutre
✔ des stratégies de couverture dynamique :
•	Delta-Hedging
•	Delta-Gamma Hedging
Les calculs sont réalisés à partir de données de marché réelles du titre Société Générale (GLE.PA).
L’objectif dépasse la simple valorisation : il consiste à comprendre le risque, modéliser l’incertitude et mesurer la performance des stratégies de couverture dans un environnement financier réaliste.
Projet Black Scholes
2. Contenu du dépôt
•	Téléchargement et traitement des données
•	Fonctions de pricing Black-Scholes
•	Moteur de simulation Monte-Carlo
•	Calcul des Greeks
•	Module de Delta-Hedging dynamique
•	Module de Delta-Gamma Hedging dynamique
•	Visualisations & analyses
2. Langage : 
Langage : R
Packages principaux :
quantmod
ggplot2
stats
3. Actif sous-jacent
Actif	Marché	Ticker	Raison du choix
Société Générale	Euronext Paris	GLE.PA	Liquidité élevée et pertinence financière
Période étudiée : 1 an
4. Paramètres de marché
•	Volatilité annualisée : σ = 34,97 %
•	Spot actuel : S = 67,98 €
•	Strike : K = 70 €
•	Taux sans risque (OAT 10 ans) : r = 3,5 %
•	Maturité : T = 1 an
5. Valorisation d’option
5.1. Résultats Black-Scholes
Résultat	Valeur
Prix du Call	10,48 €
Prix du Put	8,16 €
✔ Le prix du Call est croissant et convexe en fonction du spot.
5.2. Simulation Monte-Carlo
 Objectifs
(1) Visualiser la dynamique stochastique du sous-jacent
(2) Estimer la valeur théorique de l’option
50 000 simulations sous mesure risque-neutre :
Indicateur	Valeur
Prix estimé MC	10,4446 €
IC 95 %	[10,3350 ; 10,5543]
➡️ Convergence asymptotique vers Black-Scholes
✔ Loi des grands nombres
✔ TCL
6. Greeks
Greek	Interprétation	Valeur
Delta	Sensibilité au spot	0,6080
Gamma	Convexité	0,025572
Vega	Sensibilité à la volatilité	41,3317
Theta	Décroissance temporelle	−8,3076
Rho	Sensibilité au taux	30,8510
🔎 Points clés
✔ forte dépendance à la volatilité
✔ convexité significative
✔ theta négatif (time-decay)
7. Couverture dynamique
7.1 Delta-Hedging (rééquilibrage quotidien)
Principe :
•	Short Call → achat de Δ actions
•	Auto-financement
•	Ajustement quotidien
Position	PnL simulé
Short Call	−0,0636 €
Long Call	+0,0636 €
📌 Erreur résiduelle = Gamma + discrétisation
7.2. Delta-Gamma Hedging
Ajout d’une seconde option (strike 1,2K)
Position	PnL simulé
Short Call	+0,0205 €
Long Call	−0,0205 €
✔ Gamma ≈ 0 sur la majeure partie de l’horizon
✔ Réduction nette de l’erreur de réplication
✔ Résidu dû au rééquilibrage discret
8. Enseignements majeurs
•	Monte-Carlo valide Black-Scholes
•	Delta-Hedging supprime le risque directionnel
•	Le Gamma génère une erreur résiduelle
•	Delta-Gamma Hedging
➜ meilleure qualité de réplication
•	Les coûts augmentent à l’approche de l’échéance
9. Compétences démontrées
✔ Modélisation stochastique
✔ Valorisation dérivés
✔ Mesure du risque
✔ Traitement de données financières
✔ Implémentation algorithmique
✔ Analyse critique des stratégies de couverture
🏦 Pertinent pour :
•	Finance de marché
•	Gestion des risques
•	Ingénierie financière
•	Quantitative analysis
10. Pistes d’amélioration
🔹 volatilité stochastique (Heston)
🔹 sauts (Merton)
🔹 options américaines (LSM)
🔹 surface de volatilité
🔹 coûts de transaction
🔹 calibration empirique
 Auteur
Nguyen Hoang Phuc PHAN
Master 1 – MBFA
Université de Rennes (France)


