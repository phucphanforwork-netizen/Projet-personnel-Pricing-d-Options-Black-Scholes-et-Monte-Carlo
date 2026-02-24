# 📌 Valorisation d’Options Européennes — Modèle de Black-Scholes & Simulation de Monte-Carlo
## 🎓 Master 1 MBFA – Ingénierie Économique & Financière  
Université de Rennes — 2025-2026  
**Auteur : Nguyen Hoang Phuc PHAN**

---

## 1️⃣ Présentation du projet
Ce projet développe un cadre complet de valorisation et de gestion du risque des options européennes (Call & Put) reposant sur :
✔ la formule fermée de Black-Scholes-Merton  
✔ la simulation de Monte-Carlo en mesure risque-neutre  
✔ des stratégies de couverture dynamique :
- Delta-Hedging  
- Delta-Gamma Hedging  
Les calculs sont réalisés à partir de données de marché réelles du titre **Société Générale (GLE.PA)**.
L’objectif dépasse la simple valorisation :  
il consiste à comprendre le risque, modéliser l’incertitude et mesurer la performance des stratégies de couverture dans un environnement financier réaliste.

---

## 2️⃣ Contenu du dépôt
- Téléchargement et traitement des données
- Fonctions de pricing Black-Scholes
- Moteur de simulation Monte-Carlo
- Calcul des Greeks
- Module de Delta-Hedging dynamique
- Module de Delta-Gamma Hedging dynamique
- Visualisations & analyses

---
## 3️⃣ Langage et outils
**Langage : R**
**Packages principaux :**
- quantmod  
- ggplot2  
- stats  

---

## 4️⃣ Actif sous-jacent
| Actif | Marché | Ticker | Raison du choix |
|------|--------|--------|----------------|
| Société Générale | Euronext Paris | GLE.PA | Liquidité élevée et pertinence financière |
Période étudiée : **1 an**

---

## 5️⃣ Paramètres de marché
- Volatilité annualisée : **σ = 34,91 %**  
- Spot actuel : **S = 67,96 €**  
- Strike : **K = 68 €**  
- Taux sans risque (OAT 10 ans) : **r = 3,44 %**  
- Maturité : **T = 1 an**

---

## 6️⃣ Valorisation d’option

### 🔹 6.1 Résultats Black-Scholes

| Résultat | Valeur |
|---------|--------|
| Prix du Call | **10,43 €** |
| Prix du Put  | **8,17 €** |
✔ Le prix du Call est **croissant et convexe** en fonction du spot.

---

### 🔹 6.2 Simulation Monte-Carlo
**Objectifs :**
1. Visualiser la dynamique stochastique du sous-jacent  
2. Estimer la valeur théorique de l’option  
**50 000 simulations sous mesure risque-neutre :**

| Indicateur | Valeur |
|-----------|--------|
| Prix estimé MC | **10,415 €** |
| IC 95 % | **[10,3059 ; 10,524]** |

➡️ Convergence asymptotique vers Black-Scholes  
✔ Loi des grands nombres  
✔ Théorème central limite  

---

## 7️⃣ Greeks

| Greek | Interprétation | Valeur |
|------|----------------|--------|
| Delta | Sensibilité au spot | **0,6076** |
| Gamma | Convexité | **0,025612** |
| Vega  | Sensibilité à la volatilité | **41,2938** |
| Theta | Décroissance temporelle | **−8,2870** |
| Rho   | Sensibilité au taux | **30,8426** |

**Points clés :**
✔ forte dépendance à la volatilité  
✔ convexité significative  
✔ theta négatif (time-decay)  

---

## 8️⃣ Couverture dynamique

### 🔹 8.1 Delta-Hedging (rééquilibrage quotidien)
Principe :
- Short Call → achat de Δ actions  
- Auto-financement  
- Ajustement quotidien 

| Position | PnL simulé |
|---------|-----------|
| Short Call | **−0,0826 €** |
| Long Call  | **+0,0826 €** |

📌 Erreur résiduelle = **Gamma + discrétisation**

### 🔹 8.2 Delta-Gamma Hedging
Ajout d’une seconde option (strike = 1,2K)

| Position | PnL simulé |
|---------|-----------|
| Short Call | **+0,0274 €** |
| Long Call  | **−0,0274 €** |

✔ Gamma presque nul  
✔ Réduction significative de l’erreur de réplication  

---

## 9️⃣ Conclusion
- Monte-Carlo valide le modèle de Black-Scholes  
- Le Delta-Hedging supprime le risque directionnel  
- Le Gamma génère une erreur résiduelle de couverture  
- Le Delta-Gamma Hedging permet une **meilleure qualité de réplication**  
- Les coûts de couverture augmentent à l’approche de l’échéance  

---

## 👤 Auteur
**Nguyen Hoang Phuc PHAN**  
Master 1 – MBFA  
Université de Rennes

---

