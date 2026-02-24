install.packages("quantmod")  # Lấy dữ liệu giá cổ phiếu từ Yahoo
install.packages("ggplot2")   # Vẽ đồ thị đẹp (có thể bỏ, nếu bạn muốn dùng base R)

# ==========================
# 1) Packages & Données
# ==========================

library(quantmod)  # Pour télécharger les données de marché (Yahoo Finance)
library(ggplot2)   # Pour la visualisation
library(stats)     # Pour pnorm(), rnorm() etc. (déjà inclus de base)

# --------------------------
# Téléchargement des données GLE.PA sur 1 an
# --------------------------

# Retirer les données durant une dernière année:
date_debut<- as.Date("2024-12-30")
date_fin   <- as.Date("2025-12-30")
getSymbols("GLE.PA", src = "yahoo", from = date_debut, to = date_fin, auto.assign = TRUE)

# Objet retourné : GLE.PA (xts)
# On extrait les cours de clôture
close_prices <- Cl(GLE.PA)

tail(close_prices)

# --------------------------
# Calcul de la volatilité annualisée
# --------------------------

# Rendements journaliers:
returns_daily <- dailyReturn(close_prices, type = "arithmetic")

tail(returns_daily)

# Volatilité quotidienne (écart-type des rendements)
vol_daily <- sd(returns_daily, na.rm = TRUE)

# Annualisation (252 jours de trading)
sigma <- as.numeric(vol_daily * sqrt(252))
cat(sprintf("Volatilité annuelle : %.2f%%\n", sigma * 100))

# Spot actuel (dernier cours de clôture)
S <- as.numeric(last(close_prices))
cat(sprintf("Spot actuel : %.2f EUR\n", S))

# ==========================
# 2) Fonctions de pricing
# ==========================

# 2.1. Black-Scholes (Call & Put européens)
pricer_option <- function(S, K, T, r, sigma) {
  # S : spot
  # K : strike
  # T : maturité en années
  # r : taux sans risque
  # sigma : volatilité annualisée
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  call <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  put  <- - S + call + K * exp(-r*T)
  
  return(list(call = call, put = put))
}

# 2.2. Monte Carlo – Mouvement Brownien Géométrique
monte_carlo_option <- function(S, K, T, r, sigma, n_sims = 50000, type_option = "call") {
  set.seed(50)  # Pour reproductibilité
  
  # Tirages Z ~ N(0,1)
  Z <- rnorm(n_sims)
  
  # Simulation de S_T
  S_T <- S * exp((r - 0.5 * sigma^2) * T + sigma * sqrt(T) * Z)
  
  # Payoffs
  if (type_option == "call") {
    payoffs <- pmax(S_T - K, 0)
  } else {
    payoffs <- pmax(K - S_T, 0)
  }
  
  # Prix présent (actualisation)
  prix_mc <- exp(-r * T) * mean(payoffs)
  
  # Écart-type du payoff actualisé
  ecart_type <- sd(payoffs * exp(-r * T))
  erreur_standard <- ecart_type / sqrt(n_sims)
  
  return(list(prix = prix_mc, erreur_standard = erreur_standard))
}

# 2.3. Les coefficients grecs
#Delta (sensibilité par rapport à S)
calcul_delta <- function(S, K, T, r, sigma, type_option = "call") {
  # Sécurité : si T est très petit → gestion cas limite
  if (T <= 1e-5) {
    if (type_option == "call") {
      return(ifelse(S > K, 1.0, 0.0))
    } else {
      return(ifelse(S < K, -1.0, 0.0))
    }
  }
  
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  if (type_option == "call") {
    delta <- pnorm(d1)
  } else {
    delta <- pnorm(d1) - 1
  }
  
  return(delta)
}

# Gamma : même pour Call et Put
calcul_gamma <- function(S, K, T, r, sigma) {
  if (T <= 1e-5) return(0)
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  gamma <- dnorm(d1) / (S * sigma * sqrt(T))
  return(gamma)
}
# Vega : même pour Call et Put
# Attention : ici Vega = dérivée par rapport à sigma (par "1.00" de volatilité, pas par 1%)
calcul_vega <- function(S, K, T, r, sigma) {
  if (T <= 1e-5) return(0)
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  vega <- S * dnorm(d1) * sqrt(T)
  return(vega)
}

# Theta : dépend du type d'option
# Theta ici est exprimé "par an"
calcul_theta <- function(S, K, T, r, sigma, type_option = "call") {
  if (T <= 1e-5) return(0)
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  terme1 <- -(S * dnorm(d1) * sigma) / (2 * sqrt(T))
  
  if (type_option == "call") {
    theta <- terme1 - r * K * exp(-r * T) * pnorm(d2)
  } else {
    theta <- terme1 + r * K * exp(-r * T) * pnorm(-d2)
  }
  return(theta)
}

# Rho : dépend du type d'option
calcul_rho <- function(S, K, T, r, sigma, type_option = "call") {
  if (T <= 1e-5) return(0)
  
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  if (type_option == "call") {
    rho <- K * T * exp(-r * T) * pnorm(d2)
  } else {
    rho <- -K * T * exp(-r * T) * pnorm(-d2)
  }
  
  return(rho)
}

# ==========================
# 3) Exécution des calculs
# ==========================

K <- round(S)# Strike
T_mat <- 1   # Maturité en années
r <- 0.035    # Taux sans risque annuel (3.5% = Taux OAT) 
r <- log(1 + r)

# Pricing théorique (Black-Scholes)
prix_bs <- pricer_option(S, K, T_mat, r, sigma)
prix_call_bs <- prix_bs$call
prix_put_bs  <- prix_bs$put

cat(sprintf("Prix théorique Call (BS) : %.2f EUR\n", prix_call_bs))
cat(sprintf("Prix théorique Put  (BS) : %.2f EUR\n", prix_put_bs))

# Pricing Monte Carlo
res_mc <- monte_carlo_option(S, K, T_mat, r, sigma, n_sims = 100000, type_option = "call")
prix_mc <- res_mc$prix
erreur_standard <- res_mc$erreur_standard

confiance_95 <- 1.96 * erreur_standard

cat(sprintf("Prix Monte Carlo : %.4f EUR\n", prix_mc))
cat(sprintf("Précision (95%%) : +/- %.4f EUR\n", confiance_95))
cat(sprintf("Intervalle       : [%.4f ; %.4f]\n", prix_mc - confiance_95, prix_mc + confiance_95))

# Greeks pour un Call européen
delta_actuel <- calcul_delta(S, K, T_mat, r, sigma, type_option = "call")
gamma_actuel <- calcul_gamma(S, K, T_mat, r, sigma)
vega_actuel  <- calcul_vega(S, K, T_mat, r, sigma)
theta_actuel <- calcul_theta(S, K, T_mat, r, sigma, type_option = "call")
rho_actuel   <- calcul_rho(S, K, T_mat, r, sigma, type_option = "call")

cat(sprintf("Delta : %.4f\n", delta_actuel))
cat(sprintf("Gamma : %.6f\n", gamma_actuel))
cat(sprintf("Vega  : %.4f\n", vega_actuel))
cat(sprintf("Theta : %.4f (par an)\n", theta_actuel))
cat(sprintf("Rho   : %.4f\n", rho_actuel))

cat(sprintf("Pour couvrir un Call vendu, il faut détenir %.4f actions.\n", delta_actuel))

# ==========================================================
# 4) Delta Hedging dynamique et Delta-Gamma Hedging synamique
# ==========================================================
# 4.1) Delta Hedging dynamique
# ==========================================================

simuler_delta_hedging <- function(S, K, T, r, sigma,
                                  nb_steps = 252,
                                  position = c("short", "long")) {
  position <- match.arg(position)  # "short" ou "long"
  
  # 1) Discrétisation du temps
  dt    <- T / nb_steps
  dates <- seq(0, T, length.out = nb_steps + 1)
  
  # 2) Trajectoire de marché unique (GBM sous Q)
  set.seed(50)
  Z      <- rnorm(nb_steps)
  S_path <- numeric(nb_steps + 1)
  S_path[1] <- S
  
  for (i in 1:nb_steps) {
    S_path[i + 1] <- S_path[i] * exp((r - 0.5 * sigma^2) * dt +
                                       sigma * sqrt(dt) * Z[i])
  }
  
  # 3) Prix initial de l’option (Black-Scholes)
  prix_init <- pricer_option(S, K, T, r, sigma)$call
  
  # 4) Initialisation du portefeuille selon la position
  if (position == "short") {
    # Cas 1 : on vend le call -> on reçoit le premium
    cash <- prix_init
    # Pour neutraliser Delta : y = +Delta_C (on ACHÈTE des actions)
    delta_opt <- calcul_delta(S, K, T, r, sigma, type_option = "call")
    stock_holdings <- delta_opt   # nombre d’actions détenues
  } else {
    # Cas 2 : on achète le call -> on paie le premium
    cash <- -prix_init
    # Pour neutraliser Delta : y = -Delta_C (on VEND des actions)
    delta_opt <- calcul_delta(S, K, T, r, sigma, type_option = "call")
    stock_holdings <- -delta_opt  # nombre d’actions (position vendeuse)
  }
  
  # Achat / vente d’actions financé par le cash (auto-financement)
  cash <- cash - stock_holdings * S
  
  # 5) Vecteurs pour stocker l’historique
  option_deltas   <- numeric(nb_steps + 1)  # Delta de l’option
  stock_positions <- numeric(nb_steps + 1)  # position en actions (y_t)
  trades          <- numeric(nb_steps + 1)  # nombre d’actions achetées / vendues
  cash_history    <- numeric(nb_steps + 1)  # évolution du compte cash
  
  option_deltas[1]   <- delta_opt
  stock_positions[1] <- stock_holdings
  trades[1]          <- stock_holdings      # transaction initiale
  cash_history[1]    <- cash
  
  # 6) Boucle de rebalancement dynamique
  for (i in 2:(nb_steps + 1)) {
    temps_restant <- T - dates[i]
    
    # Le cash porte intérêt au taux sans risque
    cash <- cash * exp(r * dt)
    
    if (i < (nb_steps + 1)) {
      # 6.1 Avant maturité : recalcul du Delta de l’option
      delta_opt_new <- calcul_delta(S_path[i], K, temps_restant,
                                    r, sigma, type_option = "call")
      
      # Delta cible pour la position en actions
      if (position == "short") {
        cible_stock <-  delta_opt_new   # y = +Delta_C
      } else {
        cible_stock <- -delta_opt_new   # y = -Delta_C
      }
      
      # Variation de la position en actions
      transaction <- cible_stock - stock_holdings
      stock_holdings <- stock_holdings + transaction
      
      # Auto-financement : achat/vente financé par le cash
      cash <- cash - transaction * S_path[i]
      
      # Sauvegarde des historiques
      option_deltas[i]   <- delta_opt_new
      stock_positions[i] <- stock_holdings
      trades[i]          <- transaction
      cash_history[i]    <- cash
      
    } else {
      # 6.2 Dernier jour : liquidation
      payoff_final <- max(S_path[i] - K, 0)
      
      # On clôture la position en actions
      cash <- cash + stock_holdings * S_path[i]
      stock_holdings <- 0
      
      # Payoff de l’option selon la position
      if (position == "short") {
        cash <- cash - payoff_final   # on RÈGLE le payoff du call vendu
      } else {
        cash <- cash + payoff_final   # on REÇOIT le payoff du call acheté
      }
      
      option_deltas[i]   <- 0
      stock_positions[i] <- 0
      trades[i]          <- 0
      cash_history[i]    <- cash
    }
  }
  
  # 7) Résultat final de la stratégie de couverture
  return(list(
    pnl_final       = cash,          # PnL final de la couverture
    S_path          = S_path,        # trajectoire du sous-jacent
    dates           = dates,         # temps (en années)
    option_deltas   = option_deltas, # Delta de l’option à chaque date
    stock_positions = stock_positions, # position en actions
    trades          = trades,        # quantités achetées/vendues
    cash_history    = cash_history   # évolution du compte cash
  ))
}

# ===================================================
# 4.1.1. Delta-Hedging pour la vente de loption
# ===================================================
res_hedge_short <- simuler_delta_hedging(S, K, T_mat, r, sigma, nb_steps = 252,
                                   position = "short")
pnl_final_short <- res_hedge_short$pnl_final
S_path_short    <- res_hedge_short$S_path
dates_short     <- res_hedge_short$dates
deltas_short    <- res_hedge_short$option_deltas
cat(sprintf("PnL Final de la couverture de position courte: %.4f EUR\n", pnl_final_short))

hedge_table_short <- data.frame(
  t       = dates_short,
  S       = S_path_short,
  Delta   = res_hedge_short$option_deltas,
  Stock_y = res_hedge_short$stock_positions,
  Trade   = res_hedge_short$trades,
  Cash    = res_hedge_short$cash_history
)
head(hedge_table_short, 10)

# ===================================================
# 4.1.2. Delta-Hedging pour l'achat de l'option
# ===================================================
res_hedge_long <- simuler_delta_hedging(S, K, T_mat, r, sigma, nb_steps = 252,
                                    position = "long")
pnl_final_long <- res_hedge_long$pnl_final
S_path_long    <- res_hedge_long$S_path
dates_long     <- res_hedge_long$dates
deltas_long    <- res_hedge_long$option_deltas
cat(sprintf("PnL Final de la couverture de postion longue: %.4f EUR\n", pnl_final_long))

hedge_table_long <- data.frame(
  t       = dates_long,
  S       = S_path_long,
  Delta   = res_hedge_long$option_deltas,
  Stock_y = res_hedge_long$stock_positions,
  Trade   = res_hedge_long$trades,
  Cash    = res_hedge_long$cash_history
)
head(hedge_table_long, 10)


# ==========================================================
# 4.2) Delta-Gamma Hedging dynamique (short ou long Call)
# ==========================================================

simuler_delta_gamma_hedging <- function(S, K1, K2, T, r, sigma,
                                        nb_steps = 252,
                                        position = c("short", "long")) {
  position <- match.arg(position)   # "short" ou "long"
  
  dt    <- T / nb_steps
  dates <- seq(0, T, length.out = nb_steps + 1)
  
  # 1) Trajectoire unique du sous-jacent (GBM)
  set.seed(50)
  Z      <- rnorm(nb_steps)
  S_path <- numeric(nb_steps + 1)
  S_path[1] <- S
  
  for (i in 1:nb_steps) {
    S_path[i + 1] <- S_path[i] * exp((r - 0.5 * sigma^2) * dt +
                                       sigma * sqrt(dt) * Z[i])
  }
  
  # 2) Prix & Greeks initiaux des deux options (Call1, Call2)
  C1_0 <- pricer_option(S, K1, T, r, sigma)$call   # option à couvrir
  C2_0 <- pricer_option(S, K2, T, r, sigma)$call   # option de couverture
  
  Delta1_0 <- calcul_delta(S, K1, T, r, sigma, type_option = "call")
  Gamma1_0 <- calcul_gamma(S, K1, T, r, sigma)
  
  Delta2_0 <- calcul_delta(S, K2, T, r, sigma, type_option = "call")
  Gamma2_0 <- calcul_gamma(S, K2, T, r, sigma)
  
  # xc = quantité d'option 1 (signée)
  # short 1 call -> xc = -1 ; long 1 call -> xc = +1
  xc <- ifelse(position == "short", -1, 1)
  
  eps <- 1e-8
  if (abs(Gamma2_0) < eps) {
    stop("Gamma de l'option de couverture (K2) trop faible au départ.")
  }
  
  # 3) Quantités initiales g (option 2) et y (actions)
  # Système:
  #   xc*Gamma1 + g*Gamma2 = 0
  #   xc*Delta1 + g*Delta2 + y = 0
  g <- - xc * Gamma1_0 / Gamma2_0
  y <- - (xc * Delta1_0 + g * Delta2_0)
  
  # 4) Cash initial :
  # on paie/reçoit xc*C1_0, puis on finance g et y
  # si xc = -1 (short) -> cash = +C1_0 ...
  cash <- - xc * C1_0
  cash <- cash - g * C2_0 - y * S    # auto-financement
  
  # Historiques
  hist_y      <- numeric(nb_steps + 1)
  hist_g      <- numeric(nb_steps + 1)
  hist_deltaP <- numeric(nb_steps + 1)
  hist_gammaP <- numeric(nb_steps + 1)
  cash_hist   <- numeric(nb_steps + 1)
  
  hist_y[1]      <- y
  hist_g[1]      <- g
  hist_deltaP[1] <- xc * Delta1_0 + g * Delta2_0 + y
  hist_gammaP[1] <- xc * Gamma1_0 + g * Gamma2_0
  cash_hist[1]   <- cash
  
  # 5) Rebalancement dynamique
  for (i in 2:(nb_steps + 1)) {
    t_now <- dates[i]
    S_now <- S_path[i]
    T_remain <- T - t_now
    
    # Cash porte intérêt
    cash <- cash * exp(r * dt)
    
    if (i < (nb_steps + 1)) {
      # Recalcul des Greeks
      C2_now <- pricer_option(S_now, K2, T_remain, r, sigma)$call
      
      Delta1 <- calcul_delta(S_now, K1, T_remain, r, sigma, "call")
      Gamma1 <- calcul_gamma(S_now, K1, T_remain, r, sigma)
      
      Delta2 <- calcul_delta(S_now, K2, T_remain, r, sigma, "call")
      Gamma2 <- calcul_gamma(S_now, K2, T_remain, r, sigma)
      
      if (abs(Gamma2) < eps) {
        # Gamma2 ~ 0: giữ nguyên vị thế
        hist_y[i]      <- y
        hist_g[i]      <- g
        hist_deltaP[i] <- xc * Delta1 + g * Delta2 + y
        hist_gammaP[i] <- xc * Gamma1 + g * Gamma2
        cash_hist[i]   <- cash
        next
      }
      
      # Nouvelles quantités (toujours Delta-Gamma neutre)
      g_new <- - xc * Gamma1 / Gamma2
      y_new <- - (xc * Delta1 + g_new * Delta2)
      
      # Transactions
      delta_g <- g_new - g
      delta_y <- y_new - y
      
      cash <- cash - delta_g * C2_now - delta_y * S_now
      
      g <- g_new
      y <- y_new
      
      hist_g[i]      <- g
      hist_y[i]      <- y
      hist_deltaP[i] <- xc * Delta1 + g * Delta2 + y
      hist_gammaP[i] <- xc * Gamma1 + g * Gamma2
      cash_hist[i]   <- cash
      
    } else {
      # 6) A maturité : liquidation
      payoff1 <- max(S_now - K1, 0)  # payoff Call1
      payoff2 <- max(S_now - K2, 0)  # payoff Call2
      
      # Fermeture de toutes les positions
      # xc * payoff1 : + si long call, - si short call
      cash <- cash + y * S_now + g * payoff2 + xc * payoff1
      
      hist_y[i]      <- 0
      hist_g[i]      <- 0
      hist_deltaP[i] <- NA
      hist_gammaP[i] <- NA
      cash_hist[i]   <- cash
    }
  }
  
  return(list(
    pnl_final  = cash,
    S_path     = S_path,
    dates      = dates,
    y_actions  = hist_y,
    g_options  = hist_g,
    delta_port = hist_deltaP,
    gamma_port = hist_gammaP,
    cash       = cash_hist
  ))
}

# ===================================================
# 4.2.1. Delta-Hedging pour la vente de l'option
# ===================================================
K2 <- K * 1.2  # ví dụ chọn strike khác để hedge gamma

res_dg_short <- simuler_delta_gamma_hedging(
  S, K1 = K, K2 = K2,
  T = T_mat, r = r, sigma = sigma,
  nb_steps = 252,
  position = "short"
)
cat(sprintf("PnL Delta-Gamma (short call): %.4f EUR\n", res_dg_short$pnl_final))

# ===================================================
# 4.2.2. Delta-Hedging pour l'achat de l'option
# ===================================================
res_dg_long <- simuler_delta_gamma_hedging(
  S, K1 = K, K2 = K2,
  T = T_mat, r = r, sigma = sigma,
  nb_steps = 252,
  position = "long"
)

cat(sprintf("PnL Delta-Gamma (long call): %.4f EUR\n", res_dg_long$pnl_final))

# ==========================
# 5) Visualisations (4 graphiques)
# ==========================

# 5.1. Profil Black-Scholes (Call vs S)
min(close_prices)
max(close_prices)

S_range <- seq(20, 70, by = 1)
calls <- sapply(S_range, function(s) pricer_option(s, K, T_mat, r, sigma)$call)

df_bs <- data.frame(S = S_range, Call = calls)

p1 <- ggplot(df_bs, aes(x = S, y = Call)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = K, linetype = "dashed", color = "red") +
  labs(
    title = "Black-Scholes – Prix du l'Option d'achat (Call) selon le prix de l'action aujourd'hui (Spot)",
    x = "Prix de l'action aujourd'hui",
    y = "Prix de l'option d'achat"
  ) +
  theme_minimal()

# 5.2. Monte Carlo – Trajectoires simulées
nb_scenarios <- 500
nb_steps <- 252
dt <- T_mat / nb_steps
t_axis <- seq(0, T_mat, length.out = nb_steps + 1)

S_paths <- matrix(NA, nrow = nb_steps + 1, ncol = nb_scenarios)
S_paths[1, ] <- S

set.seed(123)
for (i in 2:(nb_steps + 1)) {
  Z <- rnorm(nb_scenarios)
  S_paths[i, ] <- S_paths[i - 1, ] * exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z)
}

df_mc <- data.frame(
  t = rep(t_axis, times = nb_scenarios),
  S = as.vector(S_paths),
  scenario = rep(1:nb_scenarios, each = nb_steps + 1)
)

S_average <- rowMeans(S_paths)
df_avg <- data.frame(t = t_axis, S_avg = S_average)

p2 <- ggplot() +
  geom_line(data = df_mc, aes(x = t, y = S, group = scenario), alpha = 0.3) +
  geom_line(data = df_avg, aes(x = t, y = S_avg), size = 1.2) +
  geom_hline(yintercept = K, linetype = "dashed", color = "red") +
  labs(
    title = paste0("Simulations Monte Carlo (", nb_scenarios, " scénarios)"),
    x = "Temps (années)",
    y = "Prix de l'action"
  ) +
  theme_minimal()

# 5.3. Convergence Monte Carlo
N_simulations_visu <- 50000
set.seed(50)
Z_conv <- rnorm(N_simulations_visu)
S_T_conv <- S * exp((r - 0.5 * sigma^2) * T_mat + sigma * sqrt(T_mat) * Z_conv)
payoffs_conv <- pmax(S_T_conv - K, 0)

prix_cumules <- exp(-r * T_mat) * cumsum(payoffs_conv) / seq_len(N_simulations_visu)

df_conv <- data.frame(
  n = 1:N_simulations_visu,
  prix = prix_cumules
)

p3 <- ggplot(df_conv, aes(x = n, y = prix)) +
  geom_line(size = 0.7) +
  geom_hline(yintercept = prix_call_bs, linetype = "dashed", color = "red") +
  labs(
    title = "Convergence du prix Monte Carlo vers Black-Scholes",
    x = "Nombre de simulations",
    y = "Prix estimé du Call"
  ) +
  theme_minimal()

# 5.4. Delta Hedging– évolution du Delta
df_delta_short <- data.frame(
  t = dates_short,
  delta = deltas_short
)

p4 <- ggplot(df_delta_short, aes(x = t, y = delta)) +
  geom_line(color = "darkblue", size = 1) +
  geom_area(alpha = 0.3, fill = "darkblue") +
  labs(
    title = sprintf("Stratégie Delta Hedging (PnL final : %.2f €)", pnl_final_short),
    x = "Temps (années)",
    y = "Delta (nombre d'actions détenues)"
  ) +
  theme_minimal()

# 5.5. Évolution du Gamma
df_gamma <- data.frame(
  t = res_dg_short$dates,
  gamma = res_dg_short$gamma_port
)

df_gamma2 <- na.omit(df_gamma)

p5 <- ggplot(df_gamma2, aes(x=t, y=gamma)) +
  geom_line(color="darkred", size=1) +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(
    title="Gamma du portefeuille sous couverture Delta–Gamma",
    x="Temps (années)",
    y="Gamma du portefeuille"
  ) +
  theme_minimal()

#5.6. Positions de couverture
df_positions <- data.frame(
  t = res_dg_short$dates,
  Actions = res_dg_short$y_actions,
  Option2 = res_dg_short$g_options
)

p6<- ggplot(df_positions, aes(x = t)) +
  geom_line(aes(y = Actions, color = "Actions"), size = 1) +
  geom_line(aes(y = Option2, color = "Option de couverture"), size = 1) +
  labs(
    title = "Évolution des positions de couverture",
    x = "Temps (années)",
    y = "Quantité détenue"
  ) +
  scale_color_manual(values = c("darkgreen", "purple")) +
  theme_minimal()

# Affichage des 4 graphiques dans RStudio (vous pouvez les imprimer un par un)
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)