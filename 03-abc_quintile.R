## 03-abc_quintile.R : FiTABM の ABC 再校正（不平等を含む）          #shuusei20251121

library(tidyverse)                                                      #shuusei20251121
library(lubridate)                                                      #shuusei20251121
library(parallel)                                                       #shuusei20251121
library(pbapply)                                                       #shuusei20251121  # 進捗バー付き apply

pboptions(type = "txt")                                                #shuusei20251121  # コンソールにテキストの進捗バーを出す

source("01-required_functions.R")                                       #shuusei20251121
source("02-run_functions.R")                                           #shuusei20251121

## 1. データ読み込み（歴史シナリオ）                                 #shuusei20251121
load_data()                                                             #shuusei20251121

## 2. SES 五分位別 CAPACITY のターゲット（MW）                         #shuusei20251121
## Table 2 の CAPACITY 行 [kW] を 1000 で割って MW に変換
target_cap_Q <- c(                                                      #shuusei20251121
  Q1 = 288900,                                                          #shuusei20251121
  Q2 = 318020,                                                          #shuusei20251121
  Q3 = 425880,                                                          #shuusei20251121
  Q4 = 547930,                                                          #shuusei20251121
  Q5 = 512270                                                           #shuusei20251121
) / 1000                                                                #shuusei20251121   # 単位: MW

## 3. 1 パラメータセットに対するサマリー統計量                        #shuusei20251126
simulate_summary <- function(w, t_base, alpha, 
                             n_agents = n_agents_abc) {                #shuusei20251126
  ## クインタイル別 threshold を生成                                  #shuusei20251126
  t_Q <- threshold_from_t_alpha(t_base, alpha)                          #shuusei20251126
  
  res <- run_model(number_of_agents = n_agents, rn = 1,                 #shuusei20251126
                   w = w, threshold = mean(t_Q),                        #shuusei20251126
                   threshold_Q = t_Q)                                   #shuusei20251126
  
  avg_u_run <- res[[1]]
  
  ## --- ここから下は元の simulate_summary と同じ ---                  #shuusei20251126
  cutoff <- dmy("01oct2015")
  idx <- which(avg_u_run$time_series <= cutoff)
  
  dep_model <- avg_u_run$tot_inst_cap[idx]
  dep_real  <- deployment$real_cap[idx]
  
  err_dep <- mean(abs(dep_model - dep_real))
  
  row_q <- which(avg_u_run$time_series == cutoff)
  if (length(row_q) != 1) stop("cutoff date not found in avg_u_run")
  
  cap_dec <- as.numeric(avg_u_run[row_q, paste0("cap_dec", 1:10)])
  cap_Q_model <- calc_quintile_cap(cap_dec)
  
  total_cap_Q <- sum(cap_Q_model, na.rm = TRUE)
  if (total_cap_Q < 1e-6) {
    return(c(err_dep = 1e6, err_Q = 1e6))
  }
  
  rel_err_Q <- abs(cap_Q_model - target_cap_Q) / target_cap_Q
  err_Q <- mean(rel_err_Q)
  
  c(err_dep = err_dep, err_Q = err_Q)
}                                                                       #shuusei20251126

## 4. ABC 設定（ガウス事前分布）                                      #shuusei20251122
set.seed(123)                                                           #shuusei20251122
n_sim <- 500                                                         #shuusei20251122  # 本番 ABC で回す本数

## エージェント数の設定（予備スキャン / 本番ABC）                     #shuusei20251118
n_agents_scan <- 500                                                  #shuusei20251118  # 予備スキャン用（軽め）  
n_agents_abc  <- 500                                                  #shuusei20251118  # 本番ABC用（やや重め）

## 4-1. 事前分布の中心を探すための「予備スキャン」                     #shuusei20251122
use_auto_prior   <- TRUE                                                #shuusei20251122  # TRUE: 一様分布から自動で prior の平均を作る
n_scan           <- 500                                                 #shuusei20251122  # 予備スキャンで回す本数
top_k_for_prior  <- 5                                                  #shuusei20251122  # 距離が小さい上位何本から prior 平均を作るか

if (use_auto_prior) {                                                   #shuusei20251122
  
  ## 4-1-0. 予備スキャン用クラスタの作成                               #shuusei20251122
  cl_scan <- makeCluster(max(1, detectCores() - 1))                     #shuusei20251122
  clusterExport(cl_scan, varlist = ls())                                #shuusei20251122
  clusterEvalQ(cl_scan, {                                               #shuusei20251122
    load_libraries()                                                    #shuusei20251122
  })                                                                    #shuusei20251122
  clusterSetRNGStream(cl_scan, 456)                                     #shuusei20251122
  
  ## 4-1-1. pblapply で n_scan 本を並列に予備スキャン                   #shuusei20251122
  
  scan_list <- pblapply(1:n_scan, cl = cl_scan, FUN = function(i) {   #shuusei20251126
    ## U(0,1) から w_inc, w_soc, w_ec, t_base, alpha をサンプリング    #shuusei20251126
    repeat {                                                          #shuusei20251126
      draw <- runif(5)                                                #shuusei20251126
      names(draw) <- c("w_inc","w_soc","w_ec","t_base","alpha")       #shuusei20251126
      
      ## 3つの効用重みの和が 1 以下                                   #shuusei20251126
      sum_w3 <- draw["w_inc"] + draw["w_soc"] + draw["w_ec"]          #shuusei20251126
      if (sum_w3 >= 1) next                                           #shuusei20251126
      
      ## alpha は大きすぎないよう軽く制約（例: <= 0.3）              #shuusei20251126
      if (draw["alpha"] < -0.3 || draw["alpha"] > 0.3) next              #shuusei20251126
      
      break                                                           #shuusei20251126
    }                                                                 #shuusei20251126
    
    w_inc_tmp  <- draw["w_inc"]                                       #shuusei20251126
    w_soc_tmp  <- draw["w_soc"]                                       #shuusei20251126
    w_ec_tmp   <- draw["w_ec"]                                        #shuusei20251126
    w_cap_tmp  <- 1 - (w_inc_tmp + w_soc_tmp + w_ec_tmp)              #shuusei20251126
    t_base_tmp <- draw["t_base"]                                      #shuusei20251126
    alpha_tmp  <- draw["alpha"]                                       #shuusei20251126
    
    w_vec_tmp <- c(w_inc_tmp, w_soc_tmp, w_ec_tmp, w_cap_tmp)         #shuusei20251126
    t_Q_tmp   <- threshold_from_t_alpha(t_base_tmp, alpha_tmp)        #shuusei20251126
    
    summ_tmp <- simulate_summary(w_vec_tmp, t_base_tmp, alpha_tmp,    #shuusei20251126
                                 n_agents = n_agents_scan)            #shuusei20251126
    
    cat(sprintf("[SCAN] %3d / %3d  err_dep = %.3f  err_Q = %.3f\n",   #shuusei20251126
                i, n_scan, summ_tmp["err_dep"], summ_tmp["err_Q"]))   #shuusei20251126
    
    c(w_inc   = w_inc_tmp,                                            #shuusei20251126
      w_soc   = w_soc_tmp,                                            #shuusei20251126
      w_ec    = w_ec_tmp,                                             #shuusei20251126
      t_base  = t_base_tmp,                                           #shuusei20251126
      alpha   = alpha_tmp,                                            #shuusei20251126
      err_dep = unname(summ_tmp["err_dep"]),                          #shuusei20251126
      err_Q   = unname(summ_tmp["err_Q"]))                            #shuusei20251126
  })
  
  
  stopCluster(cl_scan)                                                  #shuusei20251122
  
  ## 4-1-2. リストを行列にまとめる                                     #shuusei20251122
  scan_res <- do.call(rbind, scan_list)                                 #shuusei20251122
  scan_df  <- as.data.frame(scan_res)                                   #shuusei20251122
  
  ## 4-1-3. 誤差を標準化して距離を定義                                  #shuusei20251122
  scan_df$dist <- scale(scan_df$err_dep) + scale(scan_df$err_Q)         #shuusei20251122
  
  ## 4-1-3a. 予備スキャンの誤差分布をコンソールに表示                   #shuusei20251118
  cat("\n[SCAN] err_dep summary:\n")                                    #shuusei20251118
  print(summary(scan_df$err_dep))                                       #shuusei20251118
  cat("\n[SCAN] err_Q summary:\n")                                      #shuusei20251118
  print(summary(scan_df$err_Q))                                         #shuusei20251118
  
  ## 4-1-4. 距離の小さい上位 top_k_for_prior 本だけを抽出               #shuusei20251122
  best_scan <- scan_df[order(scan_df$dist), ][1:top_k_for_prior, ]      #shuusei20251122
  
  ## 4-1-5. その平均を prior の平均として採用                           #shuusei20251122
  prior_mean <- c(                                                       #shuusei20251126
    w_inc   = mean(best_scan$w_inc),                                     #shuusei20251126
    w_soc   = mean(best_scan$w_soc),                                     #shuusei20251126
    w_ec    = mean(best_scan$w_ec),                                      #shuusei20251126
    t_base  = mean(best_scan$t_base),                                    #shuusei20251126
    alpha   = mean(best_scan$alpha)                                      #shuusei20251126
  )                                                                      #shuusei20251126
  
  prior_sd <- c(                                                         #shuusei20251126
    w_inc   = 0.10,                                                      #shuusei20251126
    w_soc   = 0.10,                                                      #shuusei20251126
    w_ec    = 0.10,                                                      #shuusei20251126
    t_base  = 0.10,                                                      #shuusei20251126
    alpha   = 0.05                                                       #shuusei20251126
  )                                                                      #shuusei20251126
  
  cat("\n[SCAN] 予備スキャンから得られた prior の平均:\n")               #shuusei20251122
  print(prior_mean)                                                     #shuusei20251122
  
} else {                                                                 #shuusei20251122
  ## 4-2. 自分で決め打ちの prior 平均を入れる場合はこちらを編集         #shuusei20251122
  prior_mean <- c(                                                       #shuusei20251126
    w_inc  = 0.25,                                                       #shuusei20251126
    w_soc  = 0.25,                                                       #shuusei20251126
    w_ec   = 0.25,                                                       #shuusei20251126
    t_base = 0.50,                                                       #shuusei20251126
    alpha  = 0.10                                                        #shuusei20251126
  )                                                                      #shuusei20251126
  
  prior_sd <- c(                                                         #shuusei20251126
    w_inc  = 0.10,                                                       #shuusei20251126
    w_soc  = 0.10,                                                       #shuusei20251126
    w_ec   = 0.10,                                                       #shuusei20251126
    t_base = 0.10,                                                       #shuusei20251126
    alpha  = 0.05                                                        #shuusei20251126
  )                                                                      #shuusei20251126
}


param_draws <- matrix(NA_real_, nrow = n_sim, ncol = 6,               #shuusei20251126
                      dimnames = list(NULL,                            #shuusei20251126
                                      c("w_inc","w_soc","w_ec",        #shuusei20251126
                                        "w_cap","t_base","alpha")))    #shuusei20251126
err_dep_all <- numeric(n_sim)                                          #shuusei20251121
err_Q_all   <- numeric(n_sim)                                          #shuusei20251121
distances   <- numeric(n_sim)                                          #shuusei20251121

cat("\n[ABC] start: n_sim =", n_sim,                                   #shuusei20251118
    ", n_agents_abc =", n_agents_abc, "\n")                            #shuusei20251118
cat("[ABC] prior_mean:\n")                                             #shuusei20251118
print(prior_mean)                                                      #shuusei20251118
cat("[ABC] prior_sd:\n")                                               #shuusei20251118
print(prior_sd)                                                        #shuusei20251118

## 5. 事前からサンプリングしてシミュレーション（並列版）             #shuusei20251121
cl <- makeCluster(max(1, detectCores() - 1))                            #shuusei20251121

## まずグローバル環境の関数・オブジェクトをワーカーに送る            #shuusei20251121
clusterExport(cl, varlist = ls())                                       #shuusei20251121

## ワーカー側でも load_libraries() を呼んで同じパッケージをロード    #shuusei20251121
clusterEvalQ(cl, {                                                      #shuusei20251121
  load_libraries()                                                      #shuusei20251121
})                                                                       #shuusei20251121

## 乱数シード（並列でも再現性を確保）                                #shuusei20251121
clusterSetRNGStream(cl, 123)                                            #shuusei20251121

## parLapply → pblapply に変更して、進捗 & 各 run の誤差を表示        #shuusei20251121
sim_results <- pblapply(1:n_sim, cl = cl, FUN = function(i) {          #shuusei20251126
  ## 5 変数 (w_inc,w_soc,w_ec,t_base,alpha) を正規事前からサンプリング #shuusei20251126
  repeat {                                                             #shuusei20251126
    draw <- rnorm(5,                                                   #shuusei20251126
                  mean = prior_mean[c("w_inc","w_soc","w_ec",          #shuusei20251126
                                      "t_base","alpha")],             #shuusei20251126
                  sd   = prior_sd[c("w_inc","w_soc","w_ec",            #shuusei20251126
                                    "t_base","alpha")])               #shuusei20251126
    names(draw) <- c("w_inc","w_soc","w_ec","t_base","alpha")          #shuusei20251126
    
    ## 0〜1 の範囲チェック                                            #shuusei20251126
    if (any(draw[c("w_inc","w_soc","w_ec","t_base")] < 0) ||           #shuusei20251126
        any(draw[c("w_inc","w_soc","w_ec","t_base")] > 1)) next        #shuusei20251126
    
    sum_w3 <- draw["w_inc"] + draw["w_soc"] + draw["w_ec"]             #shuusei20251126
    if (sum_w3 >= 1) next                                              #shuusei20251126
    
    if (draw["alpha"] < -0.3 || draw["alpha"] > 0.3) next                 #shuusei20251126
    
    break                                                              #shuusei20251126
  }                                                                    #shuusei20251126
  
  w_inc  <- draw["w_inc"]                                              #shuusei20251126
  w_soc  <- draw["w_soc"]                                              #shuusei20251126
  w_ec   <- draw["w_ec"]                                               #shuusei20251126
  t_base <- draw["t_base"]                                             #shuusei20251126
  alpha  <- draw["alpha"]                                              #shuusei20251126
  w_cap  <- 1 - (w_inc + w_soc + w_ec)                                 #shuusei20251126
  
  w_vec  <- c(w_inc, w_soc, w_ec, w_cap)                               #shuusei20251126
  t_Q    <- threshold_from_t_alpha(t_base, alpha)                      #shuusei20251126
  
  summ <- simulate_summary(w_vec, t_base, alpha,                       #shuusei20251126
                           n_agents = n_agents_abc)                    #shuusei20251126
  
  cat(sprintf("[ABC] i = %3d / %3d  err_dep = %.3f  err_Q = %.3f\n",   #shuusei20251126
              i, n_sim, summ["err_dep"], summ["err_Q"]))               #shuusei20251126
  
  list(                                                                 #shuusei20251126
    err_dep = unname(summ["err_dep"]),                                 #shuusei20251126
    err_Q   = unname(summ["err_Q"]),                                   #shuusei20251126
    params  = c(w_inc, w_soc, w_ec, w_cap, t_base, alpha)              #shuusei20251126
  )                                                                     #shuusei20251126
})

stopCluster(cl)                                                         #shuusei20251121

## parLapply の結果をベクトル・行列に詰め直す                         #shuusei20251121
err_dep_all[] <- vapply(sim_results, `[[`, numeric(1), "err_dep")       #shuusei20251121
err_Q_all[]   <- vapply(sim_results, `[[`, numeric(1), "err_Q")         #shuusei20251121

param_mat <- do.call(rbind, lapply(sim_results, `[[`, "params"))        #shuusei20251121
rownames(param_mat) <- NULL                                             #shuusei20251121
param_draws[,] <- param_mat                                             #shuusei20251121

## 6. 標準偏差で割って正規化 → 距離計算                             #shuusei20251121
sd_dep <- sd(err_dep_all, na.rm = TRUE)                                 #shuusei20251121
sd_Q   <- sd(err_Q_all,   na.rm = TRUE)                                 #shuusei20251121

if (sd_dep == 0 || is.na(sd_dep)) sd_dep <- 1                            #shuusei20251121
if (sd_Q   == 0 || is.na(sd_Q))   sd_Q   <- 1                            #shuusei20251121

err_dep_norm <- err_dep_all / sd_dep                                    #shuusei20251121
err_Q_norm   <- err_Q_all   / sd_Q                                      #shuusei20251121

distances <- err_dep_norm + err_Q_norm                                  #shuusei20251121
distances[!is.finite(distances)] <- Inf                                 #shuusei20251121

## 6a. ABC 全体の誤差・距離の要約を表示                               #shuusei20251118
cat("\n[ABC] err_dep_all summary:\n")                                   #shuusei20251118
print(summary(err_dep_all))                                             #shuusei20251118
cat("\n[ABC] err_Q_all summary:\n")                                     #shuusei20251118
print(summary(err_Q_all))                                               #shuusei20251118
cat("\n[ABC] distance summary:\n")                                      #shuusei20251118
print(summary(distances))                                               #shuusei20251118

## 7. 上位 1% を事後サンプルとして採用                               #shuusei20251121
n_keep <- max(1, floor(n_sim * 0.01))                                   #shuusei20251121
keep   <- order(distances)[1:n_keep]                                    #shuusei20251121

allowed_params_quint <- as.data.frame(param_draws[keep, ])              #shuusei20251121

write_tsv(allowed_params_quint,                                         #shuusei20251121
          "Data/allowed_params_quintile.txt",                           #shuusei20251121
          col_names = FALSE)                                            #shuusei20251121
