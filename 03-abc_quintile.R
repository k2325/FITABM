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

## 3. 1 パラメータセットに対するサマリー統計量                        #shuusei20251121
simulate_summary <- function(w, t, n_agents = n_agents_abc) {           #shuusei20251118
  res <- run_model(number_of_agents = n_agents, rn = 1,                 #shuusei20251121
                   w = w, threshold = t)                                #shuusei20251121
  
  avg_u_run <- res[[1]]                                                 #shuusei20251121
  
  ## 3-1. 2010〜2015/9/30 までの全国累積容量の誤差                    #shuusei20251121
  cutoff <- dmy("01oct2015")                                           #shuusei20251121
  idx <- which(avg_u_run$time_series <= cutoff)                         #shuusei20251121
  
  dep_model <- avg_u_run$tot_inst_cap[idx]                              #shuusei20251121
  dep_real  <- deployment$real_cap[idx]                                 #shuusei20251121
  
  err_dep <- mean(abs(dep_model - dep_real))                            #shuusei20251121
  
  ## 3-2. 2015/9/30 時点の Q1〜Q5 CAPACITY（MW）                      #shuusei20251121
  row_q <- which(avg_u_run$time_series == cutoff)                       #shuusei20251121
  if (length(row_q) != 1) stop("cutoff date not found in avg_u_run")    #shuusei20251121
  
  cap_dec <- as.numeric(avg_u_run[row_q,                                #shuusei20251121
                                  paste0("cap_dec", 1:10)])             #shuusei20251121
  cap_Q_model <- calc_quintile_cap(cap_dec)                             #shuusei20251121
  
  ## 3-2a. 全く導入がない（またはほぼゼロ）のケースに強いペナルティ    #shuusei20251122
  total_cap_Q <- sum(cap_Q_model, na.rm = TRUE)                         #shuusei20251122
  if (total_cap_Q < 1e-6) {                                             #shuusei20251122
    # 完全に導入ゼロの run は ABC から事実上除外したいので            #shuusei20251122
    # err_dep, err_Q を非常に大きな値にして返す                        #shuusei20251122
    return(c(err_dep = 1e6, err_Q = 1e6))                               #shuusei20251122
  }                                                                     #shuusei20251122
  
  rel_err_Q <- abs(cap_Q_model - target_cap_Q) / target_cap_Q           #shuusei20251121
  err_Q <- mean(rel_err_Q)                                              #shuusei20251121
  
  c(err_dep = err_dep, err_Q = err_Q)                                   #shuusei20251121
}                                                                       #shuusei20251121

## 4. ABC 設定（ガウス事前分布）                                      #shuusei20251122
set.seed(123)                                                           #shuusei20251122
n_sim <- 500                                                         #shuusei20251122  # 本番 ABC で回す本数

## エージェント数の設定（予備スキャン / 本番ABC）                     #shuusei20251118
n_agents_scan <- 500                                                  #shuusei20251118  # 予備スキャン用（軽め）  
n_agents_abc  <- 500                                                  #shuusei20251118  # 本番ABC用（やや重め）

## 4-1. 事前分布の中心を探すための「予備スキャン」                     #shuusei20251122
use_auto_prior   <- TRUE                                                #shuusei20251122  # TRUE: 一様分布から自動で prior の平均を作る
n_scan           <- 250                                                 #shuusei20251122  # 予備スキャンで回す本数
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
  
  scan_list <- pblapply(1:n_scan, cl = cl_scan, FUN = function(i) {     #shuusei20251122
    ## 一様分布 U(0,1) から w_inc, w_soc, w_ec, t をサンプリング        #shuusei20251126
    ## w_inc + w_soc + w_ec ≤ 1 を満たすまで引き直し，残りを w_cap に   #shuusei20251126
    repeat {                                                            #shuusei20251126
      draw <- runif(4)                                                  #shuusei20251126
      names(draw) <- c("w_inc","w_soc","w_ec","t")                      #shuusei20251126
      
      if (draw["w_inc"] + draw["w_soc"] + draw["w_ec"] >= 1) next       #shuusei20251126
      break                                                             #shuusei20251126
    }                                                                   #shuusei20251126
    
    w_inc_tmp <- draw["w_inc"]                                          #shuusei20251126
    w_soc_tmp <- draw["w_soc"]                                          #shuusei20251126
    w_ec_tmp  <- draw["w_ec"]                                           #shuusei20251126
    w_cap_tmp <- 1 - (w_inc_tmp + w_soc_tmp + w_ec_tmp)                 #shuusei20251126
    w_vec_tmp <- c(w_inc_tmp, w_soc_tmp, w_ec_tmp, w_cap_tmp)           #shuusei20251126
    t_tmp <- draw["t"]                                                  #shuusei20251126
    
    ## この θ でサマリー統計（誤差）を計算                              #shuusei20251122
    summ_tmp <- simulate_summary(w_vec_tmp, t_tmp,                      #shuusei20251122
                                 n_agents = n_agents_scan)             #shuusei20251122
    
    cat(sprintf("[SCAN] %3d / %3d  err_dep = %.3f  err_Q = %.3f\n",     #shuusei20251122
                i, n_scan, summ_tmp["err_dep"], summ_tmp["err_Q"]))     #shuusei20251122
    
    c(w_inc   = w_inc_tmp,                                              #shuusei20251126
      w_soc   = w_soc_tmp,                                              #shuusei20251126
      w_ec    = w_ec_tmp,                                               #shuusei20251126
      t       = t_tmp,                                                  #shuusei20251126
      err_dep = unname(summ_tmp["err_dep"]),                            #shuusei20251126
      err_Q   = unname(summ_tmp["err_Q"]))                              #shuusei20251126
  })                                                                    #shuusei20251122
  
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
  prior_mean <- c(                                                       #shuusei20251122
    w_inc = mean(best_scan$w_inc),                                      #shuusei20251126
    w_soc = mean(best_scan$w_soc),                                      #shuusei20251126
    w_ec  = mean(best_scan$w_ec),                                       #shuusei20251126
    t     = mean(best_scan$t)                                           #shuusei20251122
  )                                                                     #shuusei20251122
  
  prior_sd <- c(                                                         #shuusei20251122
    w_inc = 0.10,                                                       #shuusei20251125
    w_soc = 0.10,                                                       #shuusei20251122
    w_ec  = 0.10,                                                       #shuusei20251122
    t     = 0.10                                                        #shuusei20251122
  )                                                                     #shuusei20251122
  
  cat("\n[SCAN] 予備スキャンから得られた prior の平均:\n")               #shuusei20251122
  print(prior_mean)                                                     #shuusei20251122
  
} else {                                                                 #shuusei20251122
  ## 4-2. 自分で決め打ちの prior 平均を入れる場合はこちらを編集         #shuusei20251122
  prior_mean <- c(                                                       #shuusei20251122
    w_inc = 0.25,   # 3 つの効用をだいたい均等にスタート              #shuusei20251125
    w_soc = 0.25,                                                       #shuusei20251122
    w_ec  = 0.25,                                                       #shuusei20251122
    t     = 0.50                                                        #shuusei20251122
  )                                                                     #shuusei20251122
  
  prior_sd <- c(                                                         #shuusei20251122
    w_inc = 0.10,                                                       #shuusei20251125
    w_soc = 0.10,                                                       #shuusei20251122
    w_ec  = 0.10,                                                       #shuusei20251122
    t     = 0.10                                                        #shuusei20251122
  )                                                                     #shuusei20251122
}


param_draws <- matrix(NA_real_, nrow = n_sim, ncol = 5,                 #shuusei20251121
                      dimnames = list(NULL,                             #shuusei20251121
                                      c("w_inc","w_soc","w_ec",         #shuusei20251121
                                        "w_cap","t")))                  #shuusei20251121
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
sim_results <- pblapply(1:n_sim, cl = cl, FUN = function(i) {           #shuusei20251121
  ## 5-1. w_inc, w_soc, w_ec, t を正規事前からサンプリング             #shuusei20251126
  repeat {                                                              #shuusei20251126
    draw <- rnorm(4,                                                    #shuusei20251126
                  mean = prior_mean[c("w_inc","w_soc","w_ec","t")],     #shuusei20251126
                  sd   = prior_sd[c("w_inc","w_soc","w_ec","t")])       #shuusei20251126
    names(draw) <- c("w_inc","w_soc","w_ec","t")                        #shuusei20251126
    
    ## 0〜1 の範囲チェック                                            #shuusei20251126
    if (any(draw < 0) || any(draw > 1)) next                            #shuusei20251126
    
    ## 3つの効用重みの合計が 1 以下かチェック                         #shuusei20251126
    sum_3 <- draw["w_inc"] + draw["w_soc"] + draw["w_ec"]               #shuusei20251126
    if (sum_3 >= 1) next                                                #shuusei20251126
    
    break                                                               #shuusei20251126
  }                                                                     #shuusei20251126
  
  w_inc <- draw["w_inc"]                                                #shuusei20251126
  w_soc <- draw["w_soc"]                                                #shuusei20251126
  w_ec  <- draw["w_ec"]                                                 #shuusei20251126
  w_cap <- 1 - sum_3                                                    #shuusei20251126
  
  w_vec <- c(w_inc, w_soc, w_ec, w_cap)                                 #shuusei20251126
  t_val <- draw["t"]                                                    #shuusei20251126
  
  ## 各パラメータセットのサマリー統計量                             #shuusei20251121
  summ <- simulate_summary(w_vec, t_val, n_agents = n_agents_abc)       #shuusei20251118
  
  cat(sprintf("[ABC] i = %3d / %3d  err_dep = %.3f  err_Q = %.3f\n",    #shuusei20251121
              i, n_sim, summ["err_dep"], summ["err_Q"]))                #shuusei20251121
  
  list(                                                                  #shuusei20251121
    err_dep = unname(summ["err_dep"]),                                  #shuusei20251121
    err_Q   = unname(summ["err_Q"]),                                    #shuusei20251121
    params  = c(w_inc, w_soc, w_ec, w_cap, t_val)                       #shuusei20251126
  )                                                                      #shuusei20251121
})                                                                       #shuusei20251121

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
