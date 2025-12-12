#以下、01-required_functions.R
# Contents
# 1. Historical scenarios & general - functions
# 2. Future scenarios - functions
# 3. Miscellaneous


##################################################################################
################# 1. Historical scenarios & general - functions ##################
##################################################################################
# 1.1 Set-up 
# 1.2 Agent/modelling functions
# 1.3 Cost calculation
# 1.4 Processing

#------------------------------- 1.1 Set-up -------------------------------------#

load_data <- function(start_date, end_date, FiT_end_date, FiT_type, red_frac, init_fit, final_fit, exp_tar,
                      dep_caps = F, cap){
  # end_date: date up to which simulation will run
  # FiT_type: real_h, linear, perc_red
  if(missing(start_date)) start_date <- "1apr2010"
  if(missing(end_date)) end_date <- "1sep2016"
  if(missing(FiT_end_date)) FiT_end_date <- end_date
  if(missing(FiT_type)) {
    FiT_type <- "real_h"
  }
  if(missing(red_frac)) red_frac <- 0.03
  if(missing(init_fit)) init_fit <- 49.43 
  if(missing(final_fit)) final_fit <- 4.18
  if(missing(exp_tar)) exp_tar <- 4
  
  load_libraries()
  # DO NOT USE plyr LIBRARY 
  if (FiT_type == "real_h"){
    start_date <- "1jan2010"
    FiT_end_date <- "1oct2016"
    end_date <- "1oct2016"
    if (end_date < FiT_end_date) end_date <- FiT_end_date
  }
  
  FiT_end_date <- dmy(FiT_end_date)
  end_date <- dmy(end_date)
  start_date <- dmy(start_date)
  
  if(!is.Date(end_date)) stop("Your end date is not a valid date")
  if(!(FiT_type == "real_h" | FiT_type == "perc_red" | FiT_type == "ann_perc_red" | FiT_type == "linear")) stop("FiT type not recognised")
  if(init_fit < 0 | final_fit <0) stop("FiTs have to be positive")
  
  
  elec_price_time <<- read_csv("Data/electricityprices.csv",          #shuusei20251211
                               col_names = F, col_types = cols())     #shuusei20251211
  ## electricityprices.csv は 2010–2023 年の実測 年平均単価 (p/kWh) を持つ前提。  #shuusei20251211
  ## 歴史シナリオでは、その年の値を年内の全ての月で共通して使う。               #shuusei20251211
  
  owner_occupiers <<- read_csv("Data/owner_occupiers.csv", col_names = F, col_types = cols()) %>% mutate(X2 = X2*1000)
  
  #---------------------------------------------------------#
  # Load factor data
  
  LF <- read_csv("Data/LF_mean.csv", col_types = cols())
  
  # Filter out empty rows, group by region, find mean & std. dev. over all years, arrange
  # in alphabetical order.
  LF <<- LF %>% filter(!is.na(Region)) %>% group_by(Region) %>% 
    summarise(LF = mean(Weighted.mean), std_dev = sd(Weighted.mean)) %>% 
    arrange(Region) %>% mutate(Label = LETTERS[1:11])
  
  
  
  #---------------------------------------------------------#
  # Feed-in tariff data
  
  set_FiT(start_date, end_date, FiT_end_date, FiT_type, red_frac, init_fit, final_fit, exp_tar)
  
  #---------------------------------------------------------#
  # Deployment caps
  run_w_cap <<- F
  if (dep_caps == T) {
    set_dep_caps(start_date, end_date, FiT_end_date, FiT_type, cap, exp_tar)
    dep_cap_0 <<- dep_cap
    FiT_0 <<- FiT
    run_w_cap <<- T
  }
  #---------------------------------------------------------#
  
  
  # Population data
  
  population <- read_csv("Data/population_mid2012.csv", col_names = FALSE, col_types = cols()) %>% arrange(X1)
  
  # 旧: 全人口で region_weights を作っていた行はコメントアウトか退避   #shuusei20251205
  # region_weights <<- population$X2/sum(population$X2)
  
  if (!exists("tenure_region_counts", inherits = TRUE)) {               #shuusei20251205
    init_tenure_region_counts()                                         #shuusei20251205
  }                                                                      #shuusei20251205
  owner_region_weights <- tenure_region_counts$own /                    #shuusei20251205
    sum(tenure_region_counts$own)                                       #shuusei20251205
  region_weights <<- owner_region_weights                                #shuusei20251205
  
  rm(population)                                                         #shuusei20251205
  
  #---------------------------------------------------------#
  # PV cost data
  kW_price <<- read_csv('Data/PV_cost_data_est.csv', col_names = FALSE, col_types = cols()) %>%
    mutate(X1 = dmy(X1)) %>%
    filter(X1 >= start_date)
  
  ## X1 = date, X2 = fixed(£/system), X3 = marginal(£/kW)                  #shuusei20251212
  if (ncol(kW_price) < 3) {                                               #shuusei20251212
    stop("PV_cost_data_est.csv must have 3 columns: time_series,fixed,marginal") #shuusei20251212
  }                                                                       #shuusei20251212
  
  
  #---------------------------------------------------------#
  # Electricity use data
  
  means <- read_csv("Data/mean-electricity.csv", col_types = cols())
  medians <- read_csv("Data/median-electricity.csv", col_types = cols())
  
  mus <<- data.frame(matrix(ncol = 5, nrow = 10))
  sigmas <<- data.frame(matrix(ncol = 5, nrow = 10))
  
  for (i in 1:5){
    mean <- means[[i+1]]
    median <- medians[[i+1]]
    mus[[i]] <<- log(median) 
    sigmas[[i]] <<- sqrt(2*log(mean/median))
  }
  
  income_thresh <<- means$income
  
  rm(mean, median, i, means, medians)
  
  
  #---------------------------------------------------------#
  # Real deployment data
  if(exists("deployment")) {
    cat("\nDeployment data is already loaded - if you want to reload it, delete the 'deployment' variable\n")
  } else {
    ts <- seq(dmy("01jan2010"), end_date, by = '1 month')
    
    all_inst_cap <- process_inst_data() %>% 
      filter(technology_type == "Photovoltaic", installed_capacity <= 10, installationtype == "Domestic")
    current_cap <- vector(length = length(ts))
    avg_cap <- vector(length = length(ts))
    for (i in 1:length(ts)) {
      date_now <- ts[i] + months(1)
      installed_now <- filter(all_inst_cap, commissioned_date < date_now)
      current_cap[i] <- sum(installed_now$installed_capacity)
      avg_cap[i] <- current_cap[i]/nrow(installed_now)
    }
    
    
    deployment <<- data.frame(time_series = dmy("01feb2010") + months(0:(length(ts)-1)), 
                              real_cap = current_cap/1000, avg_cap = avg_cap)
    rm(all_inst_cap, installed_now, current_cap, date_now)
  }
  #---------------------------------------------------------#
  
}

load_libraries <- function(){
  library(tidyverse)
  library(stringr)
  library(reshape2)
  library(lubridate)
  library(magrittr)
}

#---- Tenure別所得分布 & 所得帯別電力消費のパラメータ -----------------#shuusei20251205
lognorm_from_mean_median <- function(mean_year, median_year){           #shuusei20251211
  ## mean_year, median_year : 年間所得 (£/year)                         #shuusei20251211
  sigma   <- sqrt(2 * log(mean_year / median_year))                     #shuusei20251211
  mu_year <- log(median_year)                                           #shuusei20251211
  list(mu_year = mu_year, sigma = sigma)                                #shuusei20251211
}                                                                       #shuusei20251211

lognorm_from_mean_median_elec <- function(mean_yr, median_yr){          #shuusei20251205
  sigma <- sqrt(2 * log(mean_yr / median_yr))                           #shuusei20251205
  mu    <- log(median_yr)                                               #shuusei20251205
  list(mu = mu, sigma = sigma)                                          #shuusei20251205
}                                                                       #shuusei20251205

init_income_elec_params <- function(){                                  #shuusei20251211
  ## テニュア別所得分布 (EHS 2011‑12, 年間 mean/median, £/year)        #shuusei20251211
  ## Owner occupiers: mean = 40504, median = 31216                      #shuusei20251211
  ## Social renters : mean = 17550, median = 15287                      #shuusei20251211
  ## Private renters: mean = 30146, median = 23400                      #shuusei20251211
  own  <- lognorm_from_mean_median(40504, 31216)                        #shuusei20251211
  priv <- lognorm_from_mean_median(30146, 23400)                        #shuusei20251211
  soc  <- lognorm_from_mean_median(17550, 15287)                        #shuusei20251211
  
  income_params_tenure <<- list(                                        #shuusei20251211
    own  = own,                                                         #shuusei20251211
    priv = priv,                                                        #shuusei20251211
    soc  = soc                                                          #shuusei20251211
  )                                                                     #shuusei20251211
  
  ## owner‑occupied の所得帯別電力消費 (kWh/yr, NEED)                  #shuusei20251205
  mean_elec   <- c(3400, 3800, 3900, 4200, 4600,                        #shuusei20251205
                   4800, 5000, 5300, 6000, 6800)                        #shuusei20251205
  median_elec <- c(2600, 3000, 3200, 3600, 3900,                        #shuusei20251205
                   4100, 4300, 4600, 5200, 5600)                        #shuusei20251205
  
  lower_inc <- c(0, 15000, 20000, 30000, 40000,                         #shuusei20251205
                 50000, 60000, 70000, 100000, 150000)                   #shuusei20251205
  upper_inc <- c(14999, 19999, 29999, 39999, 49999,                     #shuusei20251205
                 59999, 69999, 99999, 149999, Inf)                      #shuusei20251205
  
  mu_vec    <- numeric(10)                                              #shuusei20251205
  sigma_vec <- numeric(10)                                              #shuusei20251205
  for (i in 1:10){                                                      #shuusei20251205
    tmp          <- lognorm_from_mean_median_elec(mean_elec[i],        #shuusei20251205
                                                  median_elec[i])      #shuusei20251205
    mu_vec[i]    <- tmp$mu                                             #shuusei20251205
    sigma_vec[i] <- tmp$sigma                                          #shuusei20251205
  }                                                                     #shuusei20251205
  
  elec_bands_owners <<- data.frame(                                    #shuusei20251205
    band  = 1:10,                                                       #shuusei20251205
    lower = lower_inc,                                                  #shuusei20251205
    upper = upper_inc,                                                  #shuusei20251205
    mu    = mu_vec,                                                     #shuusei20251205
    sigma = sigma_vec                                                   #shuusei20251205
  )                                                                     #shuusei20251205
}                                                                       #shuusei20251205

init_tenure_region_counts <- function(){                                #shuusei20251205
  ## QS405UK から手入力したテニュア別地域世帯数                       #shuusei20251205
  tenure_region_counts <<- data.frame(                                  #shuusei20251205
    region_name = c("East Midlands","East","London","North East",       #shuusei20251205
                    "North West","Scotland","South East","South West",  #shuusei20251205
                    "Wales","West Midlands","Yorkshire and The Humber"),#shuusei20251205
    code        = LETTERS[1:11],                                       #shuusei20251205
    own  = c(1287409, 1655621, 1618315, 702693, 1957351,               #shuusei20251205
             1470986, 2443797, 1544074, 883130, 1504324, 1435200),     #shuusei20251205
    soc  = c(300423, 380331, 785993, 259506, 550481,                   #shuusei20251205
             576419, 487473, 301520, 214911, 435170, 402653),          #shuusei20251205
    priv = c(307772, 387083, 861865, 167736, 501717,                   #shuusei20251205
             325372, 624193, 419047, 204635, 355415, 386206)           #shuusei20251205
  )                                                                    #shuusei20251205
  
  total_own  <<- sum(tenure_region_counts$own)                          #shuusei20251205
  total_soc  <<- sum(tenure_region_counts$soc)                          #shuusei20251205
  total_priv <<- sum(tenure_region_counts$priv)                         #shuusei20251205
}                                                                       #shuusei20251205

process_inst_data <- function(){
  a <- read_csv("Data/all_inst_1.csv", skip = 2, col_types = cols())
  b <- read_csv("Data/all_inst_2.csv", skip = 2, col_types = cols())
  
  all_inst <- rbind(a, b)
  
  all_inst$InstallationType <- as.factor(all_inst$InstallationType)
  
  names(all_inst) %<>% str_replace_all(" \\(.*\\)", "") %>% str_replace_all(" ", "_") %>% str_to_lower 
  
  all_inst %<>% filter(technology_type == "Photovoltaic")
  
  all_inst %<>% select(technology_type, installed_capacity, commissioned_date, installationtype) 
  
  all_inst$commissioned_date %<>% dmy
  
  return(all_inst)
}

set_FiT <- function(start_date, end_date, FiT_end_date, FiT_type, red_frac, init_fit, final_fit, exp_tar){
  if (end_date == FiT_end_date){
    FiT_zero <- NULL
  } else {
    FiT_zero <- data.frame(time_series = seq(FiT_end_date + months(1), end_date, by = '1 month'), FiT = 0,
                           FiT_large = 0, exp_tar = 0)
  }
  time_series <- seq(start_date, FiT_end_date, by = '1 month')
  if (FiT_type == "real_h"){
    FiT <<- rbind(read_csv("Data/FiT_levels.csv", col_types = cols()) %>% mutate(time_series = dmy(time_series)) %>% 
                    filter(time_series <= end_date), FiT_zero)
  }
  
  if (FiT_type == "linear"){
    
    FiT <<- rbind(data.frame(time_series = time_series, FiT = seq(init_fit, final_fit, length.out = length(time_series)),
                             FiT_large = seq(init_fit, final_fit, length.out = length(time_series)), exp_tar = exp_tar),
                  FiT_zero)
  }
  
  if (FiT_type == "perc_red") {
    
    FiT <<- rbind(data.frame(time_series = time_series, FiT = geomSeries(init_fit, 1-red_frac, length(time_series)),
                             FiT_large = geomSeries(init_fit, 1-red_frac, length(time_series)), exp_tar = exp_tar),
                  FiT_zero)
  }
  if (FiT_type == "ann_perc_red") {
    #  time_series <- seq(dmy("01jan2010"), end_date, by = '1 month')
    
    year_series <- year(seq(dmy("01jan2010"), end_date, by = '1 year'))
    FiT_yr <- geomSeries(init_fit, 1-red_frac, length(year_series))
    FiT <- data.frame(time_series = time_series, FiT = NA)
    FiT$FiT <- sapply(FiT$time_series, function(x) FiT_yr[which(year_series == year(x))])
    FiT <- FiT %>% mutate(FiT_large = FiT, exp_tar = exp_tar)
    FiT <<- rbind(FiT, FiT_zero)
  }
  
  if(FiT_type == "dep_cap") {
    
    FiT <- data.frame(time_series = time_series, FiT = NA)
    FiT$FiT[1] <- init_fit
    FiT <<- rbind(FiT, FiT_zero)
  }
}

geomSeries <- function(a, r, n){
  series <- vector(length = n)
  for (i in 1:n) series[i] = a*r^(i-1)
  series
}

set_dep_caps <- function(start_date, end_date, FiT_end_date, FiT_type, cap, exp_tar) {
  
  if (FiT_type == "real_f"){
    
    dep_cap <- read_csv("Data/real_dep_cap.csv", skip = 1, col_names = c("q_dates", "orig_cap", "FiT"), col_types = cols()) 
    dep_cap %<>% mutate(cap = orig_cap, inst_cap = NA) %>%
      mutate(q_dates = dmy(q_dates))
    FiT_list <- dep_cap$FiT
    dep_cap_n <- dep_cap %<>% select(q_dates, orig_cap, cap, inst_cap)
    
  } else if (FiT_type == "real_f_ext") {
    dep_cap <- read_csv("Data/real_dep_cap.csv", skip = 1, col_names = c("q_dates", "orig_cap", "FiT"), 
                        col_types = cols()) %>% mutate(q_dates = dmy(q_dates)) %>% select(q_dates, orig_cap)
    dep_cap_ext <- data.frame(q_dates = seq(dmy("1apr2019"), dmy("1jan2021"), by = '3 month'),
                              orig_cap = 62.1:69.1)
    dep_cap <- rbind(dep_cap, dep_cap_ext)
    dep_cap_n <- dep_cap %>% mutate(cap = orig_cap, inst_cap = NA) 
    FiT_list <- c(seq(4.18, by = -0.07, length.out = 18), 0)
  } else {  
    dep_cap_n <- data.frame(q_dates = seq(start_date, FiT_end_date, by = '3 months'), 
                            orig_cap = cap, cap = cap, inst_cap = NA)
    FiT_list <- FiT$FiT[FiT$time_series %in% dep_cap_n$q_dates]
  }
  
  FiT_new <- rep(FiT_list, 3)
  time_series <- seq(start_date, FiT_end_date, by = '1 month')
  FiT_new <- FiT_new[order(match(FiT_new, FiT_list))][1:length(time_series)]
  FiT_n <- data.frame(time_series = time_series, FiT = FiT_new,
                      FiT_large = FiT_new, exp_tar = exp_tar)
  FiT_list_n <- c(FiT_list, 0)
  
  if(FiT_end_date != end_date) {
    dep_cap_zero <- data.frame(q_dates = seq(tail(dep_cap_n$q_dates, 1) + months(3), end_date, by = '3 months'),
                               orig_cap = 0, cap = 0, inst_cap = NA)
    FiT_zero <- data.frame(time_series = seq(FiT_end_date + months(1), end_date, by = '1 month'), FiT = 0,
                           FiT_large = 0, exp_tar = 0)
    dep_cap <<- rbind(dep_cap_n, dep_cap_zero)
    FiT_list <<- FiT_list
    FiT <<- rbind(FiT_n, FiT_zero)
  } else {
    dep_cap <<- dep_cap_n
    FiT <<- FiT_n
    FiT_list <<- FiT_list_n
    
  }
}


initialise_vars <- function() {
  avg_u <<- NULL
  cost <<- NULL
  tot_cost <<- NULL
  cost_priv <<- NULL
  tot_cost_priv <<- NULL
  LCOE_avg <<- NULL
  LCOE_data <<- NULL
  if (run_w_cap == TRUE) FiT_levels <<- NULL
}

#------------------------- 1.2 Agent/model functions ----------------------------#

Household_Agent <- function(a, b, c, d) {
  # a = Y for adopter, N for non-adopter (char)
  # b = income (annual, £)                                            #shuusei20251205
  # c = tenure: "own","priv","soc"                                    #shuusei20251205
  # d = UK region (A–K)                                                #shuusei20251205
  structure(list(factor(a, levels= c("Y", "N")), 
                 b, 
                 factor(c, levels = c("own","priv","soc")),            #shuusei20251205
                 factor(d, levels= c(LETTERS[1:11])), 
                 NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
                 NULL),  
            class = "Household", 
            names = c("status", "income", "tenure", "region",          #shuusei20251205
                      "LF", "consumption", 
                      "inst_cap", "network", "u_inc", "u_ec", "u_soc", "u_cap", "u_tot", "FiT", "exp_tar", "date"))
}

extract <- function(x, str) { # x is adopters or agents (a list of Household objects)
  if (length(x) > 0) unname(unlist(sapply(x, function (x) x[str])))
}

# income と meet_demand の関係をエクスポートする補助関数              #shuusei20251127
export_meet_demand_vs_income <- function(agents,                        #shuusei20251127
                                         file_path = "Data/meet_demand_income.csv") { #shuusei20251127
  inc  <- extract(agents, "income")                                     #shuusei20251127
  lf   <- extract(agents, "LF")                                         #shuusei20251127
  cons <- extract(agents, "consumption")                                #shuusei20251127
  
  meet_demand <- cons / (lf * 24 * 365)                                 #shuusei20251127
  
  df <- data.frame(income      = inc,                                   #shuusei20251127
                   LF          = lf,                                    #shuusei20251127
                   consumption = cons,                                  #shuusei20251127
                   meet_demand = meet_demand)                           #shuusei20251127
  
  readr::write_csv(df, file_path)                                       #shuusei20251127
  
  return(invisible(df))                                                 #shuusei20251127
}              

# 所得デシル別の平均 meet_demand を計算し、グローバルに保存する        #shuusei20251129
compute_meet_demand_ref_by_decile <- function(agents) {                 #shuusei20251129
  dec  <- extract(agents, "inc_decile")                                 #shuusei20251129
  lf   <- extract(agents, "LF")                                         #shuusei20251129
  cons <- extract(agents, "consumption")                                #shuusei20251129
  
  meet_demand <- cons / (lf * 24 * 365)                                 #shuusei20251129
  
  ref_vec <- rep(NA_real_, 10)                                          #shuusei20251129
  for (d in 1:10) {                                                     #shuusei20251129
    idx <- which(dec == d & !is.na(meet_demand))                        #shuusei20251129
    if (length(idx) > 0) {                                              #shuusei20251129
      ref_vec[d] <- mean(meet_demand[idx], na.rm = TRUE)               #shuusei20251129
    }                                                                   #shuusei20251129
  }                                                                     #shuusei20251129
  
  meet_dmd_ref_dec <<- ref_vec                                          #shuusei20251129
  invisible(ref_vec)                                                    #shuusei20251129
}                                                                       #shuusei20251129

assign_income <- function(tenure = "own") {                             #shuusei20251205
  if (!exists("income_params_tenure", inherits = TRUE)) {               #shuusei20251205
    init_income_elec_params()                                           #shuusei20251205
  }                                                                     #shuusei20251205
  pars <- income_params_tenure[[tenure]]                                #shuusei20251205
  rlnorm(1, pars$mu_year, pars$sigma)                                   #shuusei20251205
}

assign_region <- function() {                                             #shuusei20251206
  # グローバル環境から region_weights を取り出す                        #shuusei20251206
  rw <- get("region_weights", envir = .GlobalEnv)                         #shuusei20251206
  rw <- as.numeric(rw)                                                   #shuusei20251206
  
  # 壊れていないかチェック（長さ・有限値）                              #shuusei20251206
  if (length(rw) != 11L || any(!is.finite(rw))) {                        #shuusei20251206
    stop("region_weights が壊れています: length = ", length(rw))         #shuusei20251206
  }                                                                       #shuusei20251206
  
  # 念のため合計 1 に正規化してから使う                                  #shuusei20251206
  s <- sum(rw)                                                            #shuusei20251206
  if (!is.finite(s) || s <= 0) {                                          #shuusei20251206
    stop("region_weights の合計が 0 か NA です")                         #shuusei20251206
  }                                                                       #shuusei20251206
  rw <- rw / s                                                            #shuusei20251206
  
  # sample() 失敗時には一様分布にフォールバック                         #shuusei20251206
  idx <- tryCatch(                                                        #shuusei20251206
    sample.int(11L, size = 1L, prob = rw),                                #shuusei20251206
    error = function(e) sample.int(11L, size = 1L)                        #shuusei20251206
  )                                                                       #shuusei20251206
  LETTERS[idx]                                                            #shuusei20251206
}                                                                         #shuusei20251206

assign_soc_network <- function(A, n_ag, n_l) {
  A$network <- sample(1:n_ag, n_l)
  return(A)
}

# small‑world ネットワークのフォールバック監視用カウンタ           #shuusei20251206
sw_debug <<- list(                                                     #shuusei20251206
  total_calls      = 0L,                                               #shuusei20251206
  fallback_stage1  = 0L,  # w 全部 NA/0 で一様にした回数              #shuusei20251206
  fallback_stage2  = 0L   # 最終チェックで一様サンプルに落とした回数  #shuusei20251206
)                                                                      #shuusei20251206

# 所得×地域に依存した small‑world ネットワークを構成                #shuusei20251205
assign_smallworld_network <- function(agents,
                                      k       = 10,                    #shuusei20251205
                                      alpha   = 0.05,                  #shuusei20251205
                                      p_rewire = 0.1){                 #shuusei20251205
  n <- length(agents)                                                   #shuusei20251205
  if (n <= 1L) return(agents)                                           #shuusei20251205
  
  incomes <- extract(agents, "income")                                  #shuusei20251205
  regions <- extract(agents, "region")                                  #shuusei20251205
  
  neighbours <- vector("list", n)                                       #shuusei20251205
  
  regs <- unique(regions)                                               #shuusei20251205
  for (r in regs){                                                      #shuusei20251205
    idx_r <- which(regions == r)                                        #shuusei20251205
    N_r   <- length(idx_r)                                              #shuusei20251205
    if (N_r <= 1L) next                                                 #shuusei20251205
    
    inc_r <- incomes[idx_r]                                             #shuusei20251205
    ord_local <- order(inc_r, na.last = NA)                             #shuusei20251205
    ord      <- idx_r[ord_local]                                        #shuusei20251205
    N_eff    <- length(ord)                                             #shuusei20251205
    if (N_eff <= 1L) next                                               #shuusei20251205
    
    y_r   <- incomes[ord]                                               #shuusei20251205
    med_y <- median(y_r, na.rm = TRUE)                                  #shuusei20251205
    diffs <- abs(y_r - med_y)                                           #shuusei20251205
    tau_r <- median(diffs, na.rm = TRUE)                                #shuusei20251205
    if (is.na(tau_r) || tau_r <= 0) {                                   #shuusei20251205
      tau_r <- sd(y_r, na.rm = TRUE)                                    #shuusei20251205
      if (is.na(tau_r) || tau_r <= 0) tau_r <- 1                        #shuusei20251205
    }                                                                   #shuusei20251205
    
    width <- max(1L, ceiling(alpha * N_eff))                            #shuusei20251205
    
    for (pos in seq_len(N_eff)){                                        #shuusei20251205
      i <- ord[pos]                                                     #shuusei20251205
      pos_min <- max(1L, pos - width)                                   #shuusei20251205
      pos_max <- min(N_eff, pos + width)                                #shuusei20251205
      cand_pos <- setdiff(pos_min:pos_max, pos)                         #shuusei20251205
      if (length(cand_pos) == 0L) next                                  #shuusei20251205
      cand_idx <- ord[cand_pos]                                         #shuusei20251205
      
      d <- abs(incomes[i] - incomes[cand_idx])                          #shuusei20251205
      w <- exp(-d / tau_r)                                              #shuusei20251205
      
      sw_debug$total_calls <<- sw_debug$total_calls + 1L                #shuusei20251206
      
      # w が全部 NA / 0 なら一様分布にする                              #shuusei20251206
      if (!any(is.finite(w)) || sum(w, na.rm = TRUE) <= 0) {            #shuusei20251206
        sw_debug$fallback_stage1 <<- sw_debug$fallback_stage1 + 1L      #shuusei20251206
        w <- rep(1, length(cand_idx))                                   #shuusei20251206
      }                                                                 #shuusei20251206
      
      deg_i <- length(neighbours[[i]])                                  #shuusei20251205
      n_add <- max(0L, k - deg_i)                                       #shuusei20251205
      if (n_add <= 0L) next                                             #shuusei20251205
      n_add <- min(n_add, length(cand_idx))                             #shuusei20251205
      
      ## ★候補が 1 つだけのときは sample() を使わない                     #shuusei20251206
      if (length(cand_idx) == 1L) {                                     #shuusei20251206
        sel <- cand_idx                                                 #shuusei20251206
      } else {                                                          #shuusei20251206
        ## sample() に渡す前の最終チェック                                 #shuusei20251206
        w <- as.numeric(w)                                              #shuusei20251206
        if (length(w) != length(cand_idx) ||                            #shuusei20251206
            any(!is.finite(w)) ||                                       #shuusei20251206
            sum(w) <= 0) {                                              #shuusei20251206
          sw_debug$fallback_stage2 <<- sw_debug$fallback_stage2 + 1L    #shuusei20251206
          # 何かおかしければ確率なし（＝一様）でサンプル                  #shuusei20251206
          sel <- sample(cand_idx, size = n_add, replace = FALSE)        #shuusei20251206
        } else {                                                        #shuusei20251206
          w <- w / sum(w)                                               #shuusei20251206
          sel <- sample(cand_idx, size = n_add, replace = FALSE, prob = w) #shuusei20251206
        }                                                               #shuusei20251206
      }                                                                 #shuusei20251206
      
      for (j in sel){                                                   #shuusei20251205
        if (i == j) next                                                #shuusei20251205
        if (is.null(neighbours[[i]])) neighbours[[i]] <- integer(0)     #shuusei20251205
        if (is.null(neighbours[[j]])) neighbours[[j]] <- integer(0)     #shuusei20251205
        if (!(j %in% neighbours[[i]])) neighbours[[i]] <- c(neighbours[[i]], j) #shuusei20251205
        if (!(i %in% neighbours[[j]])) neighbours[[j]] <- c(neighbours[[j]], i) #shuusei20251205
      }                                                                 #shuusei20251205
    }                                                                   #shuusei20251205
  }
  
  ## Watts–Strogatz 型のランダム再配線（ここから下は変更なし）           #shuusei20251205
  if (p_rewire > 0){                                                    #shuusei20251205
    edges <- list()                                                     #shuusei20251205
    for (i in seq_len(n)){                                              #shuusei20251205
      if (length(neighbours[[i]]) == 0) next                            #shuusei20251205
      for (j in neighbours[[i]]){                                       #shuusei20251205
        if (i < j) edges[[length(edges) + 1L]] <- c(i, j)               #shuusei20251205
      }                                                                 #shuusei20251205
    }                                                                   #shuusei20251205
    if (length(edges) > 0){                                             #shuusei20251205
      edges_mat <- do.call(rbind, edges)                                #shuusei20251205
      n_edges   <- nrow(edges_mat)                                      #shuusei20251205
      for (e_idx in seq_len(n_edges)){                                  #shuusei20251205
        i <- edges_mat[e_idx, 1]                                        #shuusei20251205
        j <- edges_mat[e_idx, 2]                                        #shuusei20251205
        if (runif(1) < p_rewire){                                       #shuusei20251205
          neighbours[[i]] <- setdiff(neighbours[[i]], j)                #shuusei20251205
          neighbours[[j]] <- setdiff(neighbours[[j]], i)                #shuusei20251205
          if (n > 2){                                                   #shuusei20251205
            repeat{                                                     #shuusei20251205
              new_j <- sample.int(n, 1)                                 #shuusei20251205
              if (new_j != i && !(new_j %in% neighbours[[i]])) break    #shuusei20251205
            }                                                           #shuusei20251205
            if (is.null(neighbours[[new_j]])) neighbours[[new_j]] <- integer(0) #shuusei20251205
            neighbours[[i]]      <- c(neighbours[[i]], new_j)           #shuusei20251205
            neighbours[[new_j]]  <- c(neighbours[[new_j]], i)           #shuusei20251205
          }                                                             #shuusei20251205
        }                                                               #shuusei20251205
      }                                                                 #shuusei20251205
    }                                                                   #shuusei20251205
  }                                                                     #shuusei20251205
  
  for (i in seq_len(n)){                                                #shuusei20251205
    if (is.null(neighbours[[i]])) neighbours[[i]] <- integer(0)         #shuusei20251205
    agents[[i]]$network <- neighbours[[i]]                              #shuusei20251205
  }                                                                     #shuusei20251205
  
  agents                                                                #shuusei20251205
}

assign_LF <- function(A) {
  A$LF <- LF$LF[which(LF$Label == A$region)]/100
  return(A)
}

assign_elec_cons <- function(A) {                                       #shuusei20251205
  if (!exists("elec_bands_owners", inherits = TRUE)) {                  #shuusei20251205
    init_income_elec_params()                                           #shuusei20251205
  }                                                                     #shuusei20251205
  
  # renter は静的母集団とする: 消費量は NA のまま                      #shuusei20251205
  if (!is.null(A$tenure) && !is.na(A$tenure) && A$tenure != "own") {    #shuusei20251205
    A$consumption <- NA_real_                                           #shuusei20251205
    return(A)                                                           #shuusei20251205
  }                                                                     #shuusei20251205
  
  y <- A$income                                                         #shuusei20251205
  if (is.na(y)) {                                                       #shuusei20251205
    A$consumption <- NA_real_                                           #shuusei20251205
    return(A)                                                           #shuusei20251205
  }                                                                     #shuusei20251205
  
  idx <- which(y >= elec_bands_owners$lower &                           #shuusei20251205
                 y <= elec_bands_owners$upper)[1]                       #shuusei20251205
  if (is.na(idx)) idx <- nrow(elec_bands_owners)                        #shuusei20251205
  
  mu    <- elec_bands_owners$mu[idx]                                    #shuusei20251205
  sigma <- elec_bands_owners$sigma[idx]                                 #shuusei20251205
  A$consumption <- rlnorm(1, mu, sigma)                                 #shuusei20251205
  return(A)                                                             #shuusei20251205
}

#tyousei
assign_u_inc <- function(A, mean_inc) {
  A$u_inc <- 1/(1+exp((mean_inc-A$income)*0.0002))
  return(A)
}


#---------------------------------------------------------#                              #shuusei202511299
# SES / 所得クインタイル関連のヘルパーとパラメータ                #shuusei202511299
#   ・デシル(1〜10) → クインタイル(Q1〜Q5) への変換             #shuusei202511299
#   ・クインタイル別の屋根上限 / cap_share0 / 予算シェア         #shuusei202511299
#---------------------------------------------------------#                              #shuusei202511299

get_quintile_index_from_decile <- function(dec) {                       #shuusei202511299
  if (is.null(dec) || is.na(dec)) return(NA_integer_)                   #shuusei202511299
  q_idx <- ceiling(dec / 2)                                             #shuusei202511299
  q_idx <- max(1L, min(5L, q_idx))                                      #shuusei202511299
  return(q_idx)                                                         #shuusei202511299
}                                                                       #shuusei202511299

# クインタイル別の屋根上限（kW）                                     #shuusei202511299
#   Q1: D1–2, Q2: D3–4, Q3: D5–6, Q4: D7–8, Q5: D9–10                  #shuusei202511299
SES_ROOF_LIMIT_Q <- c(                                                  #shuusei202511299
  0.00,  # Q1                                                           #shuusei202511299
  0.00,  # Q2                                                           #shuusei202511299
  0.00,  # Q3                                                           #shuusei202511299
  0.00,  # Q4                                                           #shuusei202511299
  0.00   # Q5                                                           #shuusei202511299
)                                                                       #shuusei202511299

# クインタイル別の cap_share0（cap_utility の基準点）                 #shuusei202511299
#   ※ 現状は全クインタイル 0.30（= 30%）で、元の実装と同じ           #shuusei202511299
SES_CAP_SHARE0_Q <- c(                                                  #shuusei202511299
  0.00,  # Q1                                                           #shuusei202511299
  0.00,  # Q2                                                           #shuusei202511299
  0.00,  # Q3                                                           #shuusei202511299
  0.00,  # Q4                                                           #shuusei202511299
  0.00   # Q5                                                           #shuusei202511299
)                                                                       #shuusei202511299

# クインタイル別の「所得に対する PV 予算シェア」                      #shuusei202511299
#   ※ 現状は全クインタイル 0.30（= 30%）で、元の実装と同じ           #shuusei202511299
#      将来 30,27,24,21,18% にしたくなったら、このベクトルだけ変更    #shuusei202511299
SES_BUDGET_SHARE_Q <- c(                                                #shuusei202511299
  0.30,  # Q1                                                           #shuusei202511299
  0.30,  # Q2                                                           #shuusei202511299
  0.30,  # Q3                                                           #shuusei202511299
  0.30,  # Q4                                                           #shuusei202511299
  0.30   # Q5          
  #shuusei202511299
)                                                                       #shuusei202511299

#tyousei
get_roof_limit_by_decile <- function(dec) {                             #shuusei202511299
  # inc_decile が無いときは屋根制約なし（Inf）                        #shuusei202511299
  if (is.null(dec) || is.na(dec)) return(Inf)                           #shuusei202511299
  
  q_idx <- get_quintile_index_from_decile(dec)                          #shuusei202511299
  return(SES_ROOF_LIMIT_Q[[q_idx]])                                     #shuusei202511299
}                                                                       #shuusei202511299


get_cap_share0_by_decile <- function(dec) {                             #shuusei202511299
  # inc_decile が無いときは従来どおり cap_share0 = 0.20 を使用        #shuusei202511299
  if (is.null(dec) || is.na(dec)) {                                     #shuusei202511299
    return(0.20)                                                        #shuusei202511299
  }                                                                     #shuusei202511299
  
  q_idx <- get_quintile_index_from_decile(dec)                          #shuusei202511299
  return(SES_CAP_SHARE0_Q[[q_idx]])                                     #shuusei202511299
}                                                                       #shuusei202511299


get_budget_share_by_decile <- function(dec) {                           #shuusei202511299
  # inc_decile（1〜10）→ クインタイル別「所得に対する PV 予算割合」   #shuusei202511299
  # デフォルト 0.24 も従来どおり維持                                  #shuusei202511299
  if (is.null(dec) || is.na(dec)) return(0.24)                          #shuusei202511299
  
  q_idx <- get_quintile_index_from_decile(dec)                          #shuusei202511299
  return(SES_BUDGET_SHARE_Q[[q_idx]])                                   #shuusei202511299
}                                                                       #shuusei202511299

assign_inst_cap <- function(A) {
  if (A$status == "N") { # otherwise agents who have already adopted can change inst_cap!
    ## 予算ベースの容量（所得クインタイル別シェア）                     #shuusei20251129
    share_Q <- get_budget_share_by_decile(A$inc_decile)                      #shuusei20251129
    
    budget_total <- share_Q * A$income                                       #shuusei20251212
    
    ## 予算 >= fixed + marginal*cap  => cap <= (budget - fixed)/marginal     #shuusei20251212
    inst_cap_budget <- (budget_total - fixed_current) / marginal_current     #shuusei20251212
    if (!is.finite(inst_cap_budget) || inst_cap_budget < 0) inst_cap_budget <- 0 #shuusei20251212
    
    ## 需要ベースの容量（診断用 & 正規化用）                            #shuusei20251129
    meet_demand <- A$consumption/(A$LF*24*365)                          #shuusei20251129
    A$meet_demand <- meet_demand                                        #shuusei20251130
    ## デフォルトは従来どおり「meet_demand 上限」                      #shuusei20251129
    roof_limit <- meet_demand                                           #shuusei20251129
    
    ## 所得デシルが付いている場合：                                   #shuusei20251129
    ##  base_roof(dec) × meet_demand / mean_meet_demand_dec             #shuusei20251129
    if (!is.null(A$inc_decile) && !is.na(A$inc_decile)) {               #shuusei20251129
      base_roof <- get_roof_limit_by_decile(A$inc_decile)              #shuusei20251129
      
      scale_fac <- 1                                                    #shuusei20251129
      if (exists("meet_dmd_ref_dec", inherits = TRUE)) {                #shuusei20251129
        md_ref <- meet_dmd_ref_dec[A$inc_decile]                        #shuusei20251129
        if (!is.na(md_ref) && md_ref > 0) {                             #shuusei20251129
          scale_fac <- meet_demand / md_ref                             #shuusei20251129
        }                                                               #shuusei20251129
      }                                                                 #shuusei20251129
      
      roof_limit <- base_roof * scale_fac                               #shuusei20251129
    }                                                                   #shuusei20251129
    
    ## raw の容量（4kW/10kW 調整の前）                                 #shuusei20251130
    ## ⇒ inst_cap_raw = min(inst_cap_budget, meet_demand)               #shuusei20251130
    inst_cap_raw <- min(inst_cap_budget, meet_demand)                   #shuusei20251130
    
    ## raw 容量と制約要因をエージェントに記録                          #shuusei202511288
    A$inst_cap_budget <- inst_cap_budget                                #shuusei202511288
    A$roof_limit      <- roof_limit                                     #shuusei202511288
    A$inst_cap_raw    <- inst_cap_raw                                   #shuusei202511288
    
    ## inst_cap_raw を決めた制約（budget / meet / tie）を記録          #shuusei20251130
    if (is.na(inst_cap_raw)) {                                          #shuusei20251130
      A$cap_raw_source <- NA_character_                                 #shuusei20251130
    } else if (inst_cap_raw <= inst_cap_budget + 1e-6 &&                #shuusei20251130
               inst_cap_raw <  meet_demand  - 1e-6) {                   #shuusei20251130
      A$cap_raw_source <- "budget"                                      #shuusei20251130
    } else if (inst_cap_raw <= meet_demand + 1e-6 &&                    #shuusei20251130
               inst_cap_raw <  inst_cap_budget - 1e-6) {                #shuusei20251130
      A$cap_raw_source <- "meet"                                        #shuusei20251130
    } else {                                                            #shuusei20251130
      A$cap_raw_source <- "tie"                                         #shuusei20251130
    }                                                                   #shuusei20251130
    
    ## 4kW vs 大容量の選択結果（デフォルト: 選択なし）                 #shuusei202511288
    A$cap_choice_type <- "no_choice"                                    #shuusei202511288
    
    ## まず raw 容量をそのまま入れる                                   #shuusei202511288
    A$inst_cap <- inst_cap_raw                                          #shuusei202511288
    
    ## 4kW を選ぶか、大容量を選ぶかの判定                             #shuusei202511288
    if (inst_cap_raw > 4) {   # would they be better off going for the lower investment? #shuusei202511288
      ret_large <- annual_return(A)                                     #shuusei202511288
      large_cap <- A$inst_cap                                           #shuusei202511288
      A$inst_cap <- 4                                                   #shuusei202511288
      ret_small <- annual_return(A)                                     #shuusei202511288
      
      if (ret_small > ret_large) {                                      #shuusei202511288
        A$inst_cap <- 4                                                 #shuusei202511288
        A$cap_choice_type <- "choose_4"                                 #shuusei202511288
      } else {                                                          #shuusei202511288
        A$inst_cap <- large_cap                                         #shuusei202511288
        A$cap_choice_type <- "choose_large"                             #shuusei202511288
      }                                                                 #shuusei202511288
      
      ## 10kW の上限でトランケート                                    #shuusei202511288
      if (A$inst_cap > 10) {                                            #shuusei202511288
        A$inst_cap <- 10                                                #shuusei202511288
        if (A$cap_choice_type == "choose_large") {                      #shuusei202511288
          A$cap_choice_type <- "choose_large_trunc10"                   #shuusei202511288
        }                                                               #shuusei202511288
      }                                                                 #shuusei202511288
    }                                                                   #shuusei202511288
  }
  
  return(A)
}


utilities <- function(A, w, ags) {
  # A is an object representing an agent
  pp <- simple_PP(A)
  A$u_ec <- (20-pp)/19
  A$u_soc <- soc_utility(A, ags)
  A$u_cap <- cap_utility(A)
  A$u_tot <- w[1]*A$u_inc + w[2]*A$u_soc + w[3]*A$u_ec +  w[4]*A$u_cap
  
  return(A)
}

decide <- function(A, threshold) {
  if (A$status == "N"){
    # A is an object representing an agent
    if (A$u_tot > threshold && A$status == "N") {
      A$status[1] <- "Y"
      
      # まずベースFiTを決定                                         #shuusei20251116
      if (A$inst_cap <= 4) {
        base_FiT <- FiT_current_small                                 #shuusei20251116
      } else {
        base_FiT <- FiT_current_large                                 #shuusei20251116
      }                                                               #shuusei20251116
      
      # 未来シミュレーションかつ低所得世帯なら上乗せ                #shuusei20251116
      eff_FiT <- base_FiT                                             #shuusei20251116
      if (exists("use_low_income_bonus") && isTRUE(use_low_income_bonus) && #shuusei20251116
          exists("low_income_cutoff") && !is.null(low_income_cutoff) &&     #shuusei20251116
          !is.na(A$income) && A$income <= low_income_cutoff) {              #shuusei20251116
        eff_FiT <- base_FiT + extra_FiT_low                           #shuusei20251116
      }                                                               #shuusei20251116
      
      A$FiT   <- eff_FiT                                              #shuusei20251116
      A$exp_tar <- exp_tar_current
      A$date    <- current_date
    }
  }
  return(A)
}

simple_PP <- function(A) {
  
  n <- 20 # economic lifetime
  
  R <- annual_return(A)
  
  if (R > 0) {                                                            #shuusei20251212
    capex <- fixed_current + marginal_current * A$inst_cap                #shuusei20251212
    pp <- capex / R                                                       #shuusei20251212
  } else pp <- 20                                                         #shuusei20251212
  
  if (pp > 20) pp <- 20
  return(pp)
}


annual_return <- function(A){
  prod <- output(A$inst_cap, A$LF)
  
  # まずシステム容量によるベースFiTを決定                           #shuusei20251116
  if (A$inst_cap <= 4){
    base_FiT <- FiT_current_small                                       #shuusei20251116
  } else {
    base_FiT <- FiT_current_large                                       #shuusei20251116
  }                                                                     #shuusei20251116
  
  # 未来シミュレーションかつ低所得世帯ならFiTを上乗せ                 #shuusei20251116
  eff_FiT <- base_FiT                                                   #shuusei20251116
  if (exists("use_low_income_bonus") && isTRUE(use_low_income_bonus) && #shuusei20251116
      exists("low_income_cutoff") && !is.null(low_income_cutoff) &&     #shuusei20251116
      !is.na(A$income) && A$income <= low_income_cutoff) {              #shuusei20251116
    eff_FiT <- base_FiT + extra_FiT_low                                 #shuusei20251116
  }                                                                     #shuusei20251116
  
  R_FiT <- prod*eff_FiT                                                 #shuusei20251116
  displaced <- export <- 0.5*prod
  R_exp <- export*exp_tar_current
  R_sav <- displaced*elec_price
  R <- R_FiT + R_sav + R_exp
}


output <- function(x, y) {
  # x = installed capacity
  # y = load factor
  P <- x*24*365*y
}

#soc_utility <- function(A, ags) {
  #neighbours <- ags[A$network]
  #links <- length(A$network)
  #no_adopters <- sum(map_int(neighbours, "status") == 1)
  #u_soc <- 1/(1+exp(1.2*((links/4)-no_adopters)))
  
#tyousei
soc_utility <- function(A, ags) {
  neighbours <- ags[A$network]
  links <- length(A$network)
  no_adopters <- sum(map_int(neighbours, "status") == 1)
  u_soc <- 1/(1+exp(1.2*((links/4)-no_adopters)))  #shuusei20251128
}

#tyousei 
#cap_utility <- function(A) {                                           #shuusei20251129
  # 所得デシルごとの cap_share0 を使う                                #shuusei20251129
#  cap_share0 <- get_cap_share0_by_decile(A$inc_decile)                #shuusei20251129
  
#  k_cap      <- 4                                                     #shuusei20251127
  # ↑ ロジスティックの傾き。大きいほど 0/1 に近づきやすい（要調整）   #shuusei20251127
  
  # 所得に対する PV 投資額の比率（費用 / 所得）                        #shuusei20251127
#  ratio <- (A$inst_cap * kW_price_current) / A$income                  #shuusei20251127
  
  # ratio = cap_share0 のとき u_cap = 0.5 になるロジスティック          #shuusei20251127
  # ratio が小さいほど u_cap が 1 に近づき、大きいほど 0 に近づく      #shuusei20251127
#  u_cap <- 1/(1 + exp((ratio - cap_share0) * k_cap))                   #shuusei20251127
  
#  return(u_cap)                                                        #shuusei20251127
#}                        												#shuusei20251129


cap_utility <- function(A) {                                           #shuusei202511299!
  capex <- fixed_current + marginal_current * A$inst_cap                #shuusei20251212
  u_cap <- 1/(1 + exp(-(0.2 * A$income - capex) * 0.0007))              #shuusei20251212
  return(u_cap)                                                        #shuusei202511299!
}                                                                       #shuusei202511299!
#---------------------------- 1.3 Cost calculation ------------------------------#
subs_cost <- function(adpts, rn, number_of_agents) {
  if (length(adpts) > 0) {
    adopt_dates <- unname((sapply(adpts, function (x) x["date"])))
    adopt_dates <- do.call("c", adopt_dates)
    
    # UK policy: subsidies were guaranteed for 25 years before 1/8/2012, 20 years thereafter
    after <- adopt_dates >= dmy("1aug2012")
    before <- adopt_dates < dmy("1aug2012")
    
    guarantee <- vector(length = length(adopt_dates))
    
    guarantee[after] <- 20
    guarantee[before] <- 25
    
    adopt_costs <- data.frame(adopt_date = adopt_dates, 
                              output = sapply(adpts, function (x) output(x$inst_cap, x$LF)), 
                              FiT = extract(adpts, "FiT"), exp_tar = extract(adpts, "exp_tar"), 
                              guarantee = guarantee)
    
    
    # find the number of owner-occupiers corresponding to the adoption year 
    adopt_costs %<>% mutate(n_owners = sapply(adopt_date, which_owner_year))
    
    adopt_costs %<>% mutate(end_date = adopt_date + years(guarantee), 
                            export = output/2, # assuming no meter is installed
                            annual_cost = output*FiT + export*exp_tar,
                            annual_cost_scaled = annual_cost*n_owners/number_of_agents,
                            tot_cost = guarantee*annual_cost,
                            tot_cost_scaled = tot_cost*n_owners/number_of_agents)
    
    adopt_costs %<>% arrange(adopt_date)
    
    tot_sub_cost <- sum(adopt_costs$tot_cost_scaled)
    
    time_series <- dmy("01jan2010") + months(1:370)
    # have found annual & total cost per installation; can now find annual cost
    annual_cost <- vector(length = length(time_series))
    for (i in 1:length(time_series))
    { existing_inst <- filter(adopt_costs, adopt_date < time_series[i], end_date >= time_series[i])
    annual_cost[i] <- sum(existing_inst$annual_cost_scaled)
    }
    
  }
  else { # no one has adopted
    time_series <- dmy("01jan2010") + months(1:370)
    annual_cost = rep(0, length(time_series))
    tot_sub_cost = 0
  }
  a <- data.frame(time_series = time_series, annual_cost = annual_cost,
                  run_number = rep(rn, length(time_series)))
  cost_results <- list(a, tot_sub_cost = tot_sub_cost)
}

priv_cost <- function(x, rn, number_of_agents) { # x = adopters
  if (length(x) > 0) {
    adopt_dates <- unname((sapply(x, function (x) x["date"])))
    adopt_dates <- do.call("c", adopt_dates)
    inst_cap <- extract(x, "inst_cap")
    
    priv_costs <- data.frame(adopt_date = adopt_dates, 
                             inst_cap = inst_cap)
    
    priv_costs %<>% mutate(PV_fixed    = sapply(adopt_date, which_PV_fixed),     #shuusei20251212
                           PV_marginal = sapply(adopt_date, which_PV_marginal),  #shuusei20251212
                           tot_cost    = PV_fixed + PV_marginal * inst_cap,      #shuusei20251212
                           n_owners    = sapply(adopt_date, which_owner_year),
                           tot_cost_scaled = tot_cost*n_owners/number_of_agents)
    
    
    tot_priv_cost <- sum(priv_costs$tot_cost_scaled)
    
    time_series <- FiT$time_series
    cum_cost <- vector(length = length(time_series))
    for (i in 1:length(time_series))
    { existing_inst <- filter(priv_costs, adopt_date < time_series[i])
    cum_cost[i] <- sum(existing_inst$tot_cost_scaled)
    }
  }
  else {   time_series <- FiT$time_series
  cum_cost = rep(0, length(time_series))
  tot_priv_cost = 0
  }
  cost_results <- list(data.frame(time_series = time_series, cum_cost = cum_cost, 
                                  run_number = rep(rn, length(time_series))), 
                       tot_priv_cost = tot_priv_cost)
}

calc_LCOE <- function(adpts, rn, number_of_agents) {
  
  if (length(adpts) > 0){
    r <<- 0.05
    
    adopt_dates <- adpts %>% sapply(function (x) x["date"]) %>% unname
    adopt_dates <- do.call("c", adopt_dates)
    
    PV_fixed    <- adopt_dates %>% sapply(which_PV_fixed)                   #shuusei20251212
    PV_marginal <- adopt_dates %>% sapply(which_PV_marginal)                #shuusei20251212
    
    lifetime <- 25 # how long do the solar panels last?
    
    after <- adopt_dates >= dmy("1aug2012")
    before <- adopt_dates < dmy("1aug2012")
    
    guarantee <- vector(length = length(adopt_dates))
    
    guarantee[after] <- 20
    guarantee[before] <- 25
    
    adopt_costs <- data.frame(adopt_date = adopt_dates, inst_cap = extract(adpts, "inst_cap"),
                              output = sapply(adpts, function (x) output(x$inst_cap, x$LF)), 
                              FiT = extract(adpts, "FiT"), exp_tar = extract(adpts, "exp_tar"), 
                              guarantee = guarantee,
                              PV_fixed = PV_fixed,                          #shuusei20251212
                              PV_marginal = PV_marginal)                    #shuusei20251212
    
    adopt_costs %<>% mutate(export = output/2, annual_cost = output*FiT + export*exp_tar,
                            cap_cost = PV_fixed + PV_marginal * inst_cap)   #shuusei20251212
    
    tot_output <- sum(adopt_costs$output)
    
    
    adopt_costs %<>% mutate(LCOE_ind = LCOE(annual_cost, cap_cost, guarantee, output),
                            n_owners = sapply(adopt_date, which_owner_year),
                            output_scaled = output*n_owners/number_of_agents,
                            weight = output/tot_output)
    
    tot_output_scaled <- sum(adopt_costs$output_scaled)
    
    adopt_costs %<>% mutate(weight_scaled = output_scaled/tot_output_scaled, 
                            run_number = as.factor(rep(rn, nrow(adopt_costs))))
    
    adopt_costs %<>% select(adopt_date, LCOE_ind, weight_scaled, run_number, output_scaled)
    
    LCOE_weighted_scaled <- sum(adopt_costs$LCOE_ind*adopt_costs$weight_scaled)
  }
  else {
    LCOE_weighted_scaled <- NA
    adopt_costs <- data.frame(adopt_date = NA, LCOE_ind = NA, weight_scaled = NA,
                              run_number = rn, output_scaled = NA)
  }
  
  
  return(list(LCOE_weighted_scaled, adopt_costs))
}

LCOE <- function(annual_cost, cap_cost, guarantee, output) {
  # annual_cost, cap_cost, guarantee, output は
  # 各インストールごとのベクトルとして想定 #shuusei20251116
  
  cost <- rep(0, length(annual_cost))      #shuusei20251116
  prod_elec <- rep(0, length(output))      #shuusei20251116
  
  for (i in 1:25){
    disc_factor <- (1+r)^i 
    
    if (i == 1) {
      # 1年目だけは設備費も含めたコスト #shuusei20251116
      yr_cost <- annual_cost + cap_cost    #shuusei20251116
    } else {
      # 2年目以降は、保証期間内だけ annual_cost、それ以降は0 #shuusei20251116
      yr_cost <- ifelse(i <= guarantee, annual_cost, 0)  #shuusei20251116
    }
    
    cost <- cost + yr_cost/disc_factor     #shuusei20251116
    prod_elec <- prod_elec + output/disc_factor  #shuusei20251116
  }
  
  LCOE <- 1000*cost/prod_elec # pounds per MWh（各インストールごとのベクトル） #shuusei20251116
}

which_PV_fixed <- function(x) {                                         #shuusei20251212
  if (!inherits(x, "Date")) x <- dmy(x)                                  #shuusei20251212
  row <- kW_price %>%                                                    #shuusei20251212
    filter(!is.na(X1), X1 <= x) %>%                                      #shuusei20251212
    arrange(desc(X1)) %>%                                                #shuusei20251212
    slice(1)                                                             #shuusei20251212
  if (nrow(row) == 0) stop("PV fixed cost not found for date <= ", x)    #shuusei20251212
  as.numeric(row$X2[[1]])                                                #shuusei20251212
}                                                                        #shuusei20251212

which_PV_marginal <- function(x) {                                      #shuusei20251212
  if (!inherits(x, "Date")) x <- dmy(x)                                  #shuusei20251212
  row <- kW_price %>%                                                    #shuusei20251212
    filter(!is.na(X1), X1 <= x) %>%                                      #shuusei20251212
    arrange(desc(X1)) %>%                                                #shuusei20251212
    slice(1)                                                             #shuusei20251212
  if (nrow(row) == 0) stop("PV marginal cost not found for date <= ", x) #shuusei20251212
  as.numeric(row$X3[[1]])                                                #shuusei20251212
}                                                                        #shuusei20251212

which_PV_cost <- function(x) {                                         #shuusei20251212
  stop("which_PV_cost() is deprecated. Use which_PV_fixed()/which_PV_marginal() and CAPEX=fixed+marginal*inst_cap.") #shuusei20251212
}                                                                       #shuusei20251212

which_owner_year <- function(x) {                        #shuusei20251116
  owner_occupiers$X2[owner_occupiers$X1 ==               #shuusei20251116
                       as.numeric(year(x))][1]           #shuusei20251116
}                                                        #shuusei20251116
#------------------------------- 1.4 Processing ---------------------------------#

append_results <- function() {
  avg_u <<- rbind(avg_u, all_res_rn[[1]])
  cost <<- rbind(cost, all_res_rn[[2]])
  tot_cost <<- rbind(tot_cost, all_res_rn[[3]])
  cost_priv <<- rbind(cost_priv, all_res_rn[[4]])
  tot_cost_priv <<- rbind(tot_cost_priv, all_res_rn[[5]])
  LCOE_avg <<- rbind(LCOE_avg, all_res_rn[[6]])
  LCOE_data <<- rbind(LCOE_data, all_res_rn[[7]])
  if (run_w_cap == TRUE) FiT_levels <<- rbind(FiT_levels, all_res_rn[[8]])
}

calc_prod <- function(indiv_data, number_of_runs){
  cum_prod <- NULL
  current_prod_run <- vector(length = number_of_runs)
  for (i in 1:length(averages$time_series)) {
    
    for (j in 1:number_of_runs) {
      current_prod_run[j] <- indiv_data %>% 
        filter(run_number == j, adopt_date < averages$time_series[i]) %>% 
        summarise(sum(output_scaled))
      current_prod_run %<>% unlist
      
    }
    current_prod <- 
      data.frame(time_series = rep(averages$time_series[i], number_of_runs), 
                 run_number = as.factor(c(1:number_of_runs)), current_prod = current_prod_run)
    
    cum_prod <- rbind(cum_prod, current_prod)
  }
  colnames(cum_prod) <- c("time_series", "run_number", "current_prod")
  cum_prod_avg <- cum_prod %>% group_by(time_series) %>% 
    summarise(current_prod = mean(current_prod))
  res_list <- list(cum_prod, cum_prod_avg)
}

summarise_results <- function(avg_u, cost, cost_priv){
  number_of_runs <- max(as.numeric(avg_u$run_number))
  averages <<- avg_u %>% group_by(time_series) %>% 
    summarise(u_inc = mean(mean_u_inc), u_ec = mean(mean_u_ec), u_soc = mean(mean_u_soc),
              u_cap = mean(mean_u_cap),
              u_tot = mean(mean_u_tot), avg_inst_cap = mean(avg_inst_cap, na.rm = TRUE),  
              sd_u_inc = sqrt(sum(sd_u_inc^2))/number_of_runs,
              sd_u_ec = sqrt(sum(sd_u_ec^2))/number_of_runs,
              sd_u_soc = sqrt(sum(sd_u_soc^2))/number_of_runs,
              sd_u_cap = sqrt(sum(sd_u_cap^2))/number_of_runs,
              sd_u_tot = sqrt(sum(sd_u_tot^2))/number_of_runs,
              tot_inst_cap = mean(tot_inst_cap, na.rm = TRUE), 
              frac_of_adopters = mean(frac_of_adopters, na.rm = TRUE),
              inst_cap_diff = mean(inst_cap_diff, na.rm = TRUE),
              frac_dec1  = mean(frac_dec1,  na.rm = TRUE),            #shuusei20251118
              frac_dec2  = mean(frac_dec2,  na.rm = TRUE),            #shuusei20251118
              frac_dec3  = mean(frac_dec3,  na.rm = TRUE),            #shuusei20251118
              frac_dec4  = mean(frac_dec4,  na.rm = TRUE),            #shuusei20251118
              frac_dec5  = mean(frac_dec5,  na.rm = TRUE),            #shuusei20251118
              frac_dec6  = mean(frac_dec6,  na.rm = TRUE),            #shuusei20251118
              frac_dec7  = mean(frac_dec7,  na.rm = TRUE),            #shuusei20251118
              frac_dec8  = mean(frac_dec8,  na.rm = TRUE),            #shuusei20251118
              frac_dec9  = mean(frac_dec9,  na.rm = TRUE),            #shuusei20251118
              frac_dec10 = mean(frac_dec10, na.rm = TRUE),            #shuusei20251118
              cap_dec1   = mean(cap_dec1,   na.rm = TRUE),   #shuusei20251121
              cap_dec2   = mean(cap_dec2,   na.rm = TRUE),   #shuusei20251121
              cap_dec3   = mean(cap_dec3,   na.rm = TRUE),   #shuusei20251121
              cap_dec4   = mean(cap_dec4,   na.rm = TRUE),   #shuusei20251121
              cap_dec5   = mean(cap_dec5,   na.rm = TRUE),   #shuusei20251121
              cap_dec6   = mean(cap_dec6,   na.rm = TRUE),   #shuusei20251121
              cap_dec7   = mean(cap_dec7,   na.rm = TRUE),   #shuusei20251121
              cap_dec8   = mean(cap_dec8,   na.rm = TRUE),   #shuusei20251121
              cap_dec9   = mean(cap_dec9,   na.rm = TRUE),   #shuusei20251121
              cap_dec10  = mean(cap_dec10,  na.rm = TRUE),   #shuusei20251121
              ## ここから新規：デシル別シェアのラン平均                 #shuusei202511288
              budget_dec1  = mean(budget_dec1,  na.rm = TRUE), #shuusei202511288
              budget_dec2  = mean(budget_dec2,  na.rm = TRUE), #shuusei202511288
              budget_dec3  = mean(budget_dec3,  na.rm = TRUE), #shuusei202511288
              budget_dec4  = mean(budget_dec4,  na.rm = TRUE), #shuusei202511288
              budget_dec5  = mean(budget_dec5,  na.rm = TRUE), #shuusei202511288
              budget_dec6  = mean(budget_dec6,  na.rm = TRUE), #shuusei202511288
              budget_dec7  = mean(budget_dec7,  na.rm = TRUE), #shuusei202511288
              budget_dec8  = mean(budget_dec8,  na.rm = TRUE), #shuusei202511288
              budget_dec9  = mean(budget_dec9,  na.rm = TRUE), #shuusei202511288
              budget_dec10 = mean(budget_dec10, na.rm = TRUE), #shuusei202511288
              roof_dec1    = mean(roof_dec1,    na.rm = TRUE), #shuusei202511288
              roof_dec2    = mean(roof_dec2,    na.rm = TRUE), #shuusei202511288
              roof_dec3    = mean(roof_dec3,    na.rm = TRUE), #shuusei202511288
              roof_dec4    = mean(roof_dec4,    na.rm = TRUE), #shuusei202511288
              roof_dec5    = mean(roof_dec5,    na.rm = TRUE), #shuusei202511288
              roof_dec6    = mean(roof_dec6,    na.rm = TRUE), #shuusei202511288
              roof_dec7    = mean(roof_dec7,    na.rm = TRUE), #shuusei202511288
              roof_dec8    = mean(roof_dec8,    na.rm = TRUE), #shuusei202511288
              roof_dec9    = mean(roof_dec9,    na.rm = TRUE), #shuusei202511288
              roof_dec10   = mean(roof_dec10,   na.rm = TRUE), #shuusei202511288
              share_budget_dec1  = mean(share_budget_dec1,  na.rm = TRUE), #shuusei202511288
              share_budget_dec2  = mean(share_budget_dec2,  na.rm = TRUE), #shuusei202511288
              share_budget_dec3  = mean(share_budget_dec3,  na.rm = TRUE), #shuusei202511288
              share_budget_dec4  = mean(share_budget_dec4,  na.rm = TRUE), #shuusei202511288
              share_budget_dec5  = mean(share_budget_dec5,  na.rm = TRUE), #shuusei202511288
              share_budget_dec6  = mean(share_budget_dec6,  na.rm = TRUE), #shuusei202511288
              share_budget_dec7  = mean(share_budget_dec7,  na.rm = TRUE), #shuusei202511288
              share_budget_dec8  = mean(share_budget_dec8,  na.rm = TRUE), #shuusei202511288
              share_budget_dec9  = mean(share_budget_dec9,  na.rm = TRUE), #shuusei202511288
              share_budget_dec10 = mean(share_budget_dec10, na.rm = TRUE), #shuusei202511288
              share_roof_dec1    = mean(share_roof_dec1,    na.rm = TRUE), #shuusei202511288
              share_roof_dec2    = mean(share_roof_dec2,    na.rm = TRUE), #shuusei202511288
              share_roof_dec3    = mean(share_roof_dec3,    na.rm = TRUE), #shuusei202511288
              share_roof_dec4    = mean(share_roof_dec4,    na.rm = TRUE), #shuusei202511288
              share_roof_dec5    = mean(share_roof_dec5,    na.rm = TRUE), #shuusei202511288
              share_roof_dec6    = mean(share_roof_dec6,    na.rm = TRUE), #shuusei202511288
              share_roof_dec7    = mean(share_roof_dec7,    na.rm = TRUE), #shuusei202511288
              share_roof_dec8    = mean(share_roof_dec8,    na.rm = TRUE), #shuusei202511288
              share_roof_dec9    = mean(share_roof_dec9,    na.rm = TRUE), #shuusei202511288
              share_roof_dec10   = mean(share_roof_dec10,   na.rm = TRUE), #shuusei202511288
              share_4kw_dec1     = mean(share_4kw_dec1,     na.rm = TRUE), #shuusei202511288
              share_4kw_dec2     = mean(share_4kw_dec2,     na.rm = TRUE), #shuusei202511288
              share_4kw_dec3     = mean(share_4kw_dec3,     na.rm = TRUE), #shuusei202511288
              share_4kw_dec4     = mean(share_4kw_dec4,     na.rm = TRUE), #shuusei202511288
              share_4kw_dec5     = mean(share_4kw_dec5,     na.rm = TRUE), #shuusei202511288
              share_4kw_dec6     = mean(share_4kw_dec6,     na.rm = TRUE), #shuusei202511288
              share_4kw_dec7     = mean(share_4kw_dec7,     na.rm = TRUE), #shuusei202511288
              share_4kw_dec8     = mean(share_4kw_dec8,     na.rm = TRUE), #shuusei202511288
              share_4kw_dec9     = mean(share_4kw_dec9,     na.rm = TRUE), #shuusei202511288
              share_4kw_dec10    = mean(share_4kw_dec10,    na.rm = TRUE), #shuusei202511288
              share_large_dec1   = mean(share_large_dec1,   na.rm = TRUE), #shuusei202511288
              share_large_dec2   = mean(share_large_dec2,   na.rm = TRUE), #shuusei202511288
              share_large_dec3   = mean(share_large_dec3,   na.rm = TRUE), #shuusei202511288
              share_large_dec4   = mean(share_large_dec4,   na.rm = TRUE), #shuusei202511288
              share_large_dec5   = mean(share_large_dec5,   na.rm = TRUE), #shuusei202511288
              share_large_dec6   = mean(share_large_dec6,   na.rm = TRUE), #shuusei202511288
              share_large_dec7   = mean(share_large_dec7,   na.rm = TRUE), #shuusei202511288
              share_large_dec8   = mean(share_large_dec8,   na.rm = TRUE), #shuusei202511288
              share_large_dec9   = mean(share_large_dec9,   na.rm = TRUE), #shuusei202511288
              share_large_dec10  = mean(share_large_dec10,  na.rm = TRUE)  #shuusei202511288
    )
  
  
  avg_cost <<- cost %>% group_by(time_series) %>% summarise(annual_cost = mean(annual_cost))
  
  avg_cost_priv <<- cost_priv %>% group_by(time_series) %>% summarise(cum_cost = mean(cum_cost))
}


# デシル別容量（cap_dec1〜10）から SES 五分位別容量を計算       #shuusei20251121
calc_quintile_cap <- function(cap_dec_vec) {                            #shuusei20251121
  stopifnot(length(cap_dec_vec) == 10)                                  #shuusei20251121
  q_caps <- c(                                                          #shuusei20251121
    Q1 = sum(cap_dec_vec[1:2],  na.rm = TRUE),                          #shuusei20251121
    Q2 = sum(cap_dec_vec[3:4],  na.rm = TRUE),                          #shuusei20251121
    Q3 = sum(cap_dec_vec[5:6],  na.rm = TRUE),                          #shuusei20251121
    Q4 = sum(cap_dec_vec[7:8],  na.rm = TRUE),                          #shuusei20251121
    Q5 = sum(cap_dec_vec[9:10], na.rm = TRUE)                           #shuusei20251121
  )                                                                     #shuusei20251121
  return(q_caps)                                                        #shuusei20251121
}                                                                       #shuusei20251121


# クインタイル別に inst_cap_budget / roof_limit の平均をプロットする関数     #shuusei202511288
plot_budget_roof_by_quintile <- function(cutoff_date = dmy("01oct2015")) { #shuusei202511288
  if (!exists("averages")) {                                              #shuusei202511288
    stop("averages が見つかりません。batch_run_func() または load_plot_sim_data() 実行後に呼んでください。") #shuusei202511288
  }                                                                       #shuusei202511288
  
  row_q <- which(averages$time_series == cutoff_date)                    #shuusei202511288
  if (length(row_q) != 1) {                                               #shuusei202511288
    stop("cutoff_date が averages$time_series に一意に見つかりません。") #shuusei202511288
  }                                                                       #shuusei202511288
  
  decs <- 1:10                                                            #shuusei202511288
  bud_dec  <- as.numeric(averages[row_q, paste0("budget_dec", decs)])     #shuusei202511288
  roof_dec <- as.numeric(averages[row_q, paste0("roof_dec",   decs)])     #shuusei202511288
  frac_dec <- as.numeric(averages[row_q, paste0("frac_dec",   decs)])     #shuusei202511288
  
  ## デシル → クインタイル（D1-2→Q1, D3-4→Q2, ...）                  #shuusei202511288
  dec_to_Q <- list(Q1 = 1:2, Q2 = 3:4, Q3 = 5:6, Q4 = 7:8, Q5 = 9:10)    #shuusei202511288
  
  ## 導入率 frac_dec を重みとして「導入世帯ベースの平均」をとる         #shuusei202511288
  make_Q_mean <- function(x_dec) {                                       #shuusei202511288
    sapply(dec_to_Q, function(idx) {                                     #shuusei202511288
      x <- x_dec[idx]                                                    #shuusei202511288
      w <- frac_dec[idx]                                                 #shuusei202511288
      if (all(is.na(x)) || all(is.na(w)) || sum(w, na.rm = TRUE) == 0) { #shuusei202511288
        return(NA_real_)                                                 #shuusei202511288
      }                                                                  #shuusei202511288
      sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)                    #shuusei202511288
    })                                                                   #shuusei202511288
  }                                                                      #shuusei202511288
  
  bud_Q  <- make_Q_mean(bud_dec)                                         #shuusei202511288
  roof_Q <- make_Q_mean(roof_dec)                                        #shuusei202511288
  
  quintiles <- factor(names(bud_Q), levels = paste0("Q", 1:5))           #shuusei202511288
  
  df_Q <- tibble(                                                        #shuusei20251130
    quintile = rep(quintiles, times = 2),                                #shuusei20251130
    type     = rep(c("inst_cap_budget", "meet_demand_limit"), each = 5), #shuusei20251130
    value_kW = c(bud_Q, roof_Q)                                          #shuusei20251130
  )                                                                       #shuusei20251130
  
  p <- ggplot(df_Q, aes(x = quintile, y = value_kW, fill = type)) +      #shuusei20251130
    geom_col(position = "dodge") +                                       #shuusei20251130
    ylab("Average capacity per adopter (kW)") +                          #shuusei20251130
    xlab("SES quintile") +                                               #shuusei20251130
    ggtitle(paste0("inst_cap_budget vs meet_demand limit by SES quintile at ",
                   format(cutoff_date, "%Y-%m-%d"))) +                   #shuusei20251130
    theme_bw()                                                           #shuusei20251130
  
  print(p)                                                               #shuusei202511288
}                                                                        #shuusei202511288

# inst_cap の制約要因と 4kW vs 大容量の選択を SES 五分位別に可視化する関数  #shuusei202511288
plot_cap_constraints_by_quintile <- function(cutoff_date = dmy("01oct2015")) { #shuusei202511288
  if (!exists("averages")) {                                           #shuusei202511288
    stop("averages が見つかりません。batch_run_func() または load_plot_sim_data() 実行後に呼んでください。") #shuusei202511288
  }                                                                    #shuusei202511288
  
  row_q <- which(averages$time_series == cutoff_date)                 #shuusei202511288
  if (length(row_q) != 1) {                                            #shuusei202511288
    stop("cutoff_date が averages$time_series に一意に見つかりません。") #shuusei202511288
  }                                                                    #shuusei202511288
  
  decs <- 1:10                                                         #shuusei202511288
  share_budget_dec <- as.numeric(averages[row_q,                       #shuusei202511288
                                          paste0("share_budget_dec",decs)]) #shuusei202511288
  share_roof_dec   <- as.numeric(averages[row_q,                       #shuusei202511288
                                          paste0("share_roof_dec",decs)])   #shuusei202511288
  share_4kw_dec    <- as.numeric(averages[row_q,                       #shuusei202511288
                                          paste0("share_4kw_dec",decs)])    #shuusei202511288
  share_large_dec  <- as.numeric(averages[row_q,                       #shuusei202511288
                                          paste0("share_large_dec",decs)])  #shuusei202511288
  frac_dec         <- as.numeric(averages[row_q,                       #shuusei202511288
                                          paste0("frac_dec",decs)])        #shuusei202511288
  
  ## decile → quintile （デシル内の導入率で重み付け平均）            #shuusei202511288
  dec_to_Q <- list(Q1 = 1:2, Q2 = 3:4, Q3 = 5:6, Q4 = 7:8, Q5 = 9:10) #shuusei202511288
  w <- frac_dec                                                       #shuusei202511288
  
  make_Q <- function(x_dec) {                                         #shuusei202511288
    sapply(dec_to_Q, function(idx) {                                  #shuusei202511288
      ww <- w[idx]                                                    #shuusei202511288
      xx <- x_dec[idx]                                                #shuusei202511288
      if (all(is.na(xx)) || all(is.na(ww)) || sum(ww, na.rm = TRUE) == 0) { #shuusei202511288
        return(NA_real_)                                              #shuusei202511288
      }                                                                #shuusei202511288
      sum(xx * ww, na.rm = TRUE) / sum(ww, na.rm = TRUE)              #shuusei202511288
    })                                                                #shuusei202511288
  }                                                                   #shuusei202511288
  
  bud_Q   <- make_Q(share_budget_dec)                                 #shuusei202511288
  roof_Q  <- make_Q(share_roof_dec)                                   #shuusei202511288
  four_Q  <- make_Q(share_4kw_dec)                                    #shuusei202511288
  large_Q <- make_Q(share_large_dec)                                  #shuusei202511288
  
  quintiles <- factor(names(bud_Q), levels = paste0("Q",1:5))         #shuusei202511288
  
  ## inst_cap_budget vs meet_demand limit のグラフ（クインタイル別）   #shuusei20251130
  df_limit <- tibble(                                                 #shuusei20251130
    quintile = rep(quintiles, times = 2),                             #shuusei20251130
    source   = rep(c("Budget limit","Meet-demand limit"), each = 5),  #shuusei20251130
    share    = c(bud_Q, roof_Q)                                       #shuusei20251130
  )                                                                   #shuusei20251130
  
  p1 <- ggplot(df_limit, aes(x = quintile, y = share, fill = source)) + #shuusei20251130
    geom_col(position = "dodge") +                                    #shuusei20251130
    ylim(0, 1) +                                                      #shuusei20251130
    ylab("Share of adopters (within quintile)") +                     #shuusei20251130
    xlab("SES quintile") +                                            #shuusei20251130
    ggtitle("inst_cap_budget vs meet_demand limit by SES quintile") + #shuusei20251130
    theme_bw()                                                        #shuusei20251130
  
  print(p1)                                                           #shuusei202511288
  
  ## 4kW vs 大容量（inst_cap_raw > 4kW の世帯のみ）                    #shuusei202511288
  df_choice <- tibble(                                                #shuusei202511288
    quintile = rep(quintiles, times = 2),                             #shuusei202511288
    choice   = rep(c("Choose 4 kW","Choose >4 kW"), each = 5),        #shuusei202511288
    share    = c(four_Q, large_Q)                                     #shuusei202511288
  )                                                                   #shuusei202511288
  
  p2 <- ggplot(df_choice, aes(x = quintile, y = share, fill = choice)) + #shuusei202511288
    geom_col(position = "dodge") +                                    #shuusei202511288
    ylim(0, 1) +                                                      #shuusei202511288
    ylab("Share among households with raw inst_cap > 4 kW") +         #shuusei202511288
    xlab("SES quintile") +                                            #shuusei202511288
    ggtitle("4 kW vs large-capacity choice by SES quintile") +        #shuusei202511288
    theme_bw()                                                        #shuusei202511288
  print(p2)                                                           #shuusei202511288
}                                                                     #shuusei202511288


load_plot_sim_data <- function(save_name, plot_u = T, plot_cost = T, plot_prod = T){
  load_use_cap <- F
  avg_u <<- read_rds(paste(save_name, "_avg_u.rds", sep = ""))
  cost <<- read_rds(paste(save_name, "_cost.rds", sep = ""))
  cost_priv <<- read_rds(paste(save_name, "_cost_priv.rds", sep = ""))
  LCOE_data <<- read_rds(paste(save_name, "_LCOE_data.rds", sep = ""))
  LCOE_avg <<- read_rds(paste(save_name, "_LCOE_avg.rds", sep = ""))
  FiT <<- read_rds(paste(save_name, "_FiT.rds", sep = ""))
  number_of_runs <- max(as.numeric(avg_u$run_number))
  if (file.exists(paste(save_name, "_FiT_levels.rds", sep = ""))){
    FiT_levels <<- read_rds(paste(save_name, "_FiT_levels.rds", sep = ""))
    load_use_cap <- T
  }
  
  # calculate averages of all runs
  
  try(past <- avg_u$inst_cap_diff)
  if (!is.null(past)) {
    summarise_results(avg_u, cost, cost_priv) 
    sum_abs_diff <- sum(abs(averages$inst_cap_diff))
  } else {
    future <- T
    summarise_results_f(avg_u, cost, cost_priv) 
  }
  # deviation from real data: sum of absolute values
  # of deviation from installed FiT capacity < 10 kW
  
  tot_subs_cost <- sum(avg_cost$annual_cost)/12
  tot_priv_cost <- tail(avg_cost_priv$cum_cost, 1)
  tot_overall_cost <- tot_subs_cost + tot_priv_cost
  #  overall_tot_cost_mean <- mean(overall_tot_cost)/1e9 # in billions
  #  overall_tot_cost_sd <- sd(overall_tot_cost)/1e9 # in billions
  
  prod_res <- calc_prod(LCOE_data, number_of_runs) # calculate total production from all installations at each date
  cum_prod <<- prod_res[[1]]
  cum_prod_avg <<- prod_res[[2]]
  
  
  if(plot_u == T){
    u_vars <- c("inc", "soc", "ec", "cap", "tot") # partial and total utilities
    yl <- c(expression(u[inc]), expression(u[soc]), expression(u[ec]), expression(u[cap]), expression(u[tot]))
    l <- 1
    for (app in u_vars) { # plot average over time of utility functions
      
      p <- ggplot() + theme_bw() +
        geom_line(data = avg_u,
                  aes(x = time_series, y = get(paste("mean_u_", app, sep = "")), 
                      group = run_number), alpha = 0.2) +
        geom_line(data = averages, aes(x = time_series, y = get(paste("u_", app, sep = ""))), size = 1) +
        geom_ribbon(data = averages, aes(x = time_series, 
                                         ymin = get(paste("u_", app, sep = "")) - get(paste("sd_u_", app, sep = "")),
                                         ymax = get(paste("u_", app, sep = "")) + get(paste("sd_u_", app, sep = ""))),
                    alpha = 0.15) +
        ylab(yl[l]) + xlab("Date") + theme(legend.position = "none") + coord_cartesian(expand = F) 
      print(p)
      l <- l+1
    }
  }
  
  print(ggplot() + theme_bw() + 
          geom_line(data = avg_u, 
                    aes(x = time_series, y = avg_inst_cap, group = run_number), alpha = 0.2) +
          geom_line(data = averages, aes(x = time_series, y = avg_inst_cap, color = "Modelled"), size = 1) +
          geom_line(data = deployment, aes(x = time_series, y = avg_cap, color = "Real"), size = 1) + ylim(0, 4) +
          ylab("Average installed capacity (kW)") + xlab("Date") +
          scale_colour_manual(name = "", values = c(Modelled = "black", Real = "blue")))
  
  
  print(ggplot() + theme_bw() +                                        #shuusei202511288
          geom_line(data = deployment, aes(x = time_series, y = real_cap, color = "Real"), size = 1) +  #shuusei202511288
          geom_line(data = avg_u, aes(x = time_series,                 #shuusei202511288
                                      y = tot_inst_cap, group = run_number), alpha = 0.2)+  #shuusei202511288
          geom_line(data = averages, aes(x = time_series, y = tot_inst_cap, color = "Modelled"), size = 1) + #shuusei202511288
          ylab("Cumulative capacity (MW)") + xlab("Date") +            #shuusei202511288
          scale_colour_manual(name = "", values = c(Modelled = "black", Real = "red")) + #shuusei202511288
          theme(legend.position = c(0.2, 0.75)) + scale_x_date(expand = c(0,0)) +  #shuusei202511288
          scale_y_continuous(expand = c(0,0), limits = c(0, max(avg_u$tot_inst_cap) + 100)) #shuusei202511288
  )                                                                    #shuusei202511288
  
  # SES 五分位別「累積導入容量（MW）」の推移（保存済み結果）        #shuusei202511288
  cap_dec_vars <- paste0("cap_dec", 1:10)                               #shuusei202511288
  if (all(cap_dec_vars %in% names(averages))) {                         #shuusei202511288
    ## 各時点ごとの cap_dec1〜10 から Q1〜Q5 を計算                  #shuusei202511288
    cap_mat   <- as.matrix(averages[, cap_dec_vars])                    #shuusei202511288
    cap_Q_mat <- t(apply(cap_mat, 1, calc_quintile_cap))                #shuusei202511288
    
    cap_Q_df <- cbind(                                                  #shuusei202511288
      time_series = averages$time_series,                               #shuusei202511288
      as.data.frame(cap_Q_mat)                                          #shuusei202511288
    )                                                                   #shuusei202511288
    
    cap_Q_long <- cap_Q_df %>%                                          #shuusei202511288
      pivot_longer(cols = starts_with("Q"),                             #shuusei202511288
                   names_to = "quintile",                               #shuusei202511288
                   values_to = "cap_MW")                                #shuusei202511288
    
    print(ggplot(cap_Q_long) + theme_bw() +                             #shuusei202511288
            geom_line(aes(x = time_series, y = cap_MW,                  #shuusei202511288
                          color = quintile)) +                          #shuusei202511288
            ylab("Cumulative capacity by SES quintile (MW)") +          #shuusei202511288
            xlab("Date"))                                               #shuusei202511288
  }                                                                     #shuusei202511288
  
  if (plot_cost == T){
    print(ggplot() + theme_bw() + 
            geom_line(data = cost, aes(x = time_series, y = annual_cost/1e6, group = run_number), alpha = 0.2)+
            geom_line(data = avg_cost, aes(x = time_series, y = annual_cost/1e6), color = "black", size = 1)+
            ylab("Annual cost (millions £)") + xlab("Date"))
    print(ggplot() + theme_bw() +
            geom_line(data = cost_priv, aes(x = time_series, y = cum_cost/1e6, group = run_number), alpha = 0.2)+
            geom_line(data = avg_cost_priv, aes(x = time_series, y = cum_cost/1e6), color = "black", size = 1)+
            ylab("Cumulative private cost (millions £)") + xlab("Date"))
    
    print(ggplot(LCOE_data) + theme_bw() + geom_point(aes(x=adopt_date, y = LCOE_ind, 
                                                          group = run_number), alpha = 0.1)+
            ylab("LCOE (£/MWh)") + xlab("Date"))
    
  }
  
  if (plot_prod == T){
    print(ggplot() + theme_bw() + 
            geom_line(data = cum_prod, aes(x = time_series, y = current_prod/1e6, group = run_number), alpha = 0.2) +
            geom_line(data = cum_prod_avg, aes(x=time_series, y = current_prod/1e6), size = 1) +
            ylab("Annual production (MWh/yr)") + xlab("Date"))
  }
  
  if (load_use_cap == T) {
    avg_FiT <- FiT_levels %>% group_by(time_series) %>% summarise(FiT = mean(FiT))
    print(ggplot() + geom_line(data = FiT_levels, aes(x = time_series, y = FiT, group = run_number,
                                                      color = run_number)) +
            geom_line(data = avg_FiT, aes(x=time_series, y = FiT), size = 1))
  }
  
  
  dep_sd <- sd(avg_u$tot_inst_cap[avg_u$time_series == max(avg_u$time_series)])
  subs_sd <- cost %>% group_by(run_number) %>% summarise(subs = sum(annual_cost)/12) %>% 
    select(subs) %>% unlist %>% sd
  priv_sd <- cost_priv %>% group_by(run_number) %>% summarise(priv = max(cum_cost)) %>% 
    select(priv) %>% unlist %>% sd
  ann_sd <- cost %>% group_by(run_number) %>% summarise(ann = max(annual_cost)) %>%
    select(ann) %>% unlist %>% sd 
  prod_sd <- cum_prod %>% group_by(run_number) %>% summarise(prod = sum(current_prod)/12) %>%
    select(prod) %>% unlist %>% sd
  
  
  cat("Final deployment at ", as.character(tail(averages$time_series, 1)), " (MW) = ", 
      tail(averages$tot_inst_cap, 1), " +/- ", dep_sd, "\n",
      "Total subsidy cost (billions £) = ", tot_subs_cost/1e9, " +/- ", subs_sd/1e9, "\n",
      "Total private cost (billion £) = ", tot_priv_cost/1e9, " +/- ", priv_sd/1e9, "\n",
      "Total cost (billions £) = ", tot_overall_cost/1e9, "\n",
      "Maximum annual cost (millions £) = ", max(avg_cost$annual_cost)/1e6, " +/- ", ann_sd/1e6, "\n",
      "Total production (GWh) = ", sum(cum_prod_avg$current_prod)/(12*1e6), " +/- ", prod_sd/1e6, "\n",
      "Weighted LCOE (£/MWh) = ", mean(LCOE_avg), "\n",
      sep = "")
  
  
}


##################################################################################
######################## 2. Future scenarios - functions #########################
##################################################################################
# 2.1 Set-up 
# 2.2 Agent/modelling functions
# 2.3 Cost calculation
# 2.4 Processing

#------------------------------- 2.1 Set-up -------------------------------------#


load_data_f <- function(start_date, end_date, FiT_end_date, FiT_type, red_frac, init_fit, final_fit, exp_tar,
                        elec_trend, PV_trend,
                        dep_caps = F, cap){
  
  # end_date: date up to which simulation will run
  # FiT_type: real_h, linear, perc_red, ann_perc_red
  if(missing(start_date)) start_date <- "1oct2016"
  if(missing(end_date)) end_date <- "31dec2021"
  if(missing(FiT_end_date)) FiT_end_date <- end_date
  if(missing(FiT_type)) FiT_type <- "real_f"
  if(missing(red_frac)) red_frac <- 0.03
  if(missing(init_fit)) init_fit <- 5
  if(missing(final_fit)) final_fit <- 0
  if(missing(exp_tar)) exp_tar <- 4.91
  if(missing(elec_trend)) elec_trend <- "mid"
  if(missing(PV_trend)) PV_trend <- 0.07
  if(missing(cap)) cap <- 50
  
  load_libraries()
  # DO NOT USE plyr LIBRARY 
  
  if(FiT_type == "real_f"){
    start_date <- "1oct2016"
    FiT_end_date <- "1mar2019"
    if (end_date < FiT_end_date) end_date <- FiT_end_date
    dep_caps <- T
  }
  
  if(FiT_type == "real_f_ext"){
    start_date <- "1oct2016"
    FiT_end_date <- "1mar2021"
    if (end_date < FiT_end_date) end_date <- FiT_end_date
    dep_caps <- T
  }
  
  FiT_end_date <- dmy(FiT_end_date)
  end_date <- dmy(end_date)
  start_date <- dmy(start_date)
  
  time_months <<- seq(start_date, end_date, by = '1 month')
  
  time_years <<- year(start_date):year(end_date)
  
  
  if(!is.Date(end_date)) stop("Your end date is not a valid date")
  if(!(FiT_type == "real_f" | FiT_type == "perc_red" | FiT_type == "ann_perc_red" | 
       FiT_type == "linear" | FiT_type == "dep_cap" | FiT_type == "real_f_ext")) stop("FiT type not recognised")
  if(init_fit < 0 | final_fit < 0) stop("FiTs have to be positive")
  if(!(elec_trend == "mid" | elec_trend == "low" | elec_trend == "high")) stop("Electricity price trend not recognised")
  
  
  #---------------------------------------------------------#
  
  elec_price_time_raw <- read_csv("Data/electricityprices.csv",        #shuusei20251211
                                  col_names = F, col_types = cols())   #shuusei20251211
  ## 2010–2023 の実データから、time_years（シミュレーション対象年）だけ切り出す。 #shuusei20251211
  elec_price_time <<- future_elec_price(elec_price_time_raw,           #shuusei20251211
                                        elec_trend)                    #shuusei20251211
  
  owner_occupiers <- read_csv("Data/owner_occupiers.csv", col_names = F, col_types = cols()) %>% mutate(X2 = X2*1000)
  
  owner_occupiers <<- data.frame(X1 = time_years, 
                                 X2 = owner_occupiers$X2[[7]])
  #---------------------------------------------------------#
  # Load factor data
  
  LF <- read_csv("Data/LF_mean.csv", col_types = cols())
  
  # Filter out empty rows, group by region, find mean & std. dev. over all years, arrange
  # in alphabetical order.
  LF <<- LF %>% filter(!is.na(Region)) %>% group_by(Region) %>% 
    summarise(LF = mean(Weighted.mean), std_dev = sd(Weighted.mean)) %>% 
    arrange(Region) %>% mutate(Label = LETTERS[1:11])
  
  
  #---------------------------------------------------------#
  # Feed-in tariff data
  
  set_FiT_f(start_date, end_date, FiT_end_date, FiT_type, red_frac, init_fit, final_fit, exp_tar)
  
  #---------------------------------------------------------#
  # Deployment caps
  run_w_cap <<- F
  if (dep_caps == T) {
    set_dep_caps(start_date, end_date, FiT_end_date, FiT_type, cap, exp_tar)
    dep_cap_0 <<- dep_cap
    FiT_0 <<- FiT
    run_w_cap <<- T
  }
  #---------------------------------------------------------#
  # Population data
  
  population <- read_csv("Data/population_mid2012.csv", col_names = FALSE, col_types = cols()) %>% arrange(X1)
  
  # 旧: 全人口で region_weights を作っていた行はコメントアウトか退避   #shuusei20251205
  # region_weights <<- population$X2/sum(population$X2)
  
  if (!exists("tenure_region_counts", inherits = TRUE)) {               #shuusei20251205
    init_tenure_region_counts()                                         #shuusei20251205
  }                                                                      #shuusei20251205
  owner_region_weights <- tenure_region_counts$own /                    #shuusei20251205
    sum(tenure_region_counts$own)                                       #shuusei20251205
  region_weights <<- owner_region_weights                                #shuusei20251205
  
  rm(population)                                                         #shuusei20251205
  
  #---------------------------------------------------------#
  # PV cost data
  init_PV <- read_csv('Data/PV_cost_data_est.csv', col_names = FALSE, col_types = cols()) %>% 
    mutate(X1 = dmy(X1))                                                #shuusei20251212
  
  if (ncol(init_PV) < 3) {                                              #shuusei20251212
    stop("PV_cost_data_est.csv must have 3 columns: time_series,fixed,marginal") #shuusei20251212
  }                                                                      #shuusei20251212
  
  kW_price <<- future_PV_price(init_PV, PV_trend, start_date, end_date)
  
  # 未来ランでは「過去に導入済み」の adopters も混ざるので、過去年月も含む表をキャッシュ #shuusei20251212
  kW_price_full <<- bind_rows(                                          #shuusei20251212
    init_PV %>% filter(X1 < start_date) %>% select(X1, X2, X3),         #shuusei20251212
    kW_price %>% select(X1, X2, X3)                                     #shuusei20251212
  ) %>% distinct(X1, .keep_all = TRUE) %>% arrange(X1)                  #shuusei20251212
  
  #---------------------------------------------------------#
  # Electricity use data
  
  means <- read_csv("Data/mean-electricity.csv", col_types = cols())
  medians <- read_csv("Data/median-electricity.csv", col_types = cols())
  
  mus <<- data.frame(matrix(ncol = 5, nrow = 10))
  sigmas <<- data.frame(matrix(ncol = 5, nrow = 10))
  
  for (i in 1:5){
    mean <- means[[i+1]]
    median <- medians[[i+1]]
    mus[[i]] <<- log(median) 
    sigmas[[i]] <<- sqrt(2*log(mean/median))
  }
  
  income_thresh <<- means$income
  
  rm(mean, median, i, means, medians)
  
  #---------------------------------------------------------#
  # Real deployment data
  if(exists("deployment")) {
    cat("\nDeployment data is already loaded - if you want to reload it, delete the 'deployment' variable\n")
  } else {
    
    ts <- seq(dmy("01jan2010"), dmy("1nov2016"), by = '1 month')
    
    all_inst_cap <- process_inst_data() %>% 
      filter(technology_type == "Photovoltaic", installed_capacity <= 10, installationtype == "Domestic")
    current_cap <- vector(length = length(ts))
    avg_cap <- vector(length = length(ts))
    for (i in 1:length(ts)) {
      date_now <- ts[i] + months(1)
      installed_now <- filter(all_inst_cap, commissioned_date < date_now)
      current_cap[i] <- sum(installed_now$installed_capacity)
      avg_cap[i] <- current_cap[i]/nrow(installed_now)
    }
    
    
    deployment <<- data.frame(time_series = dmy("01feb2010") + months(0:(length(ts)-1)), 
                              real_cap = current_cap/1000, avg_cap = avg_cap)
    rm(all_inst_cap, installed_now, current_cap, date_now)
  }
  #---------------------------------------------------------#
  
  
}

future_PV_price <- function(init_PV, x, start_date, end_date) {
  # x is annual percentage reduction
  
  no_years <- length(year(start_date):year(end_date)) + 1
  
  year_fixed <- vector(length = no_years)                               #shuusei20251212
  year_marg  <- vector(length = no_years)                               #shuusei20251212
  
  year_fixed[1] <- init_PV %>% filter(X1 == start_date) %>% select(X2) %>% unlist %>% unname #shuusei20251212
  year_marg[1]  <- init_PV %>% filter(X1 == start_date) %>% select(X3) %>% unlist %>% unname #shuusei20251212
  
  for (i in 1:(no_years-1)) {                                           #shuusei20251212
    year_fixed[i+1] <- year_fixed[i] - x*year_fixed[i]                  #shuusei20251212
    year_marg[i+1]  <- year_marg[i]  - x*year_marg[i]                   #shuusei20251212
  }                                                                     #shuusei20251212
  
  monthly_fixed <- approx(x = 1:no_years, y = year_fixed,
                          method = "linear", n = (no_years-1)*12 + 1)   #shuusei20251212
  monthly_marg  <- approx(x = 1:no_years, y = year_marg,
                          method = "linear", n = (no_years-1)*12 + 1)   #shuusei20251212
  
  monthly_cost <- data.frame(
    X1 = seq(start_date, length.out = length(monthly_fixed$y), by = '1 month'),
    X2 = monthly_fixed$y,                                               #shuusei20251212
    X3 = monthly_marg$y                                                 #shuusei20251212
  )                                                                     #shuusei20251212
  
  monthly_cost %<>% filter(X1 >= start_date, X1 <= end_date)
}


future_elec_price <- function(elec_price_time, x){                     #shuusei20251211
  ## electricityprices.csv: X1 = 年(2010–2023), X2 = 年平均単価(p/kWh)   #shuusei20251211
  ## time_years: load_data_f() 内で定義された「シミュレーション対象の年」   #shuusei20251211
  
  elec_price_time <- elec_price_time %>% arrange(X1)                   #shuusei20251211
  last_actual      <- max(elec_price_time$X1, na.rm = TRUE)            #shuusei20251211
  
  ## 1) シミュレーション期間が実データの範囲内（〜last_actual, 想定:2023）なら
  ##    high/mid/low シナリオは無視して「実データだけ」返す。               #shuusei20251211
  if (max(time_years) <= last_actual) {                                #shuusei20251211
    elec_sub <- elec_price_time %>%                                    #shuusei20251211
      filter(X1 %in% time_years)                                       #shuusei20251211
    elec_sub <- elec_sub[match(time_years, elec_sub$X1), ]             #shuusei20251211
    return(elec_sub)                                                   #shuusei20251211
  }                                                                    #shuusei20251211
  
  ## 2) もし end_date を 2023 年より先に延ばした場合だけ、
  ##    実データを基に従来通りの linear high/low/mid シナリオを使う。      #shuusei20251211
  
  # 全実データで単回帰直線をフィット                                   #shuusei20251211
  fit_lm <- lm(X2 ~ X1, data = elec_price_time)                        #shuusei20251211
  
  future_price_high <- predict(fit_lm,                                 #shuusei20251211
                               newdata = data.frame(X1 = time_years))  #shuusei20251211
  
  last_two <- tail(elec_price_time$X2, 2)                              #shuusei20251211
  future_price_low <- rep(mean(last_two), length(time_years))          #shuusei20251211
  future_price_mid <- (future_price_high + future_price_low)/2         #shuusei20251211
  
  if (x == "high") {                                                   #shuusei20251211
    future_price <- future_price_high                                  #shuusei20251211
  } else if (x == "low") {                                             #shuusei20251211
    future_price <- future_price_low                                   #shuusei20251211
  } else {  # "mid" かその他                                           #shuusei20251211
    future_price <- future_price_mid                                   #shuusei20251211
  }                                                                    #shuusei20251211
  
  elec_price <- data.frame(X1 = time_years, X2 = future_price)         #shuusei20251211
  
  ## 実データが存在する年（〜last_actual）は必ず実測値で上書きする。        #shuusei20251211
  idx_actual <- match(elec_price_time$X1, elec_price$X1)               #shuusei20251211
  valid_idx  <- which(!is.na(idx_actual))                              #shuusei20251211
  elec_price$X2[idx_actual[valid_idx]] <-                              #shuusei20251211
    elec_price_time$X2[valid_idx]                                      #shuusei20251211
  
  return(elec_price)                                                   #shuusei20251211
}                                                                      #shuusei20251211


set_FiT_f <- function(start_date, end_date, FiT_end_date, FiT_type, red_frac, init_fit, final_fit, exp_tar){
  if (end_date == FiT_end_date){
    FiT_zero <- NULL
  } else {
    FiT_zero <- data.frame(time_series = seq(FiT_end_date + months(1), end_date, by = '1 month'), FiT = 0,
                           FiT_large = 0, exp_tar = 0)
  }
  time_series <- seq(start_date, FiT_end_date, by = '1 month')
  if (FiT_type == "linear"){
    #  time_series <- seq(dmy("01jan2010"), end_date, by = '1 month')
    
    FiT <<- rbind(data.frame(time_series = time_series, FiT = seq(init_fit, final_fit, length.out = length(time_series)),
                             FiT_large = seq(init_fit, final_fit, length.out = length(time_series)), exp_tar = exp_tar),
                  FiT_zero)
  }
  if (FiT_type == "perc_red") {
    
    FiT <<- rbind(data.frame(time_series = time_series, FiT = geomSeries(init_fit, 1-red_frac, length(time_series)),
                             FiT_large = geomSeries(init_fit, 1-red_frac, length(time_series)), exp_tar = exp_tar),
                  FiT_zero)
  }
  if (FiT_type == "ann_perc_red") {
    #  time_series <- seq(dmy("01jan2010"), end_date, by = '1 month')
    
    year_series <- year(seq(dmy("01jan2010"), end_date, by = '1 year'))
    FiT_yr <- geomSeries(init_fit, 1-red_frac, length(year_series))
    FiT <- data.frame(time_series = time_series, FiT = NA)
    FiT$FiT <- sapply(FiT$time_series, function(x) FiT_yr[which(year_series == year(x))])
    FiT <- FiT %>% mutate(FiT_large = FiT, exp_tar = exp_tar)
    FiT <<- rbind(FiT, FiT_zero)
  }
  
  if(FiT_type == "dep_cap") {
    
    FiT <- data.frame(time_series = time_series, FiT = NA)
    FiT$FiT[1] <- init_fit
    FiT <<- rbind(FiT, FiT_zero)
  }
}

generate_populations_f <- function(n_agents, n_pop, dev, agent_name) {
  if(missing(n_agents)) n_agents <- 5000
  if(missing(n_pop)) n_pop <- 10
  if(missing(dev)) dev <- 25  # in MW - how far is the population's final deployment allowed to deviate
  # from real final deployment before it is rejected?
  if(missing(agent_name)) agent_name <- "agents"
  
  load_data(FiT_type = "real_h", start_date = "1jan2010", end_date = "1sep2016")
  
  batch_run_func_gen(number_of_agents = n_agents, n_des = n_pop, dev = dev, agent_name= agent_name)
}

batch_run_func_gen <- function(w, t, number_of_agents, n_des, dev, agent_name) {
  
  use_low_income_bonus <<- FALSE                          #shuusei20251116  （人口生成時は常に補助なし）
  # Set threshold and weights, electricity price
  if(missing(w)) w <- c(0.27, 0.25, 0.05, 0.43) # Weights: income & social, economic, capital
  
  if(missing(t)) threshold <- 0.74 # Adoption threshold
  else threshold <- t
  
  if(missing(number_of_agents)) number_of_agents <- 5000
  
  
  
  try(if(signif(sum(w), digits = 6) != 1) stop("Your weights don't add up to 1!"))
  
  n_pops <- 0
  i <- 1
  avg_u <<- NULL
  
  while (n_pops < n_des) {
    
    all_res <- run_model_gen(number_of_agents, i, w, threshold, n_pops, dev, agent_name) # run the model once 
    n_pops <- all_res[[1]]
    cat(i, n_pops, "\n", sep = " ")
    
    i <- i+1
    avg_u <<- rbind(avg_u, all_res[[2]])
  }
  
  averages <<- avg_u %>% group_by(time_series) %>% 
    summarise(tot_inst_cap = mean(tot_inst_cap, na.rm = TRUE), 
              inst_cap_diff = mean(inst_cap_diff, na.rm = TRUE))
  
  print(ggplot() + theme_bw() + 
          geom_line(data = deployment, aes(x = time_series, y = real_cap), color = "blue", size = 1) + 
          geom_line(data = avg_u, aes(x = time_series, 
                                      y = tot_inst_cap, group = run_number, color = run_number))+
          geom_line(data = averages, aes(x = time_series, y = tot_inst_cap), color = "black", size = 1))
  
  return()
  
}



run_model_gen <- function(number_of_agents, rn, w, threshold, n_in, dev, agent_name) {
  
  # Set up some parameters
  
  
  time_steps <- nrow(FiT) # number of months in time series
  
  agents <- rerun(number_of_agents,                                     #shuusei2025120a5
                  Household_Agent("N",                                  #shuusei20251205
                                  assign_income("own"),                #shuusei20251205
                                  "own",                               #shuusei20251205
                                  assign_region()))                    #shuusei20251205
  
  n_links <- 10                                                         #shuusei20251205
  
  mean_income <- mean(extract(agents, "income"))                        #shuusei20251205
  agents %<>% map(assign_LF) %>%                                        #shuusei20251205
    map(assign_elec_cons) %>%                                           #shuusei20251205
    map(assign_u_inc, mean_inc = mean_income)                           #shuusei2025120
  
  # 所得×地域ベースの small‑world ネットワークを構築                    #shuusei20251205
  agents <- assign_smallworld_network(agents,                           #shuusei20251205
                                      k = n_links,                      #shuusei20251205
                                      alpha = 0.05,                     #shuusei20251205
                                      p_rewire = 0.1)                   #shuusei20251205
  
  
  # Create agents: all non-adopters, assign income, size and region randomly weighted by real data
  
  # assign further characteristics based on those previously assigned.
  
  # Set up data frame to put data in
  
  avg_u <- data.frame(time_series = FiT$time_series + months(1), 
                      run_number = as.factor(rep.int(rn, time_steps)),
                      tot_inst_cap = vector(length = time_steps),
                      inst_cap_diff = vector(length = time_steps)
  )
  
  #---------------------------------------------------------#
  
  # Time evolution! 
  
  for (i in 1:time_steps) {
    # set parameters for current time
    FiT_current_small <<- FiT$FiT[[i]]/100 # p to £
    FiT_current_large <<- FiT$FiT_large[[i]]/100
    exp_tar_current <<- FiT$exp_tar[[i]]/100 # p to £
    fixed_current   <<- kW_price$X2[i]   # £/system                           #shuusei20251212
    marginal_current<<- kW_price$X3[i]   # £/kW                               #shuusei20251212
    # kW_price_current は廃止（使わない）                                     #shuusei20251212
    current_date <<- FiT$time_series[i]
    elec_index <- which(sapply(elec_price_time$X1, function(x) grep(x, current_date)) == 1)
    elec_price <<- elec_price_time[[elec_index, 2]]/100
    n_owners <<- owner_occupiers[[elec_index, 2]]
    
    agents <- agents %>% map(assign_inst_cap) %>% map(utilities, w = w, ags = agents) %>% 
      map(decide, threshold = threshold)
    
    adopters <- agents[map_chr(agents, "status") == "Y"]                 #shuusei20251212
    
    
    # Write data
    
    if (length(adopters) > 0){
      avg_u$avg_inst_cap[i] <- mean(extract(adopters, "inst_cap"))
      avg_u$tot_inst_cap[i] <- sum(extract(adopters, "inst_cap"), na.rm = TRUE)*n_owners/(1000*number_of_agents)
    }
    else{
      avg_u$avg_inst_cap[i] <- 0
      avg_u$tot_inst_cap[i] <- 0
    }
    
    avg_u$inst_cap_diff[i] <- deployment$real_cap[i] - 
      avg_u$tot_inst_cap[i]
  }
  
  if (abs(tail(avg_u$inst_cap_diff, 1)) <= dev){
    write_rds(agents, paste(agent_name, "_", n_in + 1, ".rds", sep = ""))
    rm(agents)
    n_pops <- n_in + 1
    cat("Successful: run ", rn, "\n")
  }
  else n_pops <- n_in
  
  
  
  return(list(n_pops, avg_u))
  
  
}
#------------------------- 2.2 Agent/model functions ----------------------------#
# all the sane as for historical 

#---------------------------- 2.3 Cost calculation ------------------------------#

subs_cost_f <- function(adpts, rn, number_of_agents) {
  if (length(adpts) > 0) {
    adopt_dates <- unname((sapply(adpts, function (x) x["date"])))
    adopt_dates <- do.call("c", adopt_dates)
    
    # UK policy: subsidies were guaranteed for 25 years before 1/8/2012, 20 years thereafter
    after <- adopt_dates >= dmy("1aug2012")
    before <- adopt_dates < dmy("1aug2012")
    
    guarantee <- vector(length = length(adopt_dates))
    
    guarantee[after] <- 20
    guarantee[before] <- 25
    
    adopt_costs <- data.frame(adopt_date = adopt_dates, 
                              output = sapply(adpts, function (x) output(x$inst_cap, x$LF)), 
                              FiT = extract(adpts, "FiT"), exp_tar = extract(adpts, "exp_tar"), 
                              guarantee = guarantee)
    
    
    # find the number of owner-occupiers corresponding to the adoption year 
    adopt_costs %<>% mutate(n_owners = sapply(adopt_date, which_owner_year_f))
    
    adopt_costs %<>% mutate(end_date = adopt_date + years(guarantee), 
                            export = output/2, # assuming no meter is installed
                            annual_cost = output*FiT + export*exp_tar,
                            annual_cost_scaled = annual_cost*n_owners/number_of_agents,
                            tot_cost = guarantee*annual_cost,
                            tot_cost_scaled = tot_cost*n_owners/number_of_agents)
    
    adopt_costs %<>% arrange(adopt_date)
    
    tot_sub_cost <- sum(adopt_costs$tot_cost_scaled)
    
    time_series <- dmy("01jan2010") + months(1:450)
    # have found annual & total cost per installation; can now find annual cost
    annual_cost <- vector(length = length(time_series))
    for (i in 1:length(time_series))
    { existing_inst <- filter(adopt_costs, adopt_date < time_series[i], end_date >= time_series[i])
    annual_cost[i] <- sum(existing_inst$annual_cost_scaled)
    }
    
  }
  else { # no one has adopted
    time_series <- dmy("01jan2010") + months(1:450)
    annual_cost = rep(0, length(time_series))
    tot_sub_cost = 0
  }
  a <- data.frame(time_series = time_series, annual_cost = annual_cost,
                  run_number = rep(rn, length(time_series)))
  cost_results <- list(a, tot_sub_cost = tot_sub_cost)
}


priv_cost_f <- function(x, rn, number_of_agents) { # x = adopters
  if (length(x) > 0) {
    adopt_dates <- unname((sapply(x, function (x) x["date"])))
    adopt_dates <- do.call("c", adopt_dates)
    inst_cap <- extract(x, "inst_cap")
    
    priv_costs <- data.frame(adopt_date = adopt_dates, 
                             inst_cap = inst_cap)
    
    priv_costs %<>% mutate(PV_fixed    = sapply(adopt_date, which_PV_fixed_f),    #shuusei20251212
                           PV_marginal = sapply(adopt_date, which_PV_marginal_f), #shuusei20251212
                           tot_cost    = PV_fixed + PV_marginal * inst_cap,       #shuusei20251212
                           n_owners = sapply(adopt_date, which_owner_year_f),
                           tot_cost_scaled = tot_cost*n_owners/number_of_agents)
    
    tot_priv_cost <- sum(priv_costs$tot_cost_scaled)
    
    time_series <- FiT$time_series
    cum_cost <- vector(length = length(time_series))
    for (i in 1:length(time_series))
    { existing_inst <- filter(priv_costs, adopt_date < time_series[i])
    cum_cost[i] <- sum(existing_inst$tot_cost_scaled)
    }
  }
  else {   time_series <- FiT$time_series
  cum_cost = rep(0, length(time_series))
  tot_priv_cost = 0
  }
  cost_results <- list(data.frame(time_series = time_series, cum_cost = cum_cost, 
                                  run_number = rep(rn, length(time_series))), 
                       tot_priv_cost = tot_priv_cost)
}

calc_LCOE_f <- function(adpts, rn, number_of_agents) {
  
  if (length(adpts) > 0){
    r <<- 0.05
    
    adopt_dates <- adpts %>% sapply(function (x) x["date"]) %>% unname
    adopt_dates <- do.call("c", adopt_dates)
    
    
    
    PV_fixed    <- adopt_dates %>% sapply(which_PV_fixed_f)                 #shuusei20251212
    PV_marginal <- adopt_dates %>% sapply(which_PV_marginal_f)              #shuusei20251212
    
    lifetime <- 25 # how long do the solar panels last?
    
    after <- adopt_dates >= dmy("1aug2012")
    before <- adopt_dates < dmy("1aug2012")
    
    guarantee <- vector(length = length(adopt_dates))
    
    guarantee[after] <- 20
    guarantee[before] <- 25
    
    adopt_costs <- data.frame(adopt_date = adopt_dates, inst_cap = extract(adpts, "inst_cap"),
                              output = sapply(adpts, function (x) output(x$inst_cap, x$LF)), 
                              FiT = extract(adpts, "FiT"), exp_tar = extract(adpts, "exp_tar"), 
                              guarantee = guarantee,
                              PV_fixed = PV_fixed,                          #shuusei20251212
                              PV_marginal = PV_marginal)                    #shuusei20251212
    
    adopt_costs %<>% mutate(export = output/2, annual_cost = output*FiT + export*exp_tar,
                            cap_cost = PV_fixed + PV_marginal * inst_cap)   #shuusei20251212
    
    tot_output <- sum(adopt_costs$output)
    
    
    adopt_costs %<>% mutate(LCOE_ind = LCOE(annual_cost, cap_cost, guarantee, output),
                            n_owners = sapply(adopt_date, which_owner_year_f),
                            output_scaled = output*n_owners/number_of_agents,
                            weight = output/tot_output)
    
    tot_output_scaled <- sum(adopt_costs$output_scaled)
    
    adopt_costs %<>% mutate(weight_scaled = output_scaled/tot_output_scaled, 
                            run_number = as.factor(rep(rn, nrow(adopt_costs))))
    
    adopt_costs %<>% select(adopt_date, LCOE_ind, weight_scaled, run_number, output_scaled)
    
    LCOE_weighted_scaled <- sum(adopt_costs$LCOE_ind*adopt_costs$weight_scaled)
  }
  else {
    LCOE_weighted_scaled <- NA
    adopt_costs <- data.frame(adopt_date = NA, LCOE_ind = NA, weight_scaled = NA,
                              run_number = rn, output_scaled = NA)
  }
  
  
  return(list(LCOE_weighted_scaled, adopt_costs))
}



which_PV_fixed_f <- function(x) {                                       #shuusei20251212
  if (!exists("kW_price_full", inherits = TRUE)) {                       #shuusei20251212
    stop("kW_price_full not found. Run load_data_f() first.")            #shuusei20251212
  }                                                                       #shuusei20251212
  if (!inherits(x, "Date")) x <- dmy(x)                                   #shuusei20251212
  
  row <- kW_price_full %>%                                               #shuusei20251212
    filter(!is.na(X1), X1 <= x) %>%                                      #shuusei20251212
    arrange(desc(X1)) %>%                                                #shuusei20251212
    slice(1)                                                             #shuusei20251212
  
  if (nrow(row) == 0) stop("PV fixed cost not found for date <= ", x)    #shuusei20251212
  as.numeric(row$X2[[1]])                                                #shuusei20251212
}                                                                        #shuusei20251212                                                                      #shuusei20251212

which_PV_marginal_f <- function(x) {                                    #shuusei20251212
  if (!exists("kW_price_full", inherits = TRUE)) {                        #shuusei20251212
    stop("kW_price_full not found. Run load_data_f() first.")             #shuusei20251212
  }                                                                       #shuusei20251212
  if (!inherits(x, "Date")) x <- dmy(x)                                   #shuusei20251212
  
  row <- kW_price_full %>%                                               #shuusei20251212
    filter(!is.na(X1), X1 <= x) %>%                                      #shuusei20251212
    arrange(desc(X1)) %>%                                                #shuusei20251212
    slice(1)                                                             #shuusei20251212
  
  if (nrow(row) == 0) stop("PV marginal cost not found for date <= ", x) #shuusei20251212
  as.numeric(row$X3[[1]])                                                #shuusei20251212
}                                                                        #shuusei20251212

which_PV_cost_f <- function(x) {                                       #shuusei20251212
  stop("which_PV_cost_f() is deprecated. Use which_PV_fixed_f()/which_PV_marginal_f().") #shuusei20251212
}                                                                       #shuusei20251212
which_owner_year_f <- function(x) {
  owner_occupier_h <- read_csv("Data/owner_occupiers.csv", col_names = F, col_types = "in") %>% 
    mutate(X2 = X2*1000)
  owner_occupier_all <- rbind(owner_occupier_h, owner_occupiers)
  owner_occupier_all$X2[owner_occupier_all$X1 == as.numeric(year(x))][1]
  
}


#------------------------------- 2.4 Processing ---------------------------------#

summarise_results_f <- function(avg_u, cost, cost_priv){
  number_of_runs <- max(as.numeric(avg_u$run_number))
  averages <<- avg_u %>% group_by(time_series) %>% 
    summarise(u_inc = mean(mean_u_inc), u_ec = mean(mean_u_ec), u_soc = mean(mean_u_soc),
              sd_u_inc = sqrt(sum(sd_u_inc^2))/number_of_runs,
              sd_u_ec = sqrt(sum(sd_u_ec^2))/number_of_runs,
              sd_u_soc = sqrt(sum(sd_u_soc^2))/number_of_runs,
              sd_u_cap = sqrt(sum(sd_u_cap^2))/number_of_runs,
              sd_u_tot = sqrt(sum(sd_u_tot^2))/number_of_runs,
              u_cap = mean(mean_u_cap),
              u_tot = mean(mean_u_tot), avg_inst_cap = mean(avg_inst_cap, na.rm = TRUE),  
              tot_inst_cap = mean(tot_inst_cap, na.rm = TRUE), 
              frac_of_adopters = mean(frac_of_adopters, na.rm = TRUE),
              frac_dec1  = mean(frac_dec1,  na.rm = TRUE),            #shuusei20251118
              frac_dec2  = mean(frac_dec2,  na.rm = TRUE),            #shuusei20251118
              frac_dec3  = mean(frac_dec3,  na.rm = TRUE),            #shuusei20251118
              frac_dec4  = mean(frac_dec4,  na.rm = TRUE),            #shuusei20251118
              frac_dec5  = mean(frac_dec5,  na.rm = TRUE),            #shuusei20251118
              frac_dec6  = mean(frac_dec6,  na.rm = TRUE),            #shuusei20251118
              frac_dec7  = mean(frac_dec7,  na.rm = TRUE),            #shuusei20251118
              frac_dec8  = mean(frac_dec8,  na.rm = TRUE),            #shuusei20251118
              frac_dec9  = mean(frac_dec9,  na.rm = TRUE),            #shuusei20251118
              frac_dec10 = mean(frac_dec10, na.rm = TRUE))            #shuusei20251118
  
  avg_cost <<- cost %>% group_by(time_series) %>% summarise(annual_cost = mean(annual_cost))
  
  avg_cost_priv <<- cost_priv %>% group_by(time_series) %>% summarise(cum_cost = mean(cum_cost))
}


##################################################################################
################################ 3. Miscellaneous ################################
##################################################################################

consumer_cost <- function(avg_cost, cum_prod_avg, r, start_year, end_year){
  
  annual_cost <- avg_cost %>% mutate(year = year(time_series)) %>% group_by(year) %>% 
    summarise(ann_cost = sum(annual_cost/12)) %>% mutate(n = year - 2008, d = 1/(1+r)^n, d_cost = d*ann_cost) %>%
    filter(year < end_year) %>% select(year, ann_cost, d, d_cost)
  
  d_cost <- annual_cost %>% summarise(cost = sum(d_cost)) %>% unlist
  cat("Discounted FiT cost up to ", end_year, " = Â£", d_cost/1e9, " billion", sep = "", "\n")
  
  ann_prod <- cum_prod_avg %>% mutate(year = year(time_series)) %>% group_by(year) %>% 
    summarise(ann_prod = sum(current_prod/12)) %>% filter(year < end_year)
  
  if(max(ann_prod$year) < end_year) {
    ann_prod_n <- data.frame(year = (max(ann_prod$year)+1):(end_year - 1), ann_prod = tail(cum_prod_avg$current_prod, 1))
    ann_prod <- rbind(ann_prod, ann_prod_n)
  }
  
  ann_prod %<>% select(ann_prod)
  
  ann_prod %<>% mutate(value = 0.03*ann_prod)
  cost_to_cons <- cbind(ann_prod, annual_cost) %>% mutate(cost = (ann_cost - value)*d) 
  
  cat("Discounted cost to consumers up to ", end_year, " = Â£", sum(cost_to_cons$cost)/1e9, " billion", sep = "", "\n")
  
}