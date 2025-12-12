#以下、02-required_functions.R

##################################################################################
################################ Historical (past) ###############################
##################################################################################

#--------------------------------- Batch runs -----------------------------------#

batch_run_func <- function(number_of_agents,                                              #shuusei202511288
                           number_of_runs, plot_u = T, plot_cost = T, plot_prod = T,      #shuusei202511288
                           save_name,                                                     #shuusei202511288
                           use_random_params = TRUE) {                                    #shuusei202511288
  
  use_low_income_bonus <<- FALSE                           #shuusei20251116 #shuusei202511288
  
  ## デフォルト値の設定（サンプリング前に設定しておく）                        #shuusei202511288
  if (missing(number_of_agents)) number_of_agents <- 5000                                #shuusei202511288
  if (missing(number_of_runs))  number_of_runs  <- 100                                   #shuusei202511288
  
  ## allowed_params_1000.txt を読み込み                                            #shuusei202511288
  allowed_params <- read_tsv('Data/allowed_params_1000.txt', col_names = F)             #shuusei202511288
  n_allowed <- nrow(allowed_params)                                                     #shuusei202511288
  
  ## パラメタの選び方：ランダム or 先頭から順番                                          #shuusei202511288
  if (use_random_params) {                                                              #shuusei202511288
    ## 従来どおり：ランダムに number_of_runs 行を抽出                                   #shuusei202511288
    idx_params <- sample(1:n_allowed, number_of_runs, replace = TRUE)                  #shuusei202511288
  } else {                                                                              #shuusei202511288
    ## 新モード：1 行目から順番に使う                                                   #shuusei202511288
    if (number_of_runs > n_allowed) {                                                   #shuusei202511288
      stop("number_of_runs が allowed_params_1000.txt の行数を超えています（use_random_params = FALSE のとき）。") #shuusei202511288
    }                                                                                   #shuusei202511288
    idx_params <- 1:number_of_runs                                                      #shuusei202511288
  }                                                                                     #shuusei202511288
  
  sample_for_run <- allowed_params[idx_params, , drop = FALSE]                         #shuusei202511288
  
  initialise_vars() # create variables which will store output
  
  
  for (i1 in 1:number_of_runs) {
    w <- unlist(sample_for_run[i1, 1:4])
    threshold <- unlist(sample_for_run[i1, 5])
    if (run_w_cap == TRUE) { # Reset to original values for new run 
      FiT <<- FiT_0
      dep_cap <<- dep_cap_0
    }
    
    cat(i1, w, threshold, "\n")
    
    all_res_rn <<- run_model(number_of_agents, i1, w, threshold) # run the model once 
    
    append_results() # add results of current run to previous results
    
    
  }
  
  rm(all_res_rn, current_date, envir = .GlobalEnv)
  
  summarise_results(avg_u, cost, cost_priv) # calculate averages of all runs
  
  sum_abs_diff <- sum(abs(averages$inst_cap_diff)) # deviation from real data: sum of absolute values
  # of deviation from installed FiT capacity < 10 kW
  
  overall_tot_cost <- tot_cost_priv + tot_cost # total expenditure; used for calcul
  overall_tot_cost_mean <- mean(overall_tot_cost)/1e9 # in billions
  overall_tot_cost_sd <- sd(overall_tot_cost)/1e9 # in billions
  
  prod_res <- calc_prod(LCOE_data, number_of_runs) # calculate total production from all installations at each date
  cum_prod <<- prod_res[[1]]
  cum_prod_avg <<- prod_res[[2]]
  
  
  # Plotting
  
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
  
  if (plot_cost == T){
    print(ggplot() + theme_bw() + 
            geom_line(data = cost, aes(x = time_series, y = annual_cost, group = run_number), alpha = 0.2)+
            geom_line(data = avg_cost, aes(x = time_series, y = annual_cost), color = "black", size = 1))
    print(ggplot() + theme_bw() +
            geom_line(data = cost_priv, aes(x = time_series, y = cum_cost, group = run_number), alpha = 0.2)+
            geom_line(data = avg_cost_priv, aes(x = time_series, y = cum_cost), color = "black", size = 1))
    
    print(ggplot(LCOE_data) + theme_bw() + geom_point(aes(x=adopt_date, y = LCOE_ind, 
                                                          group = run_number, color = run_number), alpha = 0.1))
    
  }
  
  if (plot_prod == T){
    print(ggplot() + theme_bw() + 
            geom_line(data = cum_prod, aes(x = time_series, y = current_prod, group = run_number,
                                           color = run_number)) +
            geom_line(data = cum_prod_avg, aes(x=time_series, y = current_prod), size = 1))
  }
  
  if (run_w_cap == TRUE) {
    avg_FiT <<- FiT_levels %>% group_by(time_series) %>% summarise(FiT = mean(FiT))
    print(ggplot() + geom_line(data = FiT_levels, aes(x = time_series, y = FiT, group = run_number,
                                                      color = run_number)) +
            geom_line(data = avg_FiT, aes(x=time_series, y = FiT), size = 1))
  }
  
  print_vars <- paste("w = ", w[1], w[2], w[3], w[4], ", t =", threshold,", n_agents =", number_of_agents)
  
  print(ggplot() + theme_bw() +                                         #shuusei202511288
          geom_line(data = deployment, aes(x = time_series, y = real_cap), color = "blue", size = 1) +  #shuusei202511288
          geom_line(data = avg_u, aes(x = time_series,                  #shuusei202511288
                                      y = tot_inst_cap, group = run_number), alpha = 0.2)+  #shuusei202511288
          geom_line(data = averages, aes(x = time_series, y = tot_inst_cap), color = "black", size = 1) + #shuusei202511288
          annotate("text", x = dmy("01jul2011"), y = 2000, label = print_vars))  #shuusei202511288
  
  # SES 五分位別「累積導入容量（MW）」の推移をプロット              #shuusei202511288
  cap_dec_vars <- paste0("cap_dec", 1:10)                               #shuusei202511288
  if (all(cap_dec_vars %in% names(averages))) {                         #shuusei202511288
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
  
  
  
  
  if(missing(save_name)){
    cat("\n", "Data not being saved!", "\n", sep = "")
  } else {
    write_rds(avg_u, paste(save_name, "_avg_u.rds", sep = ""))
    write_rds(cost, paste(save_name, "_cost.rds", sep = ""))
    write_rds(cost_priv, paste(save_name, "_cost_priv.rds", sep = ""))
    write_rds(LCOE_data, paste(save_name, "_LCOE_data.rds", sep = ""))
    write_rds(LCOE_avg, paste(save_name, "_LCOE_avg.rds", sep = ""))
    write_rds(FiT, paste(save_name, "_FiT.rds", sep = ""))
    if (run_w_cap == TRUE) write_rds(FiT_levels, paste(save_name, "_FiT_levels.rds", sep = ""))
  }
  
  to_return <- list(avg_u, cost, cost_priv, LCOE_data, LCOE_avg, FiT)
  
  if (run_w_cap == TRUE) to_return <- list(avg_u, cost, cost_priv, LCOE_data, LCOE_avg, FiT, FiT_levels)
  
  return(to_return)
  
}


#------------------------------- Individual runs --------------------------------#

run_model <- function(number_of_agents, rn, w, threshold) {
  
  # Set up some parameters
  
  
  time_steps <- nrow(FiT) # number of months in time series
  
  agents <- rerun(number_of_agents,                                     #shuusei2025120t5
                  Household_Agent("N",                                  #shuusei20251205
                                  assign_income("own"),                #shuusei20251205
                                  "own",                               #shuusei20251205
                                  assign_region()))                    #shuusei20251205
  
  n_links <- 10                                                         #shuusei20251205
  
  mean_income <- mean(extract(agents, "income"))                        #shuusei20251205
  agents %<>% map(assign_LF) %>%                                        #shuusei20251205
    map(assign_elec_cons) %>%                                           #shuusei20251205
    map(assign_u_inc, mean_inc = mean_income)                           #shuusei20251205
  
  agents <- assign_smallworld_network(agents,                           #shuusei20251205
                                      k = n_links,                      #shuusei20251205
                                      alpha = 0.05,                     #shuusei20251205
                                      p_rewire = 0.1)                   #shuusei20251205
  
  
  # 所得デシル（1〜10）を各エージェントに付与                      #shuusei20251118
  incomes <- extract(agents, "income")                                   #shuusei20251118
  dec_breaks <- quantile(incomes, probs = seq(0, 1, 0.1), na.rm = TRUE)  #shuusei20251118
  dec_vals <- findInterval(incomes, dec_breaks, all.inside = TRUE)       #shuusei20251118
  for (k in seq_along(agents)) {                                         #shuusei20251118
    agents[[k]]$inc_decile <- dec_vals[k]                                #shuusei20251118
  }                                                                      #shuusei20251118
  
  ## デシル別の平均 meet_demand を計算（roof_limit 正規化用）          #shuusei20251129
  compute_meet_demand_ref_by_decile(agents)                              #shuusei20251129
  
  adopters <- agents[map_chr(agents, "status") == "Y"]                   #shuusei20251212
  
  if (length(adopters) > 0){
    n_owners <<- owner_occupiers[[1, 2]]
    init_cap <- sum(extract(adopters, "inst_cap"), na.rm = TRUE)*n_owners/(1000*number_of_agents)
  }
  else{
    init_cap <- 0
  }
  
  # Create agents: all non-adopters, assign income, size and region randomly weighted by real data
  
  # assign further characteristics based on those previously assigned.
  
  # Set up data frame to put data in
  
  avg_u <- data.frame(time_series = FiT$time_series + months(1), 
                      run_number = as.factor(rep.int(rn, time_steps)),
                      mean_u_inc = vector(length = time_steps),
                      mean_u_ec = vector(length = time_steps),
                      mean_u_soc = vector(length = time_steps),
                      mean_u_cap = vector(length = time_steps),
                      mean_u_tot = vector(length = time_steps),
                      sd_u_inc = vector(length = time_steps),
                      sd_u_ec = vector(length = time_steps),
                      sd_u_soc = vector(length = time_steps),
                      sd_u_cap = vector(length = time_steps),
                      sd_u_tot = vector(length = time_steps),
                      frac_of_adopters = vector(length = time_steps),
                      avg_inst_cap = vector(length = time_steps),
                      tot_inst_cap = vector(length = time_steps),
                      inst_cap_diff = vector(length = time_steps),
                      frac_dec1  = vector(length = time_steps),          #shuusei20251118
                      frac_dec2  = vector(length = time_steps),          #shuusei20251118
                      frac_dec3  = vector(length = time_steps),          #shuusei20251118
                      frac_dec4  = vector(length = time_steps),          #shuusei20251118
                      frac_dec5  = vector(length = time_steps),          #shuusei20251118
                      frac_dec6  = vector(length = time_steps),          #shuusei20251118
                      frac_dec7  = vector(length = time_steps),          #shuusei20251118
                      frac_dec8  = vector(length = time_steps),          #shuusei20251118
                      frac_dec9  = vector(length = time_steps),          #shuusei20251118
                      frac_dec10 = vector(length = time_steps),           #shuusei20251118
                      cap_dec1   = vector(length = time_steps),   #shuusei20251121
                      cap_dec2   = vector(length = time_steps),   #shuusei20251121
                      cap_dec3   = vector(length = time_steps),   #shuusei20251121
                      cap_dec4   = vector(length = time_steps),   #shuusei20251121
                      cap_dec5   = vector(length = time_steps),   #shuusei20251121
                      cap_dec6   = vector(length = time_steps),   #shuusei20251121
                      cap_dec7   = vector(length = time_steps),   #shuusei20251121
                      cap_dec8   = vector(length = time_steps),   #shuusei20251121
                      cap_dec9   = vector(length = time_steps),   #shuusei20251121
                      cap_dec10  = vector(length = time_steps),   #shuusei20251121
                      ## ここから新規：デシル別の制約・4kW選択シェア          #shuusei202511288
                      budget_dec1  = vector(length = time_steps),       #shuusei202511288
                      budget_dec2  = vector(length = time_steps),       #shuusei202511288
                      budget_dec3  = vector(length = time_steps),       #shuusei202511288
                      budget_dec4  = vector(length = time_steps),       #shuusei202511288
                      budget_dec5  = vector(length = time_steps),       #shuusei202511288
                      budget_dec6  = vector(length = time_steps),       #shuusei202511288
                      budget_dec7  = vector(length = time_steps),       #shuusei202511288
                      budget_dec8  = vector(length = time_steps),       #shuusei202511288
                      budget_dec9  = vector(length = time_steps),       #shuusei202511288
                      budget_dec10 = vector(length = time_steps),       #shuusei202511288
                      roof_dec1    = vector(length = time_steps),       #shuusei202511288
                      roof_dec2    = vector(length = time_steps),       #shuusei202511288
                      roof_dec3    = vector(length = time_steps),       #shuusei202511288
                      roof_dec4    = vector(length = time_steps),       #shuusei202511288
                      roof_dec5    = vector(length = time_steps),       #shuusei202511288
                      roof_dec6    = vector(length = time_steps),       #shuusei202511288
                      roof_dec7    = vector(length = time_steps),       #shuusei202511288
                      roof_dec8    = vector(length = time_steps),       #shuusei202511288
                      roof_dec9    = vector(length = time_steps),       #shuusei202511288
                      roof_dec10   = vector(length = time_steps),       #shuusei202511288
                      share_budget_dec1  = vector(length = time_steps),  #shuusei202511288
                      share_budget_dec2  = vector(length = time_steps),  #shuusei202511288
                      share_budget_dec3  = vector(length = time_steps),  #shuusei202511288
                      share_budget_dec4  = vector(length = time_steps),  #shuusei202511288
                      share_budget_dec5  = vector(length = time_steps),  #shuusei202511288
                      share_budget_dec6  = vector(length = time_steps),  #shuusei202511288
                      share_budget_dec7  = vector(length = time_steps),  #shuusei202511288
                      share_budget_dec8  = vector(length = time_steps),  #shuusei202511288
                      share_budget_dec9  = vector(length = time_steps),  #shuusei202511288
                      share_budget_dec10 = vector(length = time_steps),  #shuusei202511288
                      share_roof_dec1    = vector(length = time_steps),  #shuusei202511288
                      share_roof_dec2    = vector(length = time_steps),  #shuusei202511288
                      share_roof_dec3    = vector(length = time_steps),  #shuusei202511288
                      share_roof_dec4    = vector(length = time_steps),  #shuusei202511288
                      share_roof_dec5    = vector(length = time_steps),  #shuusei202511288
                      share_roof_dec6    = vector(length = time_steps),  #shuusei202511288
                      share_roof_dec7    = vector(length = time_steps),  #shuusei202511288
                      share_roof_dec8    = vector(length = time_steps),  #shuusei202511288
                      share_roof_dec9    = vector(length = time_steps),  #shuusei202511288
                      share_roof_dec10   = vector(length = time_steps),  #shuusei202511288
                      share_4kw_dec1     = vector(length = time_steps),  #shuusei202511288
                      share_4kw_dec2     = vector(length = time_steps),  #shuusei202511288
                      share_4kw_dec3     = vector(length = time_steps),  #shuusei202511288
                      share_4kw_dec4     = vector(length = time_steps),  #shuusei202511288
                      share_4kw_dec5     = vector(length = time_steps),  #shuusei202511288
                      share_4kw_dec6     = vector(length = time_steps),  #shuusei202511288
                      share_4kw_dec7     = vector(length = time_steps),  #shuusei202511288
                      share_4kw_dec8     = vector(length = time_steps),  #shuusei202511288
                      share_4kw_dec9     = vector(length = time_steps),  #shuusei202511288
                      share_4kw_dec10    = vector(length = time_steps),  #shuusei202511288
                      share_large_dec1   = vector(length = time_steps),  #shuusei202511288
                      share_large_dec2   = vector(length = time_steps),  #shuusei202511288
                      share_large_dec3   = vector(length = time_steps),  #shuusei202511288
                      share_large_dec4   = vector(length = time_steps),  #shuusei202511288
                      share_large_dec5   = vector(length = time_steps),  #shuusei202511288
                      share_large_dec6   = vector(length = time_steps),  #shuusei202511288
                      share_large_dec7   = vector(length = time_steps),  #shuusei202511288
                      share_large_dec8   = vector(length = time_steps),  #shuusei202511288
                      share_large_dec9   = vector(length = time_steps),  #shuusei202511288
                      share_large_dec10  = vector(length = time_steps)   #shuusei202511288
  )
  
  
  #---------------------------------------------------------#
  
  # Time evolution! 
  
  if(run_w_cap == TRUE) {
    quarter_done <- 0 # how many quarters' capacity have already been used up?
    exceeded <- FALSE # has the total available capacity been exceeded?
  }
  
  for (i in 1:time_steps) {
    # set parameters for current time
    FiT_current_small <<- FiT$FiT[[i]]/100 # p to £
    FiT_current_large <<- FiT$FiT_large[[i]]/100
    exp_tar_current <<- FiT$exp_tar[[i]]/100 # p to £
    fixed_current   <<- kW_price$X2[i]   # £/system                           #shuusei20251212
    marginal_current<<- kW_price$X3[i]   # £/kW                               #shuusei20251212
    # kW_price_current は廃止（使わない）                                     #shuusei20251212
    current_date <<- FiT$time_series[i]
    
    yr_now <- year(current_date)                                        #shuusei20251212
    
    elec_index <- match(yr_now, elec_price_time$X1)                     #shuusei20251212
    if (is.na(elec_index)) stop("No electricity price for year: ", yr_now) #shuusei20251212
    elec_price <<- elec_price_time$X2[elec_index]/100                   #shuusei20251212
    
    owner_index <- match(yr_now, owner_occupiers$X1)                    #shuusei20251212
    if (is.na(owner_index)) stop("No owner_occupiers for year: ", yr_now) #shuusei20251212
    n_owners <<- owner_occupiers$X2[owner_index]                        #shuusei20251212
    
    agents <- agents %>% map(assign_inst_cap) %>% map(utilities, w = w, ags = agents) %>% 
      map(decide, threshold = threshold)
    
    adopters <- agents[map_chr(agents, "status") == "Y"]                 #shuusei20251212
    
    
    # Write data
    k <- map_chr(agents, "status") == "Y"                               #shuusei20251212
    avg_u$frac_of_adopters[i] <- sum(k, na.rm = TRUE)/number_of_agents  #shuusei20251212
    avg_u$mean_u_ec[i] <- mean(extract(agents, "u_ec"))
    avg_u$mean_u_inc[i] <- mean(extract(agents, "u_inc"))
    avg_u$mean_u_soc[i] <- mean(extract(agents, "u_soc"))
    avg_u$mean_u_cap[i] <- mean(extract(agents, "u_cap"))
    avg_u$mean_u_tot[i] <- mean(extract(agents, "u_tot"))
    avg_u$sd_u_ec[i] <- sd(extract(agents, "u_ec"))
    avg_u$sd_u_inc[i] <- sd(extract(agents, "u_inc"))
    avg_u$sd_u_soc[i] <- sd(extract(agents, "u_soc"))
    avg_u$sd_u_cap[i] <- sd(extract(agents, "u_cap"))
    avg_u$sd_u_tot[i] <- sd(extract(agents, "u_tot"))
    
    if (length(adopters) > 0){
      avg_u$avg_inst_cap[i] <- mean(extract(adopters, "inst_cap"))
      avg_u$tot_inst_cap[i] <- sum(extract(adopters, "inst_cap"), na.rm = TRUE)*n_owners/(1000*number_of_agents)
    }
    else{
      avg_u$avg_inst_cap[i] <- NA
      avg_u$tot_inst_cap[i] <- 0
    }
    
    avg_u$inst_cap_diff[i] <- deployment$real_cap[i] - 
      avg_u$tot_inst_cap[i]
    
    # デシル別導入率とデシル別累積容量（MW）の計算               #shuusei20251121
    deciles   <- extract(agents, "inc_decile")                         #shuusei20251121
    status    <- extract(agents, "status") == "Y"                      #shuusei20251121
    inst_caps <- extract(agents, "inst_cap")                           #shuusei20251121
    cap_src   <- extract(agents, "cap_raw_source")                     #shuusei202511288
    cap_choice<- extract(agents, "cap_choice_type")                    #shuusei202511288
    budget_vec <- extract(agents, "inst_cap_budget")                   #shuusei202511288
    roof_vec   <- extract(agents, "meet_demand")                       #shuusei20251130
    
    for (d in 1:10) {                                                  #shuusei20251121
      idx_all   <- which(deciles == d)                                 #shuusei20251121
      idx_adopt <- which(deciles == d & status)                        #shuusei20251121
      
      # 導入率（そのデシルのうち Y の割合）                         #shuusei20251121
      if (length(idx_all) > 0) {                                       #shuusei20251121
        avg_u[i, paste0("frac_dec", d)] <-                             #shuusei20251121
          sum(status[idx_all]) / length(idx_all)                       #shuusei20251121
      } else {                                                         #shuusei20251121
        avg_u[i, paste0("frac_dec", d)] <- NA                          #shuusei20251121
      }                                                                #shuusei20251121
      
      # そのデシルの「累積容量（MW）」と、制約・4kW選択・budget/roof 平均 #shuusei202511288
      if (length(idx_adopt) > 0) {                                     #shuusei20251121
        cap_d <- sum(inst_caps[idx_adopt], na.rm = TRUE) *             #shuusei20251121
          n_owners/(1000*number_of_agents)                             #shuusei20251121
        
        ## ★デシル d の導入世帯における inst_cap_budget / roof_limit の平均 #shuusei202511288
        avg_u[i, paste0("budget_dec", d)] <-                           #shuusei202511288
          mean(budget_vec[idx_adopt], na.rm = TRUE)                    #shuusei202511288
        avg_u[i, paste0("roof_dec", d)]   <-                           #shuusei202511288
          mean(roof_vec[idx_adopt],   na.rm = TRUE)                    #shuusei202511288
        
        src_d    <- cap_src[idx_adopt]                                 #shuusei202511288
        choice_d <- cap_choice[idx_adopt]                              #shuusei202511288
        n_adopt_d <- length(idx_adopt)                                 #shuusei202511288
        
        ## inst_cap_budget vs roof_limit のシェア（デシル内の導入世帯基準） #shuusei202511288
        avg_u[i, paste0("share_budget_dec", d)] <-                     #shuusei202511288
          sum(src_d == "budget", na.rm = TRUE) / n_adopt_d             #shuusei202511288
        avg_u[i, paste0("share_roof_dec", d)]   <-                     #shuusei20251130
          sum(src_d == "meet",   na.rm = TRUE) / n_adopt_d             #shuusei20251130
        
        ## inst_cap_raw > 4kW の世帯の中で、4kW vs 大容量の選択シェア   #shuusei202511288
        idx_eligible <- which(choice_d %in%                            #shuusei202511288
                                c("choose_4","choose_large","choose_large_trunc10")) #shuusei202511288
        if (length(idx_eligible) > 0) {                                #shuusei202511288
          ch_e <- choice_d[idx_eligible]                               #shuusei202511288
          n_e  <- length(ch_e)                                         #shuusei202511288
          avg_u[i, paste0("share_4kw_dec", d)] <-                      #shuusei202511288
            sum(ch_e == "choose_4", na.rm = TRUE) / n_e                #shuusei202511288
          avg_u[i, paste0("share_large_dec", d)] <-                    #shuusei202511288
            sum(ch_e %in% c("choose_large","choose_large_trunc10"),    #shuusei202511288
                na.rm = TRUE) / n_e                                    #shuusei202511288
        } else {                                                       #shuusei202511288
          avg_u[i, paste0("share_4kw_dec", d)]    <- NA                #shuusei202511288
          avg_u[i, paste0("share_large_dec", d)]  <- NA                #shuusei202511288
        }                                                              #shuusei202511288
        
      } else {                                                         #shuusei20251121
        cap_d <- 0                                                     #shuusei20251121
        avg_u[i, paste0("budget_dec", d)]       <- NA                  #shuusei202511288
        avg_u[i, paste0("roof_dec", d)]         <- NA                  #shuusei202511288
        avg_u[i, paste0("share_budget_dec", d)] <- NA                  #shuusei202511288
        avg_u[i, paste0("share_roof_dec", d)]   <- NA                  #shuusei202511288
        avg_u[i, paste0("share_4kw_dec", d)]    <- NA                  #shuusei202511288
        avg_u[i, paste0("share_large_dec", d)]  <- NA                  #shuusei202511288
      }                                                                #shuusei20251121
      
      avg_u[i, paste0("cap_dec", d)] <- cap_d                          #shuusei20251121
    }                                                                  #shuusei20251121
    
    
    ##### Deployment cap code - only runs if using a deployment cap scenario
    if (run_w_cap == TRUE){
      
      
      if ((avg_u$tot_inst_cap[i] - init_cap) > sum(dep_cap$orig_cap) && exceeded == FALSE) { # total available capacity has been exceeded;
        # all further FiTs are zero
        FiT[(i+1):nrow(FiT), 2:4] <<- 0
        exceeded <- TRUE
      }
      
      if (exceeded == FALSE && i < nrow(FiT)) { # Not yet exceeded all the caps & not in the final time step
        
        which_q <- max(which(current_date >= dep_cap$q_dates)) # which quarter are we in?
        
        ref_cap <- avg_u$tot_inst_cap[avg_u$time_series == dep_cap$q_dates[which_q]]
        if(is_empty(ref_cap)) ref_cap <- init_cap
        
        current_quarter <- avg_u$tot_inst_cap[i] - ref_cap # how much capacity has been installed so far in the quarter?
        
        dep_cap$inst_cap[which_q] <<- current_quarter
        
        current_month <- avg_u$tot_inst_cap[i] - avg_u$tot_inst_cap[i-1] # capacity installed this month
        
        excess_cap <- dep_cap$cap[which_q] - current_quarter # how much capacity is left over in the current quarter?
        
        if (current_month > 0 && which_q < nrow(dep_cap) && quarter_done == which_q) {
          # some capacity has been installed this month, not in the final quarter, and we aren't in next q's capacity already
          remaining <- current_month # the remaining installed capacity which must be assigned to this or next month
          
          for (j in 1:(nrow(dep_cap)-which_q)) { # 
            
            if (dep_cap$cap[which_q + j] - remaining < 0) { # if we are exceeding the available capacity in a quarter
              
              current_FiT_index <- min(which(FiT$FiT[i] == FiT_list))
              if (current_FiT_index == length(FiT_list)) {
                FiT[(i+1):nrow(FiT), 2:3] <<- FiT_list[current_FiT_index]
              }
              else FiT[(i+1):nrow(FiT), 2:3] <<- FiT_list[current_FiT_index + 1]
              
              remaining <- remaining - dep_cap$cap[which_q + j]
              dep_cap$cap[which_q + j] <<- 0
            } else if (dep_cap$cap[which_q + j] - remaining >= 0) { # all the capacity has been allocated; break out of the j loop
              dep_cap$cap[which_q + j] <<- dep_cap$cap[which_q + j] - remaining
              
              break
            }
          }
        }
        
        if (excess_cap < 0 && which_q < nrow(dep_cap) && quarter_done != which_q) {
          # have exceeded the allocated capacity for this quarter
          remaining <- current_quarter - dep_cap$cap[which_q]
          
          quarter_done <- which_q
          for (j in 1:(nrow(dep_cap)-which_q)) {
            
            if (dep_cap$cap[which_q + j] - remaining < 0) {
              
              current_FiT_index <- min(which(FiT$FiT[i] == FiT_list))
              if (current_FiT_index == length(FiT_list)) {
                FiT[(i+1):nrow(FiT), 2:3] <<- FiT_list[current_FiT_index]
              }
              else FiT[(i+1):nrow(FiT), 2:3] <<- FiT_list[current_FiT_index + 1]
              
              remaining <- remaining - dep_cap$cap[which_q + j]
              dep_cap$cap[which_q + j] <<- 0
              
            } else if (dep_cap$cap[which_q + j] - remaining >= 0) {
              dep_cap$cap[which_q + j] <<- dep_cap$cap[which_q + j] - remaining
              break
            }
          }
        }
        
        if (current_date == dep_cap$q_dates[which_q] + months(2) && excess_cap > 0) {
          dep_cap$cap[which_q + 1] <<- dep_cap$cap[which_q + 1] + excess_cap
        }
        
      }
      
    }
    
    #### ^^^ End of deployment cap code
    
  }
  
  if (run_w_cap == TRUE && exceeded == TRUE) cat("Total available capacity was exceeded", "\n")
  
  FiT_outp <- cbind(FiT, run_number = rep(rn, nrow(FiT)))
  
  LCOE_data <- calc_LCOE(adopters, rn, number_of_agents)
  LCOE_avg <- LCOE_data[[1]]
  LCOE_data <- LCOE_data[[2]]
  cost_subs_res <- subs_cost(adopters, rn, number_of_agents)
  cost_priv_res <- priv_cost(adopters, rn, number_of_agents)
  tot_subs_cost <- cost_subs_res[[2]]
  ann_subs_cost <- cost_subs_res[[1]]
  
  tot_priv_cost <- cost_priv_res[[2]]
  cum_priv_cost <- cost_priv_res[[1]]
  all_results <- list(avg_u, ann_subs_cost, tot_subs_cost, cum_priv_cost, 
                      tot_priv_cost, LCOE_avg, LCOE_data)
  if (run_w_cap == TRUE) {
    all_results <- list(avg_u, ann_subs_cost, tot_subs_cost, cum_priv_cost, 
                        tot_priv_cost, LCOE_avg, LCOE_data, FiT_outp)
  }
  
  cat(length(adopters), "adopters in run", rn, "\n", sep = " ")
  
  
  return(all_results)
  
  
}


##################################################################################
############################## Projections (future) ##############################
##################################################################################

#--------------------------------- Batch runs -----------------------------------#
batch_run_func_f <- function(agent_name, 
                             number_of_runs, plot_u = T, plot_cost = T, plot_prod = T, save_name,
                             low_inc_ratio = 0,              #shuusei20251116  中央所得に対する割合（例:0.8）
                             extra_FiT_low_p = 0) {          #shuusei20251116  低所得世帯への上乗せ[p/kWh]
  
  allowed_params <- read_tsv('Data/allowed_params_1000.txt', col_names = F)
  # Set threshold and weights, electricity price
  sample_for_run <- allowed_params[sample(1:nrow(allowed_params), number_of_runs, replace = TRUE), ]
  
  if(missing(agent_name)) agent_name <- "agents"
  number_of_agents <- length(read_rds(paste('Data/', agent_name, "_1.rds", sep = "")))
  
  if(missing(number_of_runs)) number_of_runs <- 100 
  
  # 低所得世帯へのFiT上乗せ設定（未来シミュレーション専用）         #shuusei20251116
  use_low_income_bonus <<- (low_inc_ratio > 0 && extra_FiT_low_p > 0)   #shuusei20251116
  low_income_ratio   <<- low_inc_ratio                                  #shuusei20251116
  extra_FiT_low      <<- extra_FiT_low_p/100    # p/kWh → £/kWh         #shuusei20251116
  
  initialise_vars() # create variables which will store output
  
  #  if (run_w_cap == TRUE) {
  #    FiT_0 <<- FiT
  #    dep_cap_0 <<- dep_cap
  #  }
  
  for (i1 in 1:number_of_runs) {
    
    if (run_w_cap == TRUE) {
      FiT <<- FiT_0
      dep_cap <<- dep_cap_0
    }
    w <- unlist(sample_for_run[i1, 1:4])
    threshold <- unlist(sample_for_run[i1, 5])
    print(i1)
    
    all_res_rn <<- run_model_f(agent_name, i1, w, threshold) # run the model once 
    
    append_results() # add results of current run to previous results
    
    
  }
  
  rm(all_res_rn, current_date, envir = .GlobalEnv)
  
  summarise_results_f(avg_u, cost, cost_priv) # calculate averages of all runs
  
  
  overall_tot_cost <- tot_cost_priv + tot_cost # total expenditure; used for calcul
  overall_tot_cost_mean <- mean(overall_tot_cost)/1e9 # in billions
  overall_tot_cost_sd <- sd(overall_tot_cost)/1e9 # in billions
  
  prod_res <- calc_prod(LCOE_data, number_of_runs) # calculate total production from all installations at each date
  cum_prod <<- prod_res[[1]]
  cum_prod_avg <<- prod_res[[2]]
  
  
  
  
  
  ############# Plotting ############
  
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
          geom_line(data = averages, aes(x = time_series, y = avg_inst_cap)) +
          geom_line(data = deployment, aes(x = time_series, y = avg_cap)))
  
  if (plot_cost == T){
    print(ggplot() + theme_bw() + 
            geom_line(data = cost, aes(x = time_series, y = annual_cost, group = run_number), alpha = 0.2)+
            geom_line(data = avg_cost, aes(x = time_series, y = annual_cost), color = "black", size = 1))
    print(ggplot() + theme_bw() +
            geom_line(data = cost_priv, aes(x = time_series, y = cum_cost, group = run_number), alpha = 0.2)+
            geom_line(data = avg_cost_priv, aes(x = time_series, y = cum_cost), color = "black", size = 1))
    
    print(ggplot(LCOE_data) + theme_bw() + geom_point(aes(x=adopt_date, y = LCOE_ind, 
                                                          group = run_number, color = run_number), alpha = 0.1))
    
  }
  
  if (plot_prod == T){
    print(ggplot() + theme_bw() + 
            geom_line(data = cum_prod, aes(x = time_series, y = current_prod, group = run_number,
                                           color = run_number)) +
            geom_line(data = cum_prod_avg, aes(x=time_series, y = current_prod), size = 1))
  }
  
  if (run_w_cap == TRUE) {
    avg_FiT <- FiT_levels %>% group_by(time_series) %>% summarise(FiT = mean(FiT))
    print(ggplot() + geom_line(data = FiT_levels, aes(x = time_series, y = FiT, group = run_number,
                                                      color = run_number)) +
            geom_line(data = avg_FiT, aes(x=time_series, y = FiT), size = 1))
  }
  
  print_vars <- paste("w = ", w[1], w[2], w[3], w[4], ", t =", threshold,", n_agents =", number_of_agents)
  
  
  print(ggplot() + theme_bw() + 
          geom_line(data = deployment %>% filter(time_series <= averages$time_series[1]), 
                    aes(x = time_series, y = real_cap), color = "blue", size = 1) + 
          geom_line(data = avg_u, aes(x = time_series, 
                                      y = tot_inst_cap, group = run_number), alpha = 0.2)+
          geom_line(data = averages, aes(x = time_series, y = tot_inst_cap), color = "black", size = 1) +
          annotate("text", x = dmy("01jul2011"), y = 2000, label = print_vars))
  
  # デシル別導入率の推移をプロット（未来シナリオ）               #shuusei20251122
  dec_vars <- paste0("frac_dec", 1:10)                                  #shuusei20251118
  dec_df <- averages %>%                                                #shuusei20251118
    select(time_series, all_of(dec_vars)) %>%                           #shuusei20251118
    pivot_longer(cols = starts_with("frac_dec"),                        #shuusei20251118
                 names_to = "decile", values_to = "frac") %>%           #shuusei20251118
    mutate(decile = str_replace(decile, "frac_dec", "D")) %>%           #shuusei20251118
    mutate(decile = factor(decile,                                      #shuusei20251122
                           levels = paste0("D", 10:1)))                 #shuusei20251122
  
  print(ggplot(dec_df) + theme_bw() +                                   #shuusei20251118
          geom_line(aes(x = time_series, y = frac, color = decile)) +   #shuusei20251118
          ylab("Fraction of adopters by income decile") +               #shuusei20251118
          xlab("Date"))                                                 #shuusei20251118
  
  
  if(missing(save_name)){
    cat("\n", "Data not being saved!", "\n", sep = "")
  } else {
    write_rds(avg_u, paste(save_name, "_avg_u.rds", sep = ""))
    write_rds(cost, paste(save_name, "_cost.rds", sep = ""))
    write_rds(cost_priv, paste(save_name, "_cost_priv.rds", sep = ""))
    write_rds(LCOE_data, paste(save_name, "_LCOE_data.rds", sep = ""))
    write_rds(LCOE_avg, paste(save_name, "_LCOE_avg.rds", sep = ""))
    write_rds(FiT, paste(save_name, "_FiT.rds", sep = ""))
    if (run_w_cap == TRUE) write_rds(FiT_levels, paste(save_name, "_FiT_levels.rds", sep = ""))#
  }
  
  to_return <- list(avg_u, cost, cost_priv, LCOE_data, LCOE_avg, FiT)
  
  if (run_w_cap == TRUE) to_return <- list(avg_u, cost, cost_priv, LCOE_data, LCOE_avg, FiT, FiT_levels)
  
  return(to_return)
  
}

#------------------------------- Individual runs --------------------------------#

run_model_f <- function(agent_name, rn, w, threshold) {
  
  # Set up some parameters
  agent_index <- sample.int(10, 1)
  
  time_steps <- nrow(FiT) # number of months in time series
  
  # agents must be generated before run
  
  agents <- read_rds(paste('Data/', agent_name, "_", agent_index, ".rds", sep = ""))
  number_of_agents <- length(agents)
  
  
  incomes <- extract(agents, "income")                                   #shuusei20251118
  
  # 低所得FiTボーナス用の所得カットオフを計算                       #shuusei20251118
  if (exists("use_low_income_bonus") && isTRUE(use_low_income_bonus)) {  #shuusei20251118
    median_income <- median(incomes, na.rm = TRUE)                       #shuusei20251118
    low_income_cutoff <<- low_income_ratio * median_income               #shuusei20251118
  }                                                                      #shuusei20251118
  
  # 所得デシル（1〜10）を各エージェントに付与                      #shuusei20251118
  dec_breaks <- quantile(incomes, probs = seq(0, 1, 0.1), na.rm = TRUE)  #shuusei20251118
  dec_vals <- findInterval(incomes, dec_breaks, all.inside = TRUE)       #shuusei20251118
  for (k in seq_along(agents)) {                                         #shuusei20251118
    agents[[k]]$inc_decile <- dec_vals[k]                                #shuusei20251118
  }                                                                      #shuusei20251118
  
  ## デシル別の平均 meet_demand を計算（roof_limit 正規化用）          #shuusei20251129
  compute_meet_demand_ref_by_decile(agents)                              #shuusei20251129
  
  # initial reference capacity:
  
  adopters <- agents[map_chr(agents, "status") == "Y"]                   #shuusei20251212
  
  if (length(adopters) > 0){
    n_owners <<- owner_occupiers[[1, 2]]
    init_cap <- sum(extract(adopters, "inst_cap"), na.rm = TRUE)*n_owners/(1000*number_of_agents)
  }
  else{
    init_cap <- 0
  }
  
  # Set up data frame to put data in
  
  avg_u <- data.frame(time_series = FiT$time_series + months(1), 
                      run_number = as.factor(rep.int(rn, time_steps)),
                      mean_u_inc = vector(length = time_steps),
                      mean_u_ec = vector(length = time_steps),
                      mean_u_soc = vector(length = time_steps),
                      mean_u_cap = vector(length = time_steps),
                      mean_u_tot = vector(length = time_steps),
                      sd_u_inc = vector(length = time_steps),
                      sd_u_ec = vector(length = time_steps),
                      sd_u_soc = vector(length = time_steps),
                      sd_u_cap = vector(length = time_steps),
                      sd_u_tot = vector(length = time_steps),
                      frac_of_adopters = vector(length = time_steps),
                      avg_inst_cap = vector(length = time_steps),
                      tot_inst_cap = vector(length = time_steps),
                      frac_dec1  = vector(length = time_steps),          #shuusei20251118
                      frac_dec2  = vector(length = time_steps),          #shuusei20251118
                      frac_dec3  = vector(length = time_steps),          #shuusei20251118
                      frac_dec4  = vector(length = time_steps),          #shuusei20251118
                      frac_dec5  = vector(length = time_steps),          #shuusei20251118
                      frac_dec6  = vector(length = time_steps),          #shuusei20251118
                      frac_dec7  = vector(length = time_steps),          #shuusei20251118
                      frac_dec8  = vector(length = time_steps),          #shuusei20251118
                      frac_dec9  = vector(length = time_steps),          #shuusei20251118
                      frac_dec10 = vector(length = time_steps)           #shuusei20251118
  )
  
  
  #---------------------------------------------------------#
  
  # Time evolution! 
  
  if(run_w_cap == TRUE) {
    quarter_done <- 0 # how many quarters' capacity have already been used up?
    exceeded <- FALSE # has the total available capacity been exceeded?
  }
  
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
    k <- map_chr(agents, "status") == "Y"                               #shuusei20251212
    avg_u$frac_of_adopters[i] <- sum(k, na.rm = TRUE)/number_of_agents  #shuusei20251212
    avg_u$mean_u_ec[i] <- mean(extract(agents, "u_ec"))
    avg_u$mean_u_inc[i] <- mean(extract(agents, "u_inc"))
    avg_u$mean_u_soc[i] <- mean(extract(agents, "u_soc"))
    avg_u$mean_u_cap[i] <- mean(extract(agents, "u_cap"))
    avg_u$mean_u_tot[i] <- mean(extract(agents, "u_tot"))
    avg_u$sd_u_ec[i] <- sd(extract(agents, "u_ec"))
    avg_u$sd_u_inc[i] <- sd(extract(agents, "u_inc"))
    avg_u$sd_u_soc[i] <- sd(extract(agents, "u_soc"))
    avg_u$sd_u_cap[i] <- sd(extract(agents, "u_cap"))
    avg_u$sd_u_tot[i] <- sd(extract(agents, "u_tot"))
    
    if (length(adopters) > 0){
      avg_u$avg_inst_cap[i] <- mean(extract(adopters, "inst_cap"))
      avg_u$tot_inst_cap[i] <- sum(extract(adopters, "inst_cap"), na.rm = TRUE)*n_owners/(1000*number_of_agents)
    }
    else{
      avg_u$avg_inst_cap[i] <- NA
      avg_u$tot_inst_cap[i] <- 0
    }
    
    # デシル別導入率の計算                                         #shuusei20251118
    deciles <- extract(agents, "inc_decile")                           #shuusei20251118
    status  <- extract(agents, "status") == "Y"                        #shuusei20251118
    for (d in 1:10) {                                                  #shuusei20251118
      idx <- which(deciles == d)                                      #shuusei20251118
      if (length(idx) > 0) {                                          #shuusei20251118
        avg_u[i, paste0("frac_dec", d)] <- sum(status[idx]) / length(idx)  #shuusei20251118
      } else {                                                        #shuusei20251118
        avg_u[i, paste0("frac_dec", d)] <- NA                         #shuusei20251118
      }                                                               #shuusei20251118
    }                                                                 #shuusei20251118
    
    ##### Deployment cap code - only runs if using a deployment cap scenario
    if (run_w_cap == TRUE){
      
      
      if ((avg_u$tot_inst_cap[i] - init_cap) > sum(dep_cap$orig_cap) && exceeded == FALSE) { # total available capacity has been exceeded;
        # all further FiTs are zero
        FiT[(i+1):nrow(FiT), 2:4] <<- 0
        exceeded <- TRUE
      }
      
      if (exceeded == FALSE && i < nrow(FiT)) { # Not yet exceeded all the caps & not in the final time step
        
        which_q <- max(which(current_date >= dep_cap$q_dates)) # which quarter are we in?
        
        ref_cap <- avg_u$tot_inst_cap[avg_u$time_series == dep_cap$q_dates[which_q]]
        if(is_empty(ref_cap)) ref_cap <- init_cap
        
        current_quarter <- avg_u$tot_inst_cap[i] - ref_cap # how much capacity has been installed so far in the quarter?
        
        dep_cap$inst_cap[which_q] <<- current_quarter
        
        current_month <- avg_u$tot_inst_cap[i] - avg_u$tot_inst_cap[i-1] # capacity installed this month
        
        excess_cap <- dep_cap$cap[which_q] - current_quarter # how much capacity is left over in the current quarter?
        
        if (current_month > 0 && which_q < nrow(dep_cap) && quarter_done == which_q) {
          # some capacity has been installed this month, not in the final quarter, and we aren't in next q's capacity already
          remaining <- current_month # the remaining installed capacity which must be assigned to this or next month
          
          for (j in 1:(nrow(dep_cap)-which_q)) { # 
            
            if (dep_cap$cap[which_q + j] - remaining < 0) { # if we are exceeding the available capacity in a quarter
              
              current_FiT_index <- min(which(FiT$FiT[i] == FiT_list))
              if (current_FiT_index == length(FiT_list)) {
                FiT[(i+1):nrow(FiT), 2:3] <<- FiT_list[current_FiT_index]
              }
              else FiT[(i+1):nrow(FiT), 2:3] <<- FiT_list[current_FiT_index + 1]
              
              remaining <- remaining - dep_cap$cap[which_q + j]
              dep_cap$cap[which_q + j] <<- 0
            } else if (dep_cap$cap[which_q + j] - remaining >= 0) { # all the capacity has been allocated; break out of the j loop
              dep_cap$cap[which_q + j] <<- dep_cap$cap[which_q + j] - remaining
              break
            }
          }
        }
        
        if (excess_cap < 0 && which_q < nrow(dep_cap) && quarter_done != which_q) {
          # have exceeded the allocated capacity for this quarter
          remaining <- current_quarter - dep_cap$cap[which_q]
          
          quarter_done <- which_q
          for (j in 1:(nrow(dep_cap)-which_q)) {
            
            if (dep_cap$cap[which_q + j] - remaining < 0) {
              
              current_FiT_index <- min(which(FiT$FiT[i] == FiT_list))
              if (current_FiT_index == length(FiT_list)) {
                FiT[(i+1):nrow(FiT), 2:3] <<- FiT_list[current_FiT_index]
              }
              else FiT[(i+1):nrow(FiT), 2:3] <<- FiT_list[current_FiT_index + 1]
              
              remaining <- remaining - dep_cap$cap[which_q + j]
              dep_cap$cap[which_q + j] <<- 0
              
            } else if (dep_cap$cap[which_q + j] - remaining >= 0) {
              dep_cap$cap[which_q + j] <<- dep_cap$cap[which_q + j] - remaining
              break
            }
          }
        }
        
        if (current_date == dep_cap$q_dates[which_q] + months(2) && excess_cap > 0) {
          dep_cap$cap[which_q + 1] <<- dep_cap$cap[which_q + 1] + excess_cap
        }
        
      }
      
    }
    
    #### ^^^ End of deployment cap code
    
  }
  
  if (run_w_cap == TRUE && exceeded == TRUE) cat("Total available capacity was exceeded", "\n")
  
  FiT_outp <- cbind(FiT, run_number = rep(rn, nrow(FiT)))
  
  LCOE_data <- calc_LCOE_f(adopters, rn, number_of_agents)
  LCOE_avg <- LCOE_data[[1]]
  LCOE_data <- LCOE_data[[2]]
  cost_subs_res <- subs_cost_f(adopters, rn, number_of_agents)
  cost_priv_res <- priv_cost_f(adopters, rn, number_of_agents)
  tot_subs_cost <- cost_subs_res[[2]]
  ann_subs_cost <- cost_subs_res[[1]]
  
  tot_priv_cost <- cost_priv_res[[2]]
  cum_priv_cost <- cost_priv_res[[1]]
  all_results <- list(avg_u, ann_subs_cost, tot_subs_cost, cum_priv_cost, 
                      tot_priv_cost, LCOE_avg, LCOE_data)
  if (run_w_cap == TRUE) {
    all_results <- list(avg_u, ann_subs_cost, tot_subs_cost, cum_priv_cost, 
                        tot_priv_cost, LCOE_avg, LCOE_data, FiT_outp)
  }
  
  cat(length(adopters), "adopters in run", rn, "\n", sep = " ")
  
  
  return(all_results)
  
  
}