#run_model / summarise_results の修正を入れる（A, B の変更）。

#03-abc_quintile.R を R から source("03-abc_quintile.R") などで実行。

#Data/allowed_params_quintile.txt が生成されます。

#既存の batch_run_func() などでこの posterior を使いたい場合は

#一番手っ取り早いのは
#allowed_params_quintile.txt を allowed_params_1000.txt にリネームして
#上書きしてしまう方法です（バックアップは取っておく）。

#そうすると、元のコードのまま batch_run_func() などが
#「全国累積導入量＋SES 分布」を再現するパラメータ集合を使うようになります。

# To run any simulations, you need to do two things:
# 1. Load the relevant data using load_data() or load_data_f()
# 2. Run the simulation using batch_run_func() or batch_run_func_f()
# The "_f" means that these functions are used for projections (2016-2022),
# while the other functions are for historical simulations 2010-2016.


## Realistic historical

# Say we want to run the realistic historical scenario (e.g. the one that 
# is designed to mimic what actually happened in the Great Britain 2010-2016.)
# This is the default for the data loading function:
<<<<<<< HEAD
source("03-abc_quintile.R")
=======

rm(list = ls())
gc()
source("03-abc_quintile.R")

>>>>>>> a09381741b50bdc64a79b2d8ddd02b1dbb2c8e53
source('01-required_functions.R')
source('02-run_functions.R')

load_data()

# To run the simulation:

results <- batch_run_func(number_of_runs = 10,number_of_agents = 500)

# The default number of agents is 5000, and the default number of runs is 100. 
# So just running batch_run_func() does 100 runs with 5000 agents. 
# I've put number_of_agents = 500 and number_of_runs = 10 here to speed things up.

# batch_run_func will automatically plot data and output some key results. If you want to save your data:

batch_run_func(number_of_runs = 20, number_of_agents = 500, save_name = "testtt")


## Realistic future

# This is a projection based on UK policy as announced, again, this is the default. 
# However, we must first generate suitable agent populations. This is done using the function
# generate_populations_f():

generate_populations_f(n_agents = 500, n_pop = 10, dev = 200)

# This generates 5 populations of 500 agents, which deviate less than 200 MW from the capacity as it was in
# October 2016 (this isn't very good - but generating 100 populations of 5000 agents which deviate < 25 MW
# is extremely time-consuming!)

# Then you can run projections the same way as historical simulations:

load_data_f()

batch_run_func_f(number_of_runs = 4)

# 低所得世帯（中央値の80%以下）に2p/kWhの追加補助を行う例        #shuusei20251116
batch_run_func_f(agent_name      = "agents",              # 生成したエージェント名  #shuusei20251116
                 number_of_runs  = 10,                    # ラン数                    #shuusei20251116
                 low_inc_ratio   = 0.8,                   # 中央所得の80%以下を低所得 #shuusei20251116
<<<<<<< HEAD
                 extra_FiT_low_p = 0)                     # 2p/kWh 上乗せ            #shuusei20251116
=======
                 extra_FiT_low_p = 0,save_name = "testt")                     # 2p/kWh 上乗せ            #shuusei20251116
>>>>>>> a09381741b50bdc64a79b2d8ddd02b1dbb2c8e53

# In practice, you only need to do the time-consuming part (generating the agent populations) once,
# then use them to run whatever scenarios you're interested in.

#########################シナリオ

load_data_f( FiT_type  = "real_f_ext")
<<<<<<< HEAD


# 実データの average capacity (kW) の時系列だけ取り出す(10/1を見る)
deployment_avg <- deployment %>%
  dplyr::select(time_series, avg_cap)
deployment_avg

# 実データの累積導入量の時系列だけ取り出す(10/1を見る)
deployment_real <- deployment %>%
  dplyr::select(time_series, real_cap)
deployment_real
=======

#所得層別の導入者の平均容量
cutoff <- dmy("01oct2016")  # 2015-09-30 時点
averages %>%
  filter(time_series == cutoff) %>%
  select(time_series, avg_cap_Q1:avg_cap_Q5)



# 実データの average capacity (kW) の時系列だけ取り出す
deployment_avg <- deployment %>%
  dplyr::select(time_series, avg_cap)
deployment_avg

# 実データの累積導入量の時系列だけ取り出す
deployment_real <- deployment %>%
  dplyr::select(time_series, real_cap)
deployment_real


# 実データ：累積導入件数（installs）だけ取り出す
deployment_installs <- deployment %>%                      
  dplyr::select(time_series, real_installs)

##########################################



library(tidyverse)
library(lubridate)

# 既存の関数を使って、全インストールの生データを取得
all_inst_cap <- process_inst_data() %>%
  filter(technology_type == "Photovoltaic",
         installed_capacity <= 10,
         installationtype == "Domestic")

real_ts <- all_inst_cap %>%
  mutate(month = floor_date(commissioned_date, "month")) %>%  # 月の1日でまとめる
  group_by(month) %>%
  summarise(
    new_installs = n(),                          # その月の新規件数
    new_cap_kw   = sum(installed_capacity)       # その月の新規容量 [kW]
  ) %>%
  arrange(month) %>%
  ungroup()

real_ts <- real_ts %>%
  mutate(
    cum_installs = cumsum(new_installs),        # 累積件数
    cum_cap_kw   = cumsum(new_cap_kw),          # 累積容量 [kW]
    cum_cap_MW   = cum_cap_kw / 1000,           # 累積容量 [MW]
    avg_cap_kw   = cum_cap_kw / cum_installs    # 平均容量 [kW/件]
  ) %>%
  rename(time_series = month)

deployment_from_csv <- real_ts %>%
  transmute(
    time_series   = time_series,
    real_cap      = cum_cap_MW,   # 累積容量 [MW]
    real_installs = cum_installs, # 累積件数
    avg_cap       = avg_cap_kw    # 平均容量 [kW]
  )
deployment_from_csv %>%
  filter(time_series >= dmy("01mar2015"),
         time_series <= dmy("01mar2016")) %>%
  select(time_series,
         cum_cap_MW   = real_cap,
         cum_installs = real_installs,
         avg_cap_kw   = avg_cap)







>>>>>>> a09381741b50bdc64a79b2d8ddd02b1dbb2c8e53

