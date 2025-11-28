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
rm(list = ls()); gc()
source("03-abc_quintile.R")


source('01-required_functions.R')
source('02-run_functions.R')

load_data()

# To run the simulation:

results <- batch_run_func(number_of_runs = 10, number_of_agents = 500)

#デシル別「導入世帯あたり平均導入量」の時系列
load_plot_sim_data("test")


# The default number of agents is 5000, and the default number of runs is 100. 
# So just running batch_run_func() does 100 runs with 5000 agents. 
# I've put number_of_agents = 500 and number_of_runs = 10 here to speed things up.

# batch_run_func will automatically plot data and output some key results. If you want to save your data:

batch_run_func(number_of_runs = 10, number_of_agents = 500, save_name = "test")


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
                 extra_FiT_low_p = 0)                     # 2p/kWh 上乗せ            #shuusei20251116

# In practice, you only need to do the time-consuming part (generating the agent populations) once,
# then use them to run whatever scenarios you're interested in.

#########################シナリオ

load_data_f( FiT_type  = "real_f_ext")



# 実データの average capacity (kW) の時系列だけ取り出す(10/1を見る)
deployment_avg <- deployment %>%
  dplyr::select(time_series, avg_cap)
deployment_avg

# 実データの累積導入量の時系列だけ取り出す(10/1を見る)
deployment_real <- deployment %>%
  dplyr::select(time_series, real_cap)
deployment_real





###############meet_demand と所得の関係
number_of_agents <- 500   # 好きな数でOK（500〜5000くらい）                        #shuusei20251127

agents <- rerun(number_of_agents,                                      #shuusei20251127
                Household_Agent("N", assign_income(),                  #shuusei20251127
                                assign_size(),                         #shuusei20251127
                                assign_region()))                      #shuusei20251127

n_links <- 10                                                          #shuusei20251127
mean_income <- mean(extract(agents, "income"))                         #shuusei20251127

agents <- agents %>%                                                   #shuusei20251127
  map(assign_LF) %>%                                                   #shuusei20251127
  map(assign_elec_cons) %>%                                            #shuusei20251127
  map(assign_u_inc, mean_inc = mean_income) %>%                        #shuusei20251127
  map(assign_soc_network, n_ag = number_of_agents, n_l = n_links)      #shuusei20251127


df_md <- export_meet_demand_vs_income(                                  #shuusei20251127
  agents,                                                                #shuusei20251127
  file_path = "Data/meet_demand_income_diag.csv"                         #shuusei20251127
)      

ggplot(df_md, aes(x = income, y = meet_demand)) +                       #shuusei20251127
  geom_point(alpha = 0.1) +                                             #shuusei20251127
  geom_smooth(method = "loess", se = FALSE) +                           #shuusei20251127
  scale_x_log10()        




##########################meet_demand と所得の関係をもっと詳しく見る。
library(tidyverse)

df_md <- readr::read_csv("Data/meet_demand_income_diag.csv")

## 所得デシル（1〜10）を付ける
df_md_dec <- df_md %>%
  mutate(
    decile = ntile(income, 10)   # 所得の順位で 10 分割
  )

## デシル別の平均 income と平均 meet_demand
mean_by_decile <- df_md_dec %>%
  group_by(decile) %>%
  summarise(
    n              = n(),
    mean_income    = mean(income),
    mean_meet_dmd  = mean(meet_demand)
  )

mean_by_decile


## デシルからクインタイルを作る
mean_by_Q <- mean_by_decile %>%
  mutate(
    Q = case_when(
      decile <= 2 ~ "Q1",
      decile <= 4 ~ "Q2",
      decile <= 6 ~ "Q3",
      decile <= 8 ~ "Q4",
      TRUE        ~ "Q5"
    )
  ) %>%
  group_by(Q) %>%
  summarise(
    n_deciles        = n(),
    mean_income_Q    = mean(mean_income),
    mean_meet_dmd_Q  = mean(mean_meet_dmd)
  ) %>%
  arrange(Q)

mean_by_Q



