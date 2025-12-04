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

## 1 行目・2 行目…と「順番に」使う
results_seq <- batch_run_func(number_of_runs    = 5,
                              number_of_agents  = 500,
                              use_random_params = FALSE,
                              save_name = "test")

#クインタイル別に
#「inst_cap_budget が効いた割合」と「roof_limit（需要×roof factor）が効いた割合」
#「raw inst_cap > 4kW の世帯のうち、4kW を採用した割合」と「大容量を採用した割合」
plot_cap_constraints_by_quintile(dmy("01oct2016"))
plot_budget_roof_by_quintile(dmy("01oct2016"))

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






## ============================================================
##  クインタイル別：所得×シェアの inst_cap_budget vs meet_demand
## ============================================================

library(tidyverse)
library(lubridate)

## FiTABM本体の関数・データ読み込み ------------------------------
source("01-required_functions.R")
source("02-run_functions.R")

## kW_price, LF, 電力消費分布などを読み込む
load_data()   # すでに呼んでいれば省略してOK

## 1. 仮想エージェントを生成し、meet_demand と income を出す -----

number_of_agents <- 500   # 好きな数でOK（500〜5000くらい）

agents <- rerun(
  number_of_agents,
  Household_Agent("N",
                  assign_income(),   # 所得をサンプリング
                  assign_size(),     # 世帯人数をサンプリング
                  assign_region())   # 地域をサンプリング
)

n_links     <- 10
mean_income <- mean(extract(agents, "income"))

agents <- agents %>%
  map(assign_LF) %>%                                        # 地域別 LF を付与
  map(assign_elec_cons) %>%                                 # 所得×世帯人数に応じた電力消費
  map(assign_u_inc, mean_inc = mean_income) %>%             # 所得効用
  map(assign_soc_network, n_ag = number_of_agents, n_l = n_links)

## income, LF, consumption から meet_demand を計算し、CSV にも保存
df_md <- export_meet_demand_vs_income(
  agents,
  file_path = "Data/meet_demand_income_diag.csv"
)
# df_md には columns: income, LF, consumption, meet_demand が入っている

## 2. クインタイル別に「所得×シェアの予算」と meet_demand を計算 -----

## どの日の PV 価格で「所得×シェア」を見るか（ここでは 2015-10-01）
d_ref <- dmy("01oct2016")

kW_price_ref <- kW_price %>%
  filter(X1 == d_ref) %>%
  pull(X2)

if (length(kW_price_ref) != 1) {
  stop("基準日の kW_price_ref が一意に決まりません。日付（d_ref）を確認してください。")
}

## クインタイル付与 & inst_cap_budget を計算
df_q <- df_md %>%
  mutate(
    # 所得クインタイル（1 = 低所得, 5 = 高所得）
    quintile = ntile(income, 5),
    
    # ★ここでクインタイルごとの「所得に対する割合」を手動で指定
    #   例：Q1〜Q5 = 30%, 27%, 24%, 21%, 18%
    budget_share = case_when(
      quintile == 1 ~ 0.30,  # Q1: 所得の 30%
      quintile == 2 ~ 0.30,  # Q2: 所得の 27%
      quintile == 3 ~ 0.30,  # Q3: 所得の 24%
      quintile == 4 ~ 0.30,  # Q4: 所得の 21%
      quintile == 5 ~ 0.30,  # Q5: 所得の 18%
    ),
    
    # 所得 ×（クインタイル別シェア）を PV に使った場合の「予算ベース容量」(kW)
    inst_cap_budget = budget_share * income / kW_price_ref
  )

## クインタイル別の平均値（確認用の表）
summary_Q <- df_q %>%
  group_by(quintile) %>%
  summarise(
    n                      = n(),
    mean_income_Q          = mean(income,      na.rm = TRUE),
    mean_meet_demand_Q     = mean(meet_demand, na.rm = TRUE),
    mean_budget_share_Q    = mean(budget_share, na.rm = TRUE),
    mean_inst_cap_budget_Q = mean(inst_cap_budget, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(quintile)

print(summary_Q)
# → Q1〜Q5ごとの平均所得 / 平均 meet_demand / 平均シェア / 平均 inst_cap_budget が出る

## 3. グラフ作成：
##    x軸 = クインタイル（Q1〜Q5）
##    点  = 各世帯の meet_demand（散布図）
##    線  = クインタイル別 平均 inst_cap_budget（折れ線）

ggplot() +
  # 各世帯の meet_demand（散布図・横方向に少しズラす）
  geom_jitter(
    data = df_q,
    aes(x = quintile, y = meet_demand),
    width = 0.15, height = 0, alpha = 0.2
  ) +
  # クインタイル別 平均 inst_cap_budget（折れ線）
  geom_line(
    data = summary_Q,
    aes(x = quintile, y = mean_inst_cap_budget_Q, group = 1),
    linewidth = 1
  ) +
  geom_point(
    data = summary_Q,
    aes(x = quintile, y = mean_inst_cap_budget_Q),
    size = 2
  ) +
  # ★クインタイル別 平均 meet_demand（折れ線＋点）を追加
  geom_line(
    data = summary_Q,
    aes(x = quintile, y = mean_meet_demand_Q, group = 1),
    linewidth = 1,
    linetype = "dashed"   # 見分けやすく点線に
  ) +
  geom_point(
    data = summary_Q,
    aes(x = quintile, y = mean_meet_demand_Q),
    size = 2,
    shape = 17            # 形を変えて区別（▲）
  ) +
  scale_x_continuous(
    breaks = 1:5,
    labels = paste0("Q", 1:5)
  ) +
  xlab("Income quintile") +
  ylab("kW") +
  ggtitle("Income (quintile-specific share) budget vs meet_demand") +
  theme_bw()

