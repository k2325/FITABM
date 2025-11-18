# To run any simulations, you need to do two things:
# 1. Load the relevant data using load_data() or load_data_f()
# 2. Run the simulation using batch_run_func() or batch_run_func_f()
# The "_f" means that these functions are used for projections (2016-2022),
# while the other functions are for historical simulations 2010-2016.


## Realistic historical

# Say we want to run the realistic historical scenario (e.g. the one that 
# is designed to mimic what actually happened in the Great Britain 2010-2016.)
# This is the default for the data loading function:
source('01-required_functions.R')
source('02-run_functions.R')

load_data()

# To run the simulation:

results <- batch_run_func(number_of_runs = 10, number_of_agents = 500)

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

batch_run_func_f(number_of_runs = 10)

# 低所得世帯（中央値の80%以下）に2p/kWhの追加補助を行う例        #shuusei20251116
batch_run_func_f(agent_name      = "agents",              # 生成したエージェント名  #shuusei20251116
                 number_of_runs  = 5,                    # ラン数                    #shuusei20251116
                 low_inc_ratio   = 0.8,                   # 中央所得の80%以下を低所得 #shuusei20251116
                 extra_FiT_low_p = 2)                     # 2p/kWh 上乗せ            #shuusei20251116

# In practice, you only need to do the time-consuming part (generating the agent populations) once,
# then use them to run whatever scenarios you're interested in.

#########################シナリオ
source("01-required_functions.R")
source("02-run_functions.R")

# 極端に高い FiT（一定）
load_data_f(
  FiT_type  = "linear",   # 線形だが init = final にすると一定になる
  init_fit  = 0,         # 20 p/kWh
  final_fit = 0,         # ずっと 20 p/kWh
  exp_tar   = 0,       # エクスポートタリフ（好きに変更可）
  dep_caps  = FALSE       # 上限容量なしにしたければ FALSE
)

res_high <- batch_run_func_f(number_of_runs = 5, save_name = "high_FiT")

