library(readr)
library(dplyr)
library(survey)

root <- "C:/Users/ksr13/Downloads/UKDA-7362-tab/tab"

interview <- read_tsv(file.path(root, "derived", "interviewfs11.tab"), show_col_types = FALSE)
general   <- read_tsv(file.path(root, "derived", "generalfs11.tab"),   show_col_types = FALSE)

dat <- interview %>%
  select(aacode, accomhh, hhinc5x) %>%                      # ← hhinc5xを使う
  left_join(general %>% select(aacode, tenure4x, aagfh11),
            by = "aacode") %>%
  mutate(across(c(accomhh, hhinc5x, tenure4x, aagfh11),
                ~ ifelse(.x < 0, NA, .x)))                  # -9などをNA


owner <- dat %>%
  filter(tenure4x == 1)     # ラベル化していない場合は数値でOK


owner <- owner %>%
  mutate(
    accomhh = factor(accomhh, levels = 1:7,
                     labels = c("戸建て","セミ","テラス","目的別フラット","改装フラット","キャラバン/ボート","その他")),
    hhinc5x = factor(hhinc5x, levels = 1:5,
                     labels = paste0("Q", 1:5))
  )
des_owner <- svydesign(ids = ~1, weights = ~aagfh11, data = owner)


tab_w <- svytable(~hhinc5x + accomhh, des_owner)
tab_w


share_w <- prop.table(tab_w, margin = 1) * 100
share_w


p_type <- prop.table(svytable(~accomhh, des_owner))
p_type


# share_w は % なので比率に戻す
p_type_given_income <- prop.table(tab_w, margin = 1)    # 行内比率（0-1）

# P(type) は全体比率（0-1）
p_type_overall <- prop.table(svytable(~accomhh, des_owner))

# 係数 = (所得条件付き比率) / (全体比率)
pref_weight <- sweep(p_type_given_income, 2, p_type_overall, "/")
pref_weight



























# ---------------------------------------------
# 04-owner_income_by_housing_type_flat_merged.R
# 持ち家世帯：所得五分位(hhinc5x)×住宅タイプの選好係数
# Flat は「目的別フラット + 改装フラット」に統合（4分類）
# ---------------------------------------------

library(readr)
library(dplyr)
library(survey)

# 0) 読み込み
root <- "C:/Users/ksr13/Downloads/UKDA-7362-tab/tab"

interview <- read_tsv(file.path(root, "derived", "interviewfs11.tab"),
                      show_col_types = FALSE)
general   <- read_tsv(file.path(root, "derived", "generalfs11.tab"),
                      show_col_types = FALSE)

# 1) 必要列だけ取り出して結合（aacode）
dat <- interview %>%
  select(aacode, accomhh, hhinc5x) %>%
  left_join(general %>% select(aacode, tenure4x, aagfh11),
            by = "aacode") %>%
  mutate(across(c(accomhh, hhinc5x, tenure4x, aagfh11),
                ~ ifelse(.x < 0, NA, .x)))  # -9などをNAへ

# 2) 持ち家だけ抽出（tenure4x==1）
owner <- dat %>% filter(tenure4x == 1)

# 3) ラベル化（見やすく）
owner <- owner %>%
  mutate(
    accomhh = factor(accomhh, levels = 1:7,
                     labels = c("戸建て","セミ","テラス",
                                "目的別フラット","改装フラット",
                                "キャラバン/ボート","その他")),
    hhinc5x = factor(hhinc5x, levels = 1:5,
                     labels = paste0("Q", 1:5))
  )

# 4) survey design（ここで重み aagfh11 を使う）
des_owner <- svydesign(ids = ~1, weights = ~aagfh11, data = owner)

# 5) (参考) 7分類のクロス表と条件付き割合
tab_w7 <- svytable(~hhinc5x + accomhh, des_owner)
share_w7 <- prop.table(tab_w7, margin = 1) * 100

# 6) 4分類（Detached/Semi/Terraced/Flat）に統合するため、
#    まず確率（比率）を作る：P(type|Q), P(type)
p_q_type7 <- prop.table(tab_w7, margin = 1)                 # P(type | Q)
p_type7   <- prop.table(svytable(~accomhh, des_owner))       # P(type)

# 7) Flat統合（目的別フラット + 改装フラット）
#    ※キャラバン/ボート・その他は4分類に含めない（ここでは除外）
p_q_4 <- cbind(
  Detached = p_q_type7[, "戸建て"],
  Semi     = p_q_type7[, "セミ"],
  Terraced = p_q_type7[, "テラス"],
  Flat     = p_q_type7[, "目的別フラット"] + p_q_type7[, "改装フラット"]
)

p_4 <- c(
  Detached = as.numeric(p_type7["戸建て"]),
  Semi     = as.numeric(p_type7["セミ"]),
  Terraced = as.numeric(p_type7["テラス"]),
  Flat     = as.numeric(p_type7["目的別フラット"] + p_type7["改装フラット"])
)

# 8) 4分類の選好係数：W(type,Q)=P(type|Q)/P(type)
pref_weight4 <- sweep(p_q_4, 2, p_4, "/")

# 9) 出力（確認）
cat("\n--- (参考) 7分類：所得×住宅タイプの割合(%) ---\n")
print(round(share_w7, 3))

cat("\n--- 4分類：全体の住宅タイプ比率 P(type) ---\n")
print(round(p_4, 6))

cat("\n--- 4分類：所得条件付き比率 P(type|Q) ---\n")
print(round(p_q_4, 6))

cat("\n--- 4分類：選好係数 Preference Weight ---\n")
print(round(pref_weight4, 6))

# 10) Excelに保存したい場合（任意：コメント外す）
# library(writexl)
# out <- as.data.frame(pref_weight4)
# out$hhinc5x <- rownames(out)
# out <- out %>% select(hhinc5x, everything())
# write_xlsx(out, "pref_weight_4class_flat_merged.xlsx")






























library(dplyr)
library(tidyr)

# -----------------------------
# 1) 表A：地域×住宅タイプ割合（Census 2011）
#    ※数値はあなたが貼ったものをそのまま入れています
# -----------------------------
A <- tribble(
  ~region, ~Detached, ~Semi, ~Terraced, ~Flat,
  "East", 0.389415814, 0.326108451, 0.213548874, 0.070926861,
  "East Midlands", 0.431202516, 0.362936720, 0.176238476, 0.029622288,
  "London", 0.091377760, 0.284691794, 0.326860345, 0.297070101,
  "North East", 0.218466670, 0.439642917, 0.287271967, 0.054618446,
  "North West", 0.247379239, 0.425638018, 0.279558444, 0.047424299,
  "South East", 0.373683248, 0.304475372, 0.219308314, 0.102533066,
  "South West", 0.394959050, 0.293873869, 0.227123182, 0.084043899,
  "Wales", 0.362327177, 0.330250360, 0.270091606, 0.037330857,
  "West Midlands", 0.323772007, 0.408346872, 0.216380913, 0.051500209,
  "Yorkshire and The Humber", 0.285765747, 0.414080964, 0.260926700, 0.039226589,
  "Scotland", 0.313673278, 0.265697974, 0.195436258, 0.225192490
)

# -----------------------------
# 2) 表B：所得×選好係数（EHS 2011由来）
# -----------------------------
B <- tribble(
  ~hhinc5x, ~Detached, ~Semi, ~Terraced, ~Flat,
  "Q1", 0.738126, 0.989998, 1.192844, 1.330805,
  "Q2", 0.678729, 1.100830, 1.183568, 1.203992,
  "Q3", 0.841554, 1.092286, 1.030766, 1.116215,
  "Q4", 1.022323, 1.078786, 0.985131, 0.634764,
  "Q5", 1.413988, 0.801465, 0.792868, 0.961509
)

# -----------------------------
# 3) long形式へ（掛け算しやすくする）
# -----------------------------
A_long <- A %>%
  pivot_longer(cols = c(Detached, Semi, Terraced, Flat),
               names_to = "type", values_to = "base")

B_long <- B %>%
  pivot_longer(cols = c(Detached, Semi, Terraced, Flat),
               names_to = "type", values_to = "weight")

# -----------------------------
# 4) スコア = base × weight
#    正規化 = score / sum(score)  （region×hhinc5xごとに合計1）
# -----------------------------
target_long <- A_long %>%
  inner_join(B_long, by = "type") %>%
  mutate(score = base * weight) %>%
  group_by(region, hhinc5x) %>%
  mutate(prob = score / sum(score)) %>%
  ungroup()

# wide形式（見やすい）
target_wide <- target_long %>%
  select(region, hhinc5x, type, prob) %>%
  pivot_wider(names_from = type, values_from = prob) %>%
  arrange(region, hhinc5x)

# -----------------------------
# 5) 出力
# -----------------------------
target_wide

# 例：London×Q5だけ確認したい場合
target_wide %>% filter(region == "London", hhinc5x == "Q5")

# 各行がちゃんと合計1になっているかチェック
check_sum <- target_wide %>%
  mutate(rowsum = Detached + Semi + Terraced + Flat) %>%
  select(region, hhinc5x, rowsum)

check_sum

write.csv(target_wide, "target_distribution_region_income.csv", row.names = FALSE)
