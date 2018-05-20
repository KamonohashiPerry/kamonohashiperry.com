library(bayesm)
library(dplyr)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

data("margarine")

#1,2,3,4,5,7の商品に関してデータを抽出し、家計IDごとにカウントし、5件以上のものに絞る。
hhid_selected <- margarine$choicePrice %>% 
                  filter(choice %in% c(1,2,3,4,5,7)) %>% 
                  group_by(hhid) %>% 
                  summarise(purc_cnt = n()) %>% 
                  filter(purc_cnt >= 5)

#今回扱う商品のカラムだけを抽出し、先ほど絞ったユーザーのリストに合致するデータでフィルターする。
choicePrice.selected <- margarine$choicePrice %>% 
                          filter(choice %in% c(1,2,3,4,5,7) & hhid %in% hhid_selected$hhid)
#並べにくいので7を6に置き換える。
choicePrice.selected$choice[choicePrice.selected$choice == 7] <- 6

#家計ごとに関する属性データの抽出
demos.selected <- margarine$demos %>% filter(hhid %in% hhid_selected$hhid)

#データサイズ
N <- nrow(choicePrice.selected)

#選択肢の数（特に使っているデータではない。）
p <- n_distinct(choicePrice.selected$choice)

#被説明変数
y <- choicePrice.selected$choice

#説明変数
X <- choicePrice.selected %>% select(3,4,5,6,7,9)

#家計の属性データから家計IDを除く
Z <- demos.selected %>% 
        select(-hhid)

#定数項を1列目に追加する
Z <- data.frame(intercept = rep(1, nrow(Z))) %>% 
        bind_cols(Z)

#家計の属性データから家計IDを抽出し、1から行数までのインデックスを付与する。
hhid_index <- demos.selected %>%
                select(hhid) %>% 
                mutate(ind = seq(1,nrow(demos.selected)))

#購買データの家計IDを抽出し、先ほどのインデックスとjoinする
hhid_x <- choicePrice.selected %>% 
            select(hhid) %>% 
            left_join(hhid_index)

#stanで扱うデータリストの作成
d.dat <- list(N_x=nrow(X), N_z=nrow(Z), 
              p_x=ncol(X), p_z=ncol(Z),
              y=y, X=X, Z=Z,
              hhid = hhid_x$ind)

#推定
d.fit <- stan("../Chapter5/Hierarchical_MNL.stan",
              data = d.dat,
              iter = 500,
              chains = 4)


draws <- extract(d.fit)
beta <- as.data.frame(draws$beta)
Delta <- as.data.frame(draws$Delta)
V_b <- as.data.frame(draws$V_b)


hhid_info <- inner_join(hhid_index, hhid_selected)

# Plot
par(mfrow = c(2,3))

# 任意の家計の番号が前半で.以降は変数の順番（家計は313、変数は6つある。）
# "PPk_Stk"  "PBB_Stk"  "PFl_Stk"  "PHse_Stk" "PGen_Stk" "PSS_Tub" 
hist(beta$`64.1`)
hist(beta$`64.2`)
hist(beta$`64.3`)
hist(beta$`64.4`)
hist(beta$`64.5`)
hist(beta$`64.6`)

par(mfrow = c(,1))
hist(V_b$`5.6`)
