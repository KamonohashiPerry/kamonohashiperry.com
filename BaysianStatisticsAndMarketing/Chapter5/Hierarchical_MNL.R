library(bayesm)
library(dplyr)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

data("margarine")

hhid_selected <- margarine$choicePrice %>% 
                  filter(choice %in% c(1,2,3,4,5,7)) %>% 
                  group_by(hhid) %>% 
                  summarise(purc_cnt = n()) %>% 
                  filter(purc_cnt >= 5)

choicePrice.selected <- margarine$choicePrice %>% 
                          filter(choice %in% c(1,2,3,4,5,7) & hhid %in% hhid_selected$hhid)
choicePrice.selected$choice[choicePrice.selected$choice == 7] <- 6

demos.selected <- margarine$demos %>% filter(hhid %in% hhid_selected$hhid)

N <- nrow(choicePrice.selected)
p <- n_distinct(choicePrice.selected$choice)

y <- choicePrice.selected$choice

X <- choicePrice.selected %>% select(3,4,5,6,7,9)

Z <- demos.selected %>% 
        select(-hhid)
Z <- data.frame(intercept = rep(1, nrow(Z))) %>% 
        bind_cols(Z)

hhid_index <- demos.selected %>%
                select(hhid) %>% 
                mutate(ind = seq(1,nrow(demos.selected)))

hhid_x <- choicePrice.selected %>% 
            select(hhid) %>% 
            left_join(hhid_index)


d.dat <- list(N_x=nrow(X), N_z=nrow(Z), 
              p_x=ncol(X), p_z=ncol(Z),
              y=y, X=X, Z=Z,
              hhid = hhid_x$ind)

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
