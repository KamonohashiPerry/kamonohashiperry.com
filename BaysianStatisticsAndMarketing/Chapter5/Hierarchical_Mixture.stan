data{
  int<lower=0> N_x; // 購買レコードの数
  int<lower=0> N_z; // 家計の数
  int<lower=0> p_x; // 購買レコードの項目数
  int<lower=0> p_z; // 家計の属性データの項目数
  
  int y[N_x]; // 選択肢
  matrix[N_x, p_x] X; // 説明変数
  matrix[N_z, p_z] Z; // 家計の属性データ
  int<lower=0> hhid[N_x];  // 家計ID

  int K; // 混合ガウス分布の要素数
}

transformed data{
  real nu;
  matrix[p_x, p_x] I; // 購買レコードの説明変数の数の正方行列
  matrix[p_x, p_x] J; // 属性データの説明変数の数の正方行列
  
  nu = p_x + 3; // 説明変数の項に3を足す
  I = diag_matrix(rep_vector(1, p_x)); // 1を繰り返しp_x個並べた対角行列を作成
  J = diag_matrix(rep_vector(1, p_x));
}

parameters{
  vector[p_x] theta_ast[N_z]; // 説明変数の数だけある、購買ごとのパラメータ
  matrix[p_z, p_x] Delta; // 属性データの説明変数の数×購買データの説明変数の数だけのパラメータ
  vector[p_x] u[N_z]; // 購買レコードごとのパラメータ
  vector[p_x] mu[K]; // 混合分布を構成する平均値
  cov_matrix[p_x] Sigma[K]; // 共分散行列
  simplex[K] pi[N_z]; // シンプレックス（各要素が[0,1]の範囲で合計が1という条件を満たす。）
}

transformed parameters{
  vector[p_x] theta[N_z]; #家計の数だけの係数ベクトル

  for(i in 1:N_z){ // 家計の数だけ繰り返す
    theta[i] = theta_ast[i] + Delta' * Z[i]'; // 係数は家計属性ごとの特徴に異質なDeltaとbeta_astの和で決まる 
  }
}

model{
  real ps[K]; // 混合正規分布の対数

  for(i in 1:N_x){ // 購買レコードの数だけ繰り返す
    y[i] ~ categorical(softmax(theta[hhid[i]] .* to_vector(X[i]))); //カテゴリカル分布にsoftmaxを組み合わせて多項ロジスティック回帰を行う

    pi[hhid[i]] ~ dirichlet(rep_vector(2, K));
    for(k in 1:K){ // 混合分布の構成要素の数だけ繰り返す
      ps[k] = log(pi[hhid[i]][k]) + multi_normal_lpdf(theta_ast[hhid[i]] | mu[k], Sigma[k]);
    }
    target += log_sum_exp(ps); // 離散パラメータを消去した形で対数尤度を表現する際に必要な計算（周辺化消去）。  
  }

  for(k in 1:K){ // 混合パラメータの数だけ繰り返す
    mu[k] ~ multi_normal(rep_vector(0, p_x), 100*Sigma[k]);
    Sigma[k] ~ inv_wishart(nu, nu*I); 
  }

  for(i in 1:p_z){ // 家計の属性データの数だけ繰り返す
    Delta[i] ~ multi_normal_cholesky(rep_vector(0, p_x), 100*J); 
  }
}
