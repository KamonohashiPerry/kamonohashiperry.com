data{
  int<lower=0> N_x; // 購買レコードの数
  int<lower=0> N_z; // 家計の数
  int<lower=0> p_x; // 購買レコードの項目数
  int<lower=0> p_z; // 家計の属性データの項目数
  
  int y[N_x]; // 選択肢
  matrix[N_x, p_x] X; // 説明変数
  matrix[N_z, p_z] Z; // 家計の属性データ
  int<lower=0> hhid[N_x];  // 家計ID
}

transformed data{
  real nu;
  matrix[p_x, p_x] I; // 説明変数の数の正方行列
  
  nu = p_x + 3; // 説明変数の項に3を足す
  I = diag_matrix(rep_vector(1, p_x)); // 1を繰り返しp_x個並べた対角行列を作成
}

parameters{
  vector[p_x] beta_ast[N_z]; // 説明変数の数だけある、家計ごとのパラメータ
  matrix[p_z, p_x] Delta; // 属性データの説明変数の数×購買データの説明変数の数だけのパラメータ
  cov_matrix[p_x] V_b; // 共分散行列
}

transformed parameters{
  vector[p_x] beta[N_z];
  matrix[p_x, p_x] L_b;
  matrix[p_x, p_x] L_d;
  
  L_b = cholesky_decompose(V_b); // 共分散行列のコレスキー因子をもとめる
  L_d = cholesky_decompose(100*V_b); // 共分散行列に0.01で割ったもののコレスキー因子をもとめる
  for(i in 1:N_z){
    beta[i] = beta_ast[i] + Delta' * Z[i]'; // 係数は家計属性ごとの特徴に異質なDeltaとbeta_astの和で決まる 
  }
}

model{
  for(i in 1:N_x){
    y[i] ~ categorical(softmax(beta[hhid[i]] .* to_vector(X[i]))); //カテゴリカル分布にsoftmaxを組み合わせて多項ロジスティック回帰を行う
  }
  for(i in 1:p_z){
    Delta[i] ~ multi_normal_cholesky(rep_vector(0, p_x), L_d); // コレスキー因子（L_d）を引数にとる多変量正規分布(推定の高速化のために用いることがある。)
  }
  beta_ast ~ multi_normal_cholesky(rep_vector(0, p_x), L_b); // コレスキー因子（L_b）を引数にとる多変量正規分布
  V_b ~ inv_wishart(nu, nu*I); // 正規分布の共分散行列の共役事前分布として逆ウィシャート分布を利用
}
