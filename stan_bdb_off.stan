data {
int<lower=1> N; //number of observations
vector[N] y; //response
int<lower=1> n_teams; //number of teams - 32
int<lower=1, upper=n_teams> team[N]; //vector of team ids 
int<lower=1, upper=n_teams> off_team[N]; //vector of offensive team ids 
int<lower=1> n_coverages; //number of coverages - 7
int<lower=1, upper=n_coverages> coverage[N]; //vector of coverage indices

//prior means for betas - going to be zero
//vector[n_teams] team_mean;
//vector[n_coverages] coverage_mean;
}

parameters {
 
  // population mean parameters
  real mu; //population mean
  vector[n_coverages] beta_coverage; // coverage effects
  vector[n_teams] gamma_defense; // team effects
  vector[n_teams] xi_offense; // offensive team effects
  matrix[n_teams, n_coverages] delta_defense_coverage; // defense-coverage effects
  
  // population variance/correlation parameters
  
  //corr_matrix[n_coverages] Omega_coverage; // correlation matrix for coverages
  //corr_matrix[n_teams] Omega_team; // correlation matrix for teams
  //corr_matrix[n_teams] Omega_off_team; // correlation matrix for offensive teams
  real<lower=0> sigma_coverage; //variance of coverage effects
  real<lower=0> sigma_defense; //variance of team effects
  real<lower=0> sigma_offense; //variance of offensive team effects
  real<lower=0> sigma_defense_coverage; //variance of def-coverage effects
  real<lower=0> sigma_noise; // random noise

}
transformed parameters {
  vector[N] mu_vec;
  
  // stick means in a vector
  for(i in 1:N) {
    mu_vec[i] = mu + beta_coverage[coverage[i]] + gamma_defense[team[i]] + xi_offense[off_team[i]] + delta_defense_coverage[team[i], coverage[i]];
  }
}
model {
 
  y ~ normal(mu_vec, sigma_noise);

  //overall mean
  mu ~ normal(0, 0.7);
  
  //unscaled beta vectors
  //to_vector(beta_coverage_raw) ~ normal(0, 1);
  //to_vector(beta_team_raw) ~ normal(0, 1);
  //beta_coverage ~ multi_normal(coverage_mean, quad_form_diag(Omega_coverage, sigma_coverage));
  //beta_team ~ multi_normal(team_mean, quad_form_diag(Omega_team, sigma_team));
  beta_coverage ~ normal(0, sigma_coverage);
  gamma_defense ~ normal(0, sigma_defense);
  xi_offense ~ normal(0, sigma_offense);
  to_vector(delta_defense_coverage) ~ normal(0, sigma_defense_coverage);
  
  //correlation matrices
  //Omega_coverage ~ lkj_corr(2);
  //Omega_team ~ lkj_corr(2);
 
  //variances
  sigma_coverage ~ gamma(2, 4);
  sigma_defense ~ gamma(2, 4);
  sigma_offense ~ gamma(2, 4);
  sigma_defense_coverage ~ gamma(2, 4);
  sigma_noise ~ gamma(1, 1);
 
}

generated quantities {
matrix[n_teams, n_coverages] team_coverage_effects;

for(i in 1:n_teams) {
 for(j in 1:n_coverages) {
  team_coverage_effects[i,j] = mu + beta_coverage[j] + gamma_defense[i] + delta_defense_coverage[i,j];
 }
};
}
