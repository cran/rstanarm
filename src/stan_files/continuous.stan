#include /pre/Columbia_copyright.stan
#include /pre/license.stan

// GLM for a Gaussian, Gamma, inverse Gaussian, or Beta outcome
functions {
#include /functions/common_functions.stan
#include /functions/continuous_likelihoods.stan
#include /functions/SSfunctions.stan

  /** 
  * test function for csr_matrix_times_vector
  *
  * @param m Integer number of rows
  * @param n Integer number of columns
  * @param w Vector (see reference manual)
  * @param v Integer array (see reference manual)
  * @param u Integer array (see reference manual)
  * @param b Vector that is multiplied from the left by the CSR matrix
  * @return A vector that is the product of the CSR matrix and b
  */
  vector test_csr_matrix_times_vector(int m, int n, vector w, 
                                      int[] v, int[] u, vector b) {
    return csr_matrix_times_vector(m, n, w, v, u, b); 
  }
}
data {
  // declares N, K, X, xbar, dense_X, nnz_x, w_x, v_x, u_x
#include /data/NKX.stan
  int<lower=0> len_y;      // length of y
  real lb_y; // lower bound on y
  real<lower=lb_y> ub_y; // upper bound on y
  vector<lower=lb_y, upper=ub_y>[len_y] y; // continuous outcome
  int<lower=1,upper=4> family; // 1 gaussian, 2 gamma, 3 inv-gaussian, 4 beta
  // declares prior_PD, has_intercept, link, prior_dist, prior_dist_for_intercept
#include /data/data_glm.stan
  // declares has_weights, weights, has_offset, offset
#include /data/weights_offset.stan
  // declares prior_{mean, scale, df}, prior_{mean, scale, df}_for_intercept, prior_{mean, scale, df}_for_aux
#include /data/hyperparameters.stan
  // declares t, p[t], l[t], q, len_theta_L, shape, scale, {len_}concentration, {len_}regularization
#include /data/glmer_stuff.stan
  // declares num_not_zero, w, v, u
#include /data/glmer_stuff2.stan
#include /data/data_betareg.stan

  int<lower=0,upper=10> SSfun; // nonlinear function indicator, 0 for identity
  vector[SSfun > 0  ? len_y : 0] input;
  vector[SSfun == 5 ? len_y : 0] Dose;
}
transformed data {
  vector[family == 3 ? len_y : 0] sqrt_y;
  vector[family == 3 ? len_y : 0] log_y;
  real sum_log_y = family == 1 ? not_a_number() : sum(log(y));
  int<lower=1> V[special_case ? t : 0, len_y] = make_V(len_y, special_case ? t : 0, v);
  int<lower=0> hs_z;                  // for tdata_betareg.stan
  // defines hs, len_z_T, len_var_group, delta, is_continuous, pos
#include /tdata/tdata_glm.stan
  // defines hs_z
#include /tdata/tdata_betareg.stan
  is_continuous = 1;

  if (family == 3) {
    sqrt_y = sqrt(y);
    log_y = log(y);
  }
}
parameters {
  real<lower=make_lower(family, link),upper=make_upper(family,link)> gamma[has_intercept];
  // declares z_beta, global, local, z_b, z_T, rho, zeta, tau
#include /parameters/parameters_glm.stan
  real<lower=0> aux_unscaled; // interpretation depends on family!
#include /parameters/parameters_betareg.stan
}
transformed parameters {
  // aux has to be defined first in the hs case
  real aux = prior_dist_for_aux == 0 ? aux_unscaled : (prior_dist_for_aux <= 2 ? 
             prior_scale_for_aux * aux_unscaled + prior_mean_for_aux :
             prior_scale_for_aux * aux_unscaled);

  vector[z_dim] omega; // used in tparameters_betareg.stan
  // defines beta, b, theta_L
#include /tparameters/tparameters_glm.stan
#include /tparameters/tparameters_betareg.stan
  
  if (prior_dist_for_aux == 0) // none
    aux = aux_unscaled;
  else {
    aux = prior_scale_for_aux * aux_unscaled;
    if (prior_dist_for_aux <= 2) // normal or student_t
      aux = aux + prior_mean_for_aux;
  }

  if (t > 0) {
    if (special_case == 1) {
      int start = 1;
      theta_L = scale .* tau * aux;
      if (t == 1) b = theta_L[1] * z_b;
      else for (i in 1:t) {
        int end = start + l[i] - 1;
        b[start:end] = theta_L[i] * z_b[start:end];
        start = end + 1;
      }
    }
    else {
      theta_L = make_theta_L(len_theta_L, p, 
                             aux, tau, scale, zeta, rho, z_T);
      b = make_b(z_b, theta_L, p, l);
    }
  }
}
model {
  vector[N] eta_z; // beta regression linear predictor for phi
#include /model/make_eta.stan
  if (t > 0) {
#include /model/eta_add_Zb.stan
  }
  if (has_intercept == 1) {
    if ((family == 1 || link == 2) || (family == 4 && link != 5)) eta = eta + gamma[1];
    else if (family == 4 && link == 5) eta = eta - max(eta) + gamma[1];
    else eta = eta - min(eta) + gamma[1];
  }
  else {
#include /model/eta_no_intercept.stan
  }

  if (SSfun > 0) { // nlmer
    matrix[len_y, K] P;
    P = reshape_vec(eta, len_y, K);
    if (SSfun < 5) {
      if (SSfun <= 2) {
        if (SSfun == 1) target += normal_lpdf(y | SS_asymp(input, P), aux);
        else target += normal_lpdf(y | SS_asympOff(input, P), aux);
      }
      else if (SSfun == 3) target += normal_lpdf(y | SS_asympOrig(input, P), aux);
      else {
        for (i in 1:len_y) P[i,1] = P[i,1] + exp(P[i,3]); // ordering constraint
        target += normal_lpdf(y | SS_biexp(input, P), aux);
      }
    }
    else {
      if (SSfun <= 7) {
        if (SSfun == 5) target += normal_lpdf(y | SS_fol(Dose, input, P), aux);
        else if (SSfun == 6) target += normal_lpdf(y | SS_fpl(input, P), aux);
        else target += normal_lpdf(y | SS_gompertz(input, P), aux);
      }
      else {
        if (SSfun == 8) target += normal_lpdf(y | SS_logis(input, P), aux);
        else if (SSfun == 9) target += normal_lpdf(y | SS_micmen(input, P), aux);
        else target += normal_lpdf(y | SS_weibull(input, P), aux);
      }
    }
  }
  else if (has_weights == 0 && prior_PD == 0) { // unweighted log-likelihoods
#include /model/make_eta_z.stan
    // adjust eta_z according to links
    if (has_intercept_z == 1) {
      if (link_phi > 1) {
        eta_z = eta_z - min(eta_z) + gamma_z[1];
      }
      else {
        eta_z = eta_z + gamma_z[1];
      }
    }
    else { // has_intercept_z == 0
#include /model/eta_z_no_intercept.stan
    }
    if (family == 1) {
      if (link == 1) 
        target += normal_lpdf(y | eta, aux);
      else if (link == 2) 
        target += normal_lpdf(y | exp(eta), aux);
      else 
        target += normal_lpdf(y | inv(eta), aux);
    }
    else if (family == 2) {
      target += GammaReg(y, eta, aux, link, sum_log_y);
    }
    else if (family == 3) {
      target += inv_gaussian(y, linkinv_inv_gaussian(eta, link), 
                             aux, sum_log_y, sqrt_y);
    }
    else if (family == 4 && link_phi == 0) {
      vector[N] mu;
      mu = linkinv_beta(eta, link);
      target += beta_lpdf(y | mu * aux, (1 - mu) * aux);
    }
    else if (family == 4 && link_phi > 0) {
      vector[N] mu;
      vector[N] mu_z;
      mu = linkinv_beta(eta, link);
      mu_z = linkinv_beta_z(eta_z, link_phi);
      target += beta_lpdf(y | rows_dot_product(mu, mu_z), 
                          rows_dot_product((1 - mu) , mu_z));
    }
  }
  else if (prior_PD == 0) { // weighted log-likelihoods
    vector[N] summands;
    if (family == 1) summands = pw_gauss(y, eta, aux, link);
    else if (family == 2) summands = pw_gamma(y, eta, aux, link);
    else if (family == 3) summands = pw_inv_gaussian(y, eta, aux, link, log_y, sqrt_y);
    else if (family == 4 && link_phi == 0) summands = pw_beta(y, eta, aux, link);
    else if (family == 4 && link_phi > 0) summands = pw_beta_z(y, eta, eta_z, link, link_phi);
    target += dot_product(weights, summands);
  }

  // Log-priors
  if (prior_dist_for_aux > 0 && prior_scale_for_aux > 0) {
    real log_half = -0.693147180559945286;
    if (prior_dist_for_aux == 1)
      target += normal_lpdf(aux_unscaled | 0, 1) - log_half;
    else if (prior_dist_for_aux == 2)
      target += student_t_lpdf(aux_unscaled | prior_df_for_aux, 0, 1) - log_half;
    else 
     target += exponential_lpdf(aux_unscaled | 1);
  }
    
#include /model/priors_glm.stan
#include /model/priors_betareg.stan
  if (t > 0) decov_lp(z_b, z_T, rho, zeta, tau, 
                      regularization, delta, shape, t, p);
}
generated quantities {
  real alpha[has_intercept];
  real omega_int[has_intercept_z];
  real mean_PPD = 0;
  if (has_intercept == 1) {
    if (dense_X) alpha[1] = gamma[1] - dot_product(xbar, beta);
    else alpha[1] = gamma[1];
  }
  if (has_intercept_z == 1) { 
    omega_int[1] = gamma_z[1] - dot_product(zbar, omega);  // adjust betareg intercept 
  }
  {
    vector[N] eta_z;
#include /model/make_eta.stan
    if (t > 0) {
#include /model/eta_add_Zb.stan
    }
    if (has_intercept == 1) {
      if (make_lower(family,link) == negative_infinity() &&
          make_upper(family,link) == positive_infinity()) eta = eta + gamma[1];
      else if (family == 4 && link == 5) {
        real max_eta = max(eta);
        alpha[1] = alpha[1] - max_eta;
        eta = eta - max_eta + gamma[1];
      }
      else {
        real min_eta = min(eta);
        alpha[1] = alpha[1] - min_eta;
        eta = eta - min_eta + gamma[1];
      }
    }
    else {
#include /model/eta_no_intercept.stan
    }
#include /model/make_eta_z.stan
    // adjust eta_z according to links
    if (has_intercept_z == 1) {
      if (link_phi > 1) {
        omega_int[1] = omega_int[1] - min(eta_z);
        eta_z = eta_z - min(eta_z) + gamma_z[1];
      }
      else {
        eta_z = eta_z + gamma_z[1];
      }
    }
    else { // has_intercept_z == 0
#include /model/eta_z_no_intercept.stan
    }
    
    if (SSfun > 0) { // nlmer
      vector[len_y] eta_nlmer;
      matrix[len_y, K] P;      
      P = reshape_vec(eta, len_y, K);
      if (SSfun < 5) {
        if (SSfun <= 2) {
          if (SSfun == 1) eta_nlmer = SS_asymp(input, P);
          else eta_nlmer = SS_asympOff(input, P);
        }
        else if (SSfun == 3) eta_nlmer = SS_asympOrig(input, P);
        else eta_nlmer = SS_biexp(input, P);
      }
      else {
        if (SSfun <= 7) {
          if (SSfun == 5) eta_nlmer = SS_fol(Dose, input, P);
          else if (SSfun == 6) eta_nlmer = SS_fpl(input, P);
          else eta_nlmer = SS_gompertz(input, P);
        }
        else {
          if (SSfun == 8) eta_nlmer = SS_logis(input, P);
          else if (SSfun == 9) eta_nlmer = SS_micmen(input, P);
          else eta_nlmer = SS_weibull(input, P);
        }
      }
      for (n in 1:len_y) mean_PPD = mean_PPD + normal_rng(eta_nlmer[n], aux);
    }
    else if (family == 1) {
      if (link > 1) eta = linkinv_gauss(eta, link);
      for (n in 1:len_y) mean_PPD = mean_PPD + normal_rng(eta[n], aux);
    }
    else if (family == 2) {
      if (link > 1) eta = linkinv_gamma(eta, link);
      for (n in 1:len_y) mean_PPD = mean_PPD + gamma_rng(aux, aux / eta[n]);
    }
    else if (family == 3) {
      if (link > 1) eta = linkinv_inv_gaussian(eta, link);
      for (n in 1:len_y) mean_PPD = mean_PPD + inv_gaussian_rng(eta[n], aux);
    }
    else if (family == 4 && link_phi == 0) { 
      eta = linkinv_beta(eta, link);
      for (n in 1:N) {
        real eta_n = eta[n];
        if (aux <= 0) mean_PPD = mean_PPD + bernoulli_rng(0.5);
        else if (eta_n >= 1) mean_PPD = mean_PPD + 1;
        else if (eta_n > 0)
          mean_PPD = mean_PPD + beta_rng(eta[n] * aux, (1 - eta[n]) * aux);
      }
    }
    else if (family == 4 && link_phi > 0) {
      eta = linkinv_beta(eta, link);
      eta_z = linkinv_beta_z(eta_z, link_phi);
      for (n in 1:N) {
        real eta_n = eta[n];
        real aux_n = eta_z[n];
        if (aux_n <= 0) mean_PPD = mean_PPD + bernoulli_rng(0.5);
        else if (eta_n >= 1) mean_PPD = mean_PPD + 1;
        else if (eta_n > 0)
          mean_PPD = mean_PPD + beta_rng(eta_n * aux_n, (1 - eta_n) * aux_n);
      }
    }
    mean_PPD = mean_PPD / len_y;
  }
}
