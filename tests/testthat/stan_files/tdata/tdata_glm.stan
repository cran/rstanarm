  int<lower=0> len_z_T = 0;
  int<lower=0> len_var_group = sum(p) * (t > 0);
  int<lower=0> len_rho = sum(p) - t;
  int<lower=0, upper=1> is_continuous = 0; // changed in continuous.stan
  int<lower=1> pos = 1;
  array[len_concentration] real<lower=0> delta;
  int<lower=0> hs;
  if (prior_dist <= 2) hs = 0;
  else if (prior_dist == 3) hs = 2;
  else if (prior_dist == 4) hs = 4;
  else hs = 0;
  
  for (i in 1:t) {
    if (p[i] > 1) {
      for (j in 1:p[i]) {
        delta[pos] = concentration[j];
        pos += 1;
      }
    }
    for (j in 3:p[i]) len_z_T += p[i] - 1;
  }
