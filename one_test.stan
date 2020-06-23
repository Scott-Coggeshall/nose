//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//


data {
  int<lower=0> K;
  int y1[K];
  real<lower=0> sens1shape[2];
  real sens1limits[2];
  real<lower=0> sens2shape[2];
  real sens2limits[2];
  real<lower=0> spec1shape[2];
  real spec1limits[2];
  real<lower=0> spec2shape[2];
  real spec2limits[2];
  real<lower=0> pd1shape[2];
}


parameters {
  real<lower=0, upper=1> sens1;
  real<lower=0, upper=1> sens2;
  real<lower=0, upper=1> spec1;
  real<lower=0, upper =1> spec2;
  real<lower=0, upper=1> pd1;
}

transformed parameters {
  
  simplex[4] p1;

  
  real<lower=sens1limits[1], upper=sens1limits[2]> sens1_transformed;
  real<lower=sens2limits[1], upper=sens2limits[2]> sens2_transformed;
  real<lower=spec1limits[1], upper=spec1limits[2]> spec1_transformed;
  real<lower=spec2limits[1], upper=spec2limits[2]> spec2_transformed;
  
  sens1_transformed = sens1*(sens1limits[2] - sens1limits[1]) + sens1limits[1];
  sens2_transformed = sens2*(sens2limits[2] - sens2limits[1]) + sens2limits[1];
  spec1_transformed = spec1*(spec1limits[2] - spec1limits[1]) + spec1limits[1];
  spec2_transformed = spec2*(spec2limits[2] - spec2limits[1]) + spec2limits[1];
  
  
  p1[1] = (1 - sens1_transformed)*(1 - sens2_transformed)*pd1 + spec1_transformed*spec2_transformed*(1 - pd1);
  p1[2] = pd1*sens2_transformed*(1 - sens1_transformed) + (1 - pd1)*spec1_transformed*(1 - spec2_transformed);
  p1[3] = pd1*sens1_transformed*(1 - sens2_transformed) + (1 - pd1)*spec2_transformed*(1 - spec1_transformed);
  p1[4] = sens1_transformed*sens2_transformed*pd1 + (1 - spec1_transformed)*(1 - spec2_transformed)*(1 - pd1);
  
}


model {
  
  sens1 ~ beta(sens1shape[1], sens1shape[2]);
  spec1 ~ beta(spec1shape[1], spec1shape[2]);
  sens2 ~ beta(sens2shape[1], sens2shape[2]);
  spec2 ~ beta(spec2shape[1], spec2shape[2]);
  pd1 ~ beta(pd1shape[1], pd1shape[2]);
  y1 ~ multinomial(p1);
}

