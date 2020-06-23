npv <- function(p_disease, test1_params, test2_params){
  
  numerator <- (1 - test1_params[1])*(1- test2_params[1])*p_disease +
    test1_params[2]*test2_params[2]*(1 - p_disease)
  
  denominator <- p_disease*(1 - test2_params[1]) + (1 - p_disease)*test2_params[2]
  
  numerator/denominator
  
}

ppv <- function(p_disease, test_sens, test_spec){
  
  numerator 
  
  
}

# NPV = P(test1 = 0 | test2 = 0) 
# = P(test1 = 0, test2 = 0)/P(test2 = 0)
# = (P(test2 = 0)^{-1})(P(test1 = 0, test2 = 0 | D) P(D) + P(test1 = 0, test2 = 0 | D^C)P(D^C))

# P(test2 = 0) = P(test2 = 0 | D)P(D) + P(test2 = 0 | D^c)P(D^c)

# generate a 2 x 2 table given 
# an underlying disease prevalence and swab characteristics.
# the 2 x 2 is represented as a 4-vector with entries
# (n_00, n_01, n_10, n_11)

p_table <- function(p_disease, test_sens, test_spec){
  
  p_00 <- prod(p_disease, 1 - test_sens) + prod(1 - p_disease, test_spec)
  
  p_11 <- prod(p_disease, test_sens) + prod(1 - p_disease, 1 - test_spec)
  
  p_01 <- p_disease*test_sens[2]*(1 - test_sens[1]) + (1 - p_disease)*test_spec[1]*(1 - test_spec[2])
  
  p_10 <- 1 - (p_00 + p_11 + p_01)
  
  c(p_00, p_01, p_10, p_11)
  
}

two_by_two <- function(n_tables, n, p_disease, test_sens, test_spec){
  
  p_00 <- prod(p_disease, 1 - test_sens) + prod(1 - p_disease, test_spec)
  
  p_11 <- prod(p_disease, test_sens) + prod(1 - p_disease, 1 - test_spec)
  
  p_01 <- p_disease*test_sens[2]*(1 - test_sens[1]) + (1 - p_disease)*test_spec[1]*(1 - test_spec[2])
  
  p_10 <- 1 - (p_00 + p_11 + p_01)
  
  tables <- t(rmultinom(n = n_tables, size = n, prob = c(p_00, p_01, p_10, p_11)))
  
  if(n_tables == 1) tables <- as.vector(tables)
  
  tables
}

site_tables <- two_by_two(n_tables = 5, n = 10000, p_disease = .06, c(.97, .97), c(.99, .99))

site_tables2 <- sapply(seq(from = .06, to = .06, length.out = 10), function(x) two_by_two(n_tables = 1, n =1000, x, c(.97, .97), c(.99, .99)))


site_tables3 <- lapply(1:1000, function(y) t(sapply(seq(from = .06, to = .3, length.out = 10), function(x) two_by_two(n_tables = 1, n = 200, p_disease = x, c(.97, .97), c(.99, .99)))))

test_sens <- c(.97, .97)
test_spec <- c(.999, .999)

p_disease <- .06
prod(p_disease, 1 - test_sens) + prod(1 - p_disease, test_spec)

prod(p_disease, test_sens) + prod(1 - p_disease, 1 - test_spec)


