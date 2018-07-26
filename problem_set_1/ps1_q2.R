# read in data
X_train = read.table("q2/data/x.dat", header = F, row.names = NULL)
y_train = read.table("q2/data/y.dat", header = F, row.names = NULL)

lwlr = function(X_train, y_train, x, tau) {
  X_train = as.matrix(X_train)
  y_train = as.matrix(y_train)
  # euclidean distance between vectors v1 and v2
  euclid_distance = function(v1, v2) sum((v1 - v2)^2)
  # absolute value of vector v
  abs_val = function(v) euclid_distance(v, 0*v)
  # weight to give r given observation v
  weight = function(r, v, tau) exp(-euclid_distance(r,v)/(2*tau^2))
  # are alpha1 and alpha2 close enough to eachother
  # this function will determine when to stop updating alpha
  good_enough = function(alpha1, alpha2, tolerance = 0.2) 
    euclid_distance(alpha1, alpha2)/abs_val(alpha1 + 0.0001) < tolerance
  # sigmoid function
  sigmoid = function(x, alpha) 1/(1 + exp(-sum(x*alpha)))
  
  get_h = function(X_train, alpha) apply(X_train, 1, function(r) sigmoid(r, alpha))
  get_w = function(X_train, x, tau) apply(X_train, 1, function(r) weight(r, x, tau))
  get_D = function(w, alpha) {
    h_alpha = get_h(X_train, alpha)
    diag(-w*h_alpha*(1 - h_alpha))
  }
  
  # generate uniform RVs between 0 and 1
  alpha = runif(ncol(X_train))
  w = get_w(X_train, x, tau)
  iters = 0
  lambda = 0.001
  while (TRUE) {
    iters = iters + 1
    z = unlist(w*(y_train - get_h(X_train, alpha)), use.names = FALSE)
    grad = t(X_train) %*% z - lambda * alpha
    D = get_D(w, alpha)
    H = t(X_train) %*% D %*% X_train - lambda * diag(ncol(X_train))
    alpha_0 = alpha
    alpha = alpha_0 - solve(H) %*% grad
    if (good_enough(alpha, alpha_0) || iters > 100) break
  }
  return (sigmoid(x, alpha) > 0.5)
}

make_plots = function(tau) {
  print(tau)
  s = seq(-1, 1, 0.01)
  grid = expand.grid(s,s)
  grid$pred = apply(grid, 1, function(r) lwlr(X_train, y_train, r, tau))
  df = data.frame(grid)
  gg = ggplot(df, aes(x = Var1, y = Var2, color = pred)) + 
    geom_point() + 
    labs(x = '', y = '', title = paste('tau =', tau))
  ggsave(paste('gg', tau, '.png', sep = ''), gg)
}

taus = c(0.01, 0.05, 0.1, 0.5, 1.0, 5.0)
sapply(taus, make_plots)