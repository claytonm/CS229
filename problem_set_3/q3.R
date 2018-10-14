get_data = function(x_folder, y_folder) {
  X = read.table(x_folder)
  X = as.matrix(X)
  y = read.table(y_folder)
  r = list("X" = X,"y" = y)
  return(r)
}

# get data
data = get_data("q3/x.dat","q3/y.dat")

# common functions
dot_product = function(l, r) {
  l = as.matrix(l)
  r = as.matrix(r)
  (t(l)%*%r)
}
l_1 = function(v) sum(abs(v))

coord_desc = function(X,y,lambda) {
  
  # function to calculate objective function
  J = function(X,y,theta,lambda) {
    v = X%*%theta-y
    return (dot_product(v,v)/2 + lambda*l_1(theta))
  }
  
  update_J = function(X,y,theta,theta_j,j,lambda) {
    theta_update = theta
    theta_update[j] = theta_j
    return(J(X,y,theta_update,lambda))
  }
  
  get_error = function(X,theta,y) {
    predicted = as.matrix(X %*% theta)
    return(sum((predicted - y)^2)^0.5)
  }
  
  tolerance = 0.01
  delta = 0.1
  n = ncol(X)
  theta = runif(n = n) # initialize theta with random uniform variables
  error_old = get_error(X,theta,y) # initialize the error
  
  while (delta > tolerance) {
    for (j in 1:n) {
      X_j = X[,j] # X_j is the jth column of X
      theta_j = theta
      theta_j[j] = 0 # theta_j is theta with the jth element set to zero
      numerator = -1*(X%*%theta_j - y) # numerator of update statement
      denominator = dot_product(X_j, X_j) # denominator of update statement
      update_positive = (numerator+lambda)/denominator # formula to update theta if theta_j is positive
      update_negative = (numerator-lambda)/denominator # formula to update theta
      
      theta_j_positive = max(update_positive, 0) # theta_j_positive is the updated value of theta_j if theta_j was positive 
      theta_j_negative = min(update_negative, 0) # theta_j_negative is the updated value of theta_j if theta_j was negative 
      
      positive_update = update_J(X,y,theta,theta_j_positive,j,lambda)
      negative_update = update_J(X,y,theta,theta_j_negative,j,lambda)
      
      if (positive_update > negative_update)
        theta[j] = theta_j_positive
      else
        theta[j] = theta_j_negative
    }
    error_new = get_error(X,theta,y)
    delta = abs(error_old-error_new)/error_old
    error_old = error_new
  }
  return(theta)
}

data = get_data("q3/x.dat","q3/y.dat")
best_theta = coord_desc(data$X,data$y,0.01)





theta = read.table("q3/theta.dat")

df = data.frame(list("x"=data$X%*%as.matrix(best_theta), "y"=data$y))
colnames(df) = c("x","y")
ggplot(df, aes(x=x,y=y)) + geom_point()