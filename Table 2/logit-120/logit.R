nrun = 30
expert_num_groups = 12
pj = 10

train_size = 90
validate_size = 60
test_size = 200

group_index = rep(1:expert_num_groups, each=pj)
results.table = NULL

library(foreach)

for (num_groups in c(1,2,3)) {
  temp = foreach(run = 1:nrun, .combine = rbind)%do%{
    true_groups = rep((1:5),num_groups)+rep(pj*(0:(num_groups-1)), each = 5)
    # Generate training data
    X <- matrix(rnorm(expert_num_groups*pj*train_size), nrow=train_size)
    y <- X[, true_groups]%*%rep(1:5, num_groups)
    p = exp(y)/(1+exp(y))
    y = rbinom(train_size,1, p)
    
    data.train = list(x=X, y=y)
    
    # Generate validate data
    X <- matrix(rnorm(expert_num_groups*pj*validate_size), nrow=validate_size)
    y <- X[, true_groups]%*%rep(1:5, num_groups)
    p = exp(y)/(1+exp(y))
    y = rbinom(validate_size,1, p)
    
    data.validate = list(x=X, y=y)
    
    # Generate test data
    X <- matrix(rnorm(expert_num_groups*pj*test_size), nrow=test_size)
    y <- X[, true_groups]%*%rep(1:5, num_groups)
    p = exp(y)/(1+exp(y))
    y = rbinom(test_size,1, p)
    
    data.test = list(x=X, y=y)
    
    true_beta = rep(0, expert_num_groups*pj)
    true_beta[true_groups] = rep(1:5, num_groups)
    # isgl_simple
    t = system.time(  result <-  sglfast::isgl_simple(data.train, data.validate, index=group_index, type = "logit") ) 
    
    y_pred = (sglfast::predict.isgl(result, data.test$x) > 0.5 )
    isgl.ccr = mean(y_pred == data.test$y)
    isgl.tpr = mean(y_pred[data.test$y==1])
    isgl.tnr = 1 - mean(y_pred[data.test$y==0])
    isgl.true_coef = length(intersect(true_groups, which(result$beta!=0)))/length(true_groups)
    isgl.noise = max(1 - length(intersect(true_groups, which(result$beta!=0)))/sum(result$beta!=0), 0)
    isgl.beta_err = sqrt(sum((result$beta - true_beta)^2))
    isgl.num_solves = result$num_solves
    isgl.time = t[3]
    
    # isgl
    t = system.time(  result <-  sglfast::isgl(data.train, data.validate, index=group_index, type = "logit") ) 
    
    y_pred = (sglfast::predict.isgl(result, data.test$x) > 0.5 )
    uisgl.ccr = mean(y_pred == data.test$y)
    uisgl.tpr = mean(y_pred[data.test$y==1])
    uisgl.tnr = 1 - mean(y_pred[data.test$y==0])
    uisgl.true_coef = length(intersect(true_groups, which(result$beta!=0)))/length(true_groups)
    uisgl.noise = max(1 - length(intersect(true_groups, which(result$beta!=0)))/sum(result$beta!=0), 0)
    uisgl.beta_err = sqrt(sum((result$beta - true_beta)^2))
    uisgl.num_solves = result$num_solves
    uisgl.time = t[3]
    
    results.row = data.frame(num_groups,
                             isgl.time,
                             isgl.num_solves,
                             isgl.ccr,
                             isgl.tpr,
                             isgl.tnr,
                             isgl.beta_err,
                             isgl.true_coef,
                             isgl.noise,
                             uisgl.time,
                             uisgl.num_solves,
                             uisgl.ccr,
                             uisgl.tpr,
                             uisgl.tnr,
                             uisgl.beta_err,
                             uisgl.true_coef,
                             uisgl.noise)
    results.row
  }
  results.table = rbind(results.table, temp)
}

save(results.table, file = 'summary.RData')

