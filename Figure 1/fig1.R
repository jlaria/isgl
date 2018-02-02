setwd('/home/juank/Documents/Doctorado/Articulo-iSgL/R')
load('colitis.RData')

train_idx <- sample(1:nrow(X),50)
X_train <- X[train_idx,]
y_train <- y[train_idx] + 0
X_validate <- X[-train_idx,]
y_validate <- y[-train_idx] + 0

data <- list(x=X_train, y=y_train)

errors_lambda2_fixed <- matrix(rep(0,100*10), nrow=100, ncol=10)
lambda2 <- seq(0.007, 0.06, length.out = 10)
lambda1 <- seq(0.03, 0.1, length.out = 100)
for(i in 1:10){
    lambda_path <- lambda1 + lambda2[i]
    alpha_path <- lambda1/lambda_path
    for(j in 1:100){
      fit.sgl <- SGL::SGL(data, membership.index, type="logit", alpha = alpha_path[j], lambdas = lambda_path[j], nlam=1, standardize = F)
      model_prediction <- X_validate %*% fit.sgl$beta
      log_sum <- sum(log(1+exp(model_prediction)))
      errors_lambda2_fixed[j,i] <- 1/length(y_validate)*(log_sum - t(model_prediction)%*%y_validate)
      }
}

plot(c(0.03,0.1),c(.3,1), type='n')
for(i in 1:10)
{
lines(lambda1,errors_lambda2_fixed[,i])
}

lines(lambda1, errors_lambda2_fixed[,8], col=2)
lambda2[7]

save.image('image.RData')


library('ggplot2')
results <- data.frame(lambda1 = rep(lambda1, 10), 
                      lambda2 = rep(round(lambda2,3), each=100),
                      err = as.vector(errors_lambda2_fixed)) 

ggplot(results, aes(x=lambda1, y=err, group=factor(lambda2))) + 
  geom_line(aes(col=factor(lambda2)),alpha=0.9)+
  scale_y_continuous("Validation Error") + 
  scale_x_continuous(expression(lambda[1]))+
  labs(color=expression(lambda[2]))+
  scale_color_discrete()
#================================
# 
# library(lasso2)
# data("Prostate")
# 
# X <- as.matrix(Prostate[,-9], ncol = 8)
# y <- Prostate$lpsa
# 
# train_idx <- sample(1:nrow(X),67)
# X_train <- X[train_idx,]
# y_train <- y[train_idx] + 0
# X_validate <- X[-train_idx,]
# y_validate <- y[-train_idx] + 0
# 
# data <- list(x=X_train, y=y_train)
# membership.index <- 1:8
# errors_lambda2_fixed <- matrix(rep(0,100*11), nrow=100, ncol=11)
# lambda2 <- seq(0.05, 1, length.out = 11)
# lambda1 <- seq(0.05, 1, length.out = 100)
# for(i in 1:11)
# {
#   lambda_path <- lambda1 + lambda2[i]
#   alpha_path <- lambda1/lambda_path
#   for(j in 1:100){
#     fit.sgl <- SGL::SGL(data, membership.index, type="linear", alpha = alpha_path[j], lambdas = lambda_path[j], nlam=1, standardize = F)
#     model_prediction <- X_validate %*% fit.sgl$beta
#     errors_lambda2_fixed[j,i] <- 1/length(y_validate)*sum((y_validate - model_prediction)^2)
#   }
# }
# 
# plot(range(lambda1),range(errors_lambda2_fixed), type='n')
# for(i in 1:11)
# {
#   lines(lambda1,errors_lambda2_fixed[,i])
# }
# 
# plot(lambda1, errors_lambda2_fixed[,3])
# 
# min(errors_lambda2_fixed)
# 
# fit.lm <- lm(lpsa~., data = Prostate[train_idx,])
# summary(fit.lm)
# 
# p <- predict.lm(fit.lm, Prostate[-train_idx,])
# 1/length(y_validate)*sum((y_validate - p)^2)
# 
