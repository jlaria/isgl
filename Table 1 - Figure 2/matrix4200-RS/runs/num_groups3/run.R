# Script to run iSgL with the simulations from Feng and Simon [2017]
 setwd('/home/juank/Documents/Doctorado/Investigacion/uranus.uc3m.es/PBS/matrix4200-RS/runs/num_groups3/')
 expert_num_groups = 200
 pj = 21
 num_groups = 3
 # Run python code...
 system('~/miniconda2/bin/python ../../base/python/main.py 90 60 4200 200 3 30') 
 results.table = NULL 
 for (i in 1:30){
 
group_index = rep(1:expert_num_groups, each=pj)

# Run R simulations
file_idx = paste('-run',i-1,'-numgroups',num_groups, sep = '')
folder_idx = paste('data', length(group_index), '/', sep='')

X_train <- as.matrix(read.csv(paste(folder_idx,'X_train',file_idx, sep=''), header = F, sep = ' '))
X_validate <- as.matrix(read.csv(paste(folder_idx,'X_validate', file_idx, sep=''), header = F, sep = ' '))
X_test <- as.matrix(read.csv(paste(folder_idx, 'X_test', file_idx, sep=''), header = F, sep = ' '))

y_train <- read.csv(paste(folder_idx,'y_train', file_idx, sep=''), header = F, sep = ' ')[,]
y_validate <- read.csv(paste(folder_idx,'y_validate', file_idx, sep=''), header = F, sep = ' ')[,]
y_test <- read.csv(paste(folder_idx,'y_test', file_idx, sep=''), header = F, sep = ' ')[,]

data.train = list(x=X_train, y=y_train)
data.validate = list(x=X_validate, y=y_validate)

# # isgl
t = system.time(  result <-  sglfast::isgl_simple(data.train, data.validate, index=group_index, type = "linear") ) 

y_pred = sglfast::predict.isgl(result, data.validate$x)
diff <- y_pred - data.validate$y
B <- 0.5/length(y_pred)*norm(diff,'F')^2 
isgl.validation_error = B

y_pred = sglfast::predict.isgl(result, X_test)
diff <- y_pred - y_test
B <- 0.5/length(y_pred)*norm(diff,'F')^2 
isgl.test_error = B

isgl.num_solves = result$num_solves
isgl.time = t[3]

# uIsgl
t = system.time(  result <- sglfast::isgl(data.train, data.validate, index = group_index, type = "linear") )

y_pred = sglfast::predict.isgl(result, data.validate$x)
diff <- y_pred - data.validate$y
B <- 0.5/length(y_pred)*norm(diff,'F')^2 
uisgl.validation_error = B

y_pred = sglfast::predict.isgl(result, X_test)
diff <- y_pred - y_test
B <- 0.5/length(y_pred)*norm(diff,'F')^2 
uisgl.test_error = B

uisgl.num_solves = result$num_solves
uisgl.time = t[3]

results.row = data.frame(num_groups,
                         isgl.time,
                         isgl.num_solves,
                         isgl.validation_error,
                         isgl.test_error,
                         uisgl.time,
                         uisgl.num_solves,
                         uisgl.validation_error,
                         uisgl.test_error  )

colnames(results.row) = c('true_num_groups',
                          'isgl.time',
                          'isgl.num_solves',
                          'isgl.validation_error',
                          'isgl.test_error',
                          'uisgl.time',
                          'uisgl.num_solves',
                          'uisgl.validation_error',
                          'uisgl.test_error')
results.table = rbind(results.table, results.row)
}
 write.csv(results.table, file = 'isgl.out')

