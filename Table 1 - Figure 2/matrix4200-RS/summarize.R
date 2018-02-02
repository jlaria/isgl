# summarize

setwd('~/simulations/PBS/')

nrun = 30
expert_num_groups = 200
pj = 21

train_size = 90
validate_size = 60

results.table = NULL

for(num_groups in c(1,2,3)){
    wd = paste0(getwd(),'/runs/num_groups', num_groups,'/')
    
    resHC0 = read.csv(paste0(wd,'HC0.out'))
    resHC  = read.csv(paste0(wd,'HC.out'))
    resISGL = read.csv(paste0(wd,'isgl.out'))
    
    temp = data.frame(num_groups= num_groups,
                      isgl.time = resISGL$isgl.time,
                      isgl.num_solves = resISGL$isgl.num_solves,
                      isgl.validation_error= resISGL$isgl.validation_error,
                      isgl.test_error= resISGL$isgl.test_error,
                      uisgl.time = resISGL$uisgl.time,
                      uisgl.num_solves= resISGL$uisgl.num_solves,
                      uisgl.validation_error= resISGL$uisgl.validation_error,
                      uisgl.test_error = resISGL$uisgl.test_error,
                      hc0.time = resHC0$runtime,
                      hc0.validation_error = resHC0$validation_err,
                      hc0.test_error = resHC0$test_err,
                      hc.time = resHC$runtime,
                      hc.validation_error = resHC$validation_err,
                      hc.test_error = resHC$test_err
                      )
    results.table = rbind(results.table, temp)
}
save(results.table, file="summary.RData")
