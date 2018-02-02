pbs_R_path = '~/miniconda2/bin/R '
pbs_jobname = 'Sim4200groups'
pbs_nodes = 16
pbs_cores = 1
pbs_walltime = '48:00:00'
run_scripts = '#!/bin/sh\n'

setwd('~/simulations/PBS')

nrun = 30
expert_num_groups = 200
pj = 21

train_size = 90
validate_size = 60

for(num_groups in c(1,2,3)){
    wd = paste0(getwd(),'/runs/num_groups', num_groups,'/')
    dir.create(paste0(wd, 'data',expert_num_groups*pj,'/'), recursive = T)
    system(paste('cp base/results', wd, '-r'))
    
    header = paste0('# Script to run iSgL with the simulations from Feng and Simon [2017]\n ',
                    'setwd(\'',getwd(),'/runs/num_groups', num_groups,'/\')\n ',
                    'expert_num_groups = ', expert_num_groups, '\n ',
                    'pj = ', pj, '\n ', 
                    'num_groups = ', num_groups, '\n ',
                    '# Run python code...\n ',
                    'system(\'~/miniconda2/bin/python ../../base/python/main.py ', train_size, ' ',
                            validate_size, ' ',
                            expert_num_groups*pj, ' ',
                            expert_num_groups, ' ',
                            num_groups, ' ', 
                            nrun,'\') \n ',
                    'results.table = NULL \n ',
                    'for (i in 1:', nrun,'){\n ')
    footer = paste0('}\n ',
                    'write.csv(results.table, file = \'isgl.out\')\n' )
    
    cat(header, readLines('base/body.R'), footer, sep='\n', file = paste0(wd,'run.R'))
    
    script = paste0('#PBS -N ', pbs_jobname, num_groups, '\n',
                    '#PBS -o ', wd, pbs_jobname, num_groups, '.o\n',
                    '#PBS -e ', wd, pbs_jobname, num_groups, '.e\n',
                    '#PBS -m abe -M jlaria@est-econ.uc3m.es\n',
                    '#PBS -l nodes=', pbs_nodes, ',walltime=',pbs_walltime, '\n',
                    '\n',
                    'cd ', wd, '\n',
                    pbs_R_path,' CMD BATCH run.R\n')
    cat(script, file = paste0(wd,'script.pbs'))
    
    run_scripts = paste0(run_scripts, 
                         'cd ', wd, '\n',
                         'qsub script.pbs\n')
}
cat(run_scripts, file='run_scripts.sh')


