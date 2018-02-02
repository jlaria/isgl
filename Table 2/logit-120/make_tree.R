pbs_R_path = '~/miniconda2/bin/R '
pbs_jobname = 'Sim120logit'
pbs_nodes = 4
pbs_cores = 1
pbs_walltime = '48:00:00'
run_scripts = '#!/bin/sh\n'

wd = paste0(getwd(), '/')

script = paste0('#PBS -N ', pbs_jobname, '\n',
                '#PBS -o ', wd, pbs_jobname,'.o\n',
                '#PBS -e ', wd, pbs_jobname,'.e\n',
                '#PBS -m abe -M jlaria@est-econ.uc3m.es\n',
                '#PBS -l nodes=', pbs_nodes, ',walltime=',pbs_walltime, '\n',
                '\n',
                'cd ', wd, '\n',
                pbs_R_path,' CMD BATCH logit.R\n')
cat(script, file = paste0(wd,'script.pbs'))

run_scripts = paste0(run_scripts, 
                     'cd ', wd, '\n',
                     'qsub script.pbs\n')
cat(run_scripts, file='run_scripts.sh')
