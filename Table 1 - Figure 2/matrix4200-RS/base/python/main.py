# -*- coding: utf-8 -*-
"""
Contact: github.com/jlaria
Original script from <https://github.com/jjfeng/nonsmooth-joint-opt>   

"""

import sys
import traceback

from method_results   import MethodResults, MethodResult
from iteration_models import Simulation_Settings, Iteration_Data
from sgl_hillclimb    import SGL_Hillclimb_Simple, SGL_Hillclimb
from common           import *
from data_generator   import DataGenerator
from multiprocessing  import Pool

def simulate(train_size, validate_size, num_features, expert_num_groups, true_num_groups, num_runs):
    # Algorithm's settings
    settings = SGL_Settings()

    settings.train_size = train_size
    settings.validate_size = validate_size
    settings.test_size = 200
    settings.snr = 2  # signal-to-noise ratio
    settings.num_features = num_features  # number of covariates
    settings.expert_num_groups = expert_num_groups  # number of groups for the computations
    settings.true_num_groups = true_num_groups  # real number of groups
    settings.spearmint_numruns = 50

    # Generate the data for the simulations

    data_gen = DataGenerator(settings)
    # run_data_iS = []
    # run_data_GS = []
    run_data_HC0 = []
    run_data_HC = []
    for i in range(num_runs):
        # Generate data Train/Validation/Test
        observed_data = data_gen.sparse_groups()

        t = 'data%i/X_train-run%i-numgroups%i' % (num_features, i, true_num_groups)
        np.savetxt( fname=t,  X=observed_data.X_train )
        t = 'data%i/X_validate-run%i-numgroups%i' %  (num_features, i, true_num_groups)
        np.savetxt( fname=t,  X=observed_data.X_validate)
        t = 'data%i/X_test-run%i-numgroups%i' %  (num_features, i, true_num_groups)
        np.savetxt( fname=t,  X=observed_data.X_test )

        t = 'data%i/y_train-run%i-numgroups%i' %  (num_features, i, true_num_groups)
        np.savetxt(fname=t, X=observed_data.y_train)
        t = 'data%i/y_validate-run%i-numgroups%i' %  (num_features, i, true_num_groups)
        np.savetxt(fname=t, X=observed_data.y_validate)
        t = 'data%i/y_test-run%i-numgroups%i' %  (num_features, i, true_num_groups)
        np.savetxt(fname=t, X=observed_data.y_test)

        settings.method="HC0"
        run_data_HC0.append(Iteration_Data(i, observed_data, settings))
        settings.method="HC"
        run_data_HC.append(Iteration_Data(i, observed_data, settings))

    pool = Pool(16)
    # Solve HC0
    settings.method = "HC0"
    results = pool.map(fit_data_for_iter_safe, run_data_HC0)

    f = open('HC0.out', 'w')
    header =  "test_err, validation_err, runtime \n"
    f.write(header)
    for i in range(num_runs):
    	stats = results[i].stats
    	row = '%f, %f, %f \n' % (stats['test_err'], stats['validation_err'], stats['runtime'])
    	f.write(row)
    f.close()

    # Solve HC
    settings.method = "HC"
    results = pool.map(fit_data_for_iter_safe, run_data_HC)
    f = open('HC.out', 'w')
    header =  "test_err, validation_err, runtime \n"
    f.write(header)
    for i in range(num_runs):
    	stats = results[i].stats
    	row = '%f, %f, %f \n' % (stats['test_err'], stats['validation_err'], stats['runtime'])
    	f.write(row)
    f.close()

def main():
    train_size = int(sys.argv[1])
    validate_size = int(sys.argv[2])
    num_features = int(sys.argv[3])
    expert_num_groups = int(sys.argv[4])
    true_num_groups = int(sys.argv[5])
    num_runs = int(sys.argv[6])
    simulate(train_size, validate_size, num_features, expert_num_groups, true_num_groups, num_runs)



class SGL_Settings(Simulation_Settings):
    results_folder = "results/sgl"
    num_train = 10
    num_validate = 3
    num_test = 200
    num_features = 30
    expert_num_groups = 3
    true_num_groups = 3
    spearmint_numruns = 100
    gs_lambdas1 = np.power(10, np.arange(-3, 1, 4.0/10))
    gs_lambdas2 = gs_lambdas1
    assert(gs_lambdas1.size == 10)
    method_result_keys = [
        "test_err",
        "validation_err",
        "beta_err",
        "runtime",
        "num_solves",
        "correct_selection",
    ]

    def print_settings(self):
        print "SETTINGS"
        obj_str = "method %s\n" % self.method
        obj_str += "expert_num_groups %d\n" % self.expert_num_groups
        obj_str += "num_features %d\n" % self.num_features
        obj_str += "t/v/t size %d/%d/%d\n" % (self.train_size, self.validate_size, self.test_size)
        obj_str += "snr %f\n" % self.snr
        obj_str += "sp runs %d\n" % self.spearmint_numruns
        obj_str += "nm_iters %d\n" % self.nm_iters
        print obj_str

    def get_true_group_sizes(self):
        assert(self.num_features % self.true_num_groups == 0)
        return [self.num_features / self.true_num_groups] * self.true_num_groups

    def get_expert_group_sizes(self):
        assert(self.num_features % self.expert_num_groups == 0)
        return [self.num_features / self.expert_num_groups] * self.expert_num_groups

#########
# FUNCTIONS FOR CHILD THREADS
#########
def fit_data_for_iter_safe(iter_data):
    result = None
    try:
        result = fit_data_for_iter(iter_data)
    except Exception as e:
        print "Exception caught in iter %d: %s" % (iter_data.i, e)
        traceback.print_exc()
    return result

def fit_data_for_iter(iter_data):
    settings = iter_data.settings

    one_vec = np.ones(settings.expert_num_groups + 1)
    initial_lambdas_set = [one_vec, one_vec * 1e-1]

    one_vec2 = np.ones(2)
    simple_initial_lambdas_set = [one_vec2, one_vec2 * 1e-1]

    method = iter_data.settings.method

    str_identifer = "%d_%d_%d_%d_%d_%d_%s_%d" % (
        settings.expert_num_groups,
        settings.num_features,
        settings.train_size,
        settings.validate_size,
        settings.test_size,
        settings.snr,
        method,
        iter_data.i,
    )
    log_file_name = "%s/tmp/log_%s.txt" % (settings.results_folder, str_identifer)
    print "log_file_name", log_file_name
    # set file buffer to zero so we can see progress
    with open(log_file_name, "w", buffering=0) as f:
        if method == "HC0":
            algo = SGL_Hillclimb_Simple(iter_data.data, settings)
            algo.run(simple_initial_lambdas_set, debug=False, log_file=f)
        elif method == "HC":
            algo = SGL_Hillclimb(iter_data.data, settings)
            algo.run(initial_lambdas_set, debug=False, log_file=f)
        else:
            raise ValueError("Method not implemented yet: %s" % settings.method)
        sys.stdout.flush()
        method_res = create_method_result(iter_data.data, algo.fmodel)

        f.write("SUMMARY\n%s" % method_res)
    return method_res

def create_method_result(data, algo, zero_threshold=1e-3):
    test_err = testerror_grouped(
        data.X_test,
        data.y_test,
        algo.best_model_params
    )

    beta_guess = np.concatenate(algo.best_model_params)


    selected_vars = get_nonzero_indices(beta_guess, threshold=zero_threshold)
    true_vars = get_nonzero_indices(data.beta_real, threshold=zero_threshold)



    correct_selection = float(np.sum(np.logical_and(selected_vars, true_vars)))/max(np.sum(true_vars), np.sum(selected_vars))

    print "validation cost", algo.best_cost, "test_err", test_err

    return MethodResult({
            "test_err": test_err,
            "validation_err": algo.best_cost,
            "beta_err": betaerror(data.beta_real, beta_guess),
            "runtime": algo.runtime,
            "num_solves": algo.num_solves,
            "correct_selection": correct_selection,
        },
        lambdas=algo.best_lambdas
    )
    
    
    
if __name__ == "__main__":
    main()

