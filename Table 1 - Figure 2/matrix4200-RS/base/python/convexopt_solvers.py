import time
from cvxpy import *
import cvxopt
from common import *
import scipy as sp
import cvxpy

SCS_MAX_ITERS = 10000
SCS_EPS = 1e-3 # default eps
SCS_HIGH_ACC_EPS = 1e-6
ECOS_TOL = 1e-12
REALDATA_MAX_ITERS = 4000

# Objective function: 0.5 * norm(y - Xb)^2 + lambda1 * lasso + 0.5 * lambda2 * ridge
class Lambda12ProblemWrapper:
    def __init__(self, X, y):
        n = X.shape[1]
        self.beta = Variable(n)
        self.lambda1 = Parameter(sign="positive")
        self.lambda2 = Parameter(sign="positive")
        objective = Minimize(0.5 * sum_squares(y - X * self.beta)
            + self.lambda1 * norm(self.beta, 1)
            + 0.5 * self.lambda2 * sum_squares(self.beta))
        self.problem = Problem(objective, [])

    def solve(self, lambda1, lambda2, quick_run=None):
        self.lambda1.value = lambda1
        self.lambda2.value = lambda2
        result = self.problem.solve(solver=SCS, verbose=VERBOSE)
        # print "self.problem.status", self.problem.status
        return self.beta.value

# Objective function: 0.5 * norm(y - Xb)^2 + lambda1 * lasso
class LassoProblemWrapper:
    def __init__(self, X, y):
        num_train = X.shape[0]
        self.beta = Variable(X.shape[1])
        self.lambda1 = Parameter(sign="positive")
        objective = Minimize(0.5/num_train * sum_squares(y - X * self.beta)
            + self.lambda1 * norm(self.beta, 1))
        self.problem = Problem(objective)

    def solve(self, lambdas, quick_run=None, warm_start=True):
        self.lambda1.value = lambdas[0]
        result = self.problem.solve(verbose=VERBOSE)
        return self.beta.value


# Objective function: 0.5 * norm(y - Xb)^2 + lambda1 * lasso + 0.5 * lambda2 * ridge
class ElasticNetProblemWrapper:
    def __init__(self, X, y):
        n = X.shape[1]
        self.beta = Variable(n)
        self.lambda1 = Parameter(sign="positive")
        self.lambda2 = Parameter(sign="positive")
        objective = Minimize(0.5 * sum_squares(y - X * self.beta)
            + self.lambda1 * norm(self.beta, 1)
            + 0.5 * self.lambda2 * sum_squares(self.beta))
        self.problem = Problem(objective, [])

    def solve(self, lambdas, quick_run=None, warm_start=True):
        self.lambda1.value = lambdas[0]
        self.lambda2.value = lambdas[1]
        result = self.problem.solve(solver=SCS, verbose=VERBOSE)
        return self.beta.value

class GroupedLassoProblemWrapper:
    def __init__(self, X, y, group_feature_sizes):
        self.group_range = range(0, len(group_feature_sizes))
        self.betas = [Variable(feature_size) for feature_size in group_feature_sizes]
        self.lambda1s = [Parameter(sign="positive") for i in self.group_range]
        self.lambda2 = Parameter(sign="positive")

        feature_start_idx = 0
        model_prediction = 0
        group_lasso_regularization = 0
        sparsity_regularization = 0
        for i in self.group_range:
            end_feature_idx = feature_start_idx + group_feature_sizes[i]
            model_prediction += X[:, feature_start_idx : end_feature_idx] * self.betas[i]
            feature_start_idx = end_feature_idx
            group_lasso_regularization += self.lambda1s[i] * norm(self.betas[i], 2)
            sparsity_regularization += norm(self.betas[i], 1)

        objective = Minimize(0.5 / y.size * sum_squares(y - model_prediction)
            + group_lasso_regularization
            + self.lambda2 * sparsity_regularization)
        self.problem = Problem(objective, [])

    def solve(self, lambdas, quick_run=False):
        for idx in self.group_range:
            self.lambda1s[idx].value = lambdas[idx]

        self.lambda2.value = lambdas[-1]

        ecos_iters = 200
        try:
            self.problem.solve(solver=ECOS, verbose=VERBOSE, abstol=ECOS_TOL, reltol=ECOS_TOL, abstol_inacc=ECOS_TOL, reltol_inacc=ECOS_TOL, max_iters=ecos_iters)
        except SolverError:
            self.problem.solve(solver=SCS, verbose=VERBOSE, eps=SCS_HIGH_ACC_EPS/100, max_iters=SCS_MAX_ITERS * 4, use_indirect=False, normalize=False, warm_start=True)

        return [b.value for b in self.betas]

class GroupedLassoClassifyProblemWrapper:
    def __init__(self, X_groups, y):
        group_feature_sizes = [g.shape[1] for g in X_groups]
        self.group_range = range(0, len(group_feature_sizes))
        self.betas = [Variable(feature_size) for feature_size in group_feature_sizes]
        self.lambda1s = [Parameter(sign="positive") for i in self.group_range]
        self.lambda2 = Parameter(sign="positive")

        feature_start_idx = 0
        model_prediction = 0
        group_lasso_regularization = 0
        sparsity_regularization = 0
        for i in self.group_range:
            model_prediction += X_groups[i] * self.betas[i]
            group_lasso_regularization += self.lambda1s[i] * norm(self.betas[i], 2)
            sparsity_regularization += norm(self.betas[i], 1)

        log_sum = 0
        for i in range(0, X_groups[0].shape[0]):
            one_plus_expyXb = vstack(0, model_prediction[i])
            log_sum += log_sum_exp(one_plus_expyXb)

        objective = Minimize(
            log_sum
            - model_prediction.T * y
            + group_lasso_regularization
            + self.lambda2 * sparsity_regularization)
        self.problem = Problem(objective, [])

    def solve(self, lambdas):
        for idx in self.group_range:
            self.lambda1s[idx].value = lambdas[idx]

        self.lambda2.value = lambdas[-1]

        result = self.problem.solve(solver=SCS, verbose=VERBOSE, max_iters=REALDATA_MAX_ITERS, use_indirect=False, normalize=True)
        print "self.problem.status", self.problem.status
        return [b.value for b in self.betas]

class GroupedLassoClassifyProblemWrapperFullCV:
    def __init__(self, X, y, group_feature_sizes):
        self.group_range = range(0, len(group_feature_sizes))
        total_features = np.sum(group_feature_sizes)
        self.beta = Variable(total_features)
        self.lambda1s = [Parameter(sign="positive") for i in self.group_range]
        self.lambda2 = Parameter(sign="positive")

        start_feature_idx = 0
        group_lasso_regularization = 0
        for i, group_feature_size in enumerate(group_feature_sizes):
            end_feature_idx = start_feature_idx + group_feature_size
            group_lasso_regularization += self.lambda1s[i] * norm(self.beta[start_feature_idx:end_feature_idx], 2)
            start_feature_idx = end_feature_idx

        model_prediction = X * self.beta
        log_sum = 0
        num_samples = X.shape[0]
        for i in range(0, num_samples):
            one_plus_expyXb = vstack(0, model_prediction[i])
            log_sum += log_sum_exp(one_plus_expyXb)

        objective = Minimize(
            log_sum
            - (X * self.beta).T * y
            + group_lasso_regularization
            + self.lambda2 * norm(self.beta, 1))
        self.problem = Problem(objective, [])

    def solve(self, lambdas):
        for idx in self.group_range:
            self.lambda1s[idx].value = lambdas[idx]

        self.lambda2.value = lambdas[-1]

        result = self.problem.solve(solver=SCS, verbose=VERBOSE, max_iters=REALDATA_MAX_ITERS, use_indirect=False, normalize=True)
        print "self.problem.status", self.problem.status
        return self.beta.value

class GroupedLassoProblemWrapperSimple:
    def __init__(self, X, y, group_feature_sizes):
        self.group_range = range(0, len(group_feature_sizes))
        self.betas = [Variable(feature_size) for feature_size in group_feature_sizes]
        self.lambda1 = Parameter(sign="positive")
        self.lambda2 = Parameter(sign="positive")

        feature_start_idx = 0
        model_prediction = 0
        group_lasso_regularization = 0
        sparsity_regularization = 0
        for i in self.group_range:
            end_feature_idx = feature_start_idx + group_feature_sizes[i]
            model_prediction += X[:, feature_start_idx : end_feature_idx] * self.betas[i]
            feature_start_idx = end_feature_idx
            group_lasso_regularization += norm(self.betas[i], 2)
            sparsity_regularization += norm(self.betas[i], 1)

        objective = Minimize(0.5 / y.size * sum_squares(y - model_prediction)
            + self.lambda1 * group_lasso_regularization
            + self.lambda2 * sparsity_regularization)
        self.problem = Problem(objective, [])

    def solve(self, lambdas, quick_run=False):
        self.lambda1.value = lambdas[0]
        self.lambda2.value = lambdas[1]

        if not quick_run:
            ecos_iters = 400
            tol=ECOS_TOL * 100
            try:
                self.problem.solve(solver=ECOS, verbose=VERBOSE, reltol=tol, abstol_inacc=tol, reltol_inacc=tol, max_iters=ecos_iters)
            except SolverError:
                self.problem.solve(solver=SCS, verbose=VERBOSE, eps=SCS_HIGH_ACC_EPS, max_iters=SCS_MAX_ITERS * 4, use_indirect=False, normalize=False, warm_start=True)
        else:
            try:
                self.problem.solve(solver=ECOS, verbose=VERBOSE)
            except SolverError:
                self.problem.solve(solver=SCS, verbose=VERBOSE, use_indirect=False, normalize=False, warm_start=True)
        return [b.value for b in self.betas]

class GroupedLassoClassifyProblemWrapperSimple:
    def __init__(self, X_groups, y):
        self.group_range = range(0, len(X_groups))
        self.betas = [Variable(Xg.shape[1]) for Xg in X_groups]
        self.lambda1 = Parameter(sign="positive")
        self.lambda2 = Parameter(sign="positive")

        feature_start_idx = 0
        model_prediction = 0
        group_lasso_regularization = 0
        sparsity_regularization = 0
        for i, Xg in enumerate(X_groups):
            model_prediction += Xg * self.betas[i]
            group_lasso_regularization += norm(self.betas[i], 2)
            sparsity_regularization += norm(self.betas[i], 1)

        log_sum = 0
        for i in range(0, X_groups[0].shape[0]):
            one_plus_expyXb = vstack(0, model_prediction[i])
            log_sum += log_sum_exp(one_plus_expyXb)

        objective = Minimize(
            log_sum
            - model_prediction.T * y
            + self.lambda1 * group_lasso_regularization
            + self.lambda2 * sparsity_regularization)
        self.problem = Problem(objective, [])

    def solve(self, lambdas):
        self.lambda1.value = lambdas[0]
        self.lambda2.value = lambdas[1]

        result = self.problem.solve(solver=SCS, verbose=VERBOSE)
        return [b.value for b in self.betas]

class GroupedLassoClassifyProblemWrapperSimpleFullCV:
    def __init__(self, X, y, feature_group_sizes):
        total_features = np.sum(feature_group_sizes)
        self.beta = Variable(total_features)
        self.lambda1 = Parameter(sign="positive")
        self.lambda2 = Parameter(sign="positive")

        start_feature_idx = 0
        group_lasso_regularization = 0
        for feature_group_size in feature_group_sizes:
            end_feature_idx = start_feature_idx + feature_group_size
            group_lasso_regularization += norm(self.beta[start_feature_idx:end_feature_idx], 2)
            start_feature_idx = end_feature_idx

        model_prediction = X * self.beta
        log_sum = 0
        num_samples = X.shape[0]
        for i in range(0, num_samples):
            one_plus_expyXb = vstack(0, model_prediction[i])
            log_sum += log_sum_exp(one_plus_expyXb)

        objective = Minimize(
            log_sum
            - model_prediction.T * y
            + self.lambda1 * group_lasso_regularization
            + self.lambda2 * norm(self.beta, 1))
        self.problem = Problem(objective, [])

    def solve(self, lambdas):
        self.lambda1.value = lambdas[0]
        self.lambda2.value = lambdas[1]

        result = self.problem.solve(solver=SCS, verbose=VERBOSE)
        return self.beta.value
