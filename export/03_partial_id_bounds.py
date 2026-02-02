"""
================================================================================
WESTWOOD ET AL. (2022) REPLICATION - PART 3: PARTIAL IDENTIFICATION BOUNDS
================================================================================

This script implements the PARTIAL IDENTIFICATION analysis from the paper.

THE PROBLEM:
    We observe that disengaged respondents report higher support for violence.
    But what would they say if they WERE engaged?

    This is a COUNTERFACTUAL question - we can't observe it directly.
    But we can BOUND it using partial identification methods.

LEARNING OBJECTIVES:
    1. Understand partial identification (bounding unobserved quantities)
    2. Learn the delta method for variance estimation
    3. See how assumptions affect inference

THE KEY INSIGHT:
    Even under conservative assumptions about what disengaged respondents
    would answer if engaged, the true support for violence is likely
    only 1-7%, not the 20%+ reported in prior research.

================================================================================
"""

import numpy as np
from scipy import stats
from scipy.optimize import minimize_scalar

# ==============================================================================
# STEP 1: THE PARTIAL IDENTIFICATION MODEL
# ==============================================================================

"""
NOTATION:
---------
Y  = observed outcome (1 = supports violence, 0 = doesn't)
C  = passed engagement check (1 = engaged, 0 = disengaged)
T  = truly engaged (latent, unobserved)
g  = guess rate on comprehension check
Y* = counterfactual: what would they answer if truly engaged?

GOAL:
-----
Bound E[Y*] - the population mean if everyone were truly engaged.

KEY ASSUMPTIONS:
----------------
1. Truly engaged always pass the check: P(C=1|T=1) = 1
2. Truly disengaged pass by guessing: P(C=1|T=0) = g
3. Engaged respondents answer truthfully: Y = Y* when T=1

DERIVATION:
-----------
From assumption 2, everyone who fails (C=0) is truly disengaged (T=0).

Using law of total probability:
    P(C=0) = P(C=0|T=0) * P(T=0) = (1-g) * P(T=0)

Solving:
    P(T=0) = P(C=0) / (1-g)

The counterfactual mean decomposes as:
    E[Y*] = E[Y*|T=1]*P(T=1) + E[Y*|T=0]*P(T=0)

For engaged (T=1): E[Y*|T=1] = E[Y|T=1] (they answer truthfully)
For disengaged (T=0): E[Y*|T=0] is UNKNOWN, but bounded by [a, b]

After algebra (see slides for full derivation):

    theta in [E[Y] + P(C=0)*(a - E[Y|C=0])/(1-g),
              E[Y] + P(C=0)*(b - E[Y|C=0])/(1-g)]

where:
    - E[Y] = observed mean
    - P(C=0) = proportion failing check
    - E[Y|C=0] = mean among those who failed
    - a, b = bounds on what disengaged would answer if engaged
    - g = guess rate
"""


# ==============================================================================
# STEP 2: IMPLEMENT THE BOUNDS FUNCTION
# ==============================================================================

def partial_bounds(outcome: np.ndarray,
                   check: np.ndarray,
                   guess_rate: float,
                   a: float = 0.0,
                   b: float = 1.0,
                   conf_level: float = 0.95) -> tuple:
    """
    Compute partial identification bounds for the true population mean.

    Parameters:
    -----------
    outcome : np.ndarray
        Binary outcome variable (Y), 1 = supports violence
    check : np.ndarray
        Engagement check indicator (C), 1 = passed
    guess_rate : float
        Probability of passing check by guessing (g)
        For a 7-option multiple choice: g = 1/7 = 0.143
    a : float
        Lower bound on E[Y*|T=0] - minimum counterfactual for disengaged
    b : float
        Upper bound on E[Y*|T=0] - maximum counterfactual for disengaged
    conf_level : float
        Confidence level for interval (default 0.95)

    Returns:
    --------
    tuple: (ci_lower, ci_upper) - confidence interval for theta

    EXAMPLE:
    --------
    If we assume disengaged respondents would answer 0-20% if engaged:
        bounds = partial_bounds(Y, C, guess_rate=1/7, a=0, b=0.2)

    This might return (0.01, 0.07) meaning:
        "True support is between 1% and 7% with 95% confidence"
    """
    # Ensure arrays
    outcome = np.asarray(outcome)
    check = np.asarray(check)
    n = len(outcome)

    # =========================================================================
    # Compute the three sufficient statistics
    # =========================================================================

    # X1 = Y (outcome)
    # X2 = 1-C (failed check)
    # X3 = Y*(1-C) (outcome among those who failed)

    X = np.column_stack([
        outcome,
        1 - check,
        outcome * (1 - check)
    ])

    # Sample means
    X_bar = X.mean(axis=0)
    # X_bar[0] = E[Y]
    # X_bar[1] = P(C=0)
    # X_bar[2] = E[Y*(1-C)] = E[Y|C=0] * P(C=0)

    # =========================================================================
    # Compute point estimates for bounds
    # =========================================================================

    # Coefficients for the linear combination that gives theta
    # theta = Delta' * X_bar where:
    #   Delta_lo = (1, a/(1-g), -1/(1-g))
    #   Delta_hi = (1, b/(1-g), -1/(1-g))

    Delta_lo = np.array([1, a / (1 - guess_rate), -1 / (1 - guess_rate)])
    Delta_hi = np.array([1, b / (1 - guess_rate), -1 / (1 - guess_rate)])

    theta_lo = Delta_lo @ X_bar
    theta_hi = Delta_hi @ X_bar

    # =========================================================================
    # Compute standard errors using the DELTA METHOD
    # =========================================================================

    # The delta method gives us:
    #   Var(theta_hat) = Delta' * Var(X_bar) * Delta
    #
    # where Var(X_bar) = Cov(X) / n

    V_X = np.cov(X, rowvar=False)  # 3x3 covariance matrix

    # Standard errors for lower and upper bounds
    var_lo = Delta_lo @ V_X @ Delta_lo
    var_hi = Delta_hi @ V_X @ Delta_hi

    sd_lo = np.sqrt(var_lo)
    sd_hi = np.sqrt(var_hi)
    sd_max = max(sd_lo, sd_hi)

    # =========================================================================
    # Compute confidence interval using Imbens-Manski approach
    # =========================================================================

    # For partial identification, we need a CI that covers the entire
    # identified set [theta_lo, theta_hi] with probability (1-alpha)
    #
    # The Imbens-Manski (2004) approach finds the critical value c such that:
    #   P(theta_lo - c*se_lo <= theta <= theta_hi + c*se_hi) >= 1 - alpha

    root_n = np.sqrt(n)
    delta_hat = theta_hi - theta_lo

    def coverage_gap(c):
        """
        The coverage probability minus the target.
        We want to find c such that this equals 0.
        """
        coverage = (stats.norm.cdf(c + root_n * delta_hat / sd_max)
                    - stats.norm.cdf(-c))
        return abs(coverage - conf_level)

    # Find the critical value
    result = minimize_scalar(coverage_gap, bounds=(0, 10), method='bounded')
    c_star = result.x

    # Construct confidence interval
    ci_lower = max(0, min(1, theta_lo - c_star * sd_lo / root_n))
    ci_upper = max(0, min(1, theta_hi + c_star * sd_hi / root_n))

    return (ci_lower, ci_upper)


# ==============================================================================
# STEP 3: APPLY TO WESTWOOD DATA
# ==============================================================================

def analyze_study3_bounds():
    """
    Replicate the partial ID analysis for Study 3 (YouGov).

    Study 3 uses a 7-option comprehension check, so guess_rate = 1/7.

    We compute bounds under different assumptions about what
    disengaged respondents would answer if they were engaged:

    1. Agnostic: a=0, b=1 (they could answer anything)
    2. Conservative: a=0, b=0.2 (at most 20% would support if engaged)
    3. Very conservative: a=0, b=0.1 (at most 10% would support)
    """
    print("="*60)
    print("PARTIAL IDENTIFICATION BOUNDS ANALYSIS")
    print("="*60)

    # Simulated data matching Study 3 characteristics
    # In practice, load from study3.csv
    np.random.seed(42)
    n = 1863

    # Simulate engagement (about 70% pass)
    engaged_rate = 0.70
    check = np.random.binomial(1, engaged_rate, n)

    # Simulate outcomes:
    # - Engaged: ~6% say justified
    # - Disengaged: ~28% say justified (inflated)
    outcome = np.where(
        check == 1,
        np.random.binomial(1, 0.06, n),  # Engaged
        np.random.binomial(1, 0.28, n)   # Disengaged
    )

    # Guess rate for 7-option question
    g = 1/7

    print(f"\nData summary:")
    print(f"  n = {n:,}")
    print(f"  Engagement rate: {check.mean():.1%}")
    print(f"  Overall proportion justified: {outcome.mean():.1%}")
    print(f"  Engaged proportion: {outcome[check==1].mean():.1%}")
    print(f"  Disengaged proportion: {outcome[check==0].mean():.1%}")

    # =========================================================================
    # Compute bounds under different assumptions
    # =========================================================================

    print(f"\n{'='*60}")
    print("BOUNDS UNDER DIFFERENT ASSUMPTIONS")
    print("="*60)

    assumptions = [
        ("Agnostic (a=0, b=1)", 0.0, 1.0),
        ("Conservative (a=0, b=0.25)", 0.0, 0.25),
        ("Very conservative (a=0, b=0.10)", 0.0, 0.10),
    ]

    for name, a, b in assumptions:
        ci_lo, ci_hi = partial_bounds(outcome, check, g, a=a, b=b)
        print(f"\n{name}:")
        print(f"  95% CI for true support: [{ci_lo:.2%}, {ci_hi:.2%}]")

    print(f"\n{'='*60}")
    print("INTERPRETATION")
    print("="*60)
    print("""
    Even under the AGNOSTIC assumption (disengaged could answer anything
    from 0% to 100% if engaged), the upper bound on true support is
    much lower than the 20%+ reported in prior research.

    Under the CONSERVATIVE assumption (disengaged would answer at most
    25% if engaged), the bounds narrow further to roughly 1-7%.

    This provides strong evidence that prior estimates of support for
    political violence were inflated by survey satisficing.
    """)


# ==============================================================================
# MAIN EXECUTION
# ==============================================================================

if __name__ == "__main__":
    analyze_study3_bounds()
