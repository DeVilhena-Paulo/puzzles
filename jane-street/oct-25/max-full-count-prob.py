# max-full-count-prob.py
#
# Solution to Jane Street's Robot Baseball puzzle (October 2025).


# -----------------------------------------------------------------------------
# Problem statement.
#
# An 'at-bat' is a game played between a _pitcher_ and a _batter_. At the end
# of the game the batter wins a number of points between 0 and 4. The goal of
# the batter is to maximise this expected final score and the goal of the
# pitcher is to minimise this expected final score. During the game there is
# a running score of balls ('b') and strikes ('s') both of which are initially
# zero. At each turn, the pitcher decides to throw either a _ball_ or a
# _strike_ and the the batter independently decides to either _wait_ or
# _swing_. There are four cases:
#
# C1. If the pitcher throws a ball and the batter waits, then 'b' is
#       incremented by one.
#
# C2. If the pitcher throws a ball and the batter swings, then 's' is
#       incremented by one.
#
# C3. If the pitcher throws a strike and the batter waits, then 's' is
#       incremented by one.
#
# C4. If the pitcher throws a strike and the batter swings, then, with
#       probability 'p', the batter hits a _home run_, otherwise (with
#       probability '1 - p'), 's' is incremented by one.
#
# There are three ending conditions:
#
# E1. If the batter hits a home run, the game ends and the batters wins 4 pts.
#
# E2. If 'b' reaches 4, the game ends and the batter wins 1 pt.
#
# E3. If 's' reaches 3, the game ends and the batter wins 0 pts.
#
# A _full count_ is the configuration where 'b = 3' and 's = 2'. Let 'q(p)' be
# the probability of a full count under the assumption that batter and pitcher
# both play optimally (and that each knows the other plays optimally). The
# answer to this puzzle are the 10 decimals of 'max_{p ∈ [0, 1]} q(p)'.


# -----------------------------------------------------------------------------
# Solution.
#
# If we write down the expected final score 'E[final score|(b, s)]' conditioned
# on the current number of balls ('b') and strikes ('s'), then we can see it as
# a bivariate function 'f(u, v)' over 'u', the probability that the pitcher
# throws a ball, and 'v', the probability that the batter waits:
#
#   E[final score|(b, s)] =
#   f(u, v) =
#     E[final score|(b + 1, s)] * u * v
#     + E[final score|(b, s + 1)] * (1 - u * v - (1 - u) * (1 - v) * p)
#     + 4 * (1 - u) * (1 - v) * p
#
# There are three key observations: (i) 'f' is symmetric ('f(u, v) = f(v, u)');
# (ii) for each fixed 'u' (resp. 'v'), the function 'v ↦ f(u, v)' (resp.
# 'u ↦ f(u, v)') is linear; and (iii) the graph of 'f' includes (exactly) one
# pair of orthogonal lines that are parallel to the 'u × v' plane. From (i) and
# (iii), we conclude that there is a point '(a, a)' such that 'v ↦ f(a, v)' and
# 'u ↦ f(u, a)' are constant. Finally, from (ii), we conclude that the optimal
# strategy the batter (resp. the pitcher) is to set 'v = a' (resp. 'u = a'),
# because, for every other choice of 'v', the line 'u ↦ f(u, v)' has a non-zero
# slope, so the pitcher can choose 'u' s.t. 'f(u, v) < f(u, a) = f(a, a)'. The
# same reasoning applies to justify that the pitcher must choose 'v = a'.
#
# To compute 'a', it suffices to compute the point where 'f' has both partial
# derivatives equal to zero:
#
#   (∂ f(u, v) / ∂ u) |_{a, a} = 0
#   -->
#   E[final score|(b + 1, s)] * a
#     + E[final score|(b, s + 1)] * (- a + (1 - a) * p)
#     - 4 * (1 - a) * p
#     = 0
#   -->
#   a = p * (4 - E[final score|(b, s + 1)]) /
#       (E[final score|(b + 1, s)] - E[final score|(b, s + 1)] * (1 + p) + 4 * p)
#
# Once we have 'a', we can compute 'E[final score|(b, s)]' as 'f(a, a)'. This
# computation depends only on 'E[final score|(b + 1, s)]',
# 'E[final score|(b, s + 1)]', and 'p'. Therefore, for each 'p', we can compute
# 'E[final score|(b, s)]' (as well as the optimal strategies for the pitcher and
# the batter at each configuration 'b' and 's') by dynamic programming using the
# following boundary constraints: 'E[final score|(4, s)] = 1' and
# 'E[final score|(b, 3)] = 0'.
#
# Finally, given the optimal strategies 'u(b, s)' for the pitcher and 'v(b, s)'
# for the batter at each configuration of 'b' and 's', we can compute the
# probability of a full count 'q(b, s)' at this configuration using the
# following recurrence (where 'u = u(b, s)' and 'v = v(b, s)'):
#
#   q(b, s) =
#     q(b + 1, s) * u * v
#     + q(b, s + 1) * (1 - u * v - (1 - u * v - (1 - u) * (1 - v) * p)
#
#   q(3, 2) = 1
#
# To compute the final answer, 'max q' up to 10 decimals, we use a simple
# iterative method where we decrease the size of an interval '[plow, phigh]'
# that contains 'p' s.t. 'q(p) = max q' until 'q(phigh) - q(plow) < 1e-11'.


# -----------------------------------------------------------------------------
# Libraries.

from fractions import Fraction


# -----------------------------------------------------------------------------
# Parameters.

B = 5 # number of possible ball scores (0, 1, 2, 3, 4)
S = 4 # number of possible strike scores (0, 1, 2, 3)


# -----------------------------------------------------------------------------
# 1. Compute probability of full count given 'p'.

def optimal_strategy(p):
    """Returns a table strategy of size '(B - 1) × (S - 1)' such that
    'strategy[b][s]' is the probability that the pitcher decides to throw
    a ball when the number of balls and strikes is respectively 'b' and 's'
    and so that the expected score is minimal."""

    # expected_score[b][s] = expected amount of points batter wins
    #   when the game ends
    expected_score = [[Fraction(0) for _ in range(S)] for _ in range(B)]

    # strategy[b][s] =
    #   prob. pitcher throws a ball assuming pitcher and batter play
    #   optimally =
    #   prob. batter waits assuming pitcher and batter play optimally
    strategy = [[Fraction(0) for _ in range(S - 1)] for _ in range(B - 1)]

    for s in range(S):
        expected_score[B - 1][s] = Fraction(1)

    for b in range(B - 2, -1, -1):
        for s in range(S - 2, -1, -1):
            strategy[b][s] = u = v = \
                (p * (4 - expected_score[b][s + 1])) / \
                (expected_score[b + 1][s] - expected_score[b][s + 1] * (1 + p) + 4 * p)

            expected_score[b][s] = \
                expected_score[b][s + 1] * (1 - u * v - (1 - u) * (1 - v) * p) + \
                expected_score[b + 1][s] * u * v + \
                4 * (1 - u) * (1 - v) * p

    return strategy


def full_count_prob(p):
    """Computes full-count probability given 'p'."""

    strategy = optimal_strategy(p)

    # q[b][s] = full-count prob. given 'b' and 's'
    q = [[Fraction(0) for _ in range(S - 1)] for _ in range(B - 1)]

    q[B - 2][S - 2] = Fraction(1)

    for s in range(S - 3, -1, -1):
        u = v = strategy[B - 2][s]
        q[B - 2][s] = \
            q[B - 2][s + 1] * (1 - u * v - (1 - u) * (1 - v) * p)

    for b in range(B - 3, -1, -1):
        u = v = strategy[b][S - 2]
        q[b][S - 2] = q[b + 1][S - 2] * u * v

    for b in range(B - 3, -1, -1):
        for s in range(S - 3, -1, -1):
            u = v = strategy[b][s]
            q[b][s] = \
                q[b][s + 1] * (1 - u * v - (1 - u) * (1 - v) * p) + \
                q[b + 1][s] * u * v

    return q[0][0]


# -----------------------------------------------------------------------------
# 2. Find maximal probability of full count.

def max_full_count_prob(precision):
    """Returns maximal full-count probability up to 'precision' decimals."""

    # initial range: [qlow, qhigh]
    plow, phigh = Fraction('1e-10'), Fraction(1)
    qlow, qhigh = full_count_prob(plow), full_count_prob(phigh)

    # when the difference between the limits of the range is
    # lower than '10 ** -precision', we've found the 'precision'
    # digits of the maximal full-count probability
    while abs(qhigh - qlow) > Fraction(1, 10 ** precision):
        # invariant: the maximal full-count probability 'q ∈ [qlow, qhigh]'

        pdelta = phigh - plow
        N = 10
        ps = [plow + (pdelta * i) / N for i in range(0, N + 1, 1)]

        qmid, i = max((full_count_prob(p), i) for i, p in enumerate(ps))

        plow  = plow + (pdelta * (i - 1)) / N
        phigh = plow + (pdelta * (i + 1)) / N

        qlow, qhigh = full_count_prob(plow), full_count_prob(phigh)

        # certify that maximal probability is in the range '[qlow, qhigh]'
        assert (qmid > qlow and qmid > qhigh)

    return max(qlow, qhigh)


# -----------------------------------------------------------------------------
# 3. Print result.

if __name__ == "__main__":
    print(f"{max_full_count_prob(11):1.11f}")
