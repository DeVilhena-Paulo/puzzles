# pentominoes.py
#
# Solution to Jane Street's Hooks 11 puzzle (September 2025).


# -----------------------------------------------------------------------------
# Running instructions.
#
#   python -m venv ./.venv
#   source ./.venv/bin/activate
#   pip install ortools
#   python pentominoes.py
#
# Note: during the first time running the script, it can take a few seconds
#   until anything appears on the screen.


# -----------------------------------------------------------------------------
# Problem statement.
#
# Partially fill a 9x9 grid with 'd' copies of every digit from 1 to 9 in a way
#   that satisfies numerous constraints:
#
# 1. The filled squares must be covered by exactly nine pentominoes each of a
#      different kind (up to rotation and reflection).
#
# 2. The sum of the squares in each pentomino must be divisible by five.
#
# 3. The grid must be partitioned into 9 distinct L-shaped hooks with sizes
#      1x1, 2x2, ..., 9x9.
#
# 4. Each hook must cover all 'd' copies of a digit d, and no other digit.
#
# 5. Every 2x2 subgrid must contain at least one unfilled square.
#
# 6. The filled squares must form one unique connected region. (The 'connected'
#      relation is the transitive closure of the base relation that relates any
#      two horizontally/vertically adjacent filled squares.)
#
# 7. The filled squares must satisfy some initial constraints: some squares are
#      already filled and, for some rows or columns, the first filled square
#      (when analysing the row/column in a given direction) must contain a
#      certain digit or be covered by a certain pentomino.


# -----------------------------------------------------------------------------
# Solution.
#
# The general idea is to encode this problem as a Constraint-Programming (CP)
# problem and use an off-the-shelf CP-solving tool such as Google's OR-Tools.
# Expressing the connectivity constraint is possible, but leads to a very slow
# search (which, in my tests, I didn't see terminate). Fortunately, this
# constraint doesn't need to be directly expressed. The other constraints are
# sufficiently restrictive that the generated solutions almost satisfy the
# connecttivity constraint. To get a fully connected solution, it suffices to
# add some simple extra constraints (two to be precise) to avoid the bad
# solutions.


# -----------------------------------------------------------------------------
# Libraries.

from ortools.sat.python import cp_model


# -----------------------------------------------------------------------------
# Parameters.

N = 9
ROW = range(N)
COL = range(N)
GRID = list((r, c) for r in ROW for c in COL)
DIGIT = range(0, N + 1)


# -----------------------------------------------------------------------------
# 1. Generate all pentominoes.
#
# We compute all possible positions a pentomino could occupy in a 9x9 grid.

# Instructions to compute the squares covered by a pentomino:
#   For each pentomino p, and for a fixed square of p, we specify the shifts
#   between p and every other square of p.
pentomino_shifts = {
    'P': [(0, 0),  (0, 1),   (1, 0),  (1, 1),  (2, 0)],
    'X': [(0, 0),  (0, 1),  (-1, 0), (0, -1),  (1, 0)],
    'F': [(0, 0),  (0, 1),   (1, 0),  (2, 0), (1, -1)],
    'V': [(0, 0),  (1, 0),   (2, 0),  (2, 1),  (2, 2)],
    'W': [(0, 0),  (1, 0),   (1, 1),  (2, 1),  (2, 2)],
    'Y': [(0, 0),  (1, 0),   (2, 0),  (3, 0), (1, -1)],
    'I': [(0, 0),  (1, 0),   (2, 0),  (3, 0),  (4, 0)],
    'T': [(0, 0),  (1, 0),   (2, 0), (0, -1),  (0, 1)],
    'Z': [(0, 0),  (1, 0),   (2, 0), (0, -1),  (2, 1)],
    'U': [(0, 0), (-1, 0), (-1, -1),  (1, 0), (1, -1)],
    'N': [(0, 0), (0, -1),   (1, 0),  (1, 1),  (1, 2)],
    'L': [(0, 0),  (1, 0),   (2, 0),  (3, 0), (3, -1)],
}

def in_bounds(cell):
    """Checks whether a cell is within bounds."""
    return 0 <= cell[0] and cell[0] < N and 0 <= cell[1] and cell[1] < N

def all_in_bounds(cells):
    """Checks whether every cell in cells are within bounds."""
    return all(map(in_bounds, cells))

def normalize(shifts):
    """Translate shifts so that it has only nonnegative shifts and it is the
    closest to (0, 0)."""
    dx = min(x for x, _ in shifts)
    dy = min(y for _, y in shifts)
    return [(x - dx, y - dy) for x, y in shifts]

def flip_x(shifts):
    """Flip shifts on the x axis."""
    return normalize([(-x, y) for x, y in shifts])

def flip_y(shifts):
    """Flip shifts on the y axis."""
    return normalize([(x, -y) for x, y in shifts])

def rotate(shifts):
    """Rotate shifts by 90 degrees to the left."""
    return normalize([(-y, x) for x, y in shifts])

def fpow(f, n, x):
    """Returns f(...(f(x))) with f applied n times."""
    return x if n <= 0 else fpow(f, n - 1, f(x))

# Flips including the identity.
flips = [lambda shifts: shifts, flip_x, flip_y]

# Rotations including the identity.
rotations = \
    list(map(lambda k: lambda shifts: fpow(rotate, k, shifts), range(4)))
# Note. The definition map(lambda k: ..., range(4)) looks overly complicated,
# but the redefinition [... for k in range (4)] would be wrong because '...' in
# this case is a lambda expression that captures variables with dynamic scope.

def all_coverings(shifts):
    """Computes the set of all possible coverings of the shape specified by
    shifts."""
    size = len(shifts)
    coverings = set()
    for r, c in GRID:
        for flip in flips:
            for rotation in rotations:
                covering = {(r + dr, c + dc) for dr, dc in rotation(flip(shifts))}
                if all_in_bounds(covering):
                    assert(len(covering) == size)
                    coverings.add(tuple(sorted(covering)))
    return coverings

# Coverings of pentominoes per pentomino type.
pentominoes = {
    pentomino: all_coverings(shifts)
    for pentomino, shifts in pentomino_shifts.items()
    # exclude 'P' because we know it won't be used (because it violates the 2x2-subgrid constraint)
    if pentomino != 'P'
}

# Pentominoes that cover a given square.
pentominoes_per_square = [ \
    [{pentomino: [i for i, shape in enumerate(shapes) if (r, c) in shape]
      for pentomino, shapes in pentominoes.items()}
     for c in COL]
    for r in ROW
]


# -----------------------------------------------------------------------------
# 2. Generate all hooks.
#
# We compute all possible positions a hook could occupy in a 9x9 grid.

# For each hook h, we specify the shifts to generate h starting from (0, 0).
hook_shifts = {
    h: [(0, 0)] + [(x, 0) for x in range(1, h)] + [(0, y) for y in range(1, h)]
    for h in range(1, N + 1)
}

# Coverings of hooks per hook type/size.
hooks = {h: all_coverings(shifts) for h, shifts in hook_shifts.items()}

# Hooks that cover a given square.
hooks_per_square = [ \
    [{h: [i for i, shape in enumerate(shapes) if (r, c) in shape]
      for h, shapes in hooks.items()}
     for c in COL]
    for r in ROW
]


# -----------------------------------------------------------------------------
# 3. Constraints

def create_digits(model):
    """Introduces N x N x (N + 1) Boolean variables to represent the
    assignement of digits in the grid."""
    digits = [
        [[model.NewBoolVar(f"digits[{r}][{c}][{d}]") for d in DIGIT]
         for c in COL]
        for r in ROW
    ]

    # one digit per square
    for r, c in GRID:
        model.Add(sum(digits[r][c]) == 1)

    return digits

def occurrences_of_digits(model, digits):
    """Introduces constraint that, for each digit d in [1..9], there are d
    occurrences of d in the grid."""
    for d in range(1, 10):
        model.Add(sum(digits[r][c][d] for r, c in GRID) == d)

def create_filled(model, digits):
    """Introduces N x N Boolean variables expressing whether a square is filled."""
    filled = [[model.NewBoolVar(f"filled[{r}][{c}]") for c in COL] for r in ROW]

    # defining conditon of filled
    for r, c in GRID:
        model.Add(digits[r][c][0] == 0).OnlyEnforceIf(filled[r][c])
        model.Add(digits[r][c][0] == 1).OnlyEnforceIf(filled[r][c].Not())

    return filled

def no_fully_filled_two_by_two_subgrid(model, filled):
    """ Add constraint that every 2x2 subgrid must have at least one unfilled square."""
    deltas = [(0, 0), (1, 0), (0, 1), (1, 1)]
    for r, c in GRID:
        if r + 1 < N and c + 1 < N:
            model.Add(sum(filled[r + dr][c + dc] for dr, dc in deltas) <= 3)

def create_hook_selection(model):
    """Introduces a number of Boolean variables hook_selection[h][i] per hook
    type h to express whether i-th hook of type h is selected."""
    hook_selection = {
        h: [
            model.NewBoolVar(f"hook_selection[{h}][{i}]")
            for i in range(len(shapes))
        ]
        for h, shapes in hooks.items()
    }

    # exactly one hook per size
    for _, selection in hook_selection.items():
        model.Add(sum(selection) == 1)

    return hook_selection

def hooks_cover_the_grid(model, hook_selection):
    """Adds constraint that every square must be covered by a hook."""
    for r, c in GRID:
        model.Add(
            sum(sum(hook_selection[h][i] for i in hooks_per_square[r][c][h])
                for h in hook_selection.keys()) == 1
        )

def create_digit_per_hook(model):
    """Introduces N Boolean variables per hook h to indicate the choice of the
    digit contained in h."""
    digit_per_hook = {
        h: {d: model.NewBoolVar(f"digit_per_hook[{h}][{d}]") for d in DIGIT[1::]}
        for h in hooks.keys()
    }

    # exactly one digit per hook
    for h, digit_selection in digit_per_hook.items():
        model.Add(sum(digit_selection.values()) == 1)

    return digit_per_hook

def occurrences_of_digit_chosen_by_hook(model, digits, hook_selection, digit_per_hook):
    """Adds constraint that for each hook h, there is a digit d that
    occurs d times in h."""
    for h, digit_selection in digit_per_hook.items():
        for d in digit_selection.keys():
            for i, shape in enumerate(hooks[h]):
                model.Add(sum(digits[r][c][d] for r, c in shape) == d) \
                     .OnlyEnforceIf(digit_per_hook[h][d]) \
                     .OnlyEnforceIf(hook_selection[h][i])

def create_pentomino_selection(model):
    """Introduces a number of Boolean variables per pentomino type
    to express whether i-th pentomino of a given type is selected."""

    # selection of pentominoes
    pentomino_selection = {
        pentomino: [
            model.NewBoolVar(f"pentomino_selection[{pentomino}][{i}]")
            for i in range(len(shapes))
        ]
        for pentomino, shapes in pentominoes.items()
    }

    # maximum one pentomino per type
    for _, selection in pentomino_selection.items():
        model.Add(sum(selection) <= 1)

    # exactly 9 pentominoes
    model.Add(sum(sum(pentomino_selection[pentomino]) for pentomino in pentomino_selection.keys()) == 9)

    return pentomino_selection

def filled_squares_are_covered_by_pentominoes(model, pentomino_selection, filled):
    """Adds constraint that every filled square is covered by exactly one pentomino."""
    for r, c in GRID:
        model.Add(
            sum(sum(pentomino_selection[pentomino][i] for i in pentominoes_per_square[r][c][pentomino])
                for pentomino in pentomino_selection.keys()) == 1
        ).OnlyEnforceIf(filled[r][c])

        model.Add(
            sum(sum(pentomino_selection[pentomino][i] for i in pentominoes_per_square[r][c][pentomino])
                for pentomino in pentomino_selection.keys()) == 0
        ).OnlyEnforceIf(filled[r][c].Not())

def sum_of_digits_covered_by_each_pentomino(model, digits, pentomino_selection):
    """"Adds constraint that the sum of digits covered by a selected pentomino
    must be divisible by 5."""

    # auxiliary variable to store the sum of digits per pentomino
    sum_per_pentomino = {
        pentomino: [model.NewIntVar(0, 45, f"sum_per_pentomino[{pentomino}][{i}]")
                    for i in range(len(shapes))]
        for pentomino, shapes in pentominoes.items()
    }

    for pentomino, shapes in pentominoes.items():
        for i, shape in enumerate(shapes):
            # defining condition on sum_per_pentomino variable
            count = sum(sum(digits[r][c][d] * d for r, c in shape) for d in DIGIT)
            model.Add(sum_per_pentomino[pentomino][i] == count)
    
            # enforcing divisibility condition (unfortunately the tool doesn't
            # allow one to simply write '_ % 5 == 0' as a constraint)
            remainder = model.NewIntVar(0, 4, "remainder")
            model.AddModuloEquality(remainder, sum_per_pentomino[pentomino][i], 5)
            model.Add(remainder == 0).OnlyEnforceIf(pentomino_selection[pentomino][i])


# -----------------------------------------------------------------------------
# 4. Initial constraints.

# Prefilled squares of the grid.
prefilled_squares = {
    (0, 4): 5,
    (1, 3): 4,
    (4, 4): 1,
    (7, 5): 8,
    (8, 4): 9
}

# Left-to-right row constraints
lr_row_constraints = {
    0: {'pentomino': 'I'},
    3: {'digit': 6},
    5: {'pentomino': 'N'},
    8: {'pentomino': 'Z'}
}

# Right-to-left row constraints
rl_row_constraints = {
    0: {'pentomino': 'U'},
    3: {'pentomino': 'X'},
    5: {'digit': 2},
    8: {'pentomino': 'V'}
}

# Up-down column 6 constraint
ud_col_6_constraint = 7

# down-up column 2 constraint
du_col_2_constraint = 3

def digits_in_prefilled_squares(model, digits):
    """Adds constraint of digits in prefilled squares."""
    for (r, c), d in prefilled_squares.items():
        model.Add(digits[r][c][d] == 1)

def add_lr_row_constraints(model, digits, filled, pentomino_selection):
    """Adds left-to-right row constraints."""

    # left-to-right row boundary
    lr_row_boundary = {
        r: [model.NewBoolVar("lr_row_boundary[{r}][{c}]") for c in COL]
        for r in lr_row_constraints.keys()
    }

    # lr_row_boundary[r][c] = 1 <->
    #   (r, c) is the first filled square when looking from left to right in r
    for r in lr_row_constraints.keys():
        for c in COL:
            # tmp = 1 <-> sum(filled[r][0:c]) = 0
            tmp = model.NewBoolVar("tmp")
            count = sum(filled[r][0:c])
            model.Add(count == 0).OnlyEnforceIf(tmp)
            model.Add(count != 0).OnlyEnforceIf(tmp.Not())
    
            # tmp = 1 /\ filled[r][c] = 1 --> lr_row_boundary[r][c] = 1
            model.Add(lr_row_boundary[r][c] == 1).OnlyEnforceIf(filled[r][c]).OnlyEnforceIf(tmp)
    
            # tmp = 0 \/ filled[r][c] = 0 --> lr_row_boundary[r][c] = 0
            model.Add(lr_row_boundary[r][c] == 0).OnlyEnforceIf(filled[r][c].Not())
            model.Add(lr_row_boundary[r][c] == 0).OnlyEnforceIf(tmp.Not())

    # enforcing left-to-right row constraints
    for r, constraints in lr_row_constraints.items():
        if 'digit' in constraints:
            for c in COL:
                d = lr_row_constraints[r]['digit']
                model.Add(digits[r][c][d] == 1).OnlyEnforceIf(lr_row_boundary[r][c])
        if 'pentomino' in constraints:
            for c in COL:
                pentomino = lr_row_constraints[r]['pentomino']
                count = sum(pentomino_selection[pentomino][i] for i in pentominoes_per_square[r][c][pentomino])
                model.Add(count == 1).OnlyEnforceIf(lr_row_boundary[r][c])

def add_rl_row_constraints(model, digits, filled, pentomino_selection):
    """Adds right-to-left row constraints."""

    # right-to-left boundary
    rl_row_boundary = {
        r: [model.NewBoolVar("lr_row_boundary[{r}][{c}]") for c in COL]
        for r in lr_row_constraints.keys()
    }

    # rl_row_boundary[r][c] = 1 <->
    #   (r, c) is the first filled square when looking from right to left in r
    for r in rl_row_constraints.keys():
        for c in COL:
            # tmp = 1 <-> sum(filled[r][c + 1::]) = 0
            tmp = model.NewBoolVar("tmp")
            count = sum(filled[r][c + 1::])
            model.Add(count == 0).OnlyEnforceIf(tmp)
            model.Add(count != 0).OnlyEnforceIf(tmp.Not())
    
            # tmp = 1 /\ filled[r][c] = 1 --> rl_row_boundary[r][c] = 1
            model.Add(rl_row_boundary[r][c] == 1).OnlyEnforceIf(filled[r][c]).OnlyEnforceIf(tmp)
    
            # tmp = 0 \/ filled[r][c] = 0 --> rl_row_boundary[r][c] = 1
            model.Add(rl_row_boundary[r][c] == 0).OnlyEnforceIf(filled[r][c].Not())
            model.Add(rl_row_boundary[r][c] == 0).OnlyEnforceIf(tmp.Not())

    # enforcing right-to-left row constraints
    for r, constraints in rl_row_constraints.items():
        if 'digit' in constraints:
            for c in COL:
                d = rl_row_constraints[r]['digit']
                model.Add(digits[r][c][d] == 1).OnlyEnforceIf(rl_row_boundary[r][c])
        if 'pentomino' in constraints:
            for c in COL:
                pentomino = rl_row_constraints[r]['pentomino']
                count = sum(pentomino_selection[pentomino][i] for i in pentominoes_per_square[r][c][pentomino])
                model.Add(count == 1).OnlyEnforceIf(rl_row_boundary[r][c])

def add_ud_col_constraints(model, digits, filled):
    """Adds up-down column 6 constraints."""

    # up-down column 6 boundary
    ud_col_6_boundary = [model.NewBoolVar(f"ud_col_6_boundary[{r}]") for r in ROW]

    # ud_col_6_boundary[r] = 1 <->
    #   filled[r][6] /\ sum(filled[i][6] for i in range(r)) = 0
    for r in ROW:
        # tmp = 1 <-> sum(filled[0:r][6]) = 0
        tmp = model.NewBoolVar("tmp")
        count = sum(filled[i][6] for i in range(r))
        model.Add(count == 0).OnlyEnforceIf(tmp)
        model.Add(count != 0).OnlyEnforceIf(tmp.Not())

        # tmp = 1 /\ filled[r][6] = 1 --> ud_col_6_boundary[r] = 1
        model.Add(ud_col_6_boundary[r] == 1).OnlyEnforceIf(filled[r][6]).OnlyEnforceIf(tmp)

        # tmp = 0 \/ filled[r][6] = 0 --> ud_col_6_boundary[r] = 0
        model.Add(ud_col_6_boundary[r] == 0).OnlyEnforceIf(filled[r][6].Not())
        model.Add(ud_col_6_boundary[r] == 0).OnlyEnforceIf(tmp.Not())

    # enforcing up-down column 6 constraint
    for r in ROW:
        model.Add(digits[r][6][7] == 1).OnlyEnforceIf(ud_col_6_boundary[r])

def add_du_col_constraints(model, digits, filled):
    """Adds down-up column 2 constraints."""

    # down-up column 2 boundary
    du_col_2_boundary = [model.NewBoolVar(f"du_col_2_boundary[{r}]") for r in ROW]

    # du_col_2_boundary[r] = 1 <->
    #   filled[r][2] /\ sum(filled[i][2] for i in range(r + 1, N)) = 0
    for r in ROW:
        # tmp = 1 <-> sum(filled[r::][2]) = 0
        tmp = model.NewBoolVar("tmp")
        count = sum(filled[i][2] for i in range(r + 1, N))
        model.Add(count == 0).OnlyEnforceIf(tmp)
        model.Add(count != 0).OnlyEnforceIf(tmp.Not())
    
        # tmp = 1 /\ filled[r][2] = 1 --> du_col_2_boundary[r][2] = 1
        model.Add(du_col_2_boundary[r] == 1).OnlyEnforceIf(filled[r][2]).OnlyEnforceIf(tmp)
    
        # tmp = 0 \/ filled[r][2] = 0 --> du_col_2_boundary[r][2] = 0
        model.Add(du_col_2_boundary[r] == 0).OnlyEnforceIf(filled[r][2].Not())
        model.Add(du_col_2_boundary[r] == 0).OnlyEnforceIf(tmp.Not())

    # enforcing down-up column 2 constraint
    for r in ROW:
        model.Add(digits[r][2][3] == 1).OnlyEnforceIf(du_col_2_boundary[r])

def add_hacks(model, filled):
  """Eliminates some bad solutions that violate connectivity."""

  model.Add(sum(filled[r][5] for r in range(6)) + sum(filled[5][6::]) != 0) \
       .OnlyEnforceIf(filled[1][6])

  frontier = [(1, 7), (2, 6), (2, 8), (3, 5), (4, 6), (4, 8), (5, 7)]
  model.Add(sum(filled[r][c] for r, c in frontier) != 0) \
       .OnlyEnforceIf(filled[2][7])


# -----------------------------------------------------------------------------
# 5. Solve.

class SolutionCounter(cp_model.CpSolverSolutionCallback):
    """Count intermediate solutions."""

    def __init__(self):
        cp_model.CpSolverSolutionCallback.__init__(self)
        self.__solution_count = 0

    def on_solution_callback(self) -> None:
        self.__solution_count += 1

    @property
    def solution_count(self) -> int:
        return self.__solution_count


def print_grids(names, grids):
    k, n, m = len(grids), len(grids[0]), len(grids[0][0])

    fgrids = [["" for _ in range(k)] for _ in range(n)]
    for i in range(n):
        for j in range(k):
            fgrids[i][j] = " ".join(grids[j][i])

    fnames = [name + ":" + (" " * (2 * m - len(name) - 2)) for name in names]

    # printing
    space = " | "
    print(space.join(fnames))
    for frow in fgrids:
        print(space.join(frow))

def ask(prompt):
    answer = input(f"{prompt} [y/n]: ").strip().lower()
    return answer in ("y", "yes")

def main():
    # ask whether user wants to count all solutions
    count_solutions = ask("count all solutions")

    # ask whether user wants to see search progress
    verbose = ask("see search progress")

    # create model
    model = cp_model.CpModel()

    # add constraints
    digits = create_digits(model)
    occurrences_of_digits(model, digits)

    filled = create_filled(model, digits)
    no_fully_filled_two_by_two_subgrid(model, filled)

    hook_selection = create_hook_selection(model)
    hooks_cover_the_grid(model, hook_selection)

    digit_per_hook = create_digit_per_hook(model)
    occurrences_of_digit_chosen_by_hook(model, digits, hook_selection, digit_per_hook)

    pentomino_selection = create_pentomino_selection(model)
    filled_squares_are_covered_by_pentominoes(model, pentomino_selection, filled)
    sum_of_digits_covered_by_each_pentomino(model, digits, pentomino_selection)

    # add initial constraints
    digits_in_prefilled_squares(model, digits)

    add_lr_row_constraints(model, digits, filled, pentomino_selection)
    add_rl_row_constraints(model, digits, filled, pentomino_selection)

    add_ud_col_constraints(model, digits, filled)
    add_du_col_constraints(model, digits, filled)

    add_hacks(model, filled)

    # create solver
    solver = cp_model.CpSolver()
    solution_counter = SolutionCounter()

    # enumerate all solutions
    if count_solutions:
        solver.parameters.enumerate_all_solutions = True

    # show search progress
    if verbose:
        solver.parameters.log_search_progress = True

    # solve
    status = solver.Solve(model, solution_counter)

    print("\nstatistics")

    if count_solutions:
        print(f"- number of solutions found: {solution_counter.solution_count}")

    print(f"- conflicts ('dead ends in the search tree'): {solver.num_conflicts}")
    print(f"- branches: {solver.num_branches}")
    print(f"- wall time: {solver.wall_time:.2f}s")
    print(f"- status: {status}, where ...")
    print(f"  ... {cp_model.OPTIMAL} = optimal")
    print(f"  ... {cp_model.INFEASIBLE} = infeasible")
    print(f"  ... {cp_model.FEASIBLE} = feasible")
    print(f"  ... {cp_model.MODEL_INVALID} = invalid")
    print(f"  ... {cp_model.UNKNOWN} = unknown\n")

    if status in (cp_model.OPTIMAL, cp_model.FEASIBLE):
        # digits
        digits_grid = [[str(sum(d * solver.Value(digits[r][c][d]) for d in DIGIT)) for c in COL] for r in ROW]

        # pentominoes
        pentominoes_grid = [['.' for _c in COL] for _r in ROW]
        for pentomino, shapes in pentominoes.items():
            for i, shape in enumerate(shapes):
                if solver.Value(pentomino_selection[pentomino][i]):
                    for r, c in shape:
                        pentominoes_grid[r][c] = pentomino

        # hooks
        hooks_grid = [['_' for _c in COL] for _r in ROW]
        for h, shapes in hooks.items():
            for i, shape in enumerate(shapes):
                if solver.Value(hook_selection[h][i]):
                    for r, c in shape:
                        hooks_grid[r][c] = str(h)

        # print
        print_grids(['digits', 'pentominoes', 'hooks'], [digits_grid, pentominoes_grid, hooks_grid])


if __name__ == "__main__":
    main()
