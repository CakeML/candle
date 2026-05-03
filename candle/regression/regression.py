import time
import common
import dependencies as deps
from common import HOL_PATH
from repl import CandleREPL, REPLError

GREAT_100_THEOREMS = [
    "100/arithmetic_geometric_mean.ml",
    "100/arithmetic.ml",
    "100/ballot.ml",
    "100/bernoulli.ml",
    "100/bertrand.ml",
    "100/primerecip.ml",
    "100/birthday.ml",
    "100/buffon.ml",
    "100/cantor.ml",
    "100/cayley_hamilton.ml",
    "100/ceva.ml",
    "100/circle.ml",
    "100/chords.ml",
    "100/combinations.ml",
    "100/constructible.ml",
    "100/cosine.ml",
    "100/cubedissection.ml",
    "100/cubic.ml",
    "100/derangements.ml",
    "100/desargues.ml",
    "100/descartes.ml",
    "100/dirichlet.ml",
    "100/div3.ml",
    "100/divharmonic.ml",
    "100/e_is_transcendental.ml",
    "100/euler.ml",
    "100/feuerbach.ml",
    "100/fourier.ml",
    "100/four_squares.ml",
    "100/friendship.ml",
    "100/fta.ml",
    "100/gcd.ml",
    "100/green.ml",
    "100/heron.ml",
    "100/isoperimetric.ml",
    "100/inclusion_exclusion.ml",
    "100/independence.ml",
    "100/isosceles.ml",
    "100/konigsberg.ml",
    "100/lagrange.ml",
    "100/leibniz.ml",
    "100/lhopital.ml",
    "100/liouville.ml",
    "100/minkowski.ml",
    "100/morley.ml",
    "100/pascal.ml",
    "100/perfect.ml",
    "100/pick.ml",
    "100/piseries.ml",
    "100/platonic.ml",
    "100/pnt.ml",
    "100/polyhedron.ml",
    "100/ptolemy.ml",
    "100/pythagoras.ml",
    "100/quartic.ml",
    "100/ramsey.ml",
    "100/ratcountable.ml",
    "100/realsuncountable.ml",
    "100/reciprocity.ml",
    "100/stirling.ml",
    "100/subsequence.ml",
    "100/thales.ml",
    "100/transcendence.ml",
    "100/triangular.ml",
    "100/two_squares.ml",
    "100/wilson.ml",
]


def run_tree(repl, node, results, elapsed=0.0):
    name = node.filepath
    start = time.monotonic()
    try:
        repl.load(name)
    except REPLError as e:
        elapsed += time.monotonic() - start
        for leaf in node.leaves():
            results[leaf.name] = (e, elapsed)
        return
    elapsed += time.monotonic() - start

    children = list(node.children.values())
    if not children:
        results[name] = (None, elapsed)
    elif len(children) == 1:
        run_tree(repl, children[0], results, elapsed)
    else:
        checkpoint = repl.checkpoint()
        run_tree(repl, children[0], results, elapsed)
        for child in children[1:]:
            repl.restore(checkpoint)
            run_tree(repl, child, results, elapsed)


def print_results(results):
    for name, (error, elapsed) in results.items():
        status = "PASS" if error is None else "FAIL"
        print(f"{status} {elapsed:.2f}s {name}")
        if error:
            print(f"  {error}")


def print_timing(results, wall_elapsed):
    naive_total = sum(elapsed for _, elapsed in results.values())
    print(f"\nWall clock: {wall_elapsed:.2f}s")
    print(f"Naive total (sum of all leaf paths): {naive_total:.2f}s")
    print(f"Time saved: {naive_total - wall_elapsed:.2f}s")


if __name__ == "__main__":
    tests = GREAT_100_THEOREMS
    root = deps.dependency_tree(str(common.relative(HOL_PATH)), tests)
    results = {}
    repl = CandleREPL()

    wall_start = time.monotonic()
    run_tree(repl, root, results)
    wall_elapsed = time.monotonic() - wall_start

    print_results(results)
    print_timing(results, wall_elapsed)
