# Persimmon

"Persimmon: Nested Family Polymorphism with Extensible Variant Types" has been published in SPLASH OOPSLA 2024. 
The paper and our supplementary materials are available [here](https://dl.acm.org/doi/10.1145/3649836).

Our implementation, along with a Dockerfile and setup instructions, is also [available on Zenodo](https://doi.org/10.5281/zenodo.10798266).

In this repository, we include the following files:

- `build.sbt`, with our SBT settings
- `src/syntax.scala`, with the representation of our system
- `src/typing.scala`, with our type checker
- `src/linkages.scala`, with our linkage creation/concatenation engine
- `src/parser.scala`, with our parser
- `src/util.scala`, with the helper functions and substitution functions
- `src/prettyprint.scala`, with pretty-printing capabilities for linkages
- `src/reduction.scala`, with our operational semantics
- `src/wf.scala`, with our well-formedness checking
- `test/*`, our testing suite
- `res/*`, with some sample programs

# Running

To run the tests, `sbt test`