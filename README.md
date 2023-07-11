# Persimmon

We include the following files:

- `build.sbt`, with our SBT settings
- `src/syntax.scala`, with the representation of our system
- `src/typing.scala`, with our type checker
- `src/linkages.scala`, with our linkage creation/concatenation engine
- `src/parser.scala`, with our parser
- `src/util.scala`, with the helper functions and substitution functions
- `src/prettyprint.scala`, with pretty-printing capabilities for linkages
- `src/reduction.scala`, with our operational semantics
- `src/wf.scala`, with our well-formedness checking
- `test/testing.scala`, our testing suite
- `res/*`, with some sample programs

# Running

To run the tests, `sbt test`