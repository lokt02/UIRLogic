# Project for MEPHI university
This project contains implementation of logic structures and operations using Scala.

Console output of current code:
```
======================
truthTable method test
======================

Таблица истинности для формулы:

TruthTableRow(Map(A -> true, B -> true),true)
TruthTableRow(Map(A -> true, B -> false),true)
TruthTableRow(Map(A -> false, B -> true),true)
TruthTableRow(Map(A -> false, B -> false),false)

TruthTableRow(Map(A -> true, B -> true),true)
TruthTableRow(Map(A -> true, B -> false),false)
TruthTableRow(Map(A -> false, B -> true),false)
TruthTableRow(Map(A -> false, B -> false),false)

TruthTableRow(Map(A -> true),false)
TruthTableRow(Map(A -> false),true)

TruthTableRow(Map(A -> true),true)
TruthTableRow(Map(A -> false),false)





==========================================
isPerfectDisjunctiveNormalForm method test
==========================================

false
true


==========================================
isLogicalExpressionsEquivalent method test
==========================================

TruthTableRow(Map(A -> true, B -> true),true)
TruthTableRow(Map(A -> true, B -> false),true)
TruthTableRow(Map(A -> false, B -> true),true)
TruthTableRow(Map(A -> false, B -> false),false)

TruthTableRow(Map(A -> true, B -> true),true)
TruthTableRow(Map(A -> true, B -> false),true)
TruthTableRow(Map(A -> false, B -> true),true)
TruthTableRow(Map(A -> false, B -> false),false)

true

TruthTableRow(Map(A -> true, B -> true),true)
TruthTableRow(Map(A -> true, B -> false),true)
TruthTableRow(Map(A -> false, B -> true),true)
TruthTableRow(Map(A -> false, B -> false),false)

TruthTableRow(Map(A -> true, B -> true),false)
TruthTableRow(Map(A -> true, B -> false),false)
TruthTableRow(Map(A -> false, B -> true),false)
TruthTableRow(Map(A -> false, B -> false),true)

false



=======================================
truthTable method test but more complex
=======================================

TruthTableRow(Map(A -> true, C -> true, B -> true),false)
TruthTableRow(Map(A -> true, C -> true, B -> false),false)
TruthTableRow(Map(A -> true, C -> false, B -> true),true)
TruthTableRow(Map(A -> true, C -> false, B -> false),false)
TruthTableRow(Map(A -> false, C -> true, B -> true),true)
TruthTableRow(Map(A -> false, C -> true, B -> false),false)
TruthTableRow(Map(A -> false, C -> false, B -> true),false)
TruthTableRow(Map(A -> false, C -> false, B -> false),false)
8

TruthTableRow(Map(A -> true, B -> true, C -> true),false)
TruthTableRow(Map(A -> true, B -> true, C -> false),true)
TruthTableRow(Map(A -> true, B -> false, C -> true),false)
TruthTableRow(Map(A -> true, B -> false, C -> false),false)
TruthTableRow(Map(A -> false, B -> true, C -> true),true)
TruthTableRow(Map(A -> false, B -> true, C -> false),false)
TruthTableRow(Map(A -> false, B -> false, C -> true),false)
TruthTableRow(Map(A -> false, B -> false, C -> false),false)
8



=============
Use case test
=============

Here given logical expression:
And(Or(And(Variable(A),Not(Variable(C))),And(Not(Variable(A)),Variable(C))),Variable(B))
Make it to the Perfect Disjunctive Normal Form.

User's answer:
Or(And(Variable(A),And(Variable(B),Not(Variable(C)))),And(Not(Variable(A)),And(Variable(B),Variable(C))))

Result:
true

is disjunctive: true
is equal: true
```