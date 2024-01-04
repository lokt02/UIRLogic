package stusysTests

import org.specs2.mutable._
import org.specs2.specification.core.SpecStructure
import ru.mephi.csit.stusys.proplogic.{Not, _}

object proplogicTests extends Specification{

  override def is: SpecStructure =
    s2"""
        Specifications:

        A 'True' expression should be true ${evalExpression(True)}
        A 'False' expression should be false ${!evalExpression(False)}
        Expression consisting of one variable should have truth table [false, true] ${
      evalTT(Variable("A")) == List(false, true)
    }
        Expression consisting of one variable should be false when we substitute false ${
      !evalExpression(Variable("A"), Map("A" -> false))
    }
        Expression consisting of one variable should be true when we substitute true ${
      evalExpression(Variable("A"), Map("A" -> true))
    }
        Expression A||B should have truth table [false, true, true, true] ${
      evalTT(Or(Variable("A"), Variable("B"))) == List(false, true, true, true)
    }
        Expression A&&B should have truth table [false, false, false, true] ${
      evalTT(And(Variable("A"), Variable("B"))) == List(false, false, false, true)
    }
        Expression !A should have truth table [true, false] ${evalTT(Not(Variable("A"))) == List(true, false)}
        Expression ((A||B)&&!C||A)&&1||0 should have truth table [false, false, true, false, true, true, true, true] ${
      evalTT(
        Or(And(Or(And(Or(Variable("A"), Variable("B")), Not(Variable("C"))), Variable("A")), True), False)
      ) == List(false, false, true, false, true, true, true, true)
    }
        Truth table of expressions A||B and !(!A&&!B) should be equal ${
      isLogicalExpressionsEquivalent(Or(Variable("A"), Variable("B")),
        Not(And(Not(Variable("A")), Not(Variable("B")))))
    }
        Truth table of expressions A||B and !A&&!B should be not equal ${
      !isLogicalExpressionsEquivalent(Or(Variable("A"), Variable("B")),
        And(Not(Variable("A")), Not(Variable("B"))))
    }
        Expression (A||B)&&!C should be not in Perfect Disjunctive Normal Form ${
      !isPerfectDisjunctiveNormalForm(And(Or(Variable("A"), Variable("B")), Not(Variable("C"))))
    }
        Expression A && B && !C || !A && B && C should be in Perfect Disjunctive Normal Form ${
      isPerfectDisjunctiveNormalForm(
        Or(And(Variable("A"), And(Variable("B"), Not(Variable("C")))),
          And(Not(Variable("A")), And(Variable("B"), Variable("C"))))
      )
    }
        Size of the truth table of the expression (A && !C || !A && C) && B should be 8 ${
      evalTT(
        And(Or(And(Variable("A"), Not(Variable("C"))), And(Not(Variable("A")), Variable("C"))), Variable("B"))
      ).size == 8
    }
      """

}