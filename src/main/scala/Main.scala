import scala.collection.mutable

object Main {
  private sealed trait LogicalExpression

  private case class And(left: LogicalExpression, right: LogicalExpression) extends LogicalExpression

  private case class Or(left: LogicalExpression, right: LogicalExpression) extends LogicalExpression

  private case class Not(expr: LogicalExpression) extends LogicalExpression

  private case class Variable(name: String) extends LogicalExpression

  private case object True extends LogicalExpression

  private case object False extends LogicalExpression

  private case class TruthTableRow(variables: Map[String, Boolean], result: Boolean)

  private def getTruthTableRow(rightTable: TruthTableRow, leftTable: TruthTableRow, operation: (Boolean, Boolean)=>Boolean): TruthTableRow = {
    val keysIntersection = leftTable.variables.keySet.intersect(rightTable.variables.keySet)
    //val variablesIntersection = leftTable.variables.toSet.intersect(rightTable.variables.toSet)
    val rightSet = rightTable.variables.toSet
    val leftSet = leftTable.variables.toSet
    if (leftTable.variables == rightTable.variables || keysIntersection == Set.empty ||
      ( (rightSet subsetOf leftSet) || (leftSet subsetOf rightSet) ) ) {
      TruthTableRow(leftTable.variables ++ rightTable.variables, operation(leftTable.result, rightTable.result))
    }
    else {
      TruthTableRow(Map.empty, result = false)
    }
  }

  private def removeInvalidRowsFromTruthTable(truthTable: mutable.LinkedHashSet[TruthTableRow], allVariables: Set[String]): mutable.LinkedHashSet[TruthTableRow] = {
    truthTable.filter( element => {
      element.variables.keySet == allVariables
    })
  }

  private def truthTable(expr: LogicalExpression): mutable.LinkedHashSet[TruthTableRow] = {
    val allVariables = getVariables(expr)
    var result: mutable.LinkedHashSet[TruthTableRow] = mutable.LinkedHashSet.empty

    result = expr match {
      case And(left, right) =>
        val leftTruthTable = truthTable(left)
        val rightTruthTable = truthTable(right)
        for {
          l <- leftTruthTable
          r <- rightTruthTable
        } yield {
          getTruthTableRow(r, l, (b1, b2) => b1 && b2)
        }

      case Or(left, right) =>
        val leftTruthTable = truthTable(left)
        val rightTruthTable = truthTable(right)
        for {
          l <- leftTruthTable
          r <- rightTruthTable
        } yield {
          getTruthTableRow(r, l, (b1, b2) => b1 || b2)
        }

      case Not(inner) =>
        val innerTruthTable = truthTable(inner)
        innerTruthTable.map(row => row.copy(result = !row.result))

      case Variable(name) =>
        mutable.LinkedHashSet(TruthTableRow(Map(name -> true), result = true), TruthTableRow(Map(name -> false), result = false))

      case True =>
        mutable.LinkedHashSet(TruthTableRow(Map("1" -> true), result = true))

      case False =>
        mutable.LinkedHashSet(TruthTableRow(Map("0" -> false), result = false))
    }

    removeInvalidRowsFromTruthTable(result, allVariables)
  }

  private def isPerfectDisjunctiveNormalForm(expr: LogicalExpression): Boolean = {
    val allVariables = getVariables(expr)

    def isPerfectDisjunctiveNormalFormHelper(expr: LogicalExpression): Boolean = expr match {
      case Or(left, right) =>
        val leftVars = getVariables(left)
        val rightVars = getVariables(right)
        leftVars == allVariables && rightVars == allVariables &&
          isPerfectDisjunctiveNormalFormHelper(left) && isPerfectDisjunctiveNormalFormHelper(right)
      case Variable(_) =>
        true
      case Not(Variable(_)) =>
        true
      case And(_, _) =>
        isPerfectDisjunctiveNormalFormHelperAfterMetConjugation(expr)
      case _ =>
        false
    }

    def isPerfectDisjunctiveNormalFormHelperAfterMetConjugation(expr: LogicalExpression): Boolean = expr match {
      case Or(_, _) =>
        false
      case Variable(_) =>
        true
      case Not(Variable(_)) =>
        true
      case And(left, right) =>
        isPerfectDisjunctiveNormalFormHelperAfterMetConjugation(left) && isPerfectDisjunctiveNormalFormHelperAfterMetConjugation(right)
      case _ =>
        false
    }

    isPerfectDisjunctiveNormalFormHelper(expr)
  }


  private def getVariables(expr: LogicalExpression): Set[String] = expr match {
    case Variable(name) =>
      Set(name)
    case And(left, right) =>
      getVariables(left) ++ getVariables(right)
    case Or(left, right) =>
      getVariables(left) ++ getVariables(right)
    case Not(inner) =>
      getVariables(inner)
    case _ =>
      Set.empty
  }

  private def isLogicalExpressionsEquivalent(exprLeft: LogicalExpression, exprRight: LogicalExpression): Boolean = {
    val truthTableLeft = truthTable(exprLeft)
    val truthTableRight = truthTable(exprRight)
    truthTableLeft == truthTableRight
  }

  def main(args: Array[String]): Unit = {
    truthTableTest()
    PDNFTest()
    equivalentTest()
    moreComplexTruthTableTest()
    taskTest()
    truthTableVariableEqualityTest()
  }

  private def truthTableVariableEqualityTest(): Unit = {
    println()
    println()
    println("=============================================")
    println("variables in LogicalExpression behaviour test")
    println("=============================================")
    println()

    val formula0 = And(Or(Variable("A"), Variable("B")), Variable("A"))
    val truthTableResult0 = truthTable(formula0)
    truthTableResult0.foreach(println)
    println()

    val formula1 = And(And(Variable("A"), Variable("B")), Variable("B"))
    val truthTableResult1 = truthTable(formula1)
    truthTableResult1.foreach(println)
    println()

    println(isLogicalExpressionsEquivalent(formula0, formula1))
    println()


    val formula2 = And(Or(Variable("A"), Variable("B")), Variable("C"))
    val truthTableResult2 = truthTable(formula2)
    truthTableResult2.foreach(println)
    println()

    val formula3 = And(Or(Variable("C"), Variable("A")), Variable("B"))
    val truthTableResult3 = truthTable(formula3)
    truthTableResult3.foreach(println)
    println()

    println(isLogicalExpressionsEquivalent(formula2, formula3))
    println()
  }

  private def moreComplexTruthTableTest(): Unit = {
    println()
    println()
    println("=======================================")
    println("truthTable method test but more complex")
    println("=======================================")
    println()
    val formula0 = And(Or(And(Variable("A"), Not(Variable("C"))), And(Not(Variable("A")), Variable("C"))), Variable("B"))
    val truthTable0 = truthTable(formula0)
    truthTable0.foreach(println)
    println(truthTable0.size)
    println()

    val formula1 = Or(And(Variable("A"), And(Variable("B"), Not(Variable("C")))),
      And(Not(Variable("A")), And(Variable("B"), Variable("C"))))
    val truthTable1 = truthTable(formula1)
    truthTable1.foreach(println)
    println(truthTable1.size)
    println()
  }

  private def taskTest(): Unit = {
    println()
    println()
    println("=============")
    println("Use case test")
    println("=============")
    println()

    val formula0 = And(
      Or(
        And(
          Variable("A"),
          Not(Variable("C")
          )
        ),
        And(
          Not(Variable("A")),
          Variable("C")
        )
      ),
      Variable("B")
      )
    println("Here given logical expression:")
    println(formula0)
    println("Make it to the Perfect Disjunctive Normal Form.")
    println()

    println("User's answer:")
    val formula1 = Or(And(Variable("A"), And(Variable("B"), Not(Variable("C")))),
                      And(Not(Variable("A")), And(Variable("B"), Variable("C"))))
    println(formula1)
    println()
    println("Result:")
    println(isLogicalExpressionsEquivalent(formula0, formula1) && isPerfectDisjunctiveNormalForm(formula1))
    println()
    println("is disjunctive: " + isPerfectDisjunctiveNormalForm(formula1))
    println("is equal: " + isLogicalExpressionsEquivalent(formula0, formula1))
  }

  private def equivalentTest(): Unit = {
    println()
    println()
    println("==========================================")
    println("isLogicalExpressionsEquivalent method test")
    println("==========================================")
    println()
    val formula0 = Or(Variable("A"), Variable("B"))
    val truthTableResult0 = truthTable(formula0)
    truthTableResult0.foreach(println)
    println()

    val formula1 = Not(And(Not(Variable("A")), Not(Variable("B"))))
    val truthTableResult1 = truthTable(formula1)
    truthTableResult1.foreach(println)
    println()

    println(isLogicalExpressionsEquivalent(formula0, formula1))
    println()


    val formula2 = Or(Variable("A"), Variable("B"))
    val truthTableResult2 = truthTable(formula2)
    truthTableResult2.foreach(println)
    println()

    val formula3 = And(Not(Variable("A")), Not(Variable("B")))
    val truthTableResult3 = truthTable(formula3)
    truthTableResult3.foreach(println)
    println()

    println(isLogicalExpressionsEquivalent(formula2, formula3))
    println()
  }

  private def PDNFTest(): Unit = {
    println()
    println()
    println("==========================================")
    println("isPerfectDisjunctiveNormalForm method test")
    println("==========================================")
    println()
    val notDisjunctive = And(Or(Variable("A"), Variable("B")), Not(Variable("C")))
    println(isPerfectDisjunctiveNormalForm(notDisjunctive))

    val disjunctive = Or(And(Variable("A"), And(Variable("B"), Not(Variable("C")))),
      And(Not(Variable("A")), And(Variable("B"), Variable("C"))))
    println(isPerfectDisjunctiveNormalForm(disjunctive))
  }

  private def truthTableTest(): Unit ={
    println()
    println()
    println("======================")
    println("truthTable method test")
    println("======================")
    println()
    //val formula = And(Or(Variable("A"), Variable("B")), Not(Or(Variable("C"), True)))
    val formula = Or(And(Or(And(Or(Variable("A"), Variable("B")), Not(Variable("C"))), Variable("A")), True), False)
    val truthTableResult = truthTable(formula)
    println("Таблица истинности для формулы:")
    truthTableResult.foreach(println)
    println()

    val formula0 = Or(Variable("A"), Variable("B"))
    val truthTableResult0 = truthTable(formula0)
    truthTableResult0.foreach(println)
    println()

    val formula1 = And(Variable("A"), Variable("B"))
    val truthTableResult1 = truthTable(formula1)
    truthTableResult1.foreach(println)
    println()

    val formula2 = Not(Variable("A"))
    val truthTableResult2 = truthTable(formula2)
    truthTableResult2.foreach(println)
    println()

    val formula3 = Variable("A")
    val truthTableResult3 = truthTable(formula3)
    truthTableResult3.foreach(println)
    println()

    val formula4 = True
    val truthTableResult4 = truthTable(formula4)
    truthTableResult4.foreach(println)
    println()

    val formula5 = False
    val truthTableResult5 = truthTable(formula5)
    truthTableResult5.foreach(println)
    println()
  }
}