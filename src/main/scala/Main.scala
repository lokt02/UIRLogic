import scala.collection.mutable

object Main {
  sealed trait LogicalExpression

  case class And(left: LogicalExpression, right: LogicalExpression) extends LogicalExpression

  case class Or(left: LogicalExpression, right: LogicalExpression) extends LogicalExpression

  case class Not(expr: LogicalExpression) extends LogicalExpression

  case class Variable(name: String) extends LogicalExpression

  case object True extends LogicalExpression

  case object False extends LogicalExpression

  case class TruthTableRow(variables: Map[String, Boolean], result: Boolean)

  def truthTable(expr: LogicalExpression): mutable.LinkedHashSet[TruthTableRow] = expr match {
    case And(left, right) =>
      val leftTruthTable = truthTable(left)
      val rightTruthTable = truthTable(right)
      for {
        l <- leftTruthTable
        r <- rightTruthTable
      } yield {
        TruthTableRow(l.variables ++ r.variables, l.result && r.result)
      }

    case Or(left, right) =>
      val leftTruthTable = truthTable(left)
      val rightTruthTable = truthTable(right)
      for {
        l <- leftTruthTable
        r <- rightTruthTable
      } yield {
        TruthTableRow(l.variables ++ r.variables, l.result || r.result)
      }

    case Not(inner) =>
      val innerTruthTable = truthTable(inner)
      innerTruthTable.map(row => row.copy(result = !row.result))

    case Variable(name) =>
      mutable.LinkedHashSet(TruthTableRow(Map(name -> true), true), TruthTableRow(Map(name -> false), false))

    case True =>
      mutable.LinkedHashSet(TruthTableRow(Map("1" -> true), true))

    case False =>
      mutable.LinkedHashSet(TruthTableRow(Map("0" -> false), false))
  }

  def isPerfectDisjunctiveNormalForm(expr: LogicalExpression): Boolean = {
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
    }

    isPerfectDisjunctiveNormalFormHelper(expr)
  }


  def getVariables(expr: LogicalExpression): Set[String] = expr match {
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

  def isLogicalExpressionsEquivalent(exprLeft: LogicalExpression, exprRight: LogicalExpression): Boolean = {
    val truthTableLeft = truthTable(exprLeft)
    val truthTableRight = truthTable(exprRight)
    truthTableLeft == truthTableRight
  }

  def main(args: Array[String]): Unit = {
    truthTableTest()
    PDNFTest()
    equivalentTest()
  }

  def equivalentTest(): Unit = {
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

  def PDNFTest(): Unit = {
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

  def truthTableTest(): Unit ={
    println()
    println()
    println("======================")
    println("truthTable method test")
    println("======================")
    println()
    //val formula = And(Or(Variable("A"), Variable("B")), Not(Or(Variable("C"), True)))
    val formula = Or(And(Or(Variable("A"), Variable("B")), Not(Variable("C"))), Variable("A"))
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