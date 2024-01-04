package ru.mephi.csit.stusys

import cats.parse.Parser
import cats.parse.Parser.string
import cats.parse.Rfc5234.{alpha, digit, sp}
import org.scalacheck.{Arbitrary, Gen}

import scala.annotation.unused
import scala.util.Random

package object proplogic {
  
  private type TruthTable = List[Boolean]
  private type Environment = Map[String, Boolean]
  private type VarSeq = Seq[String]

  private def ttRowNum(environment: Environment): Int =
    environment.toSeq.sortBy(_._1).reverse.map(_._2).zipWithIndex
      .foldRight(0)((x, r) => (if (x._1) Math.pow(2, x._2).toInt else 0) + r)

  private def Y[T](func: (LogicalExpression => T) => LogicalExpression => T): LogicalExpression => T =
    func(Y(func))(_:LogicalExpression)

  private def LEProcessWithState[T](andFunc: (T, T, LogicalExpression) => T,
                           orFunc: (T, T, LogicalExpression) => T,
                           notFunc: (T, LogicalExpression) => T,
                           varFunc: String => T,
                           trueFunc: T,
                           falseFunc: T,
                           elseFunc: T): LogicalExpression => T =
  Y(
    (f: LogicalExpression => T) =>
      (expr: LogicalExpression) => expr match {
        case And(left, right) =>
          andFunc(f(left), f(right), expr)
        case Or(left, right) =>
          orFunc(f(left), f(right), expr)
        case Not(inner) =>
          notFunc(f(inner), expr)
        case Variable(name) =>
          varFunc(name)
        case True => trueFunc
        case False => falseFunc
        case _ => elseFunc
      }
  )

  private def LEProcess[T](andFunc: (T, T) => T,
                           orFunc: (T, T) => T,
                           notFunc: T => T,
                           varFunc: String => T,
                           trueFunc: T,
                           falseFunc: T,
                           elseFunc: T): LogicalExpression => T = LEProcessWithState(
    (left: T, right: T, _: LogicalExpression) => andFunc(left, right),
    (left: T, right: T, _: LogicalExpression) => orFunc(left, right),
    (inner: T, _: LogicalExpression) => notFunc(inner),
    varFunc, trueFunc, falseFunc, elseFunc)

  def evalExpression(expression: LogicalExpression, env: Environment = Map.empty): Boolean = LEProcess(
    (left: Boolean, right: Boolean) => left && right,
    (left: Boolean, right: Boolean) => left || right,
    (inner: Boolean) => !inner,
    (name: String) => if(!env.contains(name)) throw new RuntimeException else env.getOrElse(name, false),
    true, false, false
  )(expression)

  private def fv(expression: LogicalExpression): VarSeq = LEProcess(
    (leftV: VarSeq, rightV: VarSeq) => leftV ++ rightV.dropWhile((leftV intersect rightV).contains(_)),
    (leftV: VarSeq, rightV: VarSeq) => leftV ++ rightV.dropWhile((leftV intersect rightV).contains(_)),
    (inner: VarSeq) => inner,
    (name: String) => Seq(name),
    Seq.empty, Seq.empty, Seq.empty
  )(expression)

  def showExpression(expression: LogicalExpression): String = LEProcess(
    (left: String, right: String) => s"($left and $right)",
    (left: String, right: String) => s"($left or $right)",
    (inner: String) => s"(not $inner)", (name: String) => name,
    "1", "0", ""
  )(expression)

  def parseExpression(input: String): Either[String, LogicalExpression] = {
    val openBracket = string("(")
    val closeBracket = string(")")
    val trueParser = string("1").string
    val falseParser = string("0").string

    val expressionParser: Parser[LogicalExpression] = Parser.recursive { rec: Parser[LogicalExpression] =>
      (
        openBracket *>
        (
          ((rec <* sp.rep <* Parser.string("and")) ~ (sp.rep *> rec)).map {
            case (left, right) => And(left, right)
          }.backtrack |
          ((rec <* sp.rep <* Parser.string("or")) ~ (sp.rep *> rec)).map {
            case (left, right) => Or(left, right)
          }.backtrack |
          (Parser.string("not") *> sp.rep *>rec).map(expr => Not(expr))
        )
        <* closeBracket
      ) |
      (alpha ~ digit.rep0).map(name => Variable(name._1.toString + name._2.foldRight("")((x, r) => x + r))) |
      (trueParser | falseParser).map {
        case "1" => True
        case "0" => False
      }
    }

    processParserError(expressionParser.parseAll(input))
  }

  private def processParserError(parseResult: Either[Parser.Error, LogicalExpression]): Either[String, LogicalExpression] = parseResult match {
    case Right(value) => Right(value)
    case Left(error) =>
      Left(s"error at position ${error._1 + 1}: ${
        error.input.getOrElse("(can't get expression :( )").patch(error._1, "<", 0)
      }")
  }

  def evalTT(expression: LogicalExpression): TruthTable =
    generateEnvironments(fv(expression)).map(env => (env, evalExpression(expression, env)))
      .sortBy { case (env, _) => ttRowNum(env) }.map { case (_, value) => value }

  private def generateEnvironments(variables: VarSeq): List[Environment] =
    (0 until Math.pow(2, variables.length).toInt).map { i =>
      variables.zip(i.toBinaryString.reverse.padTo(variables.length, '0').reverse).map {
        case (variable, '1') => variable -> true
        case (variable, '0') => variable -> false
      }.toMap
    }.toList

  def isLogicalExpressionsEquivalent(leftExpr: LogicalExpression, rightExpr: LogicalExpression): Boolean =
    evalTT(leftExpr) == evalTT(rightExpr)

  def isPerfectDisjunctiveNormalForm(expr: LogicalExpression): Boolean = {
    val allVariables = fv(expr)

    def isPerfectDisjunctiveNormalFormHelper(expr: LogicalExpression): Boolean = LEProcessWithState(
      (_: Boolean, _: Boolean, expr: LogicalExpression) => isPerfectDisjunctiveNormalFormHelperAfterMetConjugation(expr),
      (left: Boolean, right: Boolean, expr: LogicalExpression) => expr match {
        case Or(leftLE, rightLE) => fv(leftLE) == allVariables && fv(rightLE) == allVariables && left && right
        case _ => throw new RuntimeException
      },
      (_: Boolean, _: LogicalExpression) => true, (_: String) => true,
      false, false, false
    )(expr)

    def isPerfectDisjunctiveNormalFormHelperAfterMetConjugation(expr: LogicalExpression): Boolean = LEProcess(
      (left: Boolean, right: Boolean) => left && right,
      (_: Boolean, _: Boolean) => false,
      (_: Boolean) => true, (_: String) => true,
      false, false, false
    )(expr)

    isPerfectDisjunctiveNormalFormHelper(expr)
  }

  private def generateVars(dimension: Int): Seq[Variable] = {
    val alphabet = GeneratorLE.alphabet

    (0 until dimension).map { index =>
      val round = index / alphabet.length
      Variable(alphabet(index % alphabet.length) + (if (round > 0) round.toString else ""))
    }
  }

  def generateRandomLogicalExpression(dimension: Int): LogicalExpression = {
    val vars = generateVars(dimension)
    val random = new Random()

    def generateRandomLogicalExpressionHelper(variables: Seq[Variable], depth: Int): LogicalExpression =
      if (depth <= 0 || variables.isEmpty) variables(random.nextInt(variables.length))
      else random.nextInt(3) match {
        case 0 =>
          And(generateRandomLogicalExpressionHelper(variables, depth - 1),
            generateRandomLogicalExpressionHelper(variables, depth - 1))
        case 1 =>
          Or(generateRandomLogicalExpressionHelper(variables, depth - 1),
            generateRandomLogicalExpressionHelper(variables, depth - 1))
        case 2 =>
          Not(generateRandomLogicalExpressionHelper(variables, depth - 1))
        case _ =>
          True
      }

    generateRandomLogicalExpressionHelper(vars, dimension)
  }

  @unused
  def generateRandomEnvironment(dimension: Int): Environment = {
    val envs = generateEnvironments(generateVars(dimension).map(_.name))
    envs(new Random().nextInt(envs.length))
  }

  object GeneratorLE {
    val alphabet: Seq[String] = Seq("A", "B", "C", "D", "E", "F", "G", "H", "I",
      "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")

    private val letterGen: Gen[Char] = Gen.choose('A', 'Z')

    private val digitGen: Gen[Char] = Gen.choose('0', '9')

    private val nameGen: Gen[String] = for {
      letter <- letterGen
      digits <- Gen.listOf(digitGen)
    } yield letter + digits.mkString

    private val booleanLEGen: Gen[LogicalExpression] = Gen.oneOf(True, False)

    lazy val arbitraryLE: Arbitrary[LogicalExpression] = Arbitrary {
      Gen.sized(size => if (size <= 0) Gen.oneOf(
        nameGen.map(Variable),
        booleanLEGen
      )
      else Gen.oneOf(
        Gen.lzy(Gen.resize(size / 2, arbitraryLE.arbitrary)
          .flatMap(left => Gen.resize(size / 2, arbitraryLE.arbitrary).map(right => And(left, right)))),
        Gen.lzy(Gen.resize(size / 2, arbitraryLE.arbitrary)
          .flatMap(left => Gen.resize(size / 2, arbitraryLE.arbitrary).map(right => Or(left, right)))),
        Gen.lzy(Gen.resize(size - 1, arbitraryLE.arbitrary)
          .flatMap(inner => Not(inner)))
      )
      )
    }

    lazy val arbitraryLEWithoutConstants: Arbitrary[LogicalExpression] = Arbitrary {
      Gen.sized(size => if (size <= 0) nameGen.map(Variable)
      else Gen.oneOf(
        Gen.lzy(Gen.resize(size / 2, arbitraryLEWithoutConstants.arbitrary)
          .flatMap(left => Gen.resize(size / 2, arbitraryLEWithoutConstants.arbitrary).map(right => And(left, right)))),
        Gen.lzy(Gen.resize(size / 2, arbitraryLEWithoutConstants.arbitrary)
          .flatMap(left => Gen.resize(size / 2, arbitraryLEWithoutConstants.arbitrary).map(right => Or(left, right)))),
        Gen.lzy(Gen.resize(size - 1, arbitraryLEWithoutConstants.arbitrary)
          .flatMap(inner => Not(inner)))
      )
      )
    }
  }
}
