package stusysTests

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}
import ru.mephi.csit.stusys.proplogic._

object propertiesCheck extends Specification with ScalaCheck {

  def is: SpecStructure =
    s2"""
      Properties:
        $parser_consistency
        $testing
        $parsing_incorrect_string
        $parsing_correct_string
        ${non_existent_variable.isSuccess}
      """

  private implicit val arbitraryLE: Arbitrary[LogicalExpression] = GeneratorLE.arbitraryLE

  private def parser_consistency =
    forAll{ (expr: LogicalExpression) => parseExpression(showExpression(expr)) match {
        case Left(_) => false
        case Right(x) => expr == x
      }
    }

  private def testing = List(
      And(Variable("A"), Variable("B")),
      And(Variable("A"), And(Variable("B"), Variable("C1"))),
      And(Variable("A"), And(Variable("B"), Variable("C12"))),
      And(Variable("A101"), Or(Variable("B9"), Variable("C22"))),
      Or(Variable("A"), Variable("B1")),
      Not(Variable("A100")),
      Variable("A"),
      True,
      False,
      And(Variable("A"), True),
      And(Variable("A"), False)
    ).forall(el => parseExpression(showExpression(el)) match {
      case Left(_) => false
      case Right(expr) => el == expr
    })

  private def parsing_incorrect_string = Seq(
      "((not A and B)",
      "A)",
      "()",
      "",
      "and",
      "not",
      "(notA andB)"
    ).forall(el => parseExpression(el) match {
    case Left(_) => true
    case Right(_) => false
  })

  private def parsing_correct_string = Seq(
      "((not A) and B)",
      "A",
      "(A and B1)",
      "(not A100)",
      "(not    A100)",
      "(A100   or B)"
    ).forall(el => parseExpression(el) match {
    case Left(_) => false
    case Right(_) => true
  })

  private def non_existent_variable =
    evalExpression(Variable("A"), Map("B" -> true)) must throwA[RuntimeException]

}
