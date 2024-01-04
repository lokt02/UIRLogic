package ru.mephi.csit.stusys.proplogic

sealed trait LogicalExpression

final case class And(left: LogicalExpression, right: LogicalExpression) extends LogicalExpression
final case class Or(left: LogicalExpression, right: LogicalExpression) extends LogicalExpression
final case class Not(expr: LogicalExpression) extends LogicalExpression
final case class Variable(name: String) extends LogicalExpression
case object True extends LogicalExpression
case object False extends LogicalExpression
