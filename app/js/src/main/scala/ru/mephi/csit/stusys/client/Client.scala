package ru.mephi.csit.stusys.client

import org.scalajs.dom
import org.scalajs.dom.document
import ru.mephi.csit.stusys.proplogic._
import scalatags.JsDom.all._

object Client {
  def main(args: Array[String]): Unit = {
    document.addEventListener("DOMContentLoaded", { (e: dom.Event) =>
      setupUI()
    })
  }

  private val url = "http://localhost:8080/"

  private val input_res = span.render
  private val task_res = span.render
  private val task = span.render

  private val clickButton = input(
    `type` := "button",
    value := "Send"
  ).render

  def setupUI(): Unit = {
    val textInput = input(
      `type` := "text",
      placeholder := "Logical Expression"
    ).render
    textInput.onkeyup = (_: dom.Event) => {
      processInput(textInput.value, input_res)
    }

    requestTask()

    clickButton.onclick = (_: dom.MouseEvent) => {
      sendLogicalExpression(textInput.value)
    }

    document.body.appendChild(
      div(task, br, input_res, br, textInput, br, clickButton, br, task_res).render
    )
  }

  private def sendLogicalExpression(logicalExpression: String): Unit =
    sendRequest(url + "grade", "POST", task_res,
      "Waiting for answer from server...", "Can't send answer",
      reqParam = Option(logicalExpression))

  private def requestTask(): Unit =
    sendRequest(url + "task", "GET", task, "Waiting for task...", "Can't get task")

  private def sendRequest(url: String, method: String, node: dom.Element, waitingText: String, errorText: String,
                          successText: String = "", reqParam: Option[String] = Option(null), depth: Int = 0): Unit = {
    clickButton.setAttribute("disabled", "true")
    node.textContent = waitingText
    val xhr = new dom.XMLHttpRequest()
    xhr.open(method, url)
    xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded")
    xhr.onload = { (_: dom.Event) =>
      node.textContent = successText + xhr.responseText
      clickButton.removeAttribute("disabled")
    }
    xhr.onerror = { (_: dom.Event) =>
      node.textContent = errorText
      if (depth < 3) sendRequest(url, method, node, waitingText,
        errorText, successText, reqParam, depth + 1)
    }
    reqParam match {
      case Some(p) => xhr.send(p)
      case _ => xhr.send()
    }
  }

  private def processInput(input: String, outputNode: dom.Element): Unit =
    outputNode.textContent = processParseResult(parseExpression(input))

  private def processParseResult(parseResult: Either[String, LogicalExpression]): String = parseResult match {
    case Right(value) => showExpression(value)
    case Left(value) => "Error: " + value
  }
}
