import org.scalajs.dom.Event
import org.scalajs.dom.window.alert
import org.scalajs.jquery.jQuery

import scala.scalajs.js.{Dynamic, JSApp}

/**
  * Created by pslagle12 on 6/21/16.
  */
object Script extends JSApp {

  implicit def dynamicToString(d: Dynamic): String =
    d.asInstanceOf[String]

  private var errorMessage = ""

  private val fieldsAndErrors = List(
    ("email", "Please enter a valid email address"),
    ("phone", "Please enter a valid phone number"),
    ("pass1", "Please enter a matching password"),
    ("pass2", "please enter a matching password")
  )

  private val regexTest =
    """/^(([^<>()[\]\\.,;:\s@\"]+(\.[^<>()[\]\\.,;:\s@\"]+)*)|
  (\".+\"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])
      |(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/""".r

  private val email = jQuery("email").value
  private val phone = jQuery("phone").value
  private val pass1 = jQuery("pass1").value
  private val pass2 = jQuery("pass2").value


  def main(): Unit = {

    errorMessage = errorChecker(fieldsAndErrors)
    jQuery("validationForm").submit((event: Event) =>
      if (errorMessage == "") println("Success")
      else  println("Error")
    )

  }

  def isValidEmailAddress(address: String): Boolean =
    regexTest
      .pattern
      .matcher(address)
      .matches()

  def isValidPhone(number: Any): Boolean =
    number.isInstanceOf[Int]

  def isValidPass(pass1: String, pass2: String): Boolean =
    pass1 == pass2

/*
take the list of strings, recursively iterate
 */
  def errorChecker(list: List[(String, String)]): String = {
    val (id, message) = (list.head._1, list.head._1)
    if (list.nonEmpty) {
      id match {

        case "email" =>
          if (!isValidEmailAddress(email)) message else ""

        case "phone" =>
          if (!isValidPhone(phone)) message else ""

        case "pass1" =>
          if (!isValidPass(id, pass2)) message else ""
        case _ => ""

        errorChecker(list.tail)
      }
    } else message
  }
}





