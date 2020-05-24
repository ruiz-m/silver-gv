package viper.silver.verifier

import java.util.regex.{Matcher, Pattern}

import viper.silver.parser.FastParser.PWrapper

import scala.util.matching.Regex


object ModelParser {
  val White = PWrapper {
    import fastparse.all._

    NoTrace((("/*" ~ (!StringIn("*/") ~ AnyChar).rep ~ "*/") | ("//" ~ CharsWhile(_ != '\n').? ~ ("\n" | End)) | " " | "\t" | "\n" | "\r").rep)
  }

  import fastparse.noApi._

  import White._

  lazy val identifier: P[Unit] = P(CharIn('0' to '9', 'A' to 'Z', 'a' to 'z', ":=!$_@<>.%") ~~ CharIn('0' to '9', 'A' to 'Z', 'a' to 'z', ":=!$_@<>.%").repX)

  lazy val idnuse: P[String] = P(identifier).!.filter(a => a != "else" && a != "let")

  lazy val numeral = P(CharIn('0' to '9') ~~ CharIn('0' to '9').repX)

  lazy val modelEntry : P[(String, ModelEntry)] = P(idnuse ~ "->" ~ definition)

  lazy val definition: P[ModelEntry] = P(mapping | valueAsSingle)

  lazy val mapping: P[MapEntry] = P("{" ~/ mappingContent ~"}")

  lazy val mappingContent = P(options | valueAsElse)

  lazy val options: P[MapEntry] = P(option.rep(min=1) ~ "else" ~ "->" ~ value).map{
    case (options, els) => MapEntry(options.toMap, els)
  }

  lazy val option = P(value.rep(min=1) ~ "->" ~/ value)

  lazy val value: P[String] = P(idnuse | let | application)

  lazy val valueAsSingle : P[SingleEntry] = P(value).map(SingleEntry)

  lazy val valueAsElse : P[MapEntry] = P(value).map{
    case v => {
      boolFuncDef.parse(v) match {
        case Parsed.Success(e, _) => e
        case _ => MapEntry(Map(), v)
      }
    }
  }

  lazy val let : P[String] = P("(let" ~ "(" ~ vardef.rep(min=1) ~ ")" ~ value ~ ")").map{
    case (defs, body) =>
      defs.foldLeft(body)((currentBody, definition) => currentBody.replaceAll(Pattern.quote(definition._1), Matcher.quoteReplacement(definition._2)))
  }

  lazy val vardef : P[(String, String)] = P("(" ~ idnuse ~ value ~")")

  lazy val application: P[String] = P("(" ~ value.rep(min=1) ~")").!

  lazy val partsOfApplication: P[Seq[String]] = P("(" ~ value.rep(min=1) ~")")

  lazy val model : P[Model] = P(Start ~ modelEntry.rep ~ End).map(entries => Model(entries.toMap))

  lazy val boolFuncDef: P[MapEntry] = P(Start ~ "(or" ~ boolOption.rep(min=1) ~")" ~ End).map{
    case options => MapEntry(options.map(lhs => lhs -> "true").toMap, "false")
  }

  lazy val boolOption: P[Seq[String]] = P("(and"~ equality.rep(min=1) ~")")

  lazy val equality: P[String] = P("(=" ~ "(:var" ~ numeral ~")" ~ idnuse  ~ ")")

  def getApplication(s: String) = {
    partsOfApplication.parse(s) match {
      case Parsed.Success(parts, _) => parts
    }
  }

}
