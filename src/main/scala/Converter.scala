import scala.annotation.tailrec

object Converter {
  def apply(input: List[String]): String = {

    @tailrec
    def convert(acc: List[String], next: List[String]): List[String] =
      next match {
        case x1 :: x2 :: xs if isMatch野沢雅子語ルール(x1, x2) =>
          convert(convert野沢雅子語(x1, x2) ::: acc, xs)
        case x1 :: x2 :: xs =>
          convert(x2 :: x1 :: acc, xs)
        case x :: xs =>
          convert(x :: acc, xs)
        case Nil =>
          acc
      }

    convert(Nil, input).reverse.mkString
  }

  private[this] def isMatch野沢雅子語ルール(first: String, second: String): Boolean = {
    def is母音がa(value: String): Boolean =
      value match {
        case "あ" | "か" | "さ" | "た" | "な" | "は" | "ま" | "ら" | "が" | "ざ" | "だ" | "ば" | "ぱ" | "ー" => true
        case _ => false
      }

    def isいOrえ(value: String): Boolean =
      value match {
        case "い" | "え" => true
        case _ => false
      }

    is母音がa(first) && isいOrえ(second)
  }


  private[this] def convert野沢雅子語(first: String, second: String): List[String] = {
    val convertFirst = first match {
      case "あ" => "え"
      case "か" => "け"
      case "さ" => "せ"
      case "た" => "て"
      case "な" => "ね"
      case "は" => "へ"
      case "ま" => "め"
      case "ら" => "れ"
      case "が" => "げ"
      case "ざ" => "ぜ"
      case "だ" => "で"
      case "ば" => "べ"
      case "ぱ" => "ぺ"
      case "ー" => "ぇ"
    }

    val convertSecond = second match {
      case "い" | "え" => "ぇ"
    }

    convertSecond :: convertFirst :: Nil
  }
}