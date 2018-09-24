import scala.io.StdIn.readLine

object NozawaMasakoConverterApp {
  def main(args: Array[String]): Unit = {
    val input: List[String] = readLine().split("").toList
    println(s"野沢雅子「${Converter(input)}」")
  }
}
