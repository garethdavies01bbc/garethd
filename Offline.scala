import scala.io.Source

object Offline
{
  val predicates = Map(
    "MENTIONS" -> "http://www.bbc.co.uk/ontologies/creativework/mentions",
    "EDITORIAL_SENSITIVITY" -> "http://www.bbc.co.uk/ontologies/coreconcepts/editorialSensitivity",
    "ABOUT" -> "http://www.bbc.co.uk/ontologies/passport/predicate/About",
    "EDITORIAL_TONE" -> "http://www.bbc.co.uk/ontologies/coreconcepts/editorialTone",
    "RELEVANT_TO" -> "http://www.bbc.co.uk/ontologies/bbc/relevantTo",
    "EDITORIAL_TONE" -> "http://www.bbc.co.uk/ontologies/coreconcepts/editorialTone",
    "GENRE" -> "http://www.bbc.co.uk/ontologies/bbc/genre",
    "LANGUAGE" -> "http://www.bbc.co.uk/ontologies/coreconcepts/language",
    "FORMAT" -> "http://www.bbc.co.uk/ontologies/cwork/format",
    "CONTRIBUTOR" -> "contributor",
    "AUDIENCE" -> "audience",
  )

  case class DoesContain(domain: Boolean, predicate: Boolean)
  case class Count(domain:Int, predicate: Int)

  def completeness(c:Count): Double = (c.predicate.toDouble / c.domain.toDouble) * 100

  def inc(x: Boolean) = if (x) 1 else 0

  def myCount(l: List[(String, DoesContain)]): Count = {
    l.map(_._2).
      foldLeft(Count(0, 0)) {
        case (Count(d, p), lc) => Count(d + inc(lc.domain), p + inc(lc.predicate))
      }
  }

  def valuesMatch(a: String, b: String, l: String): (Boolean, Boolean) = (l.contains(a), l.contains(b))

  def lineSummary(domain: String, predicate: String, l: String): DoesContain = {
    valuesMatch(domain, predicate, l) match {
      case (false, _) => DoesContain(domain = false, predicate = false)
      case (true, true) => DoesContain(domain = true, predicate = true)
      case (true, false) => DoesContain(domain = true, predicate = false)
    }
  }

  def predicateData(filename: String, domain:String): Map[String, List[(String, DoesContain)]] =
    Source.fromFile(filename).getLines.toList.flatMap(line =>
      predicates.foldLeft(Map.empty[String, DoesContain]) {
        (acc, predicate) => acc + (predicate._2 -> lineSummary(domain, predicate._2, line))
      }).groupBy(_._1)

  def results(data: Map[String, List[(String, DoesContain)]]): Map[String, Double] = {
    data.transform((_, countValues) => {
      completeness(myCount(countValues))
    })
  }

  def tidyResults(results:Map[String, Double]): Map[String, String] = results.map {
    case (key, value) =>
      predicates.find(_._2 == key).get._1 -> {f"$value%1.2f" + "%"}
  }

  object Tabulator {
    def format(table: Seq[Seq[Any]]): String = table match {
      case Seq() => ""
      case _ =>
        val sizes = for (row <- table) yield for (cell <- row) yield if (cell == null) 0 else cell.toString.length
        val colSizes = for (col <- sizes.transpose) yield col.max
        val rows = for (row <- table) yield formatRow(row, colSizes)
        formatRows(rowSeparator(colSizes), rows)
    }

    def formatRows(rowSeparator: String, rows: Seq[String]): String = (
      rowSeparator ::
        rows.head ::
        rowSeparator ::
        rows.tail.toList :::
        rowSeparator ::
        List()).mkString("\n")

    def formatRow(row: Seq[Any], colSizes: Seq[Int]): String = {
      val cells = for ((item, size) <- row.zip(colSizes)) yield if (size == 0) "" else ("%" + size + "s").format(item)
      cells.mkString("|", "|", "|")
    }

    def rowSeparator(colSizes: Seq[Int]): String = colSizes map { "-" * _ } mkString("+", "+", "+")
  }

  def main(args: Array[String])
  {
    val domain = args(0)
    val filename = "/Users/davieg01/Documents/intended/passportsTest.json"

    val predicateData = Offline.predicateData(filename, domain)
    val results = Offline.results(predicateData)
    val tidyResults = Offline.tidyResults(results)

    println(s"\nCompleteness for $domain")
    println(Tabulator.format(List("Predicate", "Completeness")::tidyResults.map(x => List(x._1, x._2)).toList))
  }
}
