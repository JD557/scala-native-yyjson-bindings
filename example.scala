import lib_yyjson.all.*
import scalanative.libc.string.strlen
import scalanative.unsafe.*
import scalanative.unsigned.*
import io.circe.*
import scala.io.Source
import java.io.FileInputStream

def time[T](task: String)(f: => T): T =
  println(s"Starting $task")
  val start = System.currentTimeMillis()
  val res = f
  println(s"Done in ${System.currentTimeMillis() - start}ms")
  res

def yyNaiveStringParse(json: String): Unit = Zone {
  val cJson = toCString(json)
  val doc = yyjson_read(cJson, strlen(cJson), yyjson_read_flag(0.toUInt))
  val root = yyjson_doc_get_root(doc)
  yyjson_doc_free(doc)
}

def yyNaiveBytesParse(json: Array[Byte]): Unit = Zone {
  val doc = yyjson_read(json.at(0), json.size.toUInt, yyjson_read_flag(0.toUInt))
  val root = yyjson_doc_get_root(doc)
  yyjson_doc_free(doc)
}

def yyNaiveFileParse(path: String): Unit = Zone {
  val cPath = toCString(path)
  val doc = yyjson_read_file(cPath, yyjson_read_flag(0.toUInt), null, null)
  val root = yyjson_doc_get_root(doc)
  yyjson_doc_free(doc)
}

@main def hello =
  val path = "dados2025.json" // From https://www.kaggle.com/datasets/beatrizmsarmento/relatos-de-consumidores-do-site-consumidor-gov-br
  val data = time("data load (string)")(Source.fromFile(path).getLines().mkString("\n"))
  val dataBytes = time("data load (bytes)")((new FileInputStream(path)).readAllBytes())

  // Circe
  time("Circe parse string")(parser.parse(data))
  time("YYJSON Circe parse string")(YYCirceParser.parse(data))
  time("YYJSON Circe parse bytes")(YYCirceParser.parseByteArray(dataBytes))
  time("YYJSON Circe parse file")(YYCirceParser.parseFile(path))
  // uJson
  time("ujson parse string")(ujson.read(data))
  time("YYJSON ujson parse string")(YYUJsonParser.parse(data))
  time("YYJSON ujson parse bytes")(YYUJsonParser.parseByteArray(dataBytes))
  time("YYJSON ujson parse file")(YYUJsonParser.parseFile(path))
  // Raw
  time("YYJSON parse string")(yyNaiveStringParse(data))
  time("YYJSON parse bytes")(yyNaiveBytesParse(dataBytes))
  time("YYJSON parse file")(yyNaiveFileParse(path))
