import lib_yyjson.all.*
import scalanative.libc.string.strlen
import scalanative.unsafe.*
import scalanative.unsigned.*
import io.circe.*
import scala.annotation.switch

object YYCirceParser extends Parser {
  private def loop(value: Ptr[yyjson_val])(implicit z: Zone): Json = {
    (yyjson_get_type(value).toInt: @switch) match {
      case 0|1 => throw new RuntimeException("Invalid JSON")
      case 2 => Json.Null
      case 3 => Json.fromBoolean(yyjson_get_bool(value))
      case 4 => Json.fromDouble(yyjson_get_num(value)).get
      case 5 => Json.fromString(fromCString(yyjson_get_str(value)))
      case 6 => 
        val iter = yyjson_arr_iter_with(value)
        val iterable = Iterator.continually(yyjson_arr_iter_next(iter.toPtr))
          .takeWhile(_ != null)
          .map(loop)
          .toVector
        Json.fromValues(iterable)
      case 7 => 
        val iter = yyjson_obj_iter_with(value)
        val iterable = Iterator.continually(yyjson_obj_iter_next(iter.toPtr))
          .takeWhile(_ != null)
          .map { name =>
            fromCString(yyjson_get_str(name)) -> loop(yyjson_obj_iter_get_val(name))
          }
          .toMap
        Json.fromJsonObject(JsonObject.fromMap(iterable))
      case x => throw new RuntimeException(s"Invalid type: $x")
    }
  }

  def parse(input: String): Either[ParsingFailure, Json] = Zone {
    val cJson = toCString(input)
    val doc = yyjson_read(cJson, strlen(cJson), yyjson_read_flag(0.toUInt))
    val root = yyjson_doc_get_root(doc)

    val res = try {
      Right(loop(root))
    } catch {
      case ex: Exception => Left(ParsingFailure(ex.getMessage(), ex))
    }
    yyjson_doc_free(doc)
    res
  }

  def parseFile(path: String): Either[ParsingFailure, Json] = Zone {
    val cPath = toCString(path)
    val doc = yyjson_read_file(cPath, yyjson_read_flag(0.toUInt), null, null)
    val root = yyjson_doc_get_root(doc)

    val res = try {
      Right(loop(root))
    } catch {
      case ex: Exception => Left(ParsingFailure(ex.getMessage(), ex))
    }
    yyjson_doc_free(doc)
    res
  }
}
