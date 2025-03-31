import lib_yyjson.all.*
import scalanative.libc.string.strlen
import scalanative.unsafe.*
import scalanative.unsigned.*
import io.circe.*
import scala.annotation.switch

object YYUJsonParser {
  private def loop(value: Ptr[yyjson_val])(implicit z: Zone): ujson.Value = {
    (yyjson_get_type(value).toInt: @switch) match {
      case 0|1 => throw new RuntimeException("Invalid JSON")
      case 2 => ujson.Null
      case 3 => if (yyjson_get_bool(value)) ujson.True else ujson.False
      case 4 => ujson.Num(yyjson_get_num(value))
      case 5 => ujson.Str(fromCString(yyjson_get_str(value)))
      case 6 => 
        val iter = yyjson_arr_iter_with(value)
        val iterator = Iterator.continually(yyjson_arr_iter_next(iter.toPtr))
          .takeWhile(_ != null)
          .map(loop)
        ujson.Arr.from(iterator)
      case 7 => 
        val iter = yyjson_obj_iter_with(value)
        val iterator = Iterator.continually(yyjson_obj_iter_next(iter.toPtr))
          .takeWhile(_ != null)
          .map { name =>
            fromCString(yyjson_get_str(name)) -> loop(yyjson_obj_iter_get_val(name))
          }
        ujson.Obj.from(iterator)
      case x => throw new RuntimeException(s"Invalid type: $x")
    }
  }

  def parse(input: String): ujson.Value = Zone {
    val cJson = toCString(input)
    val doc = yyjson_read(cJson, strlen(cJson), yyjson_read_flag(0.toUInt))
    val root = yyjson_doc_get_root(doc)

    val res = try {
      loop(root)
    } catch { case ex: Exception =>
      yyjson_doc_free(doc)
      throw ex
    }
    yyjson_doc_free(doc)
    res
  }

  def parseByteArray(input: Array[Byte]): ujson.Value = Zone {
    val doc = yyjson_read(input.at(0), input.size.toUInt, yyjson_read_flag(0.toUInt))
    val root = yyjson_doc_get_root(doc)

    val res = try {
      loop(root)
    } catch { case ex: Exception =>
      yyjson_doc_free(doc)
      throw ex
    }
    yyjson_doc_free(doc)
    res
  }

  def parseFile(path: String): ujson.Value = Zone {
    val cPath = toCString(path)
    val doc = yyjson_read_file(cPath, yyjson_read_flag(0.toUInt), null, null)
    val root = yyjson_doc_get_root(doc)

    val res = try {
      loop(root)
    } catch { case ex: Exception =>
      yyjson_doc_free(doc)
      throw ex
    }
    yyjson_doc_free(doc)
    res
  }
}
