package csw.generator

import csw.logging.client.scaladsl.GenericLoggerFactory
import csw.params.core.generics.KeyType._
import csw.params.core.generics.Parameter
import csw.params.core.models.Units
import csw.params.core.models.Units.NoUnits
import icd.web.shared.IcdModels.ParameterModel

import java.nio.charset.StandardCharsets
import scala.util.Random

object ParamSetGenerator {
  val rand        = new Random()
  private val log = GenericLoggerFactory.getLogger

  private def makeCswUnits(units: String): Units = {
    Units.withNameInsensitiveOption(units).getOrElse(NoUnits)
  }

  // XXX Replace illegal chars in param names (see also ParameterUtil.tsx in csw-event-monitor)
  private def fixParamName(paramName: String): String = {
    paramName
      .replace('/', '|')
      .replace('[', '(')
      .replace(']', ')')
  }

  // Call Random.between(minInclusive: Double, maxExclusive: Double)
  private def makeRandomValue(param: ParameterModel, minValue: Double, maxValue: Double): Double = {
    val min = param.minimum
      .map { p =>
        if (p.equalsIgnoreCase("-inf")) minValue else p.toDouble
      }
      .getOrElse(minValue)
    val max = param.maximum
      .map { p =>
        if (p.equalsIgnoreCase("inf")) maxValue else p.toDouble
      }
      .getOrElse(maxValue)
    rand.between(min, max)
  }

  // Call Random.between(minInclusive: Int, maxExclusive: Int)
  private def makeRandomValue(param: ParameterModel, minValue: Int, maxValue: Int): Int = {
    val minInc = if (param.exclusiveMinimum) 1 else 0
    val min = param.minimum
      .map { p =>
        if (p.equalsIgnoreCase("-inf")) minValue else p.toInt + minInc
      }
      .getOrElse(minValue)
    val maxInc = if (param.exclusiveMinimum) 0 else 1
    val max = param.maximum
      .map { p =>
        if (p.equalsIgnoreCase("inf")) maxValue else p.toInt + maxInc
      }
      .getOrElse(maxValue)
    rand.between(min, max)
  }

  private val Alphanumeric = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".getBytes

  private def mkStr(chars: Array[Byte], length: Int): String = {
    val bytes = new Array[Byte](length)
    for (i <- 0 until length) bytes(i) = chars(rand.nextInt(chars.length))
    new String(bytes, StandardCharsets.US_ASCII)
  }

  private def makeRandomString(param: ParameterModel): String = {
    val minLength = param.minLength.getOrElse(1)
    val maxLength = param.maxLength.getOrElse(20)
    mkStr(Alphanumeric, rand.between(minLength, maxLength))
  }

  private def makeMatrixParameter(param: ParameterModel, dim1: Int, dim2: Int): Option[Parameter[_]] = {
    val paramName = fixParamName(param.name)
    param.maybeArrayType.get match {
      case "integer" | "number" =>
        def a = (1 to dim2).map(_ => makeRandomValue(param, Int.MinValue, Int.MaxValue)).toArray
        Some(
          IntMatrixKey
            .make(paramName, makeCswUnits(param.units))
            .set((1 to dim1).map(_ => a).toArray)
        )
      case "byte" =>
        def a = (1 to dim2).map(_ => makeRandomValue(param, Byte.MinValue, Byte.MaxValue).toByte).toArray
        Some(
          ByteMatrixKey
            .make(paramName, makeCswUnits(param.units))
            .set((1 to dim1).map(_ => a).toArray)
        )
      case "short" =>
        def a = (1 to dim2).map(_ => makeRandomValue(param, Short.MinValue, Short.MaxValue).toShort).toArray
        Some(
          ShortMatrixKey
            .make(paramName, makeCswUnits(param.units))
            .set((1 to dim1).map(_ => a).toArray)
        )
      case "long" =>
        def a = (1 to dim2).map(_ => makeRandomValue(param, Long.MinValue, Long.MaxValue).toLong).toArray
        Some(
          LongMatrixKey
            .make(paramName, makeCswUnits(param.units))
            .set((1 to dim1).map(_ => a).toArray)
        )
      case "float" =>
//        def a = (1 to dim2).map(_ => makeRandomValue(param, Float.MinValue, Float.MaxValue).toFloat).toArray
        def a = (1 to dim2).map(_ => makeRandomValue(param, -1.0, 1.0).toFloat).toArray
        Some(
          FloatMatrixKey
            .make(paramName, makeCswUnits(param.units))
            .set((1 to dim1).map(_ => a).toArray)
        )
      case "double" =>
//        def a = (1 to dim2).map(_ => makeRandomValue(param, Double.MinValue, Double.MaxValue)).toArray
        def a = (1 to dim2).map(_ => makeRandomValue(param, -1.0, 1.0)).toArray
        Some(
          DoubleMatrixKey
            .make(paramName, makeCswUnits(param.units))
            .set((1 to dim1).map(_ => a).toArray)
        )
      case x =>
        log.warn(s"Unsupported CSW parameter matrix type: $x")
        None
    }
  }

  private def makeArrayParameter(param: ParameterModel): Option[Parameter[_]] = {
    val paramName = fixParamName(param.name)
    val dims =
      if (param.maybeDimensions.nonEmpty) param.maybeDimensions.get
      else List(param.maxItems.getOrElse(param.minItems.getOrElse(1)))
    if (dims.size == 2)
      makeMatrixParameter(param, dims.head, dims.tail.head)
    else {
      val arraySize = dims.head
      param.maybeArrayType.get match {
        case "integer" | "number" =>
          Some(
            IntArrayKey
              .make(paramName, makeCswUnits(param.units))
              .set((1 to arraySize).map(_ => makeRandomValue(param, Int.MinValue, Int.MaxValue)).toArray)
          )
        case "byte" =>
          Some(
            ByteArrayKey
              .make(paramName, makeCswUnits(param.units))
              .set((1 to arraySize).map(_ => makeRandomValue(param, Byte.MinValue, Byte.MaxValue).toByte).toArray)
          )
        case "short" =>
          Some(
            ShortArrayKey
              .make(paramName, makeCswUnits(param.units))
              .set((1 to arraySize).map(_ => makeRandomValue(param, Short.MinValue, Short.MaxValue).toShort).toArray)
          )
        case "long" =>
          Some(
            LongArrayKey
              .make(paramName, makeCswUnits(param.units))
              .set((1 to arraySize).map(_ => makeRandomValue(param, Long.MinValue, Long.MaxValue).toLong).toArray)
          )
        case "float" =>
          Some(
            FloatArrayKey
              .make(paramName, makeCswUnits(param.units))
//              .set((1 to arraySize).map(_ => makeRandomValue(param, Float.MinValue, Float.MaxValue).toFloat).toArray)
              .set((1 to arraySize).map(_ => makeRandomValue(param, -1.0, 1.0).toFloat).toArray)
          )
        case "double" =>
          Some(
            DoubleArrayKey
              .make(paramName, makeCswUnits(param.units))
//              .set((1 to arraySize).map(_ => makeRandomValue(param, Double.MinValue, Double.MaxValue)).toArray)
              .set((1 to arraySize).map(_ => makeRandomValue(param, -1.0, 1.0)).toArray)
          )
        case x =>
          log.warn(s"Unsupported CSW parameter array type: $x")
          None
      }
    }
  }

  // Generates a parameter with a random value in the defined range
  def makeParameter(param: ParameterModel): Option[Parameter[_]] = {
    // Replace illegal chars in name (TODO: Fix icd validation)
    val paramName = fixParamName(param.name)
    if (param.maybeType.isDefined) {
      param.maybeType.get match {
        case "integer" | "number" =>
          Some(
            IntKey
              .make(paramName, makeCswUnits(param.units))
              .set(makeRandomValue(param, Int.MinValue, Int.MaxValue))
          )
        case "byte" =>
          Some(
            ByteKey
              .make(paramName, makeCswUnits(param.units))
              .set(makeRandomValue(param, Byte.MinValue, Byte.MaxValue).toByte)
          )
        case "short" =>
          Some(
            ShortKey
              .make(paramName, makeCswUnits(param.units))
              .set(makeRandomValue(param, Short.MinValue, Short.MaxValue).toShort)
          )
        case "long" =>
          Some(
            LongKey
              .make(paramName, makeCswUnits(param.units))
              .set(makeRandomValue(param, Long.MinValue, Long.MaxValue).toLong)
          )
        case "float" =>
          Some(
            FloatKey
              .make(paramName, makeCswUnits(param.units))
//              .set(makeRandomValue(param, Float.MinValue, Float.MaxValue).toFloat)
              .set(makeRandomValue(param, -1.0, 1.0).toFloat)
          )
        case "double" =>
          Some(
            DoubleKey
              .make(paramName, makeCswUnits(param.units))
//              .set(makeRandomValue(param, Double.MinValue, Double.MaxValue))
              .set(makeRandomValue(param, -1.0, 1.0))
          )
        case "boolean" =>
          Some(
            BooleanKey
              .make(paramName)
              .set(rand.nextBoolean())
          )
        case "string" =>
          Some(
            StringKey
              .make(paramName, makeCswUnits(param.units))
              .set(makeRandomString(param))
          )
        case "array" =>
          if (param.maybeArrayType.isDefined) makeArrayParameter(param) else None
        case "struct" =>
          None // XXX TODO (maybe, might be removed)
        case "taiDate" =>
          None // XXX TODO
        case "utcDate" =>
          None // XXX TODO
        case "raDec" =>
          None // XXX TODO
        case "eqCoord" =>
          None // XXX TODO
        case "solarSystemCoord" =>
          None // XXX TODO
        case "minorPlanetCoord" =>
          None // XXX TODO
        case "cometCoord" =>
          None // XXX TODO
        case "altAzCoord" =>
          None // XXX TODO
        case "coord" =>
          None // XXX TODO
      }
    }
    else if (param.maybeEnum.isDefined) {
      None // XXX TODO
    }
    else {
      // should not happen
      log.error(s"No parameter type or enum type defined for ${paramName}")
      None
    }
  }

  def makeParamSet(parameterModels: List[ParameterModel]): Set[Parameter[_]] = {
    parameterModels.flatMap(makeParameter).toSet
  }
}
