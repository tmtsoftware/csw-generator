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

  // Call Random.between(minInclusive: Double, maxExclusive: Double)
  private def makeRandomValue(param: ParameterModel, minValue: Double, maxValue: Double): Double = {
    val minInc = if (param.exclusiveMinimum) Double.MinValue else 0.0
    val min = param.minimum
      .map { p =>
        if (p.equalsIgnoreCase("-inf")) minValue else p.toDouble + minInc
      }
      .getOrElse(minValue)
    val maxInc = if (param.exclusiveMinimum) 0.0 else Double.MinValue
    val max = param.maximum
      .map { p =>
        if (p.equalsIgnoreCase("inf")) maxValue else p.toDouble + maxInc
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
    param.maybeArrayType.get match {
      case "integer" | "number" =>
        def a = (1 to dim2).map(_ => makeRandomValue(param, Int.MinValue, Int.MaxValue)).toArray
        Some(
          IntMatrixKey
            .make(param.name, makeCswUnits(param.units))
            .set((1 to dim1).map(_ => a).toArray)
        )
      case "byte" =>
        def a = (1 to dim2).map(_ => makeRandomValue(param, Byte.MinValue, Byte.MaxValue).toByte).toArray
        Some(
          ByteMatrixKey
            .make(param.name, makeCswUnits(param.units))
            .set((1 to dim1).map(_ => a).toArray)
        )
      case "short" =>
        def a = (1 to dim2).map(_ => makeRandomValue(param, Short.MinValue, Short.MaxValue).toShort).toArray
        Some(
          ShortMatrixKey
            .make(param.name, makeCswUnits(param.units))
            .set((1 to dim1).map(_ => a).toArray)
        )
      case "long" =>
        def a = (1 to dim2).map(_ => makeRandomValue(param, Long.MinValue, Long.MaxValue).toLong).toArray
        Some(
          LongMatrixKey
            .make(param.name, makeCswUnits(param.units))
            .set((1 to dim1).map(_ => a).toArray)
        )
      case "float" =>
        def a = (1 to dim2).map(_ => makeRandomValue(param, Float.MinValue, Float.MaxValue).toFloat).toArray
        Some(
          FloatMatrixKey
            .make(param.name, makeCswUnits(param.units))
            .set((1 to dim1).map(_ => a).toArray)
        )
      case "double" =>
        def a = (1 to dim2).map(_ => makeRandomValue(param, Double.MinValue, Double.MaxValue)).toArray
        Some(
          DoubleMatrixKey
            .make(param.name, makeCswUnits(param.units))
            .set((1 to dim1).map(_ => a).toArray)
        )
      case x =>
        log.warn(s"Unsupported CSW parameter matrix type: $x")
        None
    }
  }

  private def makeArrayParameter(param: ParameterModel): Option[Parameter[_]] = {
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
              .make(param.name, makeCswUnits(param.units))
              .set((1 to arraySize).map(_ => makeRandomValue(param, Int.MinValue, Int.MaxValue)).toArray)
          )
        case "byte" =>
          Some(
            ByteArrayKey
              .make(param.name, makeCswUnits(param.units))
              .set((1 to arraySize).map(_ => makeRandomValue(param, Byte.MinValue, Byte.MaxValue).toByte).toArray)
          )
        case "short" =>
          Some(
            ShortArrayKey
              .make(param.name, makeCswUnits(param.units))
              .set((1 to arraySize).map(_ => makeRandomValue(param, Short.MinValue, Short.MaxValue).toShort).toArray)
          )
        case "long" =>
          Some(
            LongArrayKey
              .make(param.name, makeCswUnits(param.units))
              .set((1 to arraySize).map(_ => makeRandomValue(param, Long.MinValue, Long.MaxValue).toLong).toArray)
          )
        case "float" =>
          Some(
            FloatArrayKey
              .make(param.name, makeCswUnits(param.units))
              .set((1 to arraySize).map(_ => makeRandomValue(param, Float.MinValue, Float.MaxValue).toFloat).toArray)
          )
        case "double" =>
          Some(
            DoubleArrayKey
              .make(param.name, makeCswUnits(param.units))
              .set((1 to arraySize).map(_ => makeRandomValue(param, Double.MinValue, Double.MaxValue)).toArray)
          )
        case x =>
          log.warn(s"Unsupported CSW parameter array type: $x")
          None
      }
    }
  }

  // Generates a parameter with a random value in the defined range
  def makeParameter(param: ParameterModel): Option[Parameter[_]] = {
    if (param.maybeType.isDefined) {
      param.maybeType.get match {
        case "integer" | "number" =>
          Some(
            IntKey
              .make(param.name, makeCswUnits(param.units))
              .set(makeRandomValue(param, Int.MinValue, Int.MaxValue))
          )
        case "byte" =>
          Some(
            ByteKey
              .make(param.name, makeCswUnits(param.units))
              .set(makeRandomValue(param, Byte.MinValue, Byte.MaxValue).toByte)
          )
        case "short" =>
          Some(
            ShortKey
              .make(param.name, makeCswUnits(param.units))
              .set(makeRandomValue(param, Short.MinValue, Short.MaxValue).toShort)
          )
        case "long" =>
          Some(
            LongKey
              .make(param.name, makeCswUnits(param.units))
              .set(makeRandomValue(param, Long.MinValue, Long.MaxValue).toLong)
          )
        case "float" =>
          Some(
            FloatKey
              .make(param.name, makeCswUnits(param.units))
              .set(makeRandomValue(param, Float.MinValue, Float.MaxValue).toFloat)
          )
        case "double" =>
          Some(
            DoubleKey
              .make(param.name, makeCswUnits(param.units))
              .set(makeRandomValue(param, Double.MinValue, Double.MaxValue))
          )
        case "boolean" =>
          Some(
            BooleanKey
              .make(param.name)
              .set(rand.nextBoolean())
          )
        case "string" =>
          Some(
            StringKey
              .make(param.name, makeCswUnits(param.units))
              .set(makeRandomString(param))
          )
        case "array" =>
          if (param.maybeArrayType.isDefined) makeArrayParameter(param) else None
        case "struct" =>
          None // XXX TODO
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
      log.error(s"No parameter type or enum type defined for ${param.name}")
      None
    }
  }

  def makeParamSet(parameterModels: List[ParameterModel]): Set[Parameter[_]] = {
    parameterModels.flatMap(makeParameter).toSet
  }
}
