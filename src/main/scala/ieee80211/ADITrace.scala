package ieee80211

import java.io._
import java.nio.{ByteBuffer, ByteOrder}

import breeze.math.Complex

import org.tukaani.xz._

object ADITrace {
  // These are compressed with xz
  // Need to be decompressed
  def binary(stream: InputStream): Seq[Complex] = {
    // Decompress input stream
    val xz  = new XZInputStream(stream)

    // sometimes, java is dumb
    val buf = new ByteArrayOutputStream()

    var nRead: Int = 0
    var keepGoing: Boolean = true
    val data = new Array[Byte](16384)

    while (keepGoing) {
      nRead = xz.read(data, 0, data.length)
      if (nRead != -1) {
        buf.write(data, 0, nRead)
      } else {
        keepGoing = false
      }
    }

    val bytes = buf.toByteArray

    val bb = ByteBuffer.wrap(bytes)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    val shorts = new Array[Short]((bytes.length + 1) / 2)
    bb.asShortBuffer().get(shorts)

    shorts.grouped(2).map { case Array(r, i) =>
      Complex(r.toDouble / 32768.0, i.toDouble / 32768.0)
    }.toSeq
  }

  def text(stream: InputStream): Seq[Complex] = {
    scala.io.Source.fromInputStream(stream).getLines().map {
      case "TEXT" => Complex(0, 0)
      case s =>
        val real :: imag :: Nil = s.split("\t").toList
        Complex(real.toDouble / 32768.0, imag.toDouble / 32768.0)
    }.toSeq.tail
  }

  def resourceStream(resource: String): InputStream = {
    val toret = getClass.getResourceAsStream(resource)
    require(toret != null, "Bad resource")
    toret
  }

  def fileStream(name: String): InputStream = {
    val toret = new FileInputStream(name)
    require(toret != null, "Bad resource")
    toret
  }

  def binaryResource(resource: String): Seq[Complex] = {
    binary(resourceStream(resource))
  }
  def binaryFile(name: String): Seq[Complex] = {
    binary(fileStream(name))
  }

  def textResource(resource: String): Seq[Complex] = {
    text(resourceStream(resource))
  }
  def textFile(name: String): Seq[Complex] = {
    text(fileStream(name))
  }
}

object ADITraceMain {
  def main(arg: Array[String]): Unit = {
    val output = ADITrace.binaryResource("/waveforms/wifi-bpsk-loopback-cable.dat.xz")
    val input  = ADITrace.textResource("/waveforms/wifi_bpsk.txt")

    println(s"Input = $input")
    println(s"Output = $output")
    println()
  }
}
