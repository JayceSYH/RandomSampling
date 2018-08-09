package DirectSamplers

import Common.OneDimSampler
import NumScala.NSArray

class NormSampler(mu: Double = 0.0, sigma: Double = 1.0) extends OneDimSampler{
  private val c = 1 / (math.sqrt(2.0 * math.Pi) * sigma)
  val pdf: (Double*) => Double = norm _

  private def sample2(): List[Double] = {
    val theta = math.random()
    val r = math.random()

    List(math.sqrt(-2.0 * math.log(r)) * math.cos(2 * math.Pi * theta) * sigma + mu,
      math.sqrt(-2.0 * math.log(r)) * math.sin(2 * math.Pi * theta) * sigma + mu)
  }

  private def norm(x: Double*) = c * math.exp(-(x.head - mu)  * (x.head - mu) / (2 * sigma * sigma))

  override def sampleOneDims(nSamples: Int): NSArray[Double] = {
    val samples = (0 until nSamples / 2).foldLeft(List[Double]())((lst, i) => lst ::: sample2())
    if (nSamples % 2 == 1)
      samples :+ sample()

    NSArray(samples)
  }

  override def sampleOneDim(): Double = sample2().head
}
