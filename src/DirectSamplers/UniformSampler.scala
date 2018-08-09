package DirectSamplers

import Common.OneDimSampler
import NumScala.NSArray

class UniformSampler(lbound: Double = 0, hbound: Double = 1) extends OneDimSampler{
  private val range = hbound - lbound
  private def uniform(x: Double*) = if (x.head <= hbound && x.head >= lbound) 1.0 / range else 0.0

  override def sampleOneDims(nSamples: Int): NSArray[Double] = {
    NSArray((0 until nSamples).foldLeft(List[Double]())((lst, i) => lst :+ math.random() * range + lbound))
  }

  override def sampleOneDim(): Double = math.random() * range + lbound

  val pdf: (Double*) => Double = uniform _
}
