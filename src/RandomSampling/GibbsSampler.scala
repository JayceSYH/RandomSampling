package RandomSampling

import Common.{OneDimSampler, Sampler}
import NumScala.NSArray

class GibbsSampler(sampledPdf: (Double*)=>Double, dims: Int,
                   conditionalDistributionSamplerGenerator: (Int, Double*)=>OneDimSampler,
                   warmUpSteps: Int = 20, initSample: NSArray[Double] = null) extends Sampler{

  val pdf: (Double*) => Double = sampledPdf
  val nDim: Int = dims
  private var data: NSArray[Double] = if (initSample == null) NSArray(new Array[Double](nDim)) else initSample
  private val warmUpProcedure: Unit = {
    (1 to warmUpSteps).foreach(_ => nextSample())
  }

  private def nextSample(): NSArray[Double] = {
    val axis = (math.random() * nDim).toInt
    val sampler = conditionalDistributionSamplerGenerator(axis, data: _*)
    val newSample = NSArray(data: _*)
    newSample(axis) = sampler.sampleOneDim()
    data = newSample
    data
  }

  override def sample(): NSArray[Double] = {
    nextSample()
  }
}


object TestGBS {
  def main(array: Array[String]): Unit = {
//    val gibbsSampler = new GibbsSampler()
  }
}
