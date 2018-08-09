package Common

import NumScala.NSArray

trait Sampler {
  def sample() : NSArray[Double]
  val pdf : (Double*)=>Double
  val nDim : Int
}

trait OneDimSampler extends Sampler {
  def sampleOneDim() : Double
  def sampleOneDims(nSamples: Int): NSArray[Double]
  val nDim: Int = 1
  override def sample(): NSArray[Double] = NSArray(sampleOneDim())
}


