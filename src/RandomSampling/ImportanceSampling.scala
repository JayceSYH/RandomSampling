package RandomSampling

import Common.Sampler

object ImportanceSampling {
  /*
   * E(p) = Sigma(x * p(x) * dx) = Sigma(x * p(x) / q(x) * q(x) * dx) = Sigma(g(x) * x * q(x) * dx)
   */
  def importanceSampling(p: (Double*)=>Double, sampler: Sampler, nSamples: Int) : Array[Double] = {
    // init expectation vector
    var expectation = new Array[Double](sampler.nDim)

    // sample from distribution q and calculate expectation of p
    (1 to nSamples).foreach(i => {
      val ratio = 1.0 / i
      val data = sampler.sample()
      val g = p(data : _*) / sampler.pdf(data : _*)
      (0 until sampler.nDim).foreach(exp => {expectation(i) = expectation(i) * (1.0 - ratio) + g * data(i) * ratio})
    })

    expectation
  }
}


object ImpTest{
  def main(args:Array[String]): Unit = {
//    ImportanceSampling.importanceSampling()
}
}