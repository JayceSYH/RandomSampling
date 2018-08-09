package RandomSampling

import Common.Sampler

import scala.math._

object MonteCarloIntegration {
  /*
   * Integration = Sigma(f(x) * dx) = Sigma(f(x) * dx / p(x) * p (x)) = Sigma(g(x) * p(x) * dx)
   */
  def integrate(f: (Double*)=>Double, bound: List[(Double, Double)], nSamples: Int)
                             (implicit pdf:(Double*)=>Double = null, sampler: Sampler = null, nDim: Int = bound.length): Double = {
    val range = bound.map{case (l, h) => h - l}
    var sampleData: ()=>Double = null
    var uniformRatio = range.foldLeft(1.0)(_ * _)

    // combine sampler and pdf into sampleData method
    if (sampler == null)
      sampleData = () => f((0 until nDim).map(i => random * range(i) + bound(i)._1) : _*) * uniformRatio
    else {
      val data = sampler.sample()
      f(data: _*) / sampler.pdf(data: _*)
    }

    // calculate integration by sample data
    var definiteIntegration = 0.0
    (1 to nSamples).foreach(i =>{
      val ratio = 1.0 / i
      definiteIntegration = definiteIntegration * (1 - ratio) + sampleData() * ratio
    })

    // return integration
    definiteIntegration
  }
}


object Test {
  def main(arg: Array[String]): Unit = {
    def gaussian(xx: Double*) = {val x = xx.head; 1.0 / sqrt(2 * Pi) * exp(- x * x / 2)}
    def circle(xx: Double*) = 2.0
    val integ = MonteCarloIntegration.integrate(circle _, List((-2, 2), (-2, 2)), 100000)
    print(integ)
  }
}