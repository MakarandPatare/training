package one.xingyi.exercise2.d

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.util.concurrent.atomic.AtomicInteger

class NonFunctionalTest extends AnyFlatSpec with should.Matchers {

  def fn[A, B](expected: A, result: B): (A => B) = {
    actual =>
      actual shouldBe expected
      result
  }

  val bizLogic = fn("a", "b")
  def bizLogic2(s: String) = s + "_wascalled"

  behavior of "Non Functionals - metrics"

  it should "return the results of the business logic when called" in {
    implicit val counter = new AtomicInteger(0)
    val decoratedBizLogic = NonFunctional.addMetrics(bizLogic2)
    decoratedBizLogic("a") shouldBe "a_wascalled"
  }

  it should "increment the counter every time the bizlogic is called" in {
    implicit val counter = new AtomicInteger(0)
    val decoratedBizLogic = NonFunctional.addMetrics(bizLogic)
    counter.get() shouldBe 0
    decoratedBizLogic("a")
    counter.get shouldBe 1
  }

  behavior of "Non Functionals - logging"

  it should "return the results of the business logic when called" in {
    implicit val logger = one.xingyi.exercise2.d.Logger.logger
    NonFunctional.addLogging(bizLogic2) apply "a" shouldBe "a_wascalled"
  }

  it should "log the error everytime bizlogic throws it" in {
    implicit val errorStrategy = one.xingyi.exercise2.d.ErrorHandler.strategy
   // implicit val counter = new AtomicInteger(0)
    val decoratedBizLogic = NonFunctional.addErrorHandling(bizLogic)
//    counter.get() shouldBe 0
//    decoratedBizLogic("a")
//    counter.get shouldBe 1
  }

  it should "increment the counter every time the bizlogic is called" in {
    //    implicit def errorStrategy[Exception, From, To] (exception: Exception, from: From): (Exception, From) => To = {
//      (exception, actual) =>
//        actual shouldBe exception
//        from
//    }
//    val decoratedBizLogic = NonFunctional.addErrorHandling(bizLogic)
//    counter.get() shouldBe 0
//    decoratedBizLogic("a")
//    counter.get shouldBe 1
  }

}
