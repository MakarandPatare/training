package one.xingyi.exercise2.d

import one.xingyi.exercise2.d.NonFunctional._

import java.util.concurrent.atomic.AtomicInteger
case class Character(health: Int = 100, isAlive: Boolean = true)
case class Attack(character: Character, damage: Int)

object Character {
  implicit val counter = new AtomicInteger(0)
  def checkHealth(health: Int): Boolean = if (health > 0) true else false

  def doDamageWithNonFunctionals: Attack => Character = applyNormalNonFunctional apply doDamage

  def doDamage(attack: Attack): Character = {
    import attack._
    val updatedHealth = if (character.health - damage < 0) 0 else character.health - damage
    new Character(updatedHealth, checkHealth(updatedHealth))
  }

  implicit def logAndRollBack(e: Exception, attack: Attack): Character = {
    println(e)
    attack.character
  }

}

object NonFunctional {
  type Decorator[From, To] = (From => To) => (From => To)

  def addLogging[From, To](fn: From => To): From => To =
    from => {
      val result = fn(from)
      println(from + " => " + result)
      result
    }

  def applyNormalNonFunctional[From, To](implicit counter: AtomicInteger, errorStrategy: (Exception, From) => To) =
    compose[From, To](
      addLogging,
      addErrorHandling,
      addMetrics,
    )


  def compose[From, To](decorators: (From => To) => (From => To)*): (From => To) => (From => To) =
    rawFn => decorators.foldLeft(rawFn)((acc, decorator) => decorator(acc))


  //currying to change everything to one input one output
  //functional composition


  def addMetrics[From, To](fn: From => To)(implicit counter: AtomicInteger): From => To =
    from => {
      counter.incrementAndGet()
      fn(from)
    }


  def addErrorHandling[From, To](fn: From => To)(implicit errorStrategy: (Exception, From) => To): From => To =
    from => {
      try {
        fn(from)
      } catch {
        case e: Exception => errorStrategy(e, from)
      }
    }
}