package one.xingyi.exercise2.d

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import one.xingyi.exercise2.d.Attack

class CharacterTest extends  AnyFlatSpec with should.Matchers{

  behavior of "Character"

  it should "take damage" in {
    Character.doDamage(Attack(Character(), 250)) shouldBe Character(750, true)
    Character.doDamage(Attack(Character(), 1000)) shouldBe Character(0, false)
    Character.doDamage(Attack(Character(), 1500)) shouldBe Character(0, false)
    Character.doDamage(Attack(Character(250), 250)) shouldBe Character(0, false)
    Character.doDamage(Attack(Character(250), 240)) shouldBe Character(10, true)
    Character.doDamage(Attack(Character(0, false), 1)) shouldBe Character(0, false)
  }

}
