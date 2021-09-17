package one.xingyi.exercise2.d

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import one.xingyi.exercise2.d.Attack

class CharacterTest extends  AnyFlatSpec with should.Matchers{

  behavior of "Character"

  it should "take damage" in {
    Character.doDamage(Attack(Character(), 25)) shouldBe Character(75, true)
    Character.doDamage(Attack(Character(), 100)) shouldBe Character(0, false)
    Character.doDamage(Attack(Character(), 150)) shouldBe Character(0, false)
    Character.doDamage(Attack(Character(25), 25)) shouldBe Character(0, false)
    Character.doDamage(Attack(Character(25), 24)) shouldBe Character(1, true)
  }
//  it should "take damage" in {
//     Character.attack(Character(1000, true), 100) shouldBe Character(900, true)
//     Character.attack(Character(1000, true), 1001) shouldBe Character(0, false)
//     Character.attack(Character(1000, true), 1000) shouldBe Character(0, true)
//  }

}
