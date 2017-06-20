import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll

val gen = Gen.choose(0, 31)
val number = Gen.choose(0, 10000000)
val number2 = Gen.choose(0, 10000000)

val myProp = Prop.forAll(number, number2, gen){ (number, number2, gen) =>
  BitFunctions.takeNLowestBits(BitFunctions.substituteLowestNBits(
    BitFunctions.takeNLowestBits(number, gen), number2, gen), gen).equals(
    BitFunctions.takeNLowestBits(number, gen))
}

myProp.check