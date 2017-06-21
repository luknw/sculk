import org.scalacheck.{Arbitrary, Gen, Prop}

val int_bits_gen = Gen.choose(0, 31)
val number = Arbitrary.arbitrary[Int] suchThat (_ >= 0)
val number2 = Arbitrary.arbitrary[Int] suchThat (_ >= 0)

val lowestBitsCheck = Prop.forAll(number, number2, int_bits_gen){ (number, number2, int_bits_gen) =>
  BitFunctions.takeNLowestBits(BitFunctions.substituteLowestNBits(
    BitFunctions.takeNLowestBits(number, int_bits_gen), number2, int_bits_gen), int_bits_gen).equals(
    BitFunctions.takeNLowestBits(number, int_bits_gen))
}

lowestBitsCheck.check

val bitByteCheck = Prop.forAll{(byte: Byte) =>
  BitFunctions.bitsToByte(BitFunctions.byteToBits(byte)) == byte
}

bitByteCheck.check

val oddNumGen = Arbitrary.arbitrary[Int] suchThat (_ % 2 == 1)

val lowestBitCheck = Prop.forAll(oddNumGen){ oddNumGen =>
  BitFunctions.getBit(oddNumGen, 0) == 1
}

lowestBitCheck.check