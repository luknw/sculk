import org.scalatest._

/**
  * Created by lucas on 21/06/17.
  */
class BitTests extends FunSuite{

    test("Byte 0 is 8 zeros in binary"){
      assert(BitFunctions.byteToBits(0).equals((1 to 8).map(_ => 0)))
    }

    test("Byte -1 is 8 ones in binary"){
      assert(BitFunctions.byteToBits(-1).equals((1 to 8).map(_ => 1)))
    }

    test("Byte -128 is 10000000 in binary"){
      assert(BitFunctions.byteToBits(-128).equals(List(1,0,0,0,0,0,0,0)))
    }

    test("Byte 127 is 01111111 in binary"){
      assert(BitFunctions.byteToBits(127).equals(List(0,1,1,1,1,1,1,1)))
    }

    test("trying to read a bit at position greater than 31 should produce IllegalArgumentException"){
      assertThrows[IllegalArgumentException](BitFunctions.getBit(2, 32))
    }

    test("Trying to read a bit at position less than 0 should produce IllegalArgumentException"){
      assertThrows[IllegalArgumentException](BitFunctions.getBit(2, -1))
    }

}
