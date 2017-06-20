import org.scalatest._

/**
  * Created by lucas on 21/06/17.
  */
class BitTests extends FunSuite{

    test("Byte 0 is just 8 binary 0's"){
        assert(BitFunctions.byteToBits(0).equals((1 to 8).map(_ => 0)))
    }
}
