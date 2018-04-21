package patmat

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(List('a', 'a', 'a', 'b', 'b', 'c'))") {
    assert(times(List('a', 'a', 'a', 'b', 'b', 'c')) === List(
      ('c', 1),
      ('b', 2),
      ('a', 3)
    ))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton detects whether only one tree is in list") {
    new TestTrees {
      assert(singleton(List(t1, t2)) === false)
      assert(singleton(List(t1)) === true)
      assert(singleton(List()) === false)
    }
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4))
    assert(combine(leaflist) === List(Leaf('x',4), Fork(Leaf('e',2),Leaf('t',3),List('e', 't'),5)))
  }

  test("until") {
    assert(until(singleton, combine)(List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))) ===
        List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Leaf('x',4),List('e', 't', 'x'),7)))
  }

  test("createCodeTree of someText") {
    assert(createCodeTree(string2Chars("someText")) ===
      Fork(
        Fork(
          Fork(
            Leaf('t',1),
            Leaf('x',1),
            List('t', 'x'),
            2
          ),
          Leaf('e',2),
          List('t', 'x', 'e'),
          4),
        Fork(
          Fork(
            Leaf('o',1),
            Leaf('s',1),
            List('o', 's'),
            2
          ),
          Fork(
            Leaf('T',1),
            Leaf('m',1),
            List('T', 'm'),
            2
          ),
          List('o', 's', 'T', 'm'),
          4
        ),
        List('t', 'x', 'e', 'o', 's', 'T', 'm'),
        8
      ))
  }

  test("decode") {
    val tree = Fork(Fork(Leaf('a', 2), Leaf('b', 2), List('a', 'b'), 4), Leaf('c', 1), List('a', 'b', 'c'), 5)
    assert(decode(tree, List(0, 0, 0, 1, 1)) === string2Chars("abc"))
  }

  test("decoded secret") {
    assert(decodedSecret.mkString("") === "huffmanestcool")
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("covert") {
    val codeTree = Fork(Fork(Leaf('a', 1), Leaf('b', 1), List('a', 'b'), 2), Leaf('d', 1), List('a', 'b', 'd'), 3)
    assert(convert(codeTree) === List(('a',List(0, 0)), ('b',List(0, 1)), ('d',List(1))))
  }

  test("decode and quick encode is identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
