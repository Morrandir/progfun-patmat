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

  test("times(\"hello, world\")") {
    val t = times(string2Chars("hello, world"))
    assert(t.contains(('h', 1)))
    assert(t.contains(('e', 1)))
    assert(t.contains(('l', 3)))
    assert(t.contains(('o', 2)))
    assert(t.contains((',', 1)))
    assert(t.contains((' ', 1)))
    assert(t.contains(('w', 1)))
    assert(t.contains(('r', 1)))
    assert(t.contains(('d', 1)))
    assert(t.size === 9)
  }

  test("makeOrderedLeafList") {
    val t = makeOrderedLeafList(times(string2Chars("hello, world")))
    assert(t === List(Leaf('h', 1), Leaf('e', 1), Leaf(',', 1), Leaf(' ', 1), Leaf('w', 1), Leaf('r', 1), Leaf('d', 1), Leaf('o', 2), Leaf('l', 3)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
