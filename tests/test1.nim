import std/[unittest, enumerate]
import nimsumtree/sumtree
import test/test_summary

const debugIteration {.used.} = 55

proc testN[C: static int]() =
  test "Create empty sumtree C=" & $C:
    let tree = SumTree[int, C].new([])
    check tree.isLeaf
    check tree.height == 0
    check tree.summary == TestSummary.default
    check tree.toSeq == newSeq[int]()

    let leftLeaf {.cursor.} = tree.leftmostLeaf.get
    check leftLeaf == newLeaf[int, C]()
    let rightLeaf {.cursor.} = tree.rightmostLeaf.get
    check rightLeaf == newLeaf[int, C]()

    for k, item in enumerate(tree.items):
      check item == k + 1

  test "Create sumtree with n numbers C=" & $C:
    var numbers: seq[int]
    for i in 1..100:
      numbers.add i

      let tree = SumTree[int, C].new(numbers)
      check tree.summary == TestSummary(count: i.Count, max: i.Max)
      check tree.toSeq == numbers

      for k, item in enumerate(tree.items):
        check item == numbers[k]

  test "Append two empty trees C=" & $C:
    var a = SumTree[int, C].new()
    var b = SumTree[int, C].new()

    a.append b

    check a.height == 0
    check a.summary == TestSummary.default
    check a.toSeq == newSeq[int]()

  test "Append two trees (non-empty + empty) C=" & $C:
    var numbers: seq[int]
    for i in 1..100:
      numbers.add i

      var a = SumTree[int, C].new(numbers)
      var b = SumTree[int, C].new()

      a.append b

      check a.summary == TestSummary(count: i.Count, max: i.Max)
      check a.toSeq == numbers

  test "Append two trees (empty + non-empty) C=" & $C:
    var numbers: seq[int]
    for i in 1..100:
      numbers.add i

      var a = SumTree[int, C].new()
      var b = SumTree[int, C].new(numbers)

      a.append b

      check a.summary == TestSummary(count: i.Count, max: i.Max)
      check a.toSeq == numbers

  test "Append two trees (single owner) C=" & $C:
    var numbers1: seq[int]
    var numbers2: seq[int]
    for i in 1..100:
      if i mod 2 == 0:
        numbers1.add i
      else:
        numbers2.add i

      var a = SumTree[int, C].new(numbers1)
      var b = SumTree[int, C].new(numbers2)

      a.append b

      check a.summary == TestSummary(count: i.Count, max: i.Max)
      check a.toSeq == numbers1 & numbers2

  test "Append two trees (multi owner) C=" & $C:
    var numbers1: seq[int]
    var numbers2: seq[int]
    for i in 1..100:
      if i mod 2 == 0:
        numbers1.add i
      else:
        numbers2.add i

      var a = SumTree[int, C].new(numbers1)
      var b = SumTree[int, C].new(numbers2)

      var c = a.clone()

      a.append b.clone()

      check a.summary == TestSummary(count: i.Count, max: i.Max)
      check a.toSeq == numbers1 & numbers2
      check c.toSeq == numbers1

testN[2]()
testN[4]()
testN[6]()
testN[8]()
testN[20]()
