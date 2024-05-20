import std/[unittest, enumerate, strformat]
import nimsumtree/sumtree
import test/test_summary

const debugIteration {.used.} = 17

template customTest(name: string, body): untyped =
  test name:
    defer:
      assertNoLeaks()
    body

proc testAppend[C: static int](iterations: int, singleOwner: bool, base: int, balance: int) =
  GC_FullCollect()
  assertNoLeaks()
  defer:
    assertNoLeaks()

  # echo &"Append two trees 1 (single owner = {singleOwner}, base: {base}, bal: {balance}) C={$C}"
  var numbers1: seq[int]
  var numbers2: seq[int]

  for i in 1..iterations:
    if i mod balance == base:
      numbers1.add i
    else:
      numbers2.add i

    var a = SumTree[int, C].new(numbers1)
    var b = SumTree[int, C].new(numbers2)

    var c: SumTree[int, C]
    if not singleOwner:
      c = a.clone()

    a.append b.clone()

    check a.summary == TestSummary(count: i.Count, max: i.Max)
    check a.toSeq == numbers1 & numbers2

    if not singleOwner:
      check c.toSeq == numbers1

proc testN[C: static int](iterations: int) =
  customTest "Create empty sumtree C=" & $C:
    defer:
      assertNoLeaks()

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

  customTest "Create sumtree with n numbers C=" & $C:
    var numbers: seq[int]
    for i in 1..iterations:
      numbers.add i

      let tree = SumTree[int, C].new(numbers)
      check tree.summary == TestSummary(count: i.Count, max: i.Max)
      check tree.toSeq == numbers

      for k, item in enumerate(tree.items):
        check item == numbers[k]

  customTest "Append two empty trees C=" & $C:
    var a = SumTree[int, C].new()
    var b = SumTree[int, C].new()

    a.append b

    check a.height == 0
    check a.summary == TestSummary.default
    check a.toSeq == newSeq[int]()

  customTest "Append two trees (non-empty + empty) C=" & $C:
    var numbers: seq[int]
    for i in 1..iterations:
      numbers.add i

      var a = SumTree[int, C].new(numbers)
      var b = SumTree[int, C].new()

      a.append b

      check a.summary == TestSummary(count: i.Count, max: i.Max)
      check a.toSeq == numbers

  customTest "Append two trees (empty + non-empty) C=" & $C:
    var numbers: seq[int]
    for i in 1..iterations:
      numbers.add i

      var a = SumTree[int, C].new()
      var b = SumTree[int, C].new(numbers)

      a.append b

      check a.summary == TestSummary(count: i.Count, max: i.Max)
      check a.toSeq == numbers

  customTest &"Append two trees of different shapes C={C}":
    for i in 2..50:
      testAppend[C](iterations, true, 0, i)
      testAppend[C](iterations, false, 0, i)

      testAppend[C](iterations, true, 1, i)
      testAppend[C](iterations, false, 1, i)

testN[2](100)
testN[4](100)
testN[6](100)
testN[8](100)
testN[20](100)
# testN[64](1000)
