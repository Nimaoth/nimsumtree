import std/[unittest, enumerate, strformat, sugar]
import nimsumtree/sumtree
import test/test_summary

const debugIteration {.used.} = 17

template customTest(name: string, body): untyped =
  test name:
    defer:
      GC_FullCollect()
      assertNoLeaks()
    block:
      body

customTest "seek end":
  var a = SumTree[int].new(@[1, 2])
  var c1 = a.initCursor
  check not c1.seekForward(End[tuple[]](), Right, ())
  check c1.didSeek
  check c1.atEnd

  a = SumTree[int].new(@[1, 2, 3, 4, 5])
  c1 = a.initCursor
  check not c1.seekForward(End[tuple[]](), Right, ())
  check c1.didSeek
  check c1.atEnd

customTest "prev/next leaf":
  var a = SumTree[int].new()
  var c1 = a.initCursor
  check c1.prevLeaf.isNone
  check c1.nextLeaf.isNone

  # One leaf
  a = SumTree[int].new(@[1, 2])
  c1 = a.initCursor
  check a.isLeaf
  check c1.prevLeaf.isNone
  check c1.nextLeaf.isNone

  c1.next(())
  check c1.prevLeaf.isNone
  check c1.nextLeaf.isNone

  # One internal, multiple leafs
  a = SumTree[int].new(@[1, 2, 3])
  c1 = a.initCursor

  when treeBase == 2:
    check a.isInternal
    check a.height == 1

  c1.next(())
  check c1.prevLeaf.isNone
  when treeBase == 2:
    check c1.nextLeaf.mapIt(@(it[].get.childItems)) == some @[3]

  c1.next(())
  check c1.prevLeaf.isNone
  when treeBase == 2:
    check c1.nextLeaf.mapIt(@(it[].get.childItems)) == some @[3]

  c1.next(())
  when treeBase == 2:
    check c1.prevLeaf.mapIt(@(it[].get.childItems)) == some @[1, 2]
  check c1.nextLeaf.isNone

  c1.next(())
  check c1.prevLeaf.isNone
  check c1.nextLeaf.isNone

customTest "prev/next item":
  # One leaf
  var a = SumTree[int].new(@[1, 2])
  var c1 = a.initCursor

  check c1.nextItem.mapIt(it[]) == 1.some

  c1.next(())
  check c1.prevItem.isNone
  check c1.item.mapIt(it[]) == 1.some
  check c1.nextItem.mapIt(it[]) == 2.some

  c1.next(())
  check c1.prevItem.mapIt(it[]) == 1.some
  check c1.item.mapIt(it[]) == 2.some
  check c1.nextItem.isNone

  c1.next(())
  check c1.prevItem.mapIt(it[]) == 2.some
  check c1.item.isNone
  check c1.nextItem.isNone

  # One internal, multiple leafs
  a = SumTree[int].new(@[1, 2, 3])
  c1 = a.initCursor

  check c1.nextItem.mapIt(it[]) == 1.some

  c1.next(())
  check c1.prevItem.isNone
  check c1.item.mapIt(it[]) == 1.some
  check c1.nextItem.mapIt(it[]) == 2.some

  c1.next(())
  check c1.prevItem.mapIt(it[]) == 1.some
  check c1.item.mapIt(it[]) == 2.some
  check c1.nextItem.mapIt(it[]) == 3.some

  c1.next(())
  check c1.prevItem.mapIt(it[]) == 2.some
  check c1.item.mapIt(it[]) == 3.some
  check c1.nextItem.isNone

  c1.next(())
  check c1.prevItem.mapIt(it[]) == 3.some
  check c1.item.isNone
  check c1.nextItem.isNone

customTest "prev":
  # One leaf
  var a = SumTree[int].new(@[1, 2])
  var c1 = a.initCursor

  check c1.nextItem.mapIt(it[]) == 1.some

  c1.prev(())
  check c1.prevItem.mapIt(it[]) == 1.some
  check c1.item.mapIt(it[]) == 2.some
  check c1.nextItem.isNone

  c1.prev(())
  check c1.prevItem.isNone
  check c1.item.mapIt(it[]) == 1.some
  check c1.nextItem.mapIt(it[]) == 2.some

  c1.prev(())
  check c1.prevItem.isNone
  check c1.item.isNone
  check c1.nextItem.mapIt(it[]) == 1.some

  # One internal, multiple leafs
  a = SumTree[int].new(@[1, 2, 3])
  c1 = a.initCursor

  check c1.nextItem.mapIt(it[]) == 1.some

  c1.prev(())
  check c1.prevItem.mapIt(it[]) == 2.some
  check c1.item.mapIt(it[]) == 3.some
  check c1.nextItem.isNone

  c1.prev(())
  check c1.prevItem.mapIt(it[]) == 1.some
  check c1.item.mapIt(it[]) == 2.some
  check c1.nextItem.mapIt(it[]) == 3.some

  c1.prev(())
  check c1.prevItem.isNone
  check c1.item.mapIt(it[]) == 1.some
  check c1.nextItem.mapIt(it[]) == 2.some

  c1.prev(())
  check c1.prevItem.isNone
  check c1.item.isNone
  check c1.nextItem.mapIt(it[]) == 1.some

proc testAppend(iterations: int, singleOwner: bool, base: int, balance: int) =
  # echo &"Append two trees 1 (single owner = {singleOwner}, base: {base}, bal: {balance}) C={$C}"
  var numbers1: seq[int]
  var numbers2: seq[int]

  for i in 1..iterations:
    if i mod balance == base:
      numbers1.add i
    else:
      numbers2.add i

    let res = numbers1 & numbers2

    var a = SumTree[int].new(numbers1)
    var b = SumTree[int].new(numbers2)

    var c: SumTree[int]
    if not singleOwner:
      c = a.clone()

    a.append(b.clone(), ())

    let items = collect:
      for i, item in enumerate(a.items(())):
        item

    check a.summary == TestSummary(count: i.Count, max: i.Max)
    check a.extent(Count, ()) == i.Count
    check a.extent(Max, ()) == i.Max
    check a.toSeq(()) == res
    check items == res

    if not singleOwner:
      check c.toSeq(()) == numbers1

proc testAdd(iterations: int, singleOwner: bool) =
  customTest &"testAdd {iterations}, {singleOwner}":
    var numbers: seq[int]
    var a = SumTree[int].new()

    for i in 1..iterations:
      var b = SumTree[int].new(numbers)
      var c: SumTree[int]
      if not singleOwner:
        c = a.clone()

      numbers.add i
      a.add(i, ())
      b.add(i, ())

      let itemsA = collect:
        for i, item in enumerate(a.items(())):
          item

      let itemsB = collect:
        for i, item in enumerate(a.items(())):
          item

      check a.summary == TestSummary(count: i.Count, max: i.Max)
      check a.extent(Count, ()) == i.Count
      check a.extent(Max, ()) == i.Max
      check a.toSeq(()) == numbers
      check itemsA == numbers

      check b.summary == TestSummary(count: i.Count, max: i.Max)
      check b.extent(Count, ()) == i.Count
      check b.extent(Max, ()) == i.Max
      check b.toSeq(()) == numbers
      check itemsB == numbers

      if not singleOwner:
        check c.toSeq(()) & @[i] == numbers

proc testN(iterations: int) =
  testAdd(iterations, true)
  testAdd(iterations, false)

  customTest &"first/last C={treeBase}":
    var a = SumTree[int].new()
    check a.first.isNone
    check a.last.isNone

    var numbers: seq[int]
    for i in 1..iterations:
      numbers.add i

      a = SumTree[int].new(numbers)
      check a.first.mapIt(it[]) == numbers[0].some
      check a.last.mapIt(it[]) == numbers[numbers.high].some

  customTest "Create empty sumtree C=" & $treeBase:
    defer:
      assertNoLeaks()

    let tree = SumTree[int].new(@[])
    check tree.isLeaf
    check tree.height == 0
    check tree.summary == TestSummary.default
    check tree.toSeq(()) == newSeq[int]()

    let leftLeaf {.cursor.} = tree.leftmostLeaf.get
    check leftLeaf == newLeaf[int]()
    let rightLeaf {.cursor.} = tree.rightmostLeaf.get
    check rightLeaf == newLeaf[int]()

    for k, item in enumerate(tree.items(())):
      check item == k + 1

  customTest "Create sumtree with n numbers C=" & $treeBase:
    var numbers: seq[int]
    for i in 1..iterations:
      numbers.add i

      let tree = SumTree[int].new(numbers)
      check tree.summary == TestSummary(count: i.Count, max: i.Max)
      check tree.toSeq(()) == numbers

      for k, item in enumerate(tree.items(())):
        check item == numbers[k]

  customTest "Append two trees (empty + empty) C=" & $treeBase:
    var a = SumTree[int].new()
    var b = SumTree[int].new()

    a.append(b, ())

    check a.height == 0
    check a.summary == TestSummary.default
    check a.extent(Count, ()) == 0.Count
    check a.extent(Max, ()) == 0.Max
    check a.toSeq(()) == newSeq[int]()

  customTest "Append two trees (non-empty + empty) C=" & $treeBase:
    var numbers: seq[int]
    for i in 1..iterations:
      numbers.add i

      var a = SumTree[int].new(numbers)
      var b = SumTree[int].new()

      a.append(b, ())

      check a.summary == TestSummary(count: i.Count, max: i.Max)
      check a.extent(Count, ()) == i.Count
      check a.extent(Max, ()) == i.Max
      check a.toSeq(()) == numbers

  customTest "Append two trees (empty + non-empty) C=" & $treeBase:
    var numbers: seq[int]
    for i in 1..iterations:
      numbers.add i

      var a = SumTree[int].new()
      var b = SumTree[int].new(numbers)

      a.append(b, ())

      check a.summary == TestSummary(count: i.Count, max: i.Max)
      check a.extent(Count, ()) == i.Count
      check a.extent(Max, ()) == i.Max
      check a.toSeq(()) == numbers

  customTest &"Append two trees of different shapes C={treeBase}":
    for i in 2..50:
      testAppend(iterations, true, 0, i)
      testAppend(iterations, false, 0, i)

      testAppend(iterations, true, 1, i)
      testAppend(iterations, false, 1, i)

  customTest "Cursor empty tree C=" & $treeBase:
    var b = SumTree[int].new()

    block:
      var c = b.initCursor
      check not c.didSeek
      check c.atEnd
      check c.startPos == ()

    block:
      var c = b.initCursor Count
      check not c.didSeek
      check c.atEnd
      check c.startPos == 0.Count

    block:
      var c = b.initCursor (Count, Max)
      check not c.didSeek
      check c.atEnd
      check c.startPos == (0.Count, 0.Max)

    block:
      var c = b.initCursor TestSummary
      check not c.didSeek
      check c.atEnd
      check c.startPos == TestSummary.default

  customTest "Cursor small tree C=" & $treeBase:
    var b = SumTree[int].new(@[2, 5])
    var c = b.initCursor (Count, Max)

    check not c.didSeek
    check not c.atEnd
    check c.startPos == (0.Count, 0.Max)

    c.next(())

    check c.didSeek
    check not c.atEnd
    check c.startPos == (0.Count, 0.Max)
    check c.endPos(()) == (1.Count, 2.Max)

    c.next(())

    check c.didSeek
    check not c.atEnd
    check c.startPos == (1.Count, 2.Max)
    check c.endPos(()) == (2.Count, 5.Max)

    c.next(())

    check c.didSeek
    check c.atEnd
    check c.startPos == (2.Count, 5.Max)
    check c.endPos(()) == (2.Count, 5.Max)

  customTest "Cursor medium tree C=" & $treeBase:
    let numbers = @[2, 16, 5, 7, 9, 13, 4, 6, 1, 67, 54, 8, 43, 3]
    var b = SumTree[int].new(numbers)
    var c = b.initCursor (Count, Max)

    check not c.didSeek
    check not c.atEnd
    check c.startPos == (0.Count, 0.Max)

    var endPos = (0.Count, 0.Max)
    var m = 0
    for i in 0..<numbers.len:
      m = max(m, numbers[i])

      c.next(())
      check c.didSeek
      check not c.atEnd
      check c.startPos == endPos
      check c.endPos(()) == ((i + 1).Count, m.Max)

      endPos = c.endPos(())

    c.next(())
    check c.atEnd
    check c.startPos == endPos
    check c.endPos(()) == endPos

  customTest "Cursor large tree C=" & $treeBase:
    var numbers = newSeq[int]()

    for i in 1..iterations:
      numbers.add i

      var b = SumTree[int].new(numbers)
      var c = b.initCursor TestSummary

      check not c.didSeek
      check not c.atEnd
      check c.startPos == TestSummary.default

      var endPos = TestSummary.default
      var m = 0
      for i in 0..<numbers.len:
        m = max(m, numbers[i])

        c.next(())
        check c.didSeek
        check not c.atEnd
        check c.startPos == endPos
        check c.endPos(()) == TestSummary(count: (i + 1).Count, max: m.Max)

        endPos = c.endPos(())

      c.next(())
      check c.atEnd
      check c.startPos == endPos
      check c.endPos(()) == endPos

  customTest "Cursor seek forward C=" & $treeBase:
    var numbers = newSeq[int]()

    for i in 1..iterations:
      numbers.add i

      var a = SumTree[int].new(numbers)
      var c1 = a.initCursor TestSummary

      var m = 0
      for i in 0..<numbers.len:
        m = max(m, numbers[i])

        var c2 = a.initCursor TestSummary

        check c1.seekForward(i.Count, Right, ()) == true
        check c2.seekForward(i.Count, Right, ()) == true

        check c1 == c2

  customTest "Cursor summary small incremental C=" & $treeBase:
    var numbers = @[2, 3, 1, 5, 4, 6]

    var a = SumTree[int].new(numbers)

    var c1 = a.initCursor (Count, Max)
    check c1.summary((Count, Max), 0.Count, Right, ()) == (0.Count, 0.Max)
    check c1.summary((Count, Max), 0.Count, Right, ()) == (0.Count, 0.Max)

    check c1.summary((Count, Max), 1.Count, Right, ()) == (1.Count, 2.Max)
    check c1.summary((Count, Max), 1.Count, Right, ()) == (0.Count, 0.Max)

    check c1.summary((Count, Max), 2.Count, Right, ()) == (1.Count, 3.Max)
    check c1.summary((Count, Max), 2.Count, Right, ()) == (0.Count, 0.Max)

    check c1.summary((Count, Max), 3.Count, Right, ()) == (1.Count, 1.Max)
    check c1.summary((Count, Max), 3.Count, Right, ()) == (0.Count, 0.Max)

    check c1.summary((Count, Max), 4.Count, Right, ()) == (1.Count, 5.Max)
    check c1.summary((Count, Max), 4.Count, Right, ()) == (0.Count, 0.Max)

    c1.reset()
    check c1.summary((Count, Max), 3.Count, Right, ()) == (3.Count, 3.Max)
    check c1.summary((Count, Max), 3.Count, Right, ()) == (0.Count, 0.Max)
    check c1.summary((Count, Max), End[(Count, Max)](), Right, ()) == (3.Count, 6.Max)
    check c1.summary((Count, Max), End[(Count, Max)](), Right, ()) == (0.Count, 0.Max)

  customTest "Cursor summary small reset C=" & $treeBase:
    var numbers = @[2, 3, 1, 5, 4, 6]

    var a = SumTree[int].new(numbers)

    var c1 = a.initCursor (Count, Max)
    check c1.summary((Count, Max), 0.Count, Right, ()) == (0.Count, 0.Max)
    check c1.summary((Count, Max), 0.Count, Right, ()) == (0.Count, 0.Max)
    c1.reset()

    check c1.summary((Count, Max), 1.Count, Right, ()) == (1.Count, 2.Max)
    check c1.summary((Count, Max), 1.Count, Right, ()) == (0.Count, 0.Max)
    c1.reset()

    check c1.summary((Count, Max), 2.Count, Right, ()) == (2.Count, 3.Max)
    check c1.summary((Count, Max), 2.Count, Right, ()) == (0.Count, 0.Max)
    c1.reset()

    check c1.summary((Count, Max), 3.Count, Right, ()) == (3.Count, 3.Max)
    check c1.summary((Count, Max), 3.Count, Right, ()) == (0.Count, 0.Max)
    c1.reset()

    check c1.summary((Count, Max), 4.Count, Right, ()) == (4.Count, 5.Max)
    check c1.summary((Count, Max), 4.Count, Right, ()) == (0.Count, 0.Max)
    c1.reset()

    check c1.summary((Count, Max), 5.Count, Right, ()) == (5.Count, 5.Max)
    check c1.summary((Count, Max), 5.Count, Right, ()) == (0.Count, 0.Max)
    c1.reset()

    check c1.summary((Count, Max), 6.Count, Right, ()) == (6.Count, 6.Max)
    check c1.summary((Count, Max), 6.Count, Right, ()) == (0.Count, 0.Max)
    c1.reset()

    check c1.summary((Count, Max), End[(Count, Max)](), Right, ()) == (6.Count, 6.Max)
    check c1.summary((Count, Max), End[(Count, Max)](), Right, ()) == (0.Count, 0.Max)
    c1.reset()

  customTest "Cursor summary large C=" & $treeBase:
    var numbers = newSeq[int]()

    for i in 1..iterations:
      numbers.add i

      var a = SumTree[int].new(numbers)
      var c1 = a.initCursor (Count, Max)

      var m = 0
      for i in 0..<numbers.len:
        var c2 = a.initCursor (Count, Max)

        if i == 0:
          check c1.summary((Count, Max), i.Count, Right, ()) == (0.Count, 0.Max)
          check c2.summary((Count, Max), i.Count, Right, ()) == (0.Count, 0.Max)
        else:
          check c1.summary((Count, Max), i.Count, Right, ()) == (1.Count, numbers[i - 1].Max)
          check c2.summary((Count, Max), i.Count, Right, ()) == (i.Count, m.Max)

        check c1 == c2

        m = max(m, numbers[i])

    customTest &"suffix C={treeBase}":
      var a = SumTree[int].new()
      var c = a.initCursor Count

      var b = c.suffix(())
      check b.toSeq(()) == newSeq[int]()

      a = SumTree[int].new(@[1, 2])
      c = a.initCursor Count

      b = c.suffix(())
      check b.toSeq(()) == @[1, 2]

      discard c.seek(1.Count, Right, ())
      b = c.suffix(())
      check b.toSeq(()) == @[2]

      let numbers = collect:
        for i in 1..100:
          i

      a = SumTree[int].new(numbers)
      c = a.initCursor Count

      for windowSize in 0..numbers.len:
        for start in 0..<numbers.len - windowSize + 1:
          discard c.seek(start.Count, Right, ())
          let b = c.slice((start + windowSize).Count, Right, ())
          check b.toSeq(()) == numbers[start..<(start + windowSize)]

testN(100)
# testN[4](100)
# testN[6](100)
# testN[8](100)
# testN[20](100)
# # testN[64](1000)
