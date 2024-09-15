import std/[strutils, sequtils, strformat, unittest, random, math, sets, monotimes]

import nimsumtree/[buffer, rope, clock]

var debugFuzz = false
var logTimes = false

template prettyIt(self: untyped): untyped =
  (("\n" & self.mapIt($it).join("\n")).indent(1, "  "))

suite "tests":
  var doc1: Buffer
  var doc2: Buffer
  var ops1 = newSeq[Operation]()
  var ops2 = newSeq[Operation]()

  proc initBuffers(initialText: string) =
    when defined(iter1) or defined(iter2) or defined(iter3):
      doc1 = initBuffer(0.ReplicaId)
      discard doc1.insertText(0, initialText)
    else:
      doc1 = initBuffer(0.ReplicaId, initialText)
    doc2 = doc1.clone(1.ReplicaId)

    ops1.setLen 0
    ops2.setLen 0

  proc sync(d1 = true, d2 = true) =
    if d1:
      if debugLog: echo "========================================= sync 2 to 1"
      if debugLog: echo doc1
      discard doc1.applyRemote(ops2)
      ops2.setLen 0
      if debugLog: echo "  doc1: '", doc1.contentString, "': ", doc1
    if d2:
      if debugLog: echo "========================================= sync 1 to 2"
      if debugLog: echo doc2
      discard doc2.applyRemote(ops1)
      ops1.setLen 0
      if debugLog: echo "  doc2: '", doc2.contentString, "': ", doc2

  template insert1(pos1, text1: untyped): untyped =
    ops1.add doc1.insertText(pos1, text1)
    if debugLog: echo "  ops1: ", ops1.prettyIt
    if debugLog: echo "  doc1: ", doc1

  template insert2(pos2, text2: untyped): untyped =
    ops2.add doc2.insertText(pos2, text2)
    if debugLog: echo "  ops2: ", ops2.prettyIt
    if debugLog: echo "  doc2: ", doc2

  template delete1(r: Slice[int]): untyped =
    ops1.add doc1.delete(r)
    if debugLog: echo "  ops1: ", ops1.prettyIt
    if debugLog: echo "  doc1: ", doc1

  template delete2(r: Slice[int]): untyped =
    ops2.add doc2.delete(r)
    if debugLog: echo "  ops2: ", ops2.prettyIt
    if debugLog: echo "  doc2: ", doc2

  template deleteRange1(r: Slice[int]): untyped =
    ops1.add doc1.deleteRange(r)
    if debugLog: echo "  ops1: ", ops1.prettyIt
    if debugLog: echo "  doc1: ", doc1

  template deleteRange2(r: Slice[int]): untyped =
    ops2.add doc2.deleteRange(r)
    if debugLog: echo "  ops2: ", ops2.prettyIt
    if debugLog: echo "  doc2: ", doc2

  var startTime: MonoTime

  setup:
    # echo "====================================================="
    initBuffers("hi")
    startTime = getMonoTime()

  teardown:
    if logTimes:
      let elapsed = getMonoTime() - startTime
      echo &"    took {elapsed.ms} ms"

    debugLog = false
    sync()
    check doc1.contentString == doc2.contentString
    check doc1.items == doc2.items

  test "undo":
    initBuffers("In 1968,")

    let op11 = doc1.insertText(3, "December of ")
    let op12 = doc2.insertText(8, " Douglas Engelbart")

    check doc1.contentString == "In December of 1968,"
    check doc2.contentString == "In 1968, Douglas Engelbart"

    discard doc1.applyRemote(op12)
    discard doc2.applyRemote(op11)

    check doc1.contentString == "In December of 1968, Douglas Engelbart"
    check doc2.contentString == "In December of 1968, Douglas Engelbart"

    let op21 = doc1.insertText(29, "C. ")
    let op22 = doc2.insertText(38, " demonstrated")

    check doc1.contentString == "In December of 1968, Douglas C. Engelbart"
    check doc2.contentString == "In December of 1968, Douglas Engelbart demonstrated"

    discard doc1.applyRemote(op22)
    discard doc2.applyRemote(op21)

    check doc1.contentString == "In December of 1968, Douglas C. Engelbart demonstrated"
    check doc2.contentString == "In December of 1968, Douglas C. Engelbart demonstrated"

    let undo1 = doc1.undo(op21[0].edit.timestamp)

    check doc1.contentString == "In December of 1968, Douglas Engelbart demonstrated"
    check doc2.contentString == "In December of 1968, Douglas C. Engelbart demonstrated"

    discard doc2.applyRemote(@[undo1])

    check doc1.contentString == "In December of 1968, Douglas Engelbart demonstrated"
    check doc2.contentString == "In December of 1968, Douglas Engelbart demonstrated"

    let undo2 = doc2.undo(op12[0].edit.timestamp)

    check doc1.contentString == "In December of 1968, Douglas Engelbart demonstrated"
    check doc2.contentString == "In December of 1968, demonstrated"

    discard doc1.applyRemote(@[undo2])

    check doc1.contentString == "In December of 1968, demonstrated"
    check doc2.contentString == "In December of 1968, demonstrated"

    let undo3 = doc2.undo(op21[0].edit.timestamp)

    check doc1.contentString == "In December of 1968, demonstrated"
    check doc2.contentString == "In December of 1968,C.  demonstrated"

    discard doc1.applyRemote(@[undo3])

    check doc1.contentString == "In December of 1968,C.  demonstrated"
    check doc2.contentString == "In December of 1968,C.  demonstrated"

  test "Replace overlapping":
    const initialContent = "Hello world!"
    var doc1: Buffer = initBuffer(0.ReplicaId, initialContent)
    var doc2: Buffer = initBuffer(1.ReplicaId, initialContent)

    let ops1 = doc1.edit([(6..<12, "?")])
    let ops2 = doc2.edit([(6..<11, "you")])

    check $doc1.visibleText == "Hello ?"
    check $doc2.visibleText == "Hello you!"

    discard doc1.applyRemote(@[ops2])
    discard doc2.applyRemote(@[ops1])

    check $doc1.visibleText == "Hello you?"
    check $doc2.visibleText == "Hello you?"

  test "insert in 1 at start":
    insert1(0, "Yo, ")
    check doc1.contentString == "Yo, hi"
    check doc2.contentString == "hi"
    sync()
    check doc1.contentString == "Yo, hi"
    check doc2.contentString == "Yo, hi"

  test "insert in 1 in middle":
    insert1(1, "ell")
    check doc1.contentString == "helli"
    check doc2.contentString == "hi"
    sync()
    check doc1.contentString == "helli"
    check doc2.contentString == "helli"

  test "insert in 1 at end":
    # debugLog = true
    insert1(2, " you")
    check doc1.contentString == "hi you"
    check doc2.contentString == "hi"
    sync()
    check doc1.contentString == "hi you"
    check doc2.contentString == "hi you"

  test "insert in 2 at start":
    insert2(0, "Yo, ")
    check doc1.contentString == "hi"
    check doc2.contentString == "Yo, hi"
    sync()
    check doc1.contentString == "Yo, hi"
    check doc2.contentString == "Yo, hi"

  test "insert in 2 in middle":
    insert2(1, "ell")
    check doc1.contentString == "hi"
    check doc2.contentString == "helli"
    sync()
    check doc1.contentString == "helli"
    check doc2.contentString == "helli"

  test "insert in 2 at end":
    insert2(2, " you")
    check doc1.contentString == "hi"
    check doc2.contentString == "hi you"
    sync()
    check doc1.contentString == "hi you"
    check doc2.contentString == "hi you"

  test "insert conflict at start 1":
    insert1(0, "H")
    check doc1.contentString == "Hhi"
    insert2(0, "Y")
    check doc2.contentString == "Yhi"
    sync()

  test "insert conflict at start 2":
    insert1(0, "Hi")
    check doc1.contentString == "Hihi"
    insert2(0, "Yo, ")
    check doc2.contentString == "Yo, hi"

    sync()

    check doc1.contentString.endsWith("i")
    check doc1.contentString.contains("Yo, ")
    check doc1.contentString.contains("Hi")

    insert1(0, "Test ")
    check doc1.contentString == "Test HiYo, hi"

    sync()

    check doc1.contentString.endsWith("i")
    check doc1.contentString.contains("Yo, ")
    check doc1.contentString.contains("Hi")
    check doc1.contentString.contains("Test ")

  test "insert conflict in middle":
    # debugLog = true
    insert1(1, "ell")
    check doc1.contentString == "helli"
    insert2(1, "oudin")
    check doc2.contentString == "houdini"
    sync()
    check doc1.contentString.startsWith("h")
    check doc1.contentString.endsWith("i")
    check doc1.contentString.contains("oudin")
    check doc1.contentString.contains("ell")

  test "insert conflict at end":
    insert1(2, "gh")
    check doc1.contentString == "high"
    insert2(2, "ll")
    check doc2.contentString == "hill"
    sync()
    check doc1.contentString.startsWith("hi")
    check doc1.contentString.contains("ll")
    check doc1.contentString.contains("gh")

  test "delete in 1":
    initBuffers("Hello world!")
    delete1(5..<11)
    check doc1.contentString == "Hello!"
    check doc2.contentString == "Hello world!"
    sync()
    check doc1.contentString == "Hello!"
    check doc2.contentString == "Hello!"
    initBuffers("Hello world!")

  test "delete in 2":
    initBuffers("Hello world!")
    delete2(5..<11)
    check doc1.contentString == "Hello world!"
    check doc2.contentString == "Hello!"
    sync()
    check doc1.contentString == "Hello!"
    check doc2.contentString == "Hello!"

  test "delete in 1 and 2 in different locations (1)":
    initBuffers("Hello world!")
    delete1(11..<12)
    delete2(5..<11)
    check doc1.contentString == "Hello world"
    check doc2.contentString == "Hello!"
    sync()
    check doc1.contentString == "Hello"
    check doc2.contentString == "Hello"

  test "delete in 1 and 2 in different locations (2)":
    initBuffers("Hello world!")
    delete1(0..<6)
    delete2(2..<9)
    check doc1.contentString == "world!"
    check doc2.contentString == "Held!"
    sync()
    check doc1.contentString == "ld!"
    check doc2.contentString == "ld!"

  test "t1":
    initBuffers("In 1968,")
    insert1(3, "December of ")
    insert2(8, " Douglas Engelbart")
    check doc1.contentString == "In December of 1968,"
    check doc2.contentString == "In 1968, Douglas Engelbart"
    sync()
    check doc1.contentString == "In December of 1968, Douglas Engelbart"
    check doc2.contentString == "In December of 1968, Douglas Engelbart"
    check doc1.items == doc2.items
    insert1(29, "C. ")
    insert2(38, " demonstrated")
    check doc1.contentString == "In December of 1968, Douglas C. Engelbart"
    check doc2.contentString == "In December of 1968, Douglas Engelbart demonstrated"
    sync()
    check doc1.contentString == "In December of 1968, Douglas C. Engelbart demonstrated"
    check doc2.contentString == "In December of 1968, Douglas C. Engelbart demonstrated"

  test "t2":
    initBuffers("In 1968,")
    delete1(0..7)
    insert2(8, " Douglas Engelbart")
    check doc1.contentString == ""
    check doc2.contentString == "In 1968, Douglas Engelbart"
    sync()
    check doc1.contentString == " Douglas Engelbart"
    check doc2.contentString == " Douglas Engelbart"

  test "t3 delete":
    initBuffers("In 1968,")
    insert2(8, " Douglas Engelbart")
    check doc1.contentString == "In 1968,"
    check doc2.contentString == "In 1968, Douglas Engelbart"
    sync()
    check doc1.contentString == "In 1968, Douglas Engelbart"
    check doc2.contentString == "In 1968, Douglas Engelbart"
    check doc1.items == doc2.items

    delete1(3..20)
    insert2(17, "C. ")
    check doc1.contentString == "In lbart"
    check doc2.contentString == "In 1968, Douglas C. Engelbart"
    sync()
    check doc1.contentString == "In C. lbart"
    check doc2.contentString == "In C. lbart"

  test "t3 deleteRange (long)":
    initBuffers("In 1968,")
    insert2(8, " Douglas Engelbart")
    check doc1.contentString == "In 1968,"
    check doc2.contentString == "In 1968, Douglas Engelbart"
    sync()
    check doc1.contentString == "In 1968, Douglas Engelbart"
    check doc2.contentString == "In 1968, Douglas Engelbart"

    deleteRange1(3..20)
    insert2(17, "C. ")
    check doc1.contentString == "In lbart"
    check doc2.contentString == "In 1968, Douglas C. Engelbart"
    sync()
    check doc1.contentString == "In C. lbart"
    check doc2.contentString == "In C. lbart"

  test "t3 deleteRange (short)":
    initBuffers("I")
    insert2(1, "D")
    check doc1.contentString == "I"
    check doc2.contentString == "ID"
    sync()
    check doc1.contentString == "ID"
    check doc2.contentString == "ID"

    deleteRange1(0..<2)
    insert2(1, "C")
    check doc1.contentString == ""
    check doc2.contentString == "ICD"
    sync()
    check doc1.contentString == "C"
    check doc2.contentString == "C"

  test "t4 conflict":
    initBuffers("In 1968,")
    insert1(8, " Engelbart")
    insert2(8, " Apollo 8")
    check doc1.contentString == "In 1968, Engelbart"
    check doc2.contentString == "In 1968, Apollo 8"
    sync()
    check doc1.contentString == "In 1968, Engelbart Apollo 8"
    check doc2.contentString == "In 1968, Engelbart Apollo 8"
    require doc1.items == doc2.items

    insert2(8, " Douglas")
    check doc1.contentString == "In 1968, Engelbart Apollo 8"
    check doc2.contentString == "In 1968, Douglas Engelbart Apollo 8"
    sync()
    check doc1.contentString == "In 1968, Douglas Engelbart Apollo 8"
    check doc2.contentString == "In 1968, Douglas Engelbart Apollo 8"
    require doc1.items == doc2.items

  proc fuzz(initialText: string, seed: int, cdf: array[3, int], iterations: int) =
    initBuffers(initialText)
    randomize(seed)
    type Op = enum
      Insert
      Delete
      Sync

    let ops = [Insert, Delete, Sync]
    let cdf = cdf.cumsummed

    for i in 0..iterations:
      # if i in {12}:
      #   debugLog = true
      defer:
        debugLog = false
      if debugLog or debugFuzz: echo "=========================================================================== ", i

      case ops.sample(cdf)
      of Insert:
        let docIdx = rand(1)
        let text = block:
          var s = ""
          for i in 0..rand(1..5):
            s.add rand('a'..'z')
          s

        if docIdx == 0:
          let location = rand(doc1.contentLength)
          assert location <= doc1.contentLength
          if debugLog or debugFuzz: echo "  Insert ", text, " at ", location, " in doc1"
          insert1(location, text)
        elif docIdx == 1:
          let location = rand(doc2.contentLength)
          assert location <= doc2.contentLength
          if debugLog or debugFuzz: echo "  Insert ", text, " at ", location, " in doc2"
          insert2(location, text)

      of Delete:
        let docIdx = rand(1)
        let contentLength = if docIdx == 0: doc1.contentLength else: doc2.contentLength
        if contentLength > 0:
          let first = rand(contentLength - 1)
          let len = rand(contentLength - first - 1) + 1
          if debugLog or debugFuzz: echo "  Delete " & $first & "..<" & $(first + len), " in doc", docIdx + 1
          if docIdx == 0:
            deleteRange1(first..<(first + len))
          elif docIdx == 1:
            deleteRange2(first..<(first + len))
        else:
          if debugLog or debugFuzz: echo "  Nothing to delete"

      of Sync:
        if debugLog or debugFuzz: echo "  Sync"
        sync()
        check doc1.contentLength == doc2.contentLength
        check doc1.contentString == doc2.contentString
        require doc1.items == doc2.items

      # if i >= 32:
      #   echo "  doc1: ", doc1
      #   echo "  doc2: ", doc2



      if debugLog: echo "  doc1: ", doc1.contentLength, "/", doc1.items.len, ", ", doc1.contentString
      if debugLog: echo "  visible: ", doc1.visibleText
      if debugLog: echo "  deleted: ", doc1.snapshot.deletedText
      if debugLog: echo "  doc2: ", doc2.contentLength, "/", doc2.items.len, ", ", doc2.contentString
      if debugLog: echo "  visible: ", doc2.visibleText
      if debugLog: echo "  deleted: ", doc2.snapshot.deletedText

      # if i == 34:
      #   debugLog = true
      #   if debugLog or debugFuzz: echo "  Sync !!!!!!!!!!!!!!!!"
      #   sync()
      #   check doc1.contentLength == doc2.contentLength
      #   check doc1.contentString == doc2.contentString
      #   require doc1.items == doc2.items

  test "fuzz insert only":
    fuzz("hello", 123, [30, 0, 5], 100)
  test "fuzz insert only":
    fuzz("hello", 123, [30, 0, 100], 100)
  for i in 0..5:
    test "fuzz insert only " & $i:
      fuzz("hello", 123*i, [30, 0, 50], 100)

  test "fuzz 1":
    fuzz("hello", 123, [30, 70, 1], 500)

  test "fuzz 2":
    fuzz("hello", 123, [30, 70, 10], 500)

  test "fuzz 3":
    fuzz("", 456789, [50, 70, 5], 500)

  test "fuzz 4":
    fuzz("", 987654, [50, 70, 100], 500)

  test "fuzz 5":
    fuzz("", 987654, [50, 70, 100], 500)

  test "fuzz 6":
    fuzz("", 147258369, [100, 30, 1], 500)

  test "fuzz 7":
    fuzz("", 147258369, [100, 30, 10], 500)

  test "fuzz 8":
    fuzz("", 147258369, [100, 30, 100], 500)

  when not defined(iter1) and not defined(iter2):
    test "t5 three docs":
      var doc1 = initBuffer(0.ReplicaId, "abcdef")
      var doc2 = initBuffer(1.ReplicaId, "abcdef")
      var doc3 = initBuffer(2.ReplicaId, "abcdef")

      let op1 = @[doc1.edit([(1..<2, "12")])]
      let op2 = @[doc2.edit([(3..<4, "34")])]
      let op3 = @[doc3.edit([(5..<6, "56")])]

      check doc1.contentString == "a12cdef"
      check doc2.contentString == "abc34ef"
      check doc3.contentString == "abcde56"

      discard doc1.applyRemote(op2)
      discard doc1.applyRemote(op3)
      discard doc2.applyRemote(op1)
      discard doc2.applyRemote(op3)
      discard doc3.applyRemote(op1)
      discard doc3.applyRemote(op2)

      check doc1.contentString == "a12c34e56"
      check doc2.contentString == "a12c34e56"
      check doc3.contentString == "a12c34e56"

  test "multi cursor":
    const initialContent = "foo(a, b)"
    var doc1: Buffer = initBuffer(0.ReplicaId, initialContent)
    var doc2: Buffer = initBuffer(1.ReplicaId, initialContent)

    let ops1 = doc1.edit([(4..<4, "["), (5..<5, "]"), (7..<7, "["), (8..<8, "]")])

    check $doc1.visibleText == "foo([a], [b])"
    check $doc2.visibleText == "foo(a, b)"

    discard doc2.applyRemote(@[ops1])

    check $doc1.visibleText == "foo([a], [b])"
    check $doc2.visibleText == "foo([a], [b])"

  test "multi cursor reverse":
    const initialContent = "foo(a, b)"
    var doc1: Buffer = initBuffer(0.ReplicaId, initialContent)
    var doc2: Buffer = initBuffer(1.ReplicaId, initialContent)

    let ops1 = doc1.edit([(7..<7, "["), (8..<8, "]"), (4..<4, "["), (5..<5, "]")])

    check $doc1.visibleText == "foo([a], [b])"
    check $doc2.visibleText == "foo(a, b)"

    discard doc2.applyRemote(@[ops1])

    check $doc1.visibleText == "foo([a], [b])"
    check $doc2.visibleText == "foo([a], [b])"
