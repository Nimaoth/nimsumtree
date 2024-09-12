import std/[strutils, sequtils, strformat, random, math, monotimes]
import clock, buffer

when isMainModule:
  var doc1: Buffer
  var doc2: Buffer
  var ops1 = newSeq[Operation]()
  var ops2 = newSeq[Operation]()

  proc initBuffers(initialText: string) =
    doc1 = initBuffer(0.ReplicaId, initialText)
    doc2 = doc1.clone()
    doc2.replicaId = 1.ReplicaId
    doc2.timestamp = initLamport(1.ReplicaId)

    ops1.setLen 0
    ops2.setLen 0

  proc sync(d1 = true, d2 = true) =
    if d1:
      doc1.applyRemote(ops2)
      ops2.setLen 0
    if d2:
      doc2.applyRemote(ops1)
      ops1.setLen 0

  template insert1(pos1, text1: untyped): untyped =
    ops1.add doc1.insertText(pos1, text1)

  template insert2(pos2, text2: untyped): untyped =
    ops2.add doc2.insertText(pos2, text2)

  template delete1(r: Slice[int]): untyped =
    ops1.add doc1.delete(r)

  template delete2(r: Slice[int]): untyped =
    ops2.add doc2.delete(r)

  template deleteRange1(r: Slice[int]): untyped =
    ops1.add doc1.deleteRange(r)

  template deleteRange2(r: Slice[int]): untyped =
    ops2.add doc2.deleteRange(r)

  proc fuzz(initialText: string, seed: int, cdf: array[3, int], iterations: int, maxDelete: int = int.high) =
    initBuffers(initialText)
    randomize(seed)
    type Op = enum
      Insert
      Delete
      Sync

    let ops = [Insert, Delete, Sync]
    let cdf = cdf.cumsummed

    var startTime: MonoTime = getMonoTime()

    for i in 0..iterations:
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
          insert1(location, text)
        elif docIdx == 1:
          let location = rand(doc2.contentLength)
          insert2(location, text)

      of Delete:
        let docIdx = rand(1)
        let contentLength = if docIdx == 0: doc1.contentLength else: doc2.contentLength
        if contentLength > 0:
          let first = rand(contentLength - 1)
          let len = min(rand(contentLength - first - 1) + 1, maxDelete)
          if docIdx == 0:
            deleteRange1(first..<(first + len))
          elif docIdx == 1:
            deleteRange2(first..<(first + len))

      of Sync:
        sync()

    let elapsed = (getMonoTime() - startTime).ms
    # echo &"Fuzz '{initialText}', seed {seed}, cdf {cdf}, iterations: {iterations}, took: {elapsed/1000} ms, {elapsed/iterations.float} per iteration"
    echo &"Initial len: {initialText.len}, iterations: {iterations}, elapsed: {elapsed/1000} s, iteration time: {elapsed / iterations.float} ms"

  fuzz("hello", 123, [30, 70, 1], 1000)
  fuzz("hello", 123, [30, 70, 10], 1000)
  fuzz("", 456789, [50, 70, 5], 1000)
  fuzz("", 987654, [50, 70, 100], 1000)
  fuzz("", 987654, [50, 70, 100], 1000)
  fuzz("", 147258369, [100, 30, 1], 1000)
  fuzz("", 147258369, [100, 30, 10], 1000)
  fuzz("", 147258369, [100, 30, 100], 1000)

  var s = ""
  for i in 0..1024:
    s.add rand('a'..'z')
  fuzz(s, 147258369, [100, 30, 10], 1000, maxDelete = 10)

  s = ""
  for i in 0..1024*1024:
    s.add rand('a'..'z')
  fuzz(s, 147258369, [100, 30, 10], 10000, maxDelete = 10)

  s = ""
  for i in 0..1024*1024*100:
    s.add rand('a'..'z')
  fuzz(s, 147258369, [100, 30, 10], 10000, maxDelete = 10)
