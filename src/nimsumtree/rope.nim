import std/[strformat, os, strformat]
import nimsumtree/[sumtree]
import uni

const ropeChunkBase {.intdefine.} = 0
when ropeChunkBase > 0:
  const chunkBase* = ropeChunkBase

elif defined(testing):
  const chunkBase* = 16
  static:
    echo &"Rope: test environment, use chunkBase = {chunkBase}"

else:
  const chunkBase* = 128

when chunkBase < 4:
  {.error: "chunkBase must be >= 4 because utf-8 characters can be encoded with " &
    "up to four bytes".}

const chunkBaseHalf* = chunkBase div 2

type
  Count* = distinct int
  Count32* = distinct uint32

  Point* = object
    row*: uint32    ## Number of newlines
    column*: uint32 ## In bytes

  ByteRange* = tuple[first, last: int] ## First inclusive, last exclusive

  TextSummary* = object
    bytes*: int
    len*: Count
    lines*: Point
    firstLineChars*: Count32
    lastLineChars*: Count32
    longestRow*: uint32
    longestRowChars*: Count32

  Chunk* = object
    data: Array[char, chunkBase]

proc `=copy`*(a: var Chunk, b: Chunk) {.error.}
proc `=dup`*(a: Chunk): Chunk {.error.}

func `$`*(a: Count): string {.borrow.}
func `+=`*(a: var Count, b: Count) {.borrow.}
func `+`*(a: Count, b: Count): Count {.borrow.}
func `-`*(a: Count, b: Count): Count {.borrow.}
func `==`*(a: Count, b: Count): bool {.borrow.}
func `<`*(a: Count, b: Count): bool {.borrow.}
func `<=`*(a: Count, b: Count): bool {.borrow.}

func `$`*(a: Count32): string {.borrow.}
func `+=`*(a: var Count32, b: Count32) {.borrow.}
func `+`*(a: Count32, b: Count32): Count32 {.borrow.}
func `==`*(a: Count32, b: Count32): bool {.borrow.}
func `<`*(a: Count32, b: Count32): bool {.borrow.}
func `<=`*(a: Count32, b: Count32): bool {.borrow.}

func `$`*(r: Chunk): string =
  result = "["
  result.add r.data.toOpenArray.join("")
  result.add "]"

func init*(_: typedesc[Point], row: int, column: int): Point =
  Point(row: row.uint32, column: column.uint32)

func clone*(a: TextSummary): TextSummary = a

func addSummary*[C](a: var Count, b: Count, cx: C) = a = (a.int + b.int).Count
func clone*(a: Count): Count = a
func cmp*[C](a: Count, b: Count, cx: C): int = cmp(a.int, b.int)
func addSummary*[C](a: var Count, b: TextSummary, cx: C) = a += b.len
func fromSummary*[C](_: typedesc[Count], a: TextSummary, cx: C): Count = a.len

func addSummary*(a: var int, b: int) = a = a + b
func `+=`*(a: var int, b: int) = a = a + b
func clone*(a: int): int = a
func cmp*[C](a: int, b: int, cx: C): int = cmp(a, b)
func addSummary*[C](a: var int, b: TextSummary, cx: C) = a += b.bytes
func `+=`*(a: var int, b: TextSummary) = a += b.bytes
func fromSummary*[C](_: typedesc[int], a: TextSummary, cx: C): int = a.bytes

func `<`*(a: Point, b: Point): bool =
  if a.row == b.row:
    return a.column < b.column
  return a.row < b.row

func `<=`*(a: Point, b: Point): bool =
  if a.row == b.row:
    return a.column <= b.column
  return a.row <= b.row

func `+=`*(a: var Point, b: Point) =
  a.row += b.row
  if b.row == 0:
    a.column += b.column
  else:
    a.column = b.column

func `+`*(a: Point, b: Point): Point =
  result = a
  result += b

func `-`*(a: Point, b: Point): Point =
  assert b <= a
  if a.row == b.row:
    Point(row: 0, column: a.column - b.column)
  else:
    Point(row: a.row - b.row, column: a.column)

func addSummary*[C](a: var Point, b: Point, cx: C) = a += b
func clone*(a: Point): Point = a
func cmp*[C](a: Point, b: Point, cx: C): int =
  result = cmp(a.row, b.row)
  if result == 0:
    result = cmp(a.column, b.column)

func addSummary*[C](a: var Point, b: TextSummary, cx: C) = a += b.lines
func fromSummary*[C](_: typedesc[Point], a: TextSummary, cx: C): Point = a.lines

func cmp*[C](a: Count, b: TextSummary, cx: C): int = cmp(a, b.len)
func cmp*[C](a: Point, b: TextSummary, cx: C): int = cmp(a, b.lines)

func addSummary*[A, B](a: var (A, B), b: (A, B)) =
  a[0] += b[0]
  a[1] += b[1]

func clone*[A, B](a: (A, B)): (A, B) = (a[0].clone(), a[1].clone())
func cmp*[A, B, C](a: A, b: (A, B), cx: C): int = cmp(a, b[0], cx)

func addSummary*[A, B, C](a: var (A, B), b: TextSummary, cx: C) =
  a[0] += A.fromSummary(b, cx)
  a[1] += B.fromSummary(b, cx)

func `+=`*(self: var TextSummary, other: TextSummary) =
  let joinedChars = self.lastLineChars + other.firstLineChars
  if joinedChars > self.longestRowChars:
    self.longestRow = self.lines.row
    self.longestRowChars = joinedChars
  if other.longestRowChars > self.longestRowChars:
    self.longestRow = self.lines.row + other.longestRow
    self.longestRowChars = other.longestRowChars

  if self.lines.row == 0:
    self.firstLineChars += other.firstLineChars

  if other.lines.row == 0:
    self.lastLineChars += other.firstLineChars
  else:
    self.lastLineChars = other.lastLineChars

  self.bytes += other.bytes
  self.len += other.len
  self.lines += other.lines

func addSummary*[C](a: var TextSummary, b: TextSummary, cx: C) =
  a += b

func fromSummary*[C](_: typedesc[TextSummary], a: TextSummary, cx: C): TextSummary = a

################### Chunk ###################

func clone*(a: Chunk): Chunk = Chunk(data: a.data)
func toChunk*(text: string): Chunk = Chunk(data: text.toOpenArray(0, text.high).toArray(chunkBase))
func toChunk*(text: openArray[char]): Chunk = Chunk(data: text.toArray(chunkBase))

func init*(_: typedesc[TextSummary], x: openArray[char]): TextSummary =
  result.bytes = x.len

  for r in x.runes:
    result.len += 1.Count

    if r == '\n'.Rune:
      result.lines += Point(row: 1, column: 0)
      result.lastLineChars = 0.Count32
    else:
      result.lines.column += r.size.uint32
      result.lastLineChars += 1.Count32

    if result.lines.row == 0:
      result.firstLineChars = result.lastLineChars

    if result.lastLineChars > result.longestRowChars:
      result.longestRow = result.lines.row
      result.longestRowChars = result.lastLineChars

func init*(_: typedesc[TextSummary], x: string): TextSummary =
  TextSummary.init(x.toOpenArray(0, x.high))

func summary*(x: Chunk): TextSummary =
  TextSummary.init(x.data.toOpenArray)

func offsetToCount*(self: Chunk, target: int): Count =
  var offset = 0
  for r in self.data.toOpenArray.runes:
    if offset >= target:
      break
    offset += r.size
    result += 1.Count

func countToOffset*(self: Chunk, target: Count): int =
  var offset = 0.Count
  for r in self.data.toOpenArray.runes:
    if offset >= target:
      break
    result += r.size
    offset += 1.Count

func offsetToPoint*(self: Chunk, target: int): Point =
  var offset = 0
  for r in self.data.toOpenArray.runes:
    if offset >= target:
      break
    if r == '\n'.Rune:
      result.row += 1
      result.column = 0
    else:
      result.column += r.size.uint32
    offset += r.size

func countToPoint*(self: Chunk, target: Count): Point =
  var offset = 0.Count
  for r in self.data.toOpenArray.runes:
    if offset >= target:
      break
    if r == '\n'.Rune:
      result.row += 1
      result.column = 0
    else:
      result.column += r.size.uint32
    offset += 1.Count

func pointToOffset*(self: Chunk, target: Point, debug = false): int =
  var point = Point()
  for r in self.data.toOpenArray.runes:
    if point >= target:
      assert point == target, &"Target {target} is inside of character '{r}'"
      break

    if r == '\n'.Rune:
      point.row += 1
      point.column = 0

      if point.row > target.row:
        assert false, &"Target {target} is beyond the end of a line with length {point.column}"
        break
    else:
      point.column += r.size.uint32

    result += r.size

func pointToCount*(self: Chunk, target: Point): Count =
  var point = Point()
  for r in self.data.toOpenArray.runes:
    if point >= target:
      assert point == target, &"Target {target} is inside of character '{r}'"
      break

    if r == '\n'.Rune:
      point.row += 1
      point.column = 0

      if point.row > target.row:
        assert false, &"Target {target} is beyond the end of a line with length {point.column}"
        break
    else:
      point.column += r.size.uint32

    inc result

func toOffset*(self: Chunk, target: int): int =
  target

func toOffset*(self: Chunk, target: Count): int =
  self.countToOffset(target)

func toOffset*(self: Chunk, target: Point): int =
  self.pointToOffset(target)

func offsetTo*(self: Chunk, target: int, D: typedesc[int]): int =
  target

func offsetTo*(self: Chunk, target: int, D: typedesc[Point]): Point =
  self.offsetToPoint(target)

func offsetTo*(self: Chunk, target: int, D: typedesc[Count]): Count =
  self.offsetToCount(target)

func clipPoint*(self: Chunk, target: Point, bias: Bias): Point =
  for (row, slice) in self.data.toOpenArray.lineRanges:
    if target.row.int != row:
      continue

    let len = slice.b - slice.a + 1
    template bytes: untyped = self.data.toOpenArray[slice]

    var column = min(target.column.int, len)
    if column == 0 or column == len or (bytes[column - 1].int < 128 and bytes[column].int < 128):
      return Point(row: row.uint32, column: column.uint32)

    while true:
      if bytes.isCharBoundary(column):
        # todo: grapheme clusters
        break

      case bias
      of Left: column -= 1
      of Right: column += 1

    return Point(row: row.uint32, column: column.uint32)

  assert false

################### Rope ###################

type
  Rope* = object
    tree*: SumTree[Chunk]

  RopeCursorT*[D] = object
    rope: ptr Rope
    chunks: Cursor[Chunk, (D, int)]
    position: D

  RopeCursor* = object
    rope: ptr Rope
    chunks: Cursor[Chunk, int]
    offset: int

proc `=copy`*(a: var Rope, b: Rope) {.error.}
proc `=dup`*(a: Rope): Rope {.error.}

func clone*(self: Rope): Rope =
  Rope(tree: self.tree.clone())

func checkInvariants*(self: Rope) =
  self.tree.checkInvariants()

func bytes*(self: Rope): int =
  return self.tree.summary.bytes

func chars*(self: Rope): int =
  return self.tree.summary.len.int

func endPoint*(self: Rope): Point =
  return self.tree.summary.lines

func lines*(self: Rope): int =
  return self.tree.summary.lines.row.int + 1

func offsetToCount*(self: Rope, target: int): Count =
  if target >= self.tree.summary.bytes:
    return self.tree.summary.len
  var cursor = self.tree.initCursor (int, Count)
  discard cursor.seekForward(target, Left, ())
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].offsetToCount(overshoot)).get(0.Count)

func offsetToPoint*(self: Rope, target: int): Point =
  if target >= self.tree.summary.bytes:
    return self.tree.summary.lines
  var cursor = self.tree.initCursor (int, Point)
  discard cursor.seekForward(target, Left, ())
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].offsetToPoint(overshoot)).get(Point.default)

func offsetTo*(self: Rope, target: int, D: typedesc): D =
  if target >= self.tree.summary.bytes:
    return self.tree.summary.lines
  var cursor = self.tree.initCursor (int, D)
  discard cursor.seekForward(target, Left, ())
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].offsetTo(overshoot, D)).get(D.default)

func countToOffset*(self: Rope, target: Count): int =
  if target >= self.tree.summary.len:
    return self.tree.summary.bytes
  var cursor = self.tree.initCursor (Count, int)
  discard cursor.seekForward(target, Left, ())
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].countToOffset(overshoot)).get(0)

func countToPoint*(self: Rope, target: Count): Point =
  if target >= self.tree.summary.len:
    return self.tree.summary.lines
  var cursor = self.tree.initCursor (Count, Point)
  discard cursor.seekForward(target, Left, ())
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].countToPoint(overshoot)).get(Point.default)

func pointToOffset*(self: Rope, target: Point): int =
  if target >= self.tree.summary.lines:
    return self.tree.summary.bytes
  var cursor = self.tree.initCursor (Point, int)
  discard cursor.seekForward(target, Left, ())
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].pointToOffset(overshoot)).get(0)

func pointToCount*(self: Rope, target: Point): Count =
  if target >= self.tree.summary.lines:
    return self.tree.summary.len
  var cursor = self.tree.initCursor (Point, Count)
  discard cursor.seekForward(target, Left, ())
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].pointToCount(overshoot)).get(0.Count)

func toOffset*(self: Rope, target: Count): int =
  self.countToOffset(target)

func toOffset*(self: Rope, target: Point): int =
  self.pointToOffset(target)

func toOffsetT*[D](self: Rope, target: D): int =
  var endPos: D = D.default
  endPos.addSummary(self.tree.summary())
  if target >= endPos:
    return self.tree.summary.len
  var cursor = self.tree.initCursor (D, int)
  discard cursor.seekForward(target, Left, ())
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].toOffset(overshoot)).get(0)

func clipPoint*(self: Rope, target: Point, bias: Bias): Point =
  var cursor = self.tree.initCursor(Point)
  discard cursor.seekForward(target, Right, ())
  let chunk = cursor.item
  if chunk.isSome:
    var overshoot = target - cursor.startPos
    return cursor.startPos + chunk.get[].clipPoint(overshoot, bias)

  return self.endPoint

func new*(_: typedesc[Rope], value: openArray[char]): Rope =
  var chunks = newSeq[Chunk]()

  var i = 0
  while i < value.len:
    let last = value.runeStart(min(i + chunkBase, value.len))
    chunks.add Chunk(data: value.toOpenArray(i, last - 1).toArray(chunkBase))
    i = last

  result.tree = SumTree[Chunk].new(chunks)

func toRope*(value: openArray[char]): Rope =
  Rope.new(value)

func new*(_: typedesc[Rope], value: string = ""): Rope =
  Rope.new(value.toOpenArray(0, value.high))

func toRope*(value: string): Rope =
  Rope.new(value)

func `$`*(r: Rope): string =
  for chunk in r.tree.items(()):
    result.add chunk.data.toOpenArray.join("")

func add*(self: var Chunk, text: openArray[char]) =
  self.data.add text

func saturatingSub(a, b: int): int = max(a - b, 0)

func add*(self: var Rope, text: openArray[char]) =
  assert text.len >= 0
  if text.len == 0:
    return

  var textLen = text.len
  var textPtr: ptr UncheckedArray[char] = cast[ptr UncheckedArray[char]](text[0].addr)
  template text: untyped = textPtr.toOpenArray(0, textLen - 1)

  self.tree.updateLast cx=(), f=proc(chunk: var Chunk) =

    var splitIndex = if chunk.data.len + textLen <= chunkBase:
      textLen
    else:
      let index = min(saturatingSub(chunkBaseHalf, chunk.data.len), textLen)
      text.runeStart(min(index, textLen))

    if splitIndex == 0:
      return

    chunk.add text[0..<splitIndex]

    textPtr = cast[ptr UncheckedArray[char]](textPtr[splitIndex].addr)
    textLen -= splitIndex

    assert textLen >= 0

  if text.len == 0:
    return

  var chunks = newSeq[Chunk]()

  var i = 0
  while i < text.len:
    var last = min(i + chunkBase, text.len)
    while not text.isCharBoundary(last):
      dec last
    chunks.add Chunk(data: text[i..<last].toArray(chunkBase))
    i = last

  self.tree.extend(chunks, ())
  self.checkInvariants()

func add*(self: var Rope, text: string) =
  self.add text.toOpenArray(0, text.high)

func add*(self: var Rope, rope: sink Rope) =
  var chunks = rope.tree.initCursor
  chunks.next(())
  let chunk = chunks.item
  if chunk.isSome:
    let lastChunk = self.tree.last
    let lastChunkSmallEnough = lastChunk.mapIt(it[].data.len < chunkBase).get(false)
    if lastChunkSmallEnough or chunk.get[].data.len < chunkBase:
      self.add(chunk.get[].data.toOpenArray)
      chunks.next(())

  self.tree.append(chunks.suffix(()), ())
  self.checkInvariants()

func addFront*(self: var Rope, text: openArray[char]) =
  let suffix = self.move
  self = Rope.new(text)
  self.add suffix
  self.checkInvariants()

func addFront*(self: var Rope, text: string) =
  self.addFront text.toOpenArray(0, text.high)

func cursor*(self {.byref.}: Rope, offset: int = 0): RopeCursor
func seekForward*(self: var RopeCursor, target: int)
func slice*(self: var RopeCursor, target: int): Rope
func suffix*(self: var RopeCursor): Rope
func cursorT*[D](self {.byref.}: Rope, position: D): RopeCursorT[D]
func cursorT*(self {.byref.}: Rope, D: typedesc): RopeCursorT[D]
func seekForward*[D](self: var RopeCursorT[D], target: D)
func slice*[D](self: var RopeCursorT[D], target: D): Rope
func suffix*[D](self: var RopeCursorT[D]): Rope
func atEnd*(self: RopeCursor): bool = self.chunks.atEnd
func atEnd*[D](self: RopeCursorT[D]): bool = self.chunks.atEnd

func replace*(self: var Rope, slice: ByteRange, text: openArray[char]) =
  var newRope = Rope.new()
  var cursor = self.cursor()
  newRope.add(cursor.slice(slice.first))
  cursor.seekForward(slice.last)
  newRope.add(text)
  newRope.add(cursor.suffix())
  self = newRope
  self.checkInvariants()

func replace*(self: var Rope, slice: ByteRange, text: string) =
  self.replace(slice, text.toOpenArray(0, text.high))

func slice*(self: Rope, slice: ByteRange): Rope =
  var cursor = self.cursor()
  cursor.seekForward(slice.first)
  result = cursor.slice(slice.last)
  result.checkInvariants()

func slice*(self: Rope, first, last: int): Rope =
  self.slice((first, last))

func sliceRows*(self: Rope, slice: ByteRange): Rope =
  let startOffset = self.pointToOffset(Point.init(slice.first, 0))
  let endOffset = self.pointToOffset(Point.init(slice.last, 0))
  self.slice((startOffset, endOffset))

func sliceRows*(self: Rope, first, last: int): Rope =
  self.sliceRows((first, last))

func validateOffset*(self: Rope, offset: int, bias: Bias, debug = false): int =
  if offset < 0:
    return 0
  if offset >= self.bytes:
    return self.bytes

  var cursor = self.tree.initCursor(int)
  discard cursor.seekForward(offset, Left, ())
  var overshoot = offset - cursor.startPos
  let chunk = cursor.item
  if chunk.isSome:
    case bias
    of Left:
      while not chunk.get[].data.toOpenArray.isCharBoundary(overshoot):
        dec overshoot
    of Right:
      while not chunk.get[].data.toOpenArray.isCharBoundary(overshoot):
        inc overshoot

    assert overshoot >= 0
    return cursor.startPos + overshoot

  return offset

func cursor*(self {.byref.}: Rope, offset: int = 0): RopeCursor =
  result.rope = self.addr
  result.chunks = self.tree.initCursor(int)
  result.offset = offset
  discard result.chunks.seek(offset, Right, ())

func cursorT*[D](self {.byref.}: Rope, position: D): RopeCursorT[D] =
  result.rope = self.addr
  result.chunks = self.tree.initCursor((D, int))
  result.position = position
  discard result.chunks.seek(position, Right, ())

func cursorT*(self {.byref.}: Rope, D: typedesc): RopeCursorT[D] =
  result.rope = self.addr
  result.chunks = self.tree.initCursor((D, int))
  result.position = D.default

func seekForward*(self: var RopeCursor, target: int) =
  assert target >= self.offset
  discard self.chunks.seekForward(target, Right, ())
  self.offset = target

func seekForward*[D](self: var RopeCursorT[D], target: D) =
  assert target >= self.position
  discard self.chunks.seekForward(target, Right, ())
  self.position = target

func seekNextRune*[D](self: var RopeCursorT[D]) =
  bind runeSize

  if self.chunks.item.isSome:
    let chunk = self.chunks.item.get
    let startPos = self.chunks.startPos[0]
    var overshoot = chunk[].toOffset(self.position - startPos)
    if overshoot < chunk[].data.len:
      overshoot += chunk[].data.toOpenArray.runeSize(overshoot)

    let newOffsetRel = chunk[].offsetTo(overshoot, D)
    self.position = startPos + newOffsetRel

    if overshoot == chunk[].data.len:
      self.chunks.next(())

func currentRune*(self: RopeCursor): Rune =
  let item = self.chunks.item
  let relOffset = self.offset - self.chunks.startPos
  if item.isSome and relOffset < item.get.data.toOpenArray.len:
    return item.get.data.toOpenArray.runeAt(relOffset)
  return 0.Rune

func currentRune*(self: RopeCursorT): Rune =
  let item = self.chunks.item
  if item.isSome:
    let startPos = self.chunks.startPos[0]
    let relOffset = item.get[].toOffset(self.position - startPos)
    if relOffset < item.get.data.toOpenArray.len:
      return item.get.data.toOpenArray.runeAt(relOffset)
  return 0.Rune

func lineBounds*(self: Rope, line: int): (int, int) =
  if line >= self.lines:
    return (self.tree.summary.bytes, self.tree.summary.bytes)

  var cursor = self.tree.initCursor (Point, int)

  discard cursor.seekForward(Point.init(line, 0), Left, ())
  let firstOvershoot = Point.init(line, 0) - cursor.startPos[0]
  let firstStartPos = cursor.startPos
  let firstOffset = cursor.item().mapIt(it[].pointToOffset(firstOvershoot)).get(0)
  let first = firstStartPos[1] + firstOffset

  discard cursor.seekForward(Point.init(line + 1, 0), Left, ())
  let lastOvershoot = Point.init(line + 1, 0) - cursor.startPos[0]
  let lastStartPos = cursor.startPos
  let lastOffset = cursor.item().mapIt(it[].pointToOffset(lastOvershoot)).get(0)
  var last = lastStartPos[1] + lastOffset

  if line < self.lines - 1:
    # byte offset includes the \n, so -1
    last -= 1

  return (first, max(first, last))

func slice*(self: var RopeCursor, target: int): Rope =
  assert target >= self.offset, &"Can't slice before current offset: {target} before {self.offset}"

  result = Rope.new()
  let startChunk = self.chunks.item
  if startChunk.isSome:
    let startIndex = self.offset - self.chunks.startPos
    let endIndex = min(target, self.chunks.endPos(())) - self.chunks.startPos
    result.add(startChunk.get[].data.toOpenArray(startIndex, endIndex - 1))

  if target > self.chunks.endPos(()):
    self.chunks.next(())
    result.add Rope(tree: self.chunks.slice(target, Right, ()))
    let endChunk = self.chunks.item
    if endChunk.isSome:
      let endIndex = target - self.chunks.startPos
      result.add(endChunk.get[].data.toOpenArray(0, endIndex - 1))

  self.offset = target
  result.checkInvariants()

func slice*[D](self: var RopeCursorT[D], target: D): Rope =
  assert target >= self.position, &"Can't slice before current position: {target} before {self.position}"

  result = Rope.new()
  let startChunk = self.chunks.item
  if startChunk.isSome:
    let startIndex = self.position - self.chunks.startPos[0]
    let endIndex = min(target, self.chunks.endPos(())[0]) - self.chunks.startPos[0]
    result.add(startChunk.get[].data.toOpenArray(startIndex, endIndex - 1))

  if target > self.chunks.endPos(())[0]:
    self.chunks.next(())
    result.add Rope(tree: self.chunks.slice(target, Right, ()))
    let endChunk = self.chunks.item
    if endChunk.isSome:
      let endIndex = target - self.chunks.startPos[0]
      result.add(endChunk.get[].data.toOpenArray(0, endIndex - 1))

  self.position = target
  result.checkInvariants()

func summary*(self: var RopeCursor, D: typedesc, target: int): D =
  assert target >= self.offset

  result = D.default
  let startChunk = self.chunks.item
  if startChunk.isSome:
    let startIndex = self.offset - self.chunks.startPos
    let endIndex = min(target, self.chunks.endPos(())) - self.chunks.startPos
    result += D.fromSummary(TextSummary.init(startChunk.get[].data.toOpenArray(startIndex, endIndex - 1)), ())

  if target > self.chunks.endPos(()):
    self.chunks.next(())
    result += self.chunks.summary(D, target, Right, ())
    let endChunk = self.chunks.item
    if endChunk.isSome:
      let endIndex = target - self.chunks.startPos
      result += D.fromSummary(TextSummary.init(endChunk.get[].data.toOpenArray(0, endIndex - 1)), ())

  self.offset = target

func summary*[D](self: var RopeCursorT[D], D2: typedesc, target: D): D2 =
  assert target >= self.position

  result = D2.default
  let startChunk = self.chunks.item
  if startChunk.isSome:
    let startIndex = startChunk.get[].toOffset(self.position - self.chunks.startPos[0])
    let endIndex = startChunk.get[].toOffset(min(target, self.chunks.endPos(())[0]) - self.chunks.startPos[0])
    result += D2.fromSummary(TextSummary.init(startChunk.get[].data.toOpenArray(startIndex, endIndex - 1)), ())

  if target > self.chunks.endPos(())[0]:
    self.chunks.next(())
    result += self.chunks.summary(D2, target, Right, ())
    let endChunk = self.chunks.item
    if endChunk.isSome:
      let endIndex = endChunk.get[].toOffset(target - self.chunks.startPos[0])
      result += D2.fromSummary(TextSummary.init(endChunk.get[].data.toOpenArray(0, endIndex - 1)), ())

  self.position = target

func suffix*(self: var RopeCursor): Rope =
  self.slice(self.rope.tree.extent(int, ()))

func suffix*[D](self: var RopeCursorT[D]): Rope =
  self.slice(self.rope.tree.extent(D, ()))

func offset*(self: RopeCursor): int =
  self.offset

iterator runes*[D](self: Rope, start: D): Rune =
  bind runeSize
  bind runeAt

  var cursor = self.tree.initCursor(D)
  discard cursor.seekForward(start, Left, ())
  if cursor.item.isSome:
    var chunk = cursor.item.get
    var overshoot = chunk[].toOffset(start - cursor.startPos)
    while not cursor.atEnd:
      chunk = cursor.item.get
      while overshoot < chunk[].data.len:
        yield chunk[].data.toOpenArray.runeAt(overshoot)
        overshoot += chunk[].data.toOpenArray.runeSize(overshoot)
      if cursor.atEnd:
        break
      cursor.next(())
      overshoot = 0

iterator runes*(self: Rope): Rune =
  for r in self.runes(0):
    yield r
