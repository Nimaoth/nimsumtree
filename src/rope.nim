import std/[unittest, enumerate, strformat, sugar, os]
import nimsumtree/[sumtree]
import uni

# const chunkBase* = 16
# const treeBase = 2
const chunkBase* = 256
const treeBase* = 12

when chunkBase < 4:
  {.error: "chunkBase must be >= 4 because utf-8 characters can be encoded with " &
    "up to four bytes".}

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
proc `+`*(a: Count, b: Count): Count {.borrow.}
proc `-`*(a: Count, b: Count): Count {.borrow.}
proc `==`*(a: Count, b: Count): bool {.borrow.}
proc `<`*(a: Count, b: Count): bool {.borrow.}
proc `<=`*(a: Count, b: Count): bool {.borrow.}

func `$`*(a: Count32): string {.borrow.}
func `+=`*(a: var Count32, b: Count32) {.borrow.}
proc `+`*(a: Count32, b: Count32): Count32 {.borrow.}
proc `==`*(a: Count32, b: Count32): bool {.borrow.}
proc `<`*(a: Count32, b: Count32): bool {.borrow.}
proc `<=`*(a: Count32, b: Count32): bool {.borrow.}

proc `$`*(r: Chunk): string =
  result = "["
  result.add r.data.toOpenArray.join("")
  result.add "]"

proc init*(_: typedesc[Point], row: int, column: int): Point =
  Point(row: row.uint32, column: column.uint32)

func clone*(a: TextSummary): TextSummary = a

func addSummary*(a: var Count, b: Count) = a = (a.int + b.int).Count
func clone*(a: Count): Count = a
func cmp*(a: Count, b: Count): int = cmp(a.int, b.int)
func addSummary*(a: var Count, b: TextSummary) = a += b.len
func fromSummary*(_: typedesc[Count], a: TextSummary): Count = a.len

func addSummary*(a: var int, b: int) = a = a + b
func `+=`*(a: var int, b: int) = a = a + b
func clone*(a: int): int = a
func addSummary*(a: var int, b: TextSummary) = a += b.bytes
func `+=`*(a: var int, b: TextSummary) = a += b.bytes
func fromSummary*(_: typedesc[int], a: TextSummary): int = a.bytes

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

func addSummary*(a: var Point, b: Point) = a += b
func clone*(a: Point): Point = a
func cmp*(a: Point, b: Point): int =
  result = cmp(a.row, b.row)
  if result == 0:
    result = cmp(a.column, b.column)

func addSummary*(a: var Point, b: TextSummary) = a += b.lines
func fromSummary*(_: typedesc[Point], a: TextSummary): Point = a.lines

func cmp*(a: Count, b: TextSummary): int = cmp(a, b.len)
func cmp*(a: Point, b: TextSummary): int = cmp(a, b.lines)

func addSummary*[A, B](a: var (A, B), b: (A, B)) =
  a[0] += b[0]
  a[1] += b[1]

func clone*[A, B](a: (A, B)): (A, B) = (a[0].clone(), a[1].clone())
func cmp*[A, B](a: A, b: (A, B)): int = cmp(a, b[0])

func addSummary*[A, B](a: var (A, B), b: TextSummary) =
  a[0] += A.fromSummary(b)
  a[1] += B.fromSummary(b)

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

func addSummary*(a: var TextSummary, b: TextSummary) =
  a += b

func fromSummary*(_: typedesc[TextSummary], a: TextSummary): TextSummary = a

################### Chunk ###################

proc clone*(a: Chunk): Chunk = Chunk(data: a.data)
proc toChunk*(text: string): Chunk = Chunk(data: text.toOpenArray(0, text.high).toArray(chunkBase))
proc toChunk*(text: openArray[char]): Chunk = Chunk(data: text.toArray(chunkBase))

proc init*(_: typedesc[TextSummary], x: openArray[char]): TextSummary =
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

proc init*(_: typedesc[TextSummary], x: string): TextSummary =
  TextSummary.init(x.toOpenArray(0, x.high))

proc summary*(x: Chunk): TextSummary =
  TextSummary.init(x.data.toOpenArray)

proc offsetToCount*(self: Chunk, target: int): Count =
  var offset = 0
  for r in self.data.toOpenArray.runes:
    if offset >= target:
      break
    offset += r.size
    result += 1.Count

proc countToOffset*(self: Chunk, target: Count): int =
  var offset = 0.Count
  for r in self.data.toOpenArray.runes:
    if offset >= target:
      break
    result += r.size
    offset += 1.Count

proc offsetToPoint*(self: Chunk, target: int): Point =
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

proc countToPoint*(self: Chunk, target: Count): Point =
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

proc pointToOffset*(self: Chunk, target: Point): int =
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

proc pointToCount*(self: Chunk, target: Point): Count =
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

proc clipPoint*(self: Chunk, target: Point, bias: Bias): Point =
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
    tree*: SumTree[Chunk, treeBase]

  RopeCursor* = object
    rope: ptr Rope
    chunks: Cursor[Chunk, int, treeBase]
    offset: int

proc `=copy`*(a: var Rope, b: Rope) {.error.}
proc `=dup`*(a: Rope): Rope {.error.}

proc checkInvariants*(self: Rope) = discard

func bytes*(self: Rope): int =
  return self.tree.summary.bytes

func chars*(self: Rope): int =
  return self.tree.summary.len.int

func lines*(self: Rope): Point =
  return self.tree.summary.lines

proc offsetToCount*(self: Rope, target: int): Count =
  if target >= self.tree.summary.bytes:
    return self.tree.summary.len
  var cursor = self.tree.initCursor (int, Count)
  discard cursor.seekForward(target, Left)
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].offsetToCount(overshoot)).get(0.Count)

proc offsetToPoint*(self: Rope, target: int): Point =
  if target >= self.tree.summary.bytes:
    return self.tree.summary.lines
  var cursor = self.tree.initCursor (int, Point)
  discard cursor.seekForward(target, Left)
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].offsetToPoint(overshoot)).get(Point.default)

proc countToOffset*(self: Rope, target: Count): int =
  if target >= self.tree.summary.len:
    return self.tree.summary.bytes
  var cursor = self.tree.initCursor (Count, int)
  discard cursor.seekForward(target, Left)
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].countToOffset(overshoot)).get(0)

proc countToPoint*(self: Rope, target: Count): Point =
  if target >= self.tree.summary.len:
    return self.tree.summary.lines
  var cursor = self.tree.initCursor (Count, Point)
  discard cursor.seekForward(target, Left)
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].countToPoint(overshoot)).get(Point.default)

proc pointToOffset*(self: Rope, target: Point): int =
  if target >= self.tree.summary.lines:
    return self.tree.summary.bytes
  var cursor = self.tree.initCursor (Point, int)
  discard cursor.seekForward(target, Left)
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].pointToOffset(overshoot)).get(0)

proc pointToCount*(self: Rope, target: Point): Count =
  if target >= self.tree.summary.lines:
    return self.tree.summary.len
  var cursor = self.tree.initCursor (Point, Count)
  discard cursor.seekForward(target, Left)
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].pointToCount(overshoot)).get(0.Count)

proc new*(_: typedesc[Rope], value: openArray[char]): Rope =
  var chunks = newSeq[Chunk]()

  var i = 0
  while i < value.len:
    let last = value.runeStart(min(i + chunkBase, value.len))
    chunks.add Chunk(data: value.toOpenArray(i, last - 1).toArray(chunkBase))
    i = last

  result.tree = SumTree[Chunk, treeBase].new(chunks)

proc toRope*(value: openArray[char]): Rope =
  Rope.new(value)

proc new*(_: typedesc[Rope], value: string = ""): Rope =
  Rope.new(value.toOpenArray(0, value.high))

proc toRope*(value: string): Rope =
  Rope.new(value)

proc `$`*(r: Rope): string =
  for chunk in r.tree:
    result.add chunk.data.toOpenArray.join("")

proc add*(self: var Chunk, text: openArray[char]) =
  self.data.add text

proc add*(self: var Rope, text: openArray[char]) =
  if text.len == 0:
    return

  var textLen = text.len
  var textPtr: ptr UncheckedArray[char] = cast[ptr UncheckedArray[char]](text[0].addr)
  template text: untyped = textPtr.toOpenArray(0, textLen - 1)

  self.tree.updateLast proc(chunk: var Chunk) =
    let splitIndex = if chunk.data.len + textLen <= chunkBase:
      textLen
    else:
      # todo: saturatingSub(chunkBase, chunk.data.len)
      text.runeStart(min(chunkBase - chunk.data.len, textLen))

    if splitIndex == 0:
      return

    chunk.add text[0..<splitIndex]

    if splitIndex < text.len:
      textPtr = cast[ptr UncheckedArray[char]](text[splitIndex].addr)
    textLen -= splitIndex

    assert textLen >= 0

  if text.len == 0:
    return

  var chunks = newSeq[Chunk]()

  var i = 0
  while i < text.len:
    let last = min(i + chunkBase, text.len)
    chunks.add Chunk(data: text[i..<last].toArray(chunkBase))
    i = last

  self.tree.extend(chunks)

proc add*(self: var Rope, text: string) =
  self.add text.toOpenArray(0, text.high)

proc add*(self: var Rope, rope: sink Rope) =
  var chunks = rope.tree.initCursor
  chunks.next()
  let chunk = chunks.item
  if chunk.isSome:
    let lastChunk = self.tree.last
    let lastChunkSmallEnough = lastChunk.mapIt(it[].data.len < chunkBase).get(false)
    if lastChunkSmallEnough or chunk.get[].data.len < chunkBase:
      self.add(chunk.get[].data.toOpenArray)
      chunks.next()

  self.tree.append(chunks.suffix())
  self.checkInvariants()

proc addFront*(self: var Rope, text: openArray[char]) =
  let suffix = self.move
  self = Rope.new(text)
  self.add suffix

proc addFront*(self: var Rope, text: string) =
  self.addFront text.toOpenArray(0, text.high)

proc cursor*(self {.byref.}: Rope, offset: int = 0): RopeCursor
proc seekForward*(self: var RopeCursor, target: int)
proc slice*(self: var RopeCursor, target: int): Rope
proc suffix*(self: var RopeCursor): Rope

proc replace*(self: var Rope, slice: ByteRange, text: openArray[char]) =
  var newRope = Rope.new()
  var cursor = self.cursor()
  newRope.add(cursor.slice(slice.first))
  cursor.seekForward(slice.last)
  newRope.add(text)
  newRope.add(cursor.suffix())
  self = newRope

proc replace*(self: var Rope, slice: ByteRange, text: string) =
  self.replace(slice, text.toOpenArray(0, text.high))

proc slice*(self: Rope, slice: ByteRange): Rope =
  var cursor = self.cursor()
  cursor.seekForward(slice.first)
  cursor.slice(slice.last)

proc slice*(self: Rope, first, last: int): Rope =
  self.slice((first, last))

proc sliceRows*(self: Rope, slice: ByteRange): Rope =
  let startOffset = self.pointToOffset(Point.init(slice.first, 0))
  let endOffset = self.pointToOffset(Point.init(slice.last, 0))
  self.slice((startOffset, endOffset))

proc sliceRows*(self: Rope, first, last: int): Rope =
  self.sliceRows((first, last))

proc cursor*(self {.byref.}: Rope, offset: int = 0): RopeCursor =
  result.rope = self.addr
  result.chunks = self.tree.initCursor int
  result.offset = offset
  discard result.chunks.seek(offset, Right)

proc seekForward*(self: var RopeCursor, target: int) =
  assert target >= self.offset
  discard self.chunks.seekForward(target, Right)
  self.offset = target

proc slice*(self: var RopeCursor, target: int): Rope =
  assert target >= self.offset

  result = Rope.new()
  let startChunk = self.chunks.item
  if startChunk.isSome:
    let startIndex = self.offset - self.chunks.startPos
    let endIndex = min(target, self.chunks.endPos) - self.chunks.startPos
    result.add(startChunk.get[].data.toOpenArray(startIndex, endIndex - 1))

  if target > self.chunks.endPos:
    self.chunks.next()
    result.add Rope(tree: self.chunks.slice(target, Right))
    let endChunk = self.chunks.item
    if endChunk.isSome:
      let endIndex = target - self.chunks.startPos
      result.add(endChunk.get[].data.toOpenArray(0, endIndex - 1))

  self.offset = target

proc summary*(self: var RopeCursor, D: typedesc[Dimension], target: int): D =
  assert target >= self.offset

  result = D.default
  let startChunk = self.chunks.item
  if startChunk.isSome:
    let startIndex = self.offset - self.chunks.startPos
    let endIndex = min(target, self.chunks.endPos) - self.chunks.startPos
    result += D.fromSummary(TextSummary.init(startChunk.get[].data.toOpenArray(0, endIndex - 1)))

  if target > self.chunks.endPos:
    self.chunks.next()
    result += self.chunks.summary(D, target, Right)
    let endChunk = self.chunks.item
    if endChunk.isSome:
      let endIndex = target - self.chunks.startPos
      result += D.fromSummary(TextSummary.init(endChunk.get[].data.toOpenArray(0, endIndex - 1)))

  self.offset = target

proc suffix*(self: var RopeCursor): Rope =
  self.slice(self.rope.tree.extent(int))

proc offset*(self: RopeCursor): int =
  self.offset
