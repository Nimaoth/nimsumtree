import std/[strformat, os, strformat, options]
import nimsumtree/[sumtree, static_array]
import uni

export static_array, arc

{.push gcsafe.}
{.push raises: [].}

var ropeDebugLog* = true

const ropeChunkBase {.intdefine.} = 0
when ropeChunkBase > 0:
  const chunkBase* = ropeChunkBase

elif defined(testing):
  const chunkBase* = 16

else:
  const chunkBase* = 128

static:
  echo &"Rope chunkBase = {chunkBase}"

when chunkBase < 4:
  {.error: "chunkBase must be >= 4 because utf-8 characters can be encoded with " &
    "up to four bytes".}

const chunkBaseHalf* = chunkBase div 2

type
  Count* = distinct int       ## Number of utf8 codepoints
  Count32* = distinct uint32  ## Number of utf8 codepoints

  Point* = object
    row*: uint32    ## Number of newlines
    column*: uint32 ## In bytes

  PointDiff* = object
    a: Point
    b: Point

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

  Range*[T] = object
    a*: T ## Inclusive
    b*: T ## Exclusive

  ChunkSlice* = object
    chunk*: ptr Chunk
    range*: Range[int]

proc `=copy`*(a: var Chunk, b: Chunk) {.error.}
proc `=dup`*(a: Chunk): Chunk {.error.}

func `$`*(a: Count): string {.borrow.}
func `+=`*(a: var Count, b: Count) {.borrow.}
func `-=`*(a: var Count, b: Count) {.borrow.}
func `+`*(a: Count, b: Count): Count {.borrow.}
func `-`*(a: Count, b: Count): Count {.borrow.}
func `==`*(a: Count, b: Count): bool {.borrow.}
func `<`*(a: Count, b: Count): bool {.borrow.}
func `<=`*(a: Count, b: Count): bool {.borrow.}

func `$`*(a: Count32): string {.borrow.}
func `+=`*(a: var Count32, b: Count32) {.borrow.}
func `-=`*(a: var Count32, b: Count32) {.borrow.}
func `+`*(a: Count32, b: Count32): Count32 {.borrow.}
func `==`*(a: Count32, b: Count32): bool {.borrow.}
func `<`*(a: Count32, b: Count32): bool {.borrow.}
func `<=`*(a: Count32, b: Count32): bool {.borrow.}

func `...`*[T](a, b: T): Range[T] = Range[T](a: a, b: b)
func len*[T](r: Range[T]): T = r.b - r.a

template chars*(self: Chunk): openArray[char] = self.data.toOpenArray
template chars*(self: ptr Chunk): openArray[char] = self.data.toOpenArray
template chars*(self: Chunk, first, last: int): openArray[char] = self.data.toOpenArray(first, last)
template chars*(self: ptr Chunk, first, last: int): openArray[char] = self.data.toOpenArray(first, last)
template chars*(self: ChunkSlice): openArray[char] = self.chunk[].data.toOpenArray[self.range.a..<self.range.b]
template chars*(self: ptr ChunkSlice): openArray[char] = self.chunk[].data.toOpenArray[self.range.a..<self.range.b]
func startPtr*(self: ChunkSlice): ptr char = self.chunk[].data[0].addr

func `$`*(r: Chunk): string {.inline.} =
  result = "["
  result.add r.chars.join("")
  result.add "]"

func init*(_: typedesc[Point], row, column: int): Point {.inline.} =
  Point(row: row.uint32, column: column.uint32)

func init*(_: typedesc[Point], row, column: uint32): Point {.inline.} =
  Point(row: row, column: column)

func clone*(a: TextSummary): TextSummary {.inline.} = a

func addSummary*[C](a: var Count, b: Count, cx: C) {.inline.} = a = (a.int + b.int).Count
func clone*(a: Count): Count {.inline.} = a
func cmp*[C](a: Count, b: Count, cx: C): int {.inline.} = cmp(a.int, b.int)
func addSummary*[C](a: var Count, b: TextSummary, cx: C) {.inline.} = a += b.len
func fromSummary*[C](_: typedesc[Count], a: TextSummary, cx: C): Count {.inline.} = a.len

func addSummary*[C](a: var int, b: int, cx: C) {.inline.} = a = a + b
func `+=`*(a: var int, b: int) {.inline.} = a = a + b
func clone*(a: int): int {.inline.} = a
func cmp*[C](a: int, b: int, cx: C): int {.inline.} = cmp(a, b)
func addSummary*[C](a: var int, b: TextSummary, cx: C) {.inline.} = a += b.bytes
func `+=`*(a: var int, b: TextSummary) {.inline.} = a += b.bytes
func fromSummary*[C](_: typedesc[int], a: TextSummary, cx: C): int {.inline.} = a.bytes

func `<`*(a: Point, b: Point): bool {.inline.} =
  if a.row == b.row:
    return a.column < b.column
  return a.row < b.row

func `<=`*(a: Point, b: Point): bool {.inline.} =
  if a.row == b.row:
    return a.column <= b.column
  return a.row <= b.row

func `+=`*(a: var Point, b: Point) {.inline.} =
  a.row += b.row
  if b.row == 0:
    a.column += b.column
  else:
    a.column = b.column

func `+`*(a: Point, b: Point): Point {.inline.} =
  result = a
  result += b

func dec*(a: var Point): Point {.inline.} =
  if a.column > 0:
    dec a.column
  else:
    assert a.row > 0
    dec a.row
    a.column = uint32.high

func pred*(a: Point): Point {.inline.} =
  if a.column > 0:
    Point(row: a.row, column: a.column - 1)
  else:
    assert a.row > 0
    Point(row: a.row - 1, column: uint32.high)

func rows*(d: PointDiff): int {.inline.} = d.a.row.int - d.b.row.int
func columns*(d: PointDiff): int {.inline.} = d.a.column.int - d.b.column.int

converter toPoint*(diff: PointDiff): Point =
  assert diff.a >= diff.b
  if diff.rows == 0:
    Point(row: 0, column: diff.columns.uint32)
  else:
    Point(row: diff.rows.uint32, column: diff.a.column)

func `+`*(point: Point, diff: PointDiff): Point =
  # diff represenents diff.a - diff.b

  if diff.a.row >= diff.b.row: # Positive diff
    if point < diff.b:
      return point

    if point.row > diff.b.row:
      return Point(row: (point.row.int + diff.rows).uint32, column: point.column)

    result.row = (point.row.int + diff.rows).uint32

    if diff.rows == 0:
      result.column = (point.column.int + diff.columns).uint32
    elif diff.rows > 0:
      result.column = diff.a.column + (point.column - diff.b.column)

  else: # Negative diff
    if point <= diff.a:
      return point

    if point <= diff.b:
      return diff.a

    if point.row > diff.b.row:
      return Point(row: (point.row.int + diff.rows).uint32, column: point.column)

    result.row = (point.row.int + diff.rows).uint32

    if diff.rows == 0:
      result.column = (point.column.int + diff.columns).uint32
    elif diff.rows > 0:
      result.column = diff.a.column + (point.column - diff.b.column)

func `-`*(a: Point, b: Point): PointDiff = PointDiff(a: a, b: b)

static:
  for x in 0..2:
    for y in 0..2:
      let a = Point.init(1, 1)
      let b = Point.init(x, y)
      let d = a - b
      let a2 = b + d
      assert a == a2

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

func addSummary*[A, B, C](a: var (A, B), b: (A, B), cx: C) =
  a[0].addSummary(b[0], cx)
  a[1].addSummary(b[1], cx)

func clone*[A, B](a: (A, B)): (A, B) = (a[0].clone(), a[1].clone())
func cmp*[C](a: int, b: (int, int), cx: C): int = cmp(a, b[0], cx)
func cmp*[A, B, C](a: A, b: (A, B), cx: C): int = cmp(a, b[0], cx)
func cmp*[A, B, C](a: B, b: (A, B), cx: C): int = cmp(a, b[1], cx)

func addSummary*[A, B, C](a: var (A, B), b: TextSummary, cx: C) =
  a[0].addSummary(A.fromSummary(b, cx), cx)
  a[1].addSummary(B.fromSummary(b, cx), cx)

func fromSummary*[A, B, C](D: typedesc[(A, B)], b: TextSummary, cx: C): (A, B) =
  result[0].addSummary(A.fromSummary(b, cx), cx)
  result[1].addSummary(B.fromSummary(b, cx), cx)

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

func init*(_: typedesc[TextSummary], r: Rune): TextSummary =
  result.bytes = r.size
  result.len = 1.Count

  if r == '\n'.Rune:
    result.lines = Point(row: 1, column: 0)
    result.lastLineChars = 0.Count32
  else:
    result.lines.column = r.size.uint32
    result.lastLineChars = 1.Count32

  if result.lines.row == 0:
    result.firstLineChars = result.lastLineChars

  if result.lastLineChars > result.longestRowChars:
    result.longestRow = result.lines.row
    result.longestRowChars = result.lastLineChars

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
  TextSummary.init(x.chars)

func offsetToCount*(self: Chunk, target: int): Count =
  var offset = 0
  for r in self.chars.runes:
    if offset >= target:
      break
    offset += r.size
    result += 1.Count

func countToOffset*(self: Chunk, target: Count): int =
  var offset = 0.Count
  for r in self.chars.runes:
    if offset >= target:
      break
    result += r.size
    offset += 1.Count

func offsetToPoint*(self: Chunk, target: int): Point =
  var offset = 0
  for r in self.chars.runes:
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
  for r in self.chars.runes:
    if offset >= target:
      break
    if r == '\n'.Rune:
      result.row += 1
      result.column = 0
    else:
      result.column += r.size.uint32
    offset += 1.Count

func pointToOffset*(self: Chunk, target: Point): int =
  var point = Point()
  for r in self.chars.runes:
    if point >= target:
      break

    if r == '\n'.Rune:
      if point.row >= target.row:
        break

      point.row += 1
      point.column = 0
    else:
      point.column += r.size.uint32

    result += r.size

func pointToOffset*(self: Chunk, target: Point, bias: Bias): int =
  var point = Point()
  for r in self.chars.runes:
    if point >= target:
      if bias == Right:
        return

      break

    if r == '\n'.Rune:
      if point.row >= target.row:
        break

      point.row += 1
      point.column = 0
    else:
      point.column += r.size.uint32

    if bias == Left and point > target:
      return

    result += r.size

func pointToCount*(self: Chunk, target: Point): Count =
  var point = Point()
  for r in self.chars.runes:
    if point >= target:
      break

    if r == '\n'.Rune:
      point.row += 1
      point.column = 0

      if point.row > target.row:
        break
    else:
      point.column += r.size.uint32

    inc result

func toOffset*(self: Chunk, target: int): int =
  target

func toOffset*(self: Chunk, target: int, bias: Bias): int =
  # todo: align to utf8 boundary according to bias
  target

func toOffset*(self: Chunk, target: Count): int =
  self.countToOffset(target)

func toOffset*(self: Chunk, target: Count, bias: Bias): int =
  self.countToOffset(target)

func toOffset*(self: Chunk, target: Point): int =
  self.pointToOffset(target)

func toOffset*(self: Chunk, target: Point, bias: Bias): int =
  self.pointToOffset(target, bias)

func offsetTo*(self: Chunk, target: int, D: typedesc[int]): int =
  target

func offsetTo*(self: Chunk, target: int, D: typedesc[Point]): Point =
  self.offsetToPoint(target)

func offsetTo*(self: Chunk, target: int, D: typedesc[Count]): Count =
  self.offsetToCount(target)

func convert*(self: Chunk, target: int, D2: typedesc[Point]): Point =
  self.offsetToPoint(target)

func convert*(self: Chunk, target: int, D2: typedesc[Count]): Count =
  self.offsetToCount(target)

func convert*(self: Chunk, target: int, D2: typedesc[int]): int =
  target

func convert*(self: Chunk, target: Count, D2: typedesc[Point]): Point =
  self.countToPoint(target)

func convert*(self: Chunk, target: Count, D2: typedesc[Count]): Count =
  target

func convert*(self: Chunk, target: Count, D2: typedesc[int]): int =
  self.countToOffset(target)

func convert*(self: Chunk, target: Point, D2: typedesc[Point]): Point =
  target

func convert*(self: Chunk, target: Point, D2: typedesc[Count]): Count =
  self.pointToCount(target)

func convert*(self: Chunk, target: Point, D2: typedesc[int]): int =
  self.pointToOffset(target)

func clipPoint*(self: Chunk, target: Point, bias: Bias): Point =
  for (row, slice) in self.chars.lineRanges:
    if target.row.int != row:
      continue

    let len = slice.b - slice.a + 1
    template bytes: untyped = self.chars[slice]

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
    rope {.cursor.}: Rope
    chunks: Cursor[Chunk, (D, int)]
    position: D
    offset: Option[int]

  RopeCursor* = object
    rope* {.cursor.}: Rope
    chunks*: Cursor[Chunk, int]
    offset: int

  RopeSlice*[D] = object
    rope*: Rope
    summary*: TextSummary
    range*: Range[D]

  RopeSliceCursor*[D, D2] = object
    ropeSlice*: RopeSlice[D2]
    cursor*: RopeCursorT[D]
    range*: Range[D]

proc `=copy`*(a: var Rope, b: Rope) {.error.}
proc `=dup`*(a: Rope): Rope {.error.}

func clone*(self: Rope): Rope {.inline.} =
  Rope(tree: self.tree.clone())

proc `=copy`*[D](a: var RopeSlice[D], b: RopeSlice[D]) {.error.}
proc `=dup`*[D](a: RopeSlice[D]): RopeSlice[D] {.error.}

func clone*[D](self: RopeSlice[D]): RopeSlice[D] {.inline.} =
  RopeSlice[D](rope: self.rope.clone(), summary: self.summary, range: self.range)

func clone*(a: RopeCursor): RopeCursor =
  result.rope = a.rope
  result.chunks = a.chunks.clone()
  result.offset = a.offset

func clone*[D](a: RopeCursorT[D]): RopeCursorT[D] =
  result.rope = a.rope
  result.chunks = a.chunks.clone()
  result.position = a.position
  result.offset = a.offset

func clone*[D, D2](a: RopeSliceCursor[D, D2]): RopeSliceCursor[D, D2] =
  result.ropeSlice = a.ropeSlice.clone()
  result.chunks = a.chunks.clone()
  result.range = a.range

func checkInvariants*(self: Rope) {.inline.} =
  self.tree.checkInvariants()

func len*(self: Rope): int {.inline.} =
  ## Length of the rope in bytes
  return self.tree.summary.bytes

func bytes*(self: Rope): int {.inline.} =
  return self.tree.summary.bytes

func chars*(self: Rope): int {.inline.} =
  return self.tree.summary.len.int

func endPoint*(self: Rope): Point {.inline.} =
  return self.tree.summary.lines

func lines*(self: Rope): int {.inline.} =
  return self.tree.summary.lines.row.int + 1

func len*[D](self: RopeSlice[D]): int {.inline.} =
  ## Length of the rope slice in bytes
  return self.summary.bytes

func bytes*[D](self: RopeSlice[D]): int {.inline.} =
  return self.summary.bytes

func runeLen*[D](self: RopeSlice[D]): int {.inline.} =
  return self.summary.len.int

func endPoint*[D](self: RopeSlice[D]): Point {.inline.} =
  return self.summary.lines

func lines*[D](self: RopeSlice[D]): int {.inline.} =
  return self.summary.lines.row.int + 1

func offsetToCount*(self: Rope, target: int): Count {.inline.} =
  if target >= self.tree.summary.bytes:
    return self.tree.summary.len
  var cursor = self.tree.initCursor (int, Count)
  discard cursor.seekForward(target, Left, ())
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].offsetToCount(overshoot)).get(0.Count)

func offsetToPoint*(self: Rope, target: int): Point {.inline.} =
  if target >= self.tree.summary.bytes:
    return self.tree.summary.lines
  var cursor = self.tree.initCursor (int, Point)
  discard cursor.seekForward(target, Left, ())
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].offsetToPoint(overshoot)).get(Point.default)

func offsetTo*(self: Rope, target: int, D: typedesc): D {.inline.} =
  if target >= self.tree.summary.bytes:
    return self.tree.summary.lines
  var cursor = self.tree.initCursor (int, D)
  discard cursor.seekForward(target, Left, ())
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].offsetTo(overshoot, D)).get(D.default)

func countToOffset*(self: Rope, target: Count): int {.inline.} =
  assert not self.tree.isNil
  if target >= self.tree.summary.len:
    return self.tree.summary.bytes
  var cursor = self.tree.initCursor (Count, int)
  discard cursor.seekForward(target, Left, ())
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].countToOffset(overshoot)).get(0)

func countToPoint*(self: Rope, target: Count): Point {.inline.} =
  if target >= self.tree.summary.len:
    return self.tree.summary.lines
  var cursor = self.tree.initCursor (Count, Point)
  discard cursor.seekForward(target, Left, ())
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].countToPoint(overshoot)).get(Point.default)

func pointToOffset*(self: Rope, target: Point): int {.inline.} =
  if target >= self.tree.summary.lines:
    return self.tree.summary.bytes
  var cursor = self.tree.initCursor (Point, int)
  discard cursor.seekForward(target, Left, ())
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].pointToOffset(overshoot)).get(0)

func pointToOffset*(self: Rope, target: Point, bias: Bias): int {.inline.} =
  if target >= self.tree.summary.lines:
    return self.tree.summary.bytes
  var cursor = self.tree.initCursor (Point, int)
  discard cursor.seekForward(target, Left, ())
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].pointToOffset(overshoot, bias)).get(0)

func pointToCount*(self: Rope, target: Point): Count {.inline.} =
  if target >= self.tree.summary.lines:
    return self.tree.summary.len
  var cursor = self.tree.initCursor (Point, Count)
  discard cursor.seekForward(target, Left, ())
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].pointToCount(overshoot)).get(0.Count)

func toOffset*(self: Rope, target: int): int {.inline.} =
  target

func toOffset*(self: Rope, target: Count): int {.inline.} =
  assert not self.tree.isNil
  self.countToOffset(target)

func toOffset*(self: Rope, target: Point): int {.inline.} =
  self.pointToOffset(target)

func toOffset*(self: Rope, target: Point, bias: Bias): int {.inline.} =
  self.pointToOffset(target, bias)

func toOffsetT*[D](self: Rope, target: D): int {.inline.} =
  var endPos: D = D.default
  endPos.addSummary(self.tree.summary())
  if target >= endPos:
    return self.tree.summary.len
  var cursor = self.tree.initCursor (D, int)
  discard cursor.seekForward(target, Left, ())
  let overshoot = target - cursor.startPos[0]
  return cursor.startPos[1] + cursor.item().mapIt(it[].toOffset(overshoot)).get(0)

func convert*(self: Rope, target: int, D2: typedesc[Point]): Point =
  self.offsetToPoint(target)

func convert*(self: Rope, target: int, D2: typedesc[Count]): Count =
  self.offsetToCount(target)

func convert*(self: Rope, target: int, D2: typedesc[int]): int =
  target

func convert*(self: Rope, target: Count, D2: typedesc[Point]): Point =
  self.countToPoint(target)

func convert*(self: Rope, target: Count, D2: typedesc[Count]): Count =
  target

func convert*(self: Rope, target: Count, D2: typedesc[int]): int =
  self.countToOffset(target)

func convert*(self: Rope, target: Point, D2: typedesc[Point]): Point =
  target

func convert*(self: Rope, target: Point, D2: typedesc[Count]): Count =
  self.pointToCount(target)

func convert*(self: Rope, target: Point, D2: typedesc[int]): int =
  self.pointToOffset(target)

func convert*[D1](self: RopeSlice, target: D1, D2: typedesc): D2 =
  # todo: this can probably be more efficient
  return self.rope.convert(self.rope.convert(self.range.a, D1) + target, D2) - self.rope.convert(self.range.a, D2)

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

  result.tree = SumTree[Chunk].new(chunks, ())

proc new*(_: typedesc[Rope], producer: proc(buffer: var string, bytesToRead: int) {.gcsafe, raises: [].}, errorIndex: var int): Option[Rope] =
  var buffer = ""
  var invalidUtf8Index = -1
  var offset = 0
  proc fillItems(items: var ItemArray[Chunk]) {.gcsafe, raises: [].} =
    if invalidUtf8Index >= 0:
      return

    for i in 0..<treeBase:
      assert buffer.len < chunkBase

      producer(buffer, chunkBase - buffer.len + 4) # + 4 because we need to check for splitting utf8 code points later
      assert buffer.len <= chunkBase + 4

      if buffer.len == 0:
        return

      let targetLast = min(chunkBase, buffer.len)
      let last = buffer.runeStart(targetLast)
      if last < targetLast - 3: # UTF codepoints are at most 4 bytes, so the start index is at most 3 bytes to the left
        invalidUtf8Index = max(last, 0)
      elif invalidUtf8Index == -1:
        invalidUtf8Index = buffer.toOpenArray(0, last - 1).validateUtf8
      if invalidUtf8Index >= 0:
        return

      items.add Chunk(data: buffer.toOpenArray(0, last - 1).toArray(chunkBase))
      offset += last

      if last < buffer.len:
        buffer.delete(0..<last.int)
      else:
        buffer.setLen(0)

  var tree = SumTree[Chunk].new(fillItems, ())
  if invalidUtf8Index != -1:
    errorIndex = offset + invalidUtf8Index
    return Rope.none
  else:
    errorIndex = -1
    return Rope(tree: tree.move).some

func toRope*(value: openArray[char]): Rope {.inline.} =
  Rope.new(value)

func new*(_: typedesc[Rope], value: string = ""): Rope {.inline.} =
  Rope.new(value.toOpenArray(0, value.high))

func toRope*(value: string): Rope {.inline.} =
  Rope.new(value)

func `$`*(r: Rope): string {.inline.} =
  result = newStringOfCap(r.len)
  for chunk in r.tree.items(()):
    for c in chunk.chars:
      result.add c

iterator iterateChunks*[D](self: RopeSlice[D]): ChunkSlice =
  assert not self.rope.tree.isNil
  let range = self.rope.toOffset(self.range.a)...self.rope.toOffset(self.range.b)
  var c = self.rope.tree.initCursor(int)
  discard c.seekForward(range.a, Bias.Right, ())
  while c.item.isSome and c.startPos < range.b:
    let sliceRange = max(range.a - c.startPos, 0)...(min(range.b, c.endPos(())) - c.startPos)
    assert sliceRange.a in 0..c.item.get[].chars.len
    assert sliceRange.b in 0..c.item.get[].chars.len
    assert sliceRange.len >= 0
    if sliceRange.len > 0:
      let slice = ChunkSlice(chunk: c.item.get, range: sliceRange)
      yield slice
    c.next(())

iterator runes*(self: RopeSlice): Rune =
  for slice in self.iterateChunks:
    for r in slice.chars.runes:
      yield r

iterator chars*(self: RopeSlice): char=
  for slice in self.iterateChunks:
    for c in slice.chars:
      yield c

func `$`*[D](r: RopeSlice[D]): string {.inline.} =
  result = newStringOfCap(r.len)
  for slice in r.iterateChunks:
    for c in slice.chars:
      result.add c

func add*(self: var Chunk, text: openArray[char]) {.inline.} =
  self.data.add text

func saturatingSub(a, b: int): int {.inline.} = max(a - b, 0)

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

func add*(self: var Rope, text: string) {.inline.} =
  self.add text.toOpenArray(0, text.high)

func add*(self: var Rope, rope: sink Rope) =
  var chunks = rope.tree.initCursor
  chunks.next(())
  let chunk = chunks.item
  if chunk.isSome:
    let lastChunk = self.tree.last
    let lastChunkSmallEnough = lastChunk.mapIt(it[].data.len < chunkBase).get(false)
    if lastChunkSmallEnough or chunk.get[].data.len < chunkBase:
      self.add(chunk.get.chars)
      chunks.next(())

  self.tree.append(chunks.suffix(()), ())
  self.checkInvariants()

func addFront*(self: var Rope, text: openArray[char]) {.inline.} =
  let suffix = self.move
  self = Rope.new(text)
  self.add suffix
  self.checkInvariants()

func addFront*(self: var Rope, text: string) {.inline.} =
  self.addFront text.toOpenArray(0, text.high)

func cursor*(self {.byref.}: Rope, offset: int = 0): RopeCursor
func cursorT*[D](self {.byref.}: Rope, position: D): RopeCursorT[D]
func cursorT*(self {.byref.}: Rope, D: typedesc): RopeCursorT[D]
func cursor*[D2](self {.byref.}: RopeSlice[D2], D: typedesc): RopeSliceCursor[D, D2] {.inline.}
func cursor*[D, D2](self {.byref.}: RopeSlice[D2], position: D): RopeSliceCursor[D, D2] {.inline.}
func seekForward*(self: var RopeCursor, target: int)
func seekForward*[D](self: var RopeCursorT[D], target: int)
func seekForward*[D](self: var RopeCursorT[D], target: D)
func slice*(self: var RopeCursor, target: int, bias: Bias = Right): RopeSlice[int]
func sliceRope*(self: var RopeCursor, target: int, bias: Bias = Right): Rope
func slice*[D](self: var RopeCursorT[D], target: D, bias: Bias = Right): RopeSlice[D]
func sliceRope*[D](self: var RopeCursorT[D], target: D, bias: Bias = Right): Rope
func suffix*(self: var RopeCursor): Rope {.inline.}
func suffix*[D](self: var RopeCursorT[D]): Rope {.inline.}
func atEnd*(self: RopeCursor): bool = self.chunks.atEnd
func atEnd*[D](self: RopeCursorT[D]): bool = self.chunks.atEnd
func summary*(self: var RopeCursor, D: typedesc, target: int): D
func summary*[D](self: var RopeCursorT[D], D2: typedesc, target: D): D2

func replace*(self: var Rope, range: Range[int], text: openArray[char]) =
  var newRope = Rope.new()
  var cursor = self.cursor()
  newRope.add(cursor.sliceRope(range.a))
  cursor.seekForward(range.b)
  newRope.add(text)
  newRope.add(cursor.suffix())
  self = newRope
  self.checkInvariants()

func replace*(self: var Rope, range: Range[int], text: string) {.inline.} =
  self.replace(range, text.toOpenArray(0, text.high))

func toRope*[D](self: RopeSlice[D], bias: Bias = Bias.Right): Rope {.inline.} =
  var cursor = self.rope.cursorT(self.range.a)
  result = cursor.sliceRope(self.range.b, bias)
  result.checkInvariants()

converter toSlice*(self: Rope): RopeSlice[int] {.inline.} =
  RopeSlice[int](rope: self.clone(), summary: self.tree.summary, range: 0...self.len)

func slice*(self: Rope, D: typedesc = int): RopeSlice[D] {.inline.} =
  RopeSlice[D](rope: self.clone(), summary: self.tree.summary, range: D.default...D.fromSummary(self.tree.summary, ()))

func slice*(self: Rope, range: Range[int], bias: Bias = Bias.Right): RopeSlice[int] {.inline.} =
  var c = self.cursor(range.a)
  let summary = c.summary(TextSummary, range.b)
  RopeSlice[int](rope: self.clone(), summary: summary, range: range)

func slice*[D](self: Rope, range: Range[D], bias: Bias = Bias.Right): RopeSlice[D] {.inline.} =
  var c = self.cursorT(range.a)
  let summary = c.summary(TextSummary, range.b)
  RopeSlice[D](rope: self.clone(), summary: summary, range: range)

func slice*[D, D2](self: RopeSlice[D2], range: Range[D], bias: Bias = Bias.Right): RopeSlice[D] {.inline.} =
  assert range.b >= range.a
  var c = self.cursor(range.a)
  let summary = c.summary(TextSummary, range.b)
  let start = self.rope.convert(self.range.a, D)
  RopeSlice[D](rope: self.rope.clone(), summary: summary, range: (range.a + start)...(range.b + start))

func slice*[D2](self: RopeSlice[D2], D: typedesc): RopeSlice[D] {.inline.} =
  self.slice(0.D...D.fromSummary(self.summary, ()))

func suffix*[D, D2](self: RopeSlice[D2], start: D): RopeSlice[D] {.inline.} =
  let last = D.fromSummary(self.summary, ())
  return self.slice(min(start, last)...last)

func `[]`*[D, D2](self: RopeSlice[D2], range: Range[D]): RopeSlice[D] {.inline.} =
  self.slice(range, Right)

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
      while not chunk.get.chars.isCharBoundary(overshoot):
        dec overshoot
    of Right:
      while not chunk.get.chars.isCharBoundary(overshoot):
        inc overshoot

    assert overshoot >= 0
    return cursor.startPos + overshoot

  return offset

func cursor*(self {.byref.}: Rope, offset: int = 0): RopeCursor =
  result.rope = self
  result.chunks = self.tree.initCursor(int)
  result.offset = offset
  discard result.chunks.seek(offset, Right, ())

func cursorT*[D](self {.byref.}: Rope, position: D): RopeCursorT[D] =
  result.rope = self
  result.chunks = self.tree.initCursor((D, int))
  result.position = position
  discard result.chunks.seek(position, Right, ())

func cursorT*(self {.byref.}: Rope, D: typedesc): RopeCursorT[D] =
  result.rope = self
  result.chunks = self.tree.initCursor((D, int))
  result.position = D.default
  result.chunks.next(())

func seekForward*(self: var RopeCursor, target: int) =
  assert target >= self.offset
  discard self.chunks.seekForward(target, Right, ())
  self.offset = target

func seekForward*[D](self: var RopeCursorT[D], target: int) =
  discard self.chunks.seekForward(target, Right, ())
  if self.chunks.atEnd:
    self.position = self.chunks.endPos(())[0]
    self.offset = target.some
    assert self.chunks.endPos(())[1] == target
  else:
    let overshoot = target - self.chunks.startPos[1]
    self.position = self.chunks.startPos[0] + self.chunks.item.get[].offsetTo(overshoot, D)
    self.offset = target.some

func seekForward*[D](self: var RopeCursorT[D], target: D) =
  assert target >= self.position
  discard self.chunks.seekForward(target, Right, ())
  self.position = target
  self.offset = int.none

func seekPrevRune*[D](self: var RopeCursorT[D]) =
  assert self.position > D.default
  bind runeStart
  self.offset = int.none

  if self.chunks.item.isSome:
    var chunk = self.chunks.item.get
    let startPos = self.chunks.startPos
    var relOffset = if self.offset.isSome:
      self.offset.get - startPos[1]
    else:
      chunk[].toOffset(self.position - startPos[0])

    if relOffset > 0:
      relOffset = chunk.chars.runeStart(relOffset - 1)
      self.position = startPos[0] + D.fromSummary(TextSummary.init(chunk.chars[0..<relOffset]), ())
      self.offset = (startPos[1] + relOffset).some
    else:
      if startPos[1] == 0:
        # This shouldn't happen I think
        assert false
        return

      self.chunks.prev(())
      assert self.chunks.item.isSome
      chunk = self.chunks.item.get
      assert chunk.data.len > 0
      relOffset = chunk.chars.runeStart(chunk[].data.len - 1)
      self.position = self.chunks.startPos[0] + D.fromSummary(TextSummary.init(chunk.chars[0..<relOffset]), ())
      self.offset = (self.chunks.startPos[1] + relOffset).some

func seekNextRune*[D](self: var RopeCursorT[D]) =
  bind runeSize

  if self.chunks.item.isSome:
    let chunk = self.chunks.item.get
    let startPos = self.chunks.startPos
    var relOffset = if self.offset.isSome:
      self.offset.get - startPos[1]
    else:
      chunk[].toOffset(self.position - startPos[0])

    if relOffset < chunk[].data.len:
      let size = chunk.chars.runeSize(relOffset)
      self.position.addSummary(TextSummary.init(chunk.chars[relOffset..<relOffset+size]), ())
      relOffset += size
      self.offset = (startPos[1] + relOffset).some

    if relOffset == chunk[].data.len:
      self.chunks.next(())

func seekPrevRune*(self: var RopeSliceCursor) =
  assert self.cursor.position > self.range.a
  self.cursor.seekPrevRune()

func seekNextRune*(self: var RopeSliceCursor) =
  assert self.cursor.position < self.range.b
  self.cursor.seekNextRune()

func currentChar*(self: RopeCursor): char =
  let item = self.chunks.item
  let relOffset = self.offset - self.chunks.startPos
  if item.isSome and relOffset < item.get.chars.len:
    return item.get.chars[relOffset]
  return '\0'

func currentChar*(self: RopeCursorT): char =
  let chunk = self.chunks.item
  if chunk.isSome:
    let chunk = chunk.get
    let startPos = self.chunks.startPos
    let relOffset = if self.offset.isSome:
      self.offset.get - startPos[1]
    else:
      chunk[].toOffset(self.position - startPos[0], Left)

    if relOffset < chunk.chars.len:
      return chunk.chars[relOffset]

  return '\0'

func currentChar*(self: RopeSliceCursor): char {.inline.} =
  self.cursor.currentChar

func currentRune*(self: RopeCursor): Rune =
  let item = self.chunks.item
  let relOffset = self.offset - self.chunks.startPos
  if item.isSome and relOffset < item.get.chars.len:
    return item.get.chars.runeAt(relOffset)
  return 0.Rune

func currentRune*(self: RopeCursorT): Rune =
  let chunk = self.chunks.item
  if chunk.isSome:
    let chunk = chunk.get
    let startPos = self.chunks.startPos
    var relOffset = if self.offset.isSome:
      self.offset.get - startPos[1]
    else:
      chunk[].toOffset(self.position - startPos[0], Left)

    while not chunk.chars.isCharBoundary(relOffset):
      relOffset -= 1

    if relOffset < chunk.chars.len:
      return chunk.chars.runeAt(relOffset)

  return 0.Rune

func currentRune*(self: RopeSliceCursor): Rune {.inline.} =
  self.cursor.currentRune

func lineRange*(self: Rope, line: int, D: typedesc): Range[D] =
  bind mapIt

  if line >= self.lines:
    return D.fromSummary(self.tree.summary, ())...D.fromSummary(self.tree.summary, ())

  var cursor = self.tree.initCursor (Point, D)

  discard cursor.seekForward(Point.init(line, 0), Left, ())
  let firstOvershoot = Point.init(line, 0) - cursor.startPos[0]
  let firstStartPos = cursor.startPos
  let firstOffset = cursor.item().mapIt(it[].convert(firstOvershoot, D)).get(D.default)
  let first = firstStartPos[1] + firstOffset

  discard cursor.seekForward(Point.init(line + 1, 0), Left, ())
  let lastOvershoot = Point.init(line + 1, 0) - cursor.startPos[0]
  let lastStartPos = cursor.startPos
  let lastOffset = cursor.item().mapIt(it[].convert(lastOvershoot, D)).get(D.default)
  var last = lastStartPos[1] + lastOffset

  if line < self.lines - 1:
    # byte offset includes the \n, so -1
    dec last

  return first...max(first, last)

func lineLen*(self: Rope, line: int): int =
  if line >= 0 and line < self.lines:
    return self.lineRange(line, int).len

  return 0

func lineRuneLen*(self: Rope, line: int): Count =
  if line >= 0 and line < self.lines:
    return self.lineRange(line, Count).len

  return 0.Count

func summary*(self: Rope): TextSummary {.inline.} = self.tree.summary

func init*(_: typedesc[TextSummary], x: Rope): TextSummary {.inline.} = x.tree.summary
func init*[T](_: typedesc[TextSummary], x: RopeSlice[T]): TextSummary {.inline.} = x.summary

func biasTo*(self: var RopeCursor, bias: Bias) =
  if self.chunks.item.isSome:
    let chunk = self.chunks.item.get
    case bias
    of Left:
      while not chunk.chars.isCharBoundary(self.offset - self.chunks.startPos):
        dec self.offset
    of Right:
      while not chunk.chars.isCharBoundary(self.offset - self.chunks.startPos):
        inc self.offset

func biasTo*(self: var RopeCursorT[int], bias: Bias) =
  if self.chunks.item.isSome:
    let chunk = self.chunks.item.get
    case bias
    of Left:
      while not chunk.chars.isCharBoundary(self.position - self.chunks.startPos[0]):
        dec self.position
    of Right:
      while not chunk.chars.isCharBoundary(self.position - self.chunks.startPos[0]):
        inc self.position

    self.offset = self.position.some

func biasTo*[D](self: var RopeCursorT[D], bias: Bias) =
  if self.chunks.item.isSome:
    let chunk = self.chunks.item.get
    let startPos = self.chunks.startPos
    var relOffset = if self.offset.isSome:
      self.offset.get - startPos[1]
    else:
      chunk[].toOffset(self.position - startPos[0])

    var changed = false
    case bias
    of Left:
      while not chunk.chars.isCharBoundary(relOffset):
        dec relOffset
        changed = true
    of Right:
      while not chunk.chars.isCharBoundary(relOffset):
        inc relOffset
        changed = true

    if changed:
      self.position = self.chunks.startPos[0]
      self.position.addSummary(TextSummary.init(chunk.chars[0..<relOffset]), ())
      self.offset = (self.chunks.startPos[1] + relOffset).some

func clip*[D](self: RopeSlice, position: D, bias: Bias): D =
  var c = self.cursor(position)
  c.cursor.biasTo(bias)
  return c.position

func slice*(self: var RopeCursor, target: int, bias: Bias = Right): RopeSlice[int] =
  assert target <= self.rope.len
  let start = self.offset
  self.seekForward(target)
  self.biasTo(bias)
  return RopeSlice[int](rope: self.rope.clone(), range: start...self.offset)

func sliceRope*(self: var RopeCursor, target: int, bias: Bias = Right): Rope =
  ## Returns a new rope representing the text from the current position to the target position.
  ## If the `target` is not on a character boundary it is snapped to the next boundary depending on `bias`

  assert target >= self.offset, &"Can't slice before current offset: {target} before {self.offset}"

  result = Rope.new()
  let startChunk = self.chunks.item
  if startChunk.isSome:
    let startIndex = self.offset - self.chunks.startPos
    assert startChunk.get.chars.isCharBoundary(startIndex)

    var endIndex = min(target, self.chunks.endPos(())) - self.chunks.startPos
    while not startChunk.get.chars.isCharBoundary(endIndex):
      case bias:
      of Left: dec endIndex
      of Right: inc endIndex

    result.add(startChunk.get.chars(startIndex, endIndex - 1))

  if target > self.chunks.endPos(()):
    self.chunks.next(())
    result.add Rope(tree: self.chunks.slice(target, Right, ()))
    let endChunk = self.chunks.item
    if endChunk.isSome:
      var endIndex = target - self.chunks.startPos
      while not endChunk.get.chars.isCharBoundary(endIndex):
        case bias:
        of Left: dec endIndex
        of Right: inc endIndex

      result.add(endChunk.get.chars(0, endIndex - 1))

  self.offset = target
  result.checkInvariants()

func slice*[D](self: var RopeCursorT[D], target: D, bias: Bias = Right): RopeSlice[D] =
  let start = self.position
  self.seekForward(target)
  self.biasTo(bias)
  return RopeSlice[D](rope: self.rope.clone(), range: start...self.position)

func sliceRope*[D](self: var RopeCursorT[D], target: D, bias: Bias = Right): Rope =
  assert target >= self.position, &"Can't slice before current position: {target} before {self.position}"

  self.offset = int.none
  result = Rope.new()
  let startChunk = self.chunks.item
  if startChunk.isSome:
    let startIndex = startChunk.get[].toOffset(self.position - self.chunks.startPos[0])
    let endIndex = startChunk.get[].toOffset(min(target, self.chunks.endPos(())[0]) - self.chunks.startPos[0], bias)
    result.add(startChunk.get.chars(startIndex, endIndex - 1))

  if target > self.chunks.endPos(())[0]:
    self.chunks.next(())
    result.add Rope(tree: self.chunks.slice(target, Right, ()))
    let endChunk = self.chunks.item
    if endChunk.isSome:
      let endIndex = endChunk.get[].toOffset(target - self.chunks.startPos[0], bias)
      result.add(endChunk.get.chars(0, endIndex - 1))

  self.position = target
  result.checkInvariants()

func summary*(self: var RopeCursor, D: typedesc, target: int): D =
  assert target >= self.offset

  result = D.default
  let startChunk = self.chunks.item
  if startChunk.isSome:
    let startIndex = self.offset - self.chunks.startPos
    let endIndex = min(target, self.chunks.endPos(())) - self.chunks.startPos
    result += D.fromSummary(TextSummary.init(startChunk.get.chars(startIndex, endIndex - 1)), ())

  if target > self.chunks.endPos(()):
    self.chunks.next(())
    result += self.chunks.summary(D, target, Right, ())
    let endChunk = self.chunks.item
    if endChunk.isSome:
      let endIndex = target - self.chunks.startPos
      result += D.fromSummary(TextSummary.init(endChunk.get.chars(0, endIndex - 1)), ())

  self.offset = target

func summary*[D](self: var RopeCursorT[D], D2: typedesc, target: D): D2 =
  assert target >= self.position
  self.offset = int.none

  result = D2.default
  let startChunk = self.chunks.item
  if startChunk.isSome:
    let startIndex = startChunk.get[].toOffset(self.position - self.chunks.startPos[0])
    let endIndex = startChunk.get[].toOffset(min(target, self.chunks.endPos(())[0]) - self.chunks.startPos[0])
    result += D2.fromSummary(TextSummary.init(startChunk.get.chars(startIndex, endIndex - 1)), ())

  if target > self.chunks.endPos(())[0]:
    self.chunks.next(())
    result += self.chunks.summary(D2, target, Right, ())
    let endChunk = self.chunks.item
    if endChunk.isSome:
      let endIndex = endChunk.get[].toOffset(target - self.chunks.startPos[0])
      result += D2.fromSummary(TextSummary.init(endChunk.get.chars(0, endIndex - 1)), ())

  self.position = target

func chunk*(self: var RopeCursor): Option[ptr Chunk] {.inline.} =
  self.chunks.item

func chunk*[D](self: var RopeCursorT[D]): Option[ptr Chunk] {.inline.} =
  self.chunks.item

func chunk*[D, D2](self: var RopeSliceCursor[D, D2]): Option[ptr ChunkSlice] {.inline.} =
  if self.cursor.item.isSome:
    let sliceRange = max(range.a - self.startPos, 0)...(min(range.b, self.endPos(())) - self.startPos)
    assert sliceRange.a in 0..self.item.get[].chars.len
    assert sliceRange.b in 0..self.item.get[].chars.len
    assert sliceRange.len >= 0
    if sliceRange.len > 0:
      let slice = ChunkSlice(chunk: self.item.get, range: sliceRange)

  return (ptr ChunkSlice).none

func resetCursor*(self: var RopeCursor) {.inline.} =
  self.chunks.resetCursor()
  self.offset = 0

func resetCursor*[D](self: var RopeCursorT[D]) {.inline.} =
  self.chunks.resetCursor()
  self.offset = 0.some
  self.position = D.default

func resetCursor*[D, D2](self: var RopeSliceCursor[D, D2]) {.inline.} =
  self.cursor.resetCursor()
  self.cursor.seekForward(self.range.a)

func chunkStartPos*(self: var RopeCursor): int {.inline.} =
  self.chunks.startPos

func chunkStartPos*[D](self: RopeCursorT[D]): (D, int) {.inline.} =
  self.chunks.startPos

func suffix*(self: var RopeCursor): Rope =
  self.sliceRope(self.rope.tree.extent(int, ()))

func suffix*[D](self: var RopeCursorT[D]): Rope =
  self.sliceRope(self.rope.tree.extent(D, ()))

func offset*(self: RopeCursor): int {.inline.} =
  self.offset

func offset*[D](self: RopeCursorT[D]): int =
  if self.offset.isSome:
    self.offset.get
  elif self.chunks.item.isSome:
    let chunk = self.chunks.item.get
    let startPos = self.chunks.startPos
    let relOffset = chunk[].toOffset(Point(self.position - startPos[0]))
    assert relOffset < chunk.chars.len
    let offset = startPos[1] + relOffset
    return offset
  elif not self.chunks.didSeek:
    return 0
  elif self.atEnd:
    return self.rope.bytes
  else:
    assert false, "Seek before using offset"
    return self.rope.bytes

func position*[D](self: RopeCursorT[D]): D {.inline.} =
  self.position

func offset*[D, D2](self: RopeSliceCursor[D, D2]): D {.inline.} =
  self.cursor.offset - self.range.a

func position*[D, D2](self: RopeSliceCursor[D, D2]): D {.inline.} =
  self.cursor.position - self.range.a

func splitLines*(self: Rope): seq[Rope] =
  var c = self.cursorT(Point.init(0, 0))
  for line in 0..<self.lines:
    c.seekForward(Point.init(line, 0))
    result.add(c.sliceRope(Point.init(line.uint32, uint32.high)))

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
        yield chunk.chars.runeAt(overshoot)
        overshoot += chunk.chars.runeSize(overshoot)
      if cursor.atEnd:
        break
      cursor.next(())
      overshoot = 0

iterator runes*(self: Rope): Rune =
  for r in self.runes(0):
    yield r

iterator iterateChunks*(self: Rope): lent Chunk =
  var c = self.tree.initCursor(int)
  c.next(())
  while c.item.isSome:
    yield c.item.get[]
    c.next(())

func cursor*[D2](self {.byref.}: RopeSlice[D2], D: typedesc): RopeSliceCursor[D, D2] {.inline.} =
  RopeSliceCursor[D, D2](ropeSlice: self.clone(), cursor: self.rope.cursorT(D),
    range: self.rope.convert(self.range.a, D)...self.rope.convert(self.range.b, D))

func cursor*[D, D2](self {.byref.}: RopeSlice[D2], position: D): RopeSliceCursor[D, D2] {.inline.} =
  let range = self.rope.convert(self.range.a, D)...self.rope.convert(self.range.b, D)
  RopeSliceCursor[D, D2](ropeSlice: self.clone(), cursor: self.rope.cursorT(range.a + position),
    range: range)

func seekForward*[D, D2](self: var RopeSliceCursor[D, D2], target: D) {.inline.} =
  self.cursor.seekForward(self.range.a + target)

func sliceRope*[D, D2](self: var RopeSliceCursor[D, D2], target: D, bias: Bias = Right): Rope {.inline.} =
  self.cursor.sliceRope(self.range.a + target, bias)

func slice*[D, D2](self: var RopeSliceCursor[D, D2], target: D, bias: Bias = Right): RopeSlice[D2] {.inline.} =
  self.cursor.slice(self.range.a + target, bias)

func suffixRope*[D, D2](self: var RopeSliceCursor[D, D2], bias: Bias = Bias.Right): Rope {.inline.} =
  self.cursor.sliceRope(self.range.b, bias)

func suffix*[D, D2](self: var RopeSliceCursor[D, D2], bias: Bias = Bias.Right): RopeSlice[D2] {.inline.} =
  self.cursor.slice(self.range.b, bias)

func atEnd*[D, D2](self: RopeSliceCursor[D, D2]): bool {.inline.} =
  self.cursor.chunks.atEnd or self.cursor.position >= self.range.b

func summary*[D, D2](self: var RopeSliceCursor[D, D2], S: typedesc, target: D): S {.inline.} =
  self.cursor.summary(S, self.range.a + target)

func startsWith*[D](s: RopeSlice[D], prefix: string): bool {.inline.} =
  let prefixLen = prefix.len
  let sLen = s.len
  var i = 0

  for slice in s.iterateChunks:
    var relOffset = 0
    while relOffset < slice.chars.len:
      if i >= prefixLen: return true
      if i >= sLen or slice.chars[relOffset] != prefix[i]: return false
      inc(i)
      inc(relOffset)

  return false

func charAt*[D, D2](self: RopeSlice[D2], position: D): char {.inline.} =
  var c: RopeSliceCursor[D, D2] = self.cursor(position)
  return c.cursor.currentChar

func runeAt*[D, D2](self: RopeSlice[D2], position: D): Rune {.inline.} =
  var c = self.cursor(position)
  return c.cursor.currentRune

func find*[D](self: RopeSlice[D], sub: string, start: Natural = 0): int {.inline.} =
  var skipTable = initSkipTable(sub)

  let last = self.len - 1
  let subLast = sub.len - 1

  if subLast == -1:
    # this was an empty needle string,
    # we count this as match in the first possible position:
    return start

  # This is an implementation of the Boyer-Moore Horspool algorithms
  # https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore%E2%80%93Horspool_algorithm
  result = -1
  var skip = start

  var c = self.cursor(int)
  while last - skip >= subLast:
    var i = subLast

    if c.position > skip + i:
      c.resetCursor()
    c.seekForward(skip + i)

    while c.currentChar == sub[i]:
      if i == 0:
        return skip
      dec i
      c.seekPrevRune()

    if c.position > skip + subLast:
      c.resetCursor()
    c.seekForward(skip + subLast)

    inc skip, skipTable[c.currentChar]
