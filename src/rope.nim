import std/[unittest, enumerate, strformat, sugar, unicode, os]
import nimsumtree/sumtree

const chunkBase = 256
const treeBase = 12
# const chunkBase = 128
# const treeBase = 12

type Count* = distinct int
type Utf* = distinct int

type Point* = object
  row*: uint32
  column*: uint32

type TextSummary* = object
  bytes*: int
  len*: Count
  lines*: Point
  firstLineChars*: uint32
  lastLineChars*: uint32
  longestRow*: uint32
  longestRowChars*: uint32

type Chunk* = object
   data: Array[char, chunkBase]

func clone*(a: TextSummary): TextSummary = a

func `$`*(a: Count): string {.borrow.}
func `+=`*(a: var Count, b: Count) {.borrow.}
func `==`*(a: Count, b: Count): bool {.borrow.}
func addSummary*(a: var Count, b: Count) = a = (a.int + b.int).Count
func clone*(a: Count): Count = a
func cmp*(a: Count, b: Count): int = cmp(a.int, b.int)

func addSummary*(a: var Count, b: TextSummary) = a += b.len
func fromSummary*(_: typedesc[Count], a: TextSummary): Count = a.len

# func `==`*(a: Point, b: Point): bool = discard
func `+=`*(a: var Point, b: Point) =
  a.row += b.row
  if b.row == 0:
    a.column += b.column
  else:
    a.column = b.column

func `+`*(a: Point, b: Point): Point =
  result = a
  result += b

func addSummary*(a: var Point, b: Point) = a += b
func clone*(a: Point): Point = a
func cmp*(a: Point, b: Point): int =
  result = cmp(a.row, b.row)
  if result == 0:
    result = cmp(a.column, b.column)

func addSummary*(a: var Point, b: TextSummary) = a += b.lines
func fromSummary*(_: typedesc[Point], a: TextSummary): Point = a.lines

# func `+=`*(a: var (Count, Point), b: (Count, Point)) =
#   a[0] += b[0]
#   a[1] += b[1]
# func addSummary*(a: var (Count, Point), b: (Count, Point)) =
#   a[0] += b[0]
#   a[1] += b[1]
# func clone*(a: (Count, Point)): (Count, Point) = (a[0].clone(), a[1].clone())
# func cmp*(a: Count, b: (Count, Point)): int = cmp(a.int, b[0].int)
func cmp*(a: Count, b: TextSummary): int = cmp(a, b.len)
func cmp*(a: Point, b: TextSummary): int = cmp(a, b.lines)

# func `+=`*[A, B](a: var (A, B), b: (A, B)) =
#   a[0] += b[0]
#   a[1] += b[1]

func addSummary*[A, B](a: var (A, B), b: (A, B)) =
  a[0] += b[0]
  a[1] += b[1]

func clone*[A, B](a: (A, B)): (A, B) = (a[0].clone(), a[1].clone())
func cmp*[A, B](a: A, b: (A, B)): int = cmp(a, b[0])

func addSummary*(a: var (int, Point), b: TextSummary) =
  a[0] += b.bytes
  a[1] += b.lines

func addSummary*(a: var (Count, Point), b: TextSummary) =
  a[0] += b.len
  a[1] += b.lines

# func addSummary*(a: var (Count, Point), b: TextSummary) =
#   a[0] += b.len
#   a[1] += b.lines

# func fromSummary*(_: typedesc[(Count, Point)], a: TextSummary): (Count, Point) = (a.count, a.max)

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

func summary*(x: Chunk): TextSummary =
  result.bytes = x.data.len

  for r in x.data.toOpenArray.runes:
    result.len += 1.Count

    if r.char == '\n':
      if result.lines.column > result.longestRowChars:
        result.longestRowChars = result.lines.column
        result.longestRow = result.lines.row

      if result.lines.row == 0:
        result.firstLineChars = result.lines.column

      result.lines.row += 1
      result.lines.column = 0

    else:
      result.lines.column += 1

  if result.lines.column > result.longestRowChars:
    result.longestRowChars = result.lines.column
    result.longestRow = result.lines.row
  if result.lines.row == 0:
    result.firstLineChars = result.lines.column

  result.lastLineChars = result.lines.column

type Rope* = object
  tree*: SumTree[Chunk, treeBase]

# func toArray*(arr: openArray[Chunk]): Array[Chunk, treeBase] =
#   result.len = arr.len
#   for i in 0..<arr.len:
#     result[i] = arr[i]

func bytes*(self: Rope): int =
  return self.tree.summary.bytes

func chars*(self: Rope): int =
  return self.tree.summary.len.int

func lines*(self: Rope): Point =
  return self.tree.summary.lines

proc offsetToPoint*(self: Chunk, target: int): Point =
  var offset = 0
  for r in self.data.toOpenArray.runes:
    if offset >= target:
      break
    if r.char == '\n':
      result.row += 1
      result.column = 0
    else:
      result.column += r.size.uint32
    offset += r.size

proc offsetToPoint*(self: Rope, offset: int): Point =
  if offset >= self.tree.summary.len.int:
    return self.tree.summary.lines
  var cursor = self.tree.initCursor (int, Point)
  discard cursor.seekForward(offset, Left)
  let overshoot = offset - cursor.first[0]
  return cursor.first[1] + cursor.item().mapIt(it.offsetToPoint(overshoot)).get(Point.default)

func toArray*[T](arr: openArray[T], C: static int): Array[T, C] =
  assert arr.len <= C
  result.len = arr.len
  for i in 0..<arr.len:
    result[i] = arr[i]

proc new*(_: typedesc[Rope], value: string = ""): Rope =
  var chunks = newSeq[Chunk]()

  var i = 0
  while i < value.len:
    let last = min(i + chunkBase, value.len)
    chunks.add Chunk(data: value.toOpenArray(i, last - 1).toArray(chunkBase))
    i = last

  result.tree = SumTree[Chunk, treeBase].new(chunks)

proc `$`*(r: Chunk): string =
  result = "["
  result.add r.data.toOpenArray.join("")
  result.add "]"

proc `$`*(r: Rope): string =
  result = "Rope("
  for chunk in r.tree:
    result.add chunk.data.toOpenArray.join("")
  result.add ")"
