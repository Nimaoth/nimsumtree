
import std/[unittest, unicode, os]
import nimsumtree/[sumtree]
import rope

# 🚻 ä
# _äb🚻\äb🚻
# äb🚻

proc testOffsetTo[T](input: T) =
  let offsetTo = [
    (0, 0, (0, 0)),
    (1, 1, (0, 1)),
    (2, 2, (0, 3)),
    (3, 2, (0, 3)),
    (4, 3, (0, 4)),
    (5, 4, (0, 8)),
    (6, 4, (0, 8)),
    (7, 4, (0, 8)),
    (8, 4, (0, 8)),

    (9, 5, (1, 0)),
    (10, 6, (1, 2)),
    (11, 6, (1, 2)),
    (12, 7, (1, 3)),
    (13, 8, (1, 7)),
    (14, 8, (1, 7)),
    (15, 8, (1, 7)),
    (16, 8, (1, 7))
  ]
  for (offset, count, point) in offsetTo:
    check input.offsetToCount(offset) == count.Count
    check input.offsetToPoint(offset) == Point(row: point[0].uint32, column: point[1].uint32)

  let countTo = [
    (0, 0, (0, 0)),
    (1, 1, (0, 1)),
    (2, 3, (0, 3)),
    (3, 4, (0, 4)),
    (4, 8, (0, 8)),
    (5, 9, (1, 0)),
    (6, 11, (1, 2)),
    (7, 12, (1, 3)),
    (8, 16, (1, 7))
  ]
  for (count, offset, point) in countTo:
    check input.countToOffset(count.Count) == offset
    check input.countToPoint(count.Count) == Point(row: point[0].uint32, column: point[1].uint32)

  let pointTo = [
    ((0, 0), 0, 0),
    ((0, 1), 1, 1),
    ((0, 3), 3, 2),
    ((0, 4), 4, 3),
    ((0, 8), 8, 4),
    ((1, 0), 9, 5),
    ((1, 2), 11, 6),
    ((1, 3), 12, 7),
    ((1, 7), 16, 8),
  ]
  for (point, offset, count) in pointTo:
    let point = Point(row: point[0].uint8, column: point[1].uint8)
    check input.pointToOffset(point) == offset
    check input.pointToCount(point) == count.Count

const testText1 = "_äb🚻\näb🚻"
when chunkBase >= testText1.len:
  test "Chunk offsetToCount":
    let chunk = testText1.toChunk
    testOffsetTo(chunk)
else:
  echo &"note: test 'Chunk offsetToCount' not executed because chunkBase ({chunkBase}) < testText1.len ({testText1.len})"

test "Rope offsetToCount":
  let rope = testText1.toRope
  testOffsetTo(rope)
