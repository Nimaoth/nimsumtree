import std/[macros]


type
  Arc*[T] = object
    value: ref T

  Summary* {.explain.} = concept var a, b
    a += b

  Item* {.explain.} = concept a, type T
    a.summary is Summary

template summaryType*(T: typedesc[Item]): untyped = typeof(T.default.summary)

type
  NodeKind* = enum Internal, Leaf
  Node*[T: Item] = object
    case kind*: NodeKind
    of Internal:
      children: seq[Arc[Node[T]]]
      summaries: seq[T.summaryType]
    of Leaf:
      summary: T.summaryType
      data: seq[T]

  SumTree*[T: Item] = Node[T]

# func newLeaf[T

when isMainModule:
  type TestSummary = object
    count: int
    max: int
    zeroes: int

  type TestSummary2 = object
    count: int
    max: int
    ones: int

  # func summaryType*(x: typedesc[int]): typedesc[Summary] = TestSummary

  func `+=`(a: var TestSummary, b: TestSummary) =
    a.count += b.count
    a.max = max(a.max, b.max)
    a.zeroes += b.zeroes

  func `+=`(a: var TestSummary2, b: TestSummary2) =
    a.count += b.count
    a.max = max(a.max, b.max)
    a.ones += b.ones

  func summary(x: int): TestSummary = TestSummary(count: 1, max: x, zeroes: if x == 0: 1 else: 0)
  # func summary(x: int): TestSummary2 = TestSummary2(count: 1, max: x, ones: if x == 1: 1 else: 0)

  var node = SumTree[int](kind: Leaf)
  echo node
