import std/[sequtils, strutils, atomics]
import clone

export clone

const debugCustomArcId {.booldefine.} = true
const debugCustomArc {.booldefine.} = false
const debugCustomArcLeaks {.booldefine.} = true

when debugCustomArc:
  import strformat

var idCounter = 0

var activeArcs = 0

proc assertNoLeaks*() =
  assert activeArcs == 0

type
  ArcData[T] = object
    when debugCustomArcId or debugCustomArc:
      id: int
    count: Atomic[uint64]
    value: T

  Arc*[T] = object
    data: ptr ArcData[T]

proc `=copy`*[T](a: var Arc[T], b: Arc[T]) {.error.}
proc `=dup`*[T](a: Arc[T]): Arc[T] {.error.}

proc `=destroy`*[T](a: Arc[T]) {.raises: [].} =
  if a.data == nil:
    return

  when debugCustomArc:
    echo "Arc.destroy _", a.data[].id, ", count: ", a.data[].count.load

  if a.data[].count.fetchSub(1, moRelease) != 1:
    return

  # todo: support thread sanitizer: use a.data[].count.load
  # see: sync.rs impl Drop for Arc
  fence(moAcquire)

  when debugCustomArcLeaks:
    dec activeArcs

  when debugCustomArc:
    echo "Arc.destroy _", a.data[].id, ", count: ", a.data[].count.load

  {.warning[BareExcept]: off.}
  try:
    `=destroy`(a.data[].value)
    `=wasMoved`(a.data[].value)
  except:
    discard
  {.warning[BareExcept]: on.}

  deallocShared(a.data)

proc `$`*[T](arc {.byref.}: Arc[T]): string =
  when debugCustomArcId or debugCustomArc:
    "Arc(_" & $arc.data[].id & ", #" &  $arc.data[].count & ", " & $arc.data[].value & ")"
  else:
    "Arc(#" & $arc.data[].count & ", " & $arc.data[].value & ")"

proc new*[T](_: typedesc[Arc], default: sink T): Arc[T] =
  result.data = cast[ptr ArcData[T]](allocShared0(sizeof(ArcData[T])))
  result.data[].count.store(1.uint64)
  result.data[].value = default.move

  when debugCustomArcId or debugCustomArc:
    inc idCounter
    when debugCustomArc:
      echo "Arc.new _", idCounter
    result.data[].id = idCounter
  when debugCustomArcLeaks:
    inc activeArcs

proc new*[T](_: typedesc[Arc[T]], default: sink T): Arc[T] =
  result.data = cast[ptr ArcData[T]](allocShared0(sizeof(ArcData[T])))
  result.data[].count.store(1.uint64)
  result.data[].value = default.move

  when debugCustomArcId or debugCustomArc:
    inc idCounter
    when debugCustomArc:
      echo "Arc.new _", idCounter
    result.data[].id = idCounter
  when debugCustomArcLeaks:
    inc activeArcs

proc new*[T](A: typedesc[Arc[T]]): Arc[T] = A.new(T.default)
proc new*(A: typedesc[Arc], T: typedesc): Arc[T] = A.new(T.default)

proc clone*[T](a {.byref.}: Arc[T]): Arc[T] =
  assert a.data != nil

  let oldSize = a.data[].count.fetchAdd(1, moRelaxed)
  assert oldSize < int64.high.uint64

  result.data = a.data

  when debugCustomArc:
    echo "Arc.clone _", a.data[].id, " -> ", a.data[].count

func id*[T](a: Arc[T]): int =
  when debugCustomArcId or debugCustomArc:
    if a.data == nil:
      -1
    else:
      a.data[].id
  else:
    -1

func count*[T](a: Arc[T]): int =
  if a.data == nil:
    -1
  else:
    a.data[].count.load(moRelaxed).int

func getMut*[T](a: var Arc[T]): var T =
  assert a.data != nil
  assert a.count == 1
  a.data[].value

func get*[T](a: Arc[T]): lent T =
  assert a.data != nil
  # assert a.count == 1
  a.data[].value


proc makeUnique*[T](a: var Arc[T]) =
  assert a.data != nil

  var expected = 1.uint64
  if not a.data[].count.compareExchange(expected, 0.uint64, moAcquire, moRelaxed):
    # Note: clone the node, not the arc

    var b = Arc.new(a.data[].value.clone())
    when debugCustomArc:
      echo &"Arc.makeUnique {a} -> {b}"

    a = b.move

  else:
    # Only reference, set count back to one
    a.data[].count.store(1, moRelease)