import std/[sequtils, strutils, atomics]
import clone

export clone

const debugCustomArcId {.booldefine.} = false
const debugCustomArc {.booldefine.} = false
const debugCustomArcLeaks {.booldefine.} = false

when debugCustomArc:
  import strformat

when debugCustomArc or debugCustomArcId:
  var idCounter = 0

when debugCustomArcLeaks:
  var activeArcs = 0

func assertNoLeaks*() {.noSideEffect.} =
  when debugCustomArcLeaks:
    {.cast(noSideEffect).}:
      if activeArcs != 0:
        debugEcho "activeArcs: ", activeArcs
        assert activeArcs != 0

type
  ArcData[T] {.acyclic.} = object
    when debugCustomArcId or debugCustomArc:
      id: int
    count: Atomic[uint64]
    value: T

  Arc*[T] {.acyclic.} = object
    data: ptr ArcData[T]

var arcCount*: int = 0

proc destroyImpl[T](a: ptr Arc[T]) {.raises: [], noSideEffect, inline, nimcall.} =
  if a[].data == nil:
    return

  when debugCustomArc:
    debugEcho "Arc.destroy _", a[].data[].id, ", count: ", a[].data[].count.load

  if a[].data[].count.fetchSub(1, moRelease) != 1:
    return

  # todo: support thread sanitizer: use a[].data[].count.load
  # see: sync.rs impl Drop for Arc
  fence(moAcquire)

  when debugCustomArcLeaks:
    {.cast(noSideEffect).}:
      dec activeArcs

  when debugCustomArc:
    debugEcho "Arc.destroy _", a[].data[].id, ", count is zero, dealloc"
    debugEcho "Arc.destroy _", a[].data[].id, ", count: ", a[].data[].count.load

  {.cast(noSideEffect).}:
    arcCount.dec

  {.warning[BareExcept]: off.}
  try:
    {.cast(noSideEffect).}:
      `=destroy`(a[].data[].value)
      `=wasMoved`(a[].data[].value)
  except:
    discard
  {.warning[BareExcept]: on.}

  {.cast(noSideEffect).}:
    deallocShared(a[].data)

proc `=destroy`*[T](a: Arc[T]) {.raises: [], noSideEffect, inline, nimcall.} =
  destroyImpl(a.unsafeAddr)

proc `=copy`*[T](a: var Arc[T], b: Arc[T]) =
  if a.data == b.data:
    return

  discard b.data[].count.fetchAdd(1, moRelaxed)

  destroyImpl(a.addr)
  `=wasMoved`(a)

  a.data = b.data

  when debugCustomArc:
    debugEcho "Arc.copy _", a.data[].id, " -> ", a.data[].count.load

proc `=dup`*[T](a: Arc[T]): Arc[T] {.nodestroy.} =
  result.data = nil
  if a.data != nil:
    discard a.data[].count.fetchAdd(1, moRelaxed)
    result.data = a.data

    when debugCustomArc:
      debugEcho "Arc.dup _", a.data[].id, " -> ", a.data[].count.load

when defined(gcOrc):
  proc `=trace`*[T](a: var Arc[T], env: pointer) =
    if a.data != nil:
      `=trace`(a.data[].value, env)

func isNil*[T](arc {.byref.}: Arc[T]): bool = arc.data.isNil

func `$`*[T](arc {.byref.}: Arc[T]): string =
  when debugCustomArcId or debugCustomArc:
    "Arc(_" & $arc.data[].id & ", #" &  $arc.data[].count.load & ", " & $arc.data[].value & ")"
  else:
    "Arc(#" & $arc.data[].count.load & ", " & $arc.data[].value & ")"

func new*[T](_: typedesc[Arc], default: sink T): Arc[T] =
  {.cast(noSideEffect).}: # todo: Verify if this is safe
    result.data = cast[ptr ArcData[T]](allocShared0(sizeof(ArcData[T])))
  result.data[].count.store(1.uint64)
  result.data[].value = default.move

  {.cast(noSideEffect).}:
    arcCount.inc
    when debugCustomArcId or debugCustomArc:
      inc idCounter
      when debugCustomArc:
        debugEcho "Arc.new _", idCounter
      result.data[].id = idCounter
  when debugCustomArcLeaks:
    {.cast(noSideEffect).}:
      inc activeArcs

func new*[T](_: typedesc[Arc[T]], default: sink T): Arc[T] =
  {.cast(noSideEffect).}: # todo: Verify if this is safe
    result.data = cast[ptr ArcData[T]](allocShared0(sizeof(ArcData[T])))
  result.data[].count.store(1.uint64)
  result.data[].value = default.move

  {.cast(noSideEffect).}:
    arcCount.inc
    when debugCustomArcId or debugCustomArc:
      inc idCounter
      when debugCustomArc:
        debugEcho "Arc.new _", idCounter
      result.data[].id = idCounter
  when debugCustomArcLeaks:
    {.cast(noSideEffect).}:
      inc activeArcs

func new*[T](A: typedesc[Arc[T]]): Arc[T] = A.new(T.default)
func new*(A: typedesc[Arc], T: typedesc): Arc[T] = A.new(T.default)

func clone*[T](a {.byref.}: Arc[T]): Arc[T] =
  assert a.data != nil

  let oldSize = a.data[].count.fetchAdd(1, moRelaxed)
  assert oldSize < int64.high.uint64

  result.data = a.data

  when debugCustomArc:
    debugEcho "Arc.clone _", a.data[].id, " -> ", a.data[].count.load

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
  a.data[].value

func getMutUnsafe*[T](a: Arc[T]): var T =
  assert a.data != nil
  a.data[].value

var arcUniqueCount*: int = 0
var arcNonUniqueCount*: int = 0

func makeUnique*[T](a: var Arc[T]) =
  assert a.data != nil

  var expected = 1.uint64
  if not a.data[].count.compareExchange(expected, 0.uint64, moAcquire, moRelaxed):
    # Note: clone the node, not the arc

    var b = Arc.new(a.data[].value.clone())
    when debugCustomArc:
      debugEcho &"Arc.makeUnique {a} -> {b}"

    a = b.move
    {.cast(noSideEffect).}:
      arcNonUniqueCount.inc

  else:
    # Only reference, set count back to one
    a.data[].count.store(1, moRelease)
    {.cast(noSideEffect).}:
      arcUniqueCount.inc