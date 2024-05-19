import std/[sequtils, strutils]
import clone

export clone

const debugCustomArc {.booldefine.} = false
when debugCustomArc:
  import strformat

var idCounter = 0

type
  Arc*[T] = object
    when debugCustomArc:
      id: ref int
    # todo: properly implement this stuff
    count: ref int
    value: ref T

proc `=copy`*[T](a: var Arc[T], b: Arc[T]) {.error.}
proc `=dup`*[T](a: Arc[T]): Arc[T] {.error.}

proc `=destroy`*[T](a: Arc[T]) {.raises: [].} =
  if a.count.isNil or a.value.isNil:
    return

  dec a.count[]

  when debugCustomArc:
    echo "destroy arc ", a.id[], ", count: ", a.count[]

  if a.count[] == 0:
    when debugCustomArc:
      echo "destroy arc value ", a.id[], ", count: ", a.count[]

    try:
      `=destroy`(a.value[])
      `=wasMoved`(a.value[])
    except Exception:
      discard

proc `$`*[T](arc {.byref.}: Arc[T]): string =
  when debugCustomArc:
    "Arc(_" & $arc.id[] & ", " & $arc.value[] & ")"
  else:
    "Arc(" & $arc.value[] & ")"

proc new*[T](_: typedesc[Arc], default: sink T): Arc[T] =
  when debugCustomArc:
    inc idCounter
    echo "Arc.new _", idCounter
    result.id = new int
    result.id[] = idCounter

  result.count = new int
  result.count[] = 1
  result.value = new T
  result.value[] = default.move

proc new*[T](_: typedesc[Arc[T]], default: sink T): Arc[T] =
  when debugCustomArc:
    inc idCounter
    echo "Arc.new _", idCounter
    result.id = new int
    result.id[] = idCounter

  result.count = new int
  result.count[] = 1
  result.value = new T
  result.value[] = default.move

proc new*[T](A: typedesc[Arc[T]]): Arc[T] = A.new(T.default)
proc new*(A: typedesc[Arc], T: typedesc): Arc[T] = A.new(T.default)

proc clone*[T](a {.byref.}: Arc[T]): Arc[T] =
  when debugCustomArc:
    result.id = a.id
  result.count = a.count
  result.count[] += 1
  result.value = a.value
  when debugCustomArc:
    echo "clone _", a.id[], " -> ", a.count[]

proc getUnique*[T: Clone](a: var Arc[T]): lent T =
  if a.count[] > 1:
    a = Arc[T].new(a.value[].clone())
  return a.value[]

func id*[T](a: Arc[T]): int =
  when debugCustomArc:
    if a.id.isNil:
      -1
    else:
      a.id[]
  else:
    -1

func count*[T](a: Arc[T]): int =
  if a.count.isNil:
    -1
  else:
    a.count[]

func set*[T](a: var Arc[T], value: sink T) =
  assert not a.value.isNil
  a.value[] = value

func get*[T](a: var Arc[T]): var T =
  assert not a.value.isNil
  a.value[]

func get*[T](a: Arc[T]): lent T =
  assert not a.value.isNil
  a.value[]
