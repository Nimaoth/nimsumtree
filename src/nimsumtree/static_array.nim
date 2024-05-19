import std/[macros, strformat, options]
import clone

type
  Array*[T; Cap: static int] = object
    data: array[Cap, T]
    len: uint8

func initArray*(T: typedesc, capacity: static int): Array[T, capacity] =
  result = Array[T, capacity].default

func low*[T; C: static int](arr: Array[T, C]): int =
  0

func high*(arr: Array): int =
  int(arr.len.int - 1)

func `[]`*[T; C: static int](arr: var Array[T, C], index: int): var T =
  assert index >= 0
  assert index < C
  return arr.data[index]

func `[]`*[T; C: static int](arr {.byref.}: Array[T, C], index: int): lent T =
  assert index >= 0
  assert index < C
  return arr.data[index]

func `[]=`*[T; C: static int](arr: var Array[T, C], index: int, value: sink T) =
  assert index >= 0
  assert index < C
  arr.data[index] = value

func add*[T; C: static int](arr: var Array[T, C], val: sink T) =
  assert arr.len < C
  arr.data[arr.len.int] = val.move
  inc arr.len

func add*[T; C: static int](arr: var Array[T, C], vals: sink Array[T, C]) =
  assert arr.len + vals.len <= C
  for i in 0..vals.high:
    arr.data[arr.len.int + i] = vals.data[i].move
  arr.len += vals.len

macro evalOnceAs(expAlias, exp: untyped,
                 letAssigneable: static[bool]): untyped =
  ## Injects `expAlias` in caller scope, to avoid bugs involving multiple
  ## substitution in macro arguments such as
  ## https://github.com/nim-lang/Nim/issues/7187.
  ## `evalOnceAs(myAlias, myExp)` will behave as `let myAlias = myExp`
  ## except when `letAssigneable` is false (e.g. to handle openArray) where
  ## it just forwards `exp` unchanged.
  expectKind(expAlias, nnkIdent)
  var val = exp

  result = newStmtList()
  # If `exp` is not a symbol we evaluate it once here and then use the temporary
  # symbol as alias
  if exp.kind != nnkSym and letAssigneable:
    val = genSym()
    result.add(newLetStmt(val, exp))

  result.add(
    newProc(name = genSym(nskTemplate, $expAlias), params = [getType(untyped)],
      body = val, procType = nnkTemplateDef))

template mapIt*[T; C: static int](arr: Array[T, C], op: untyped): untyped =
  block:
    type OutType = typeof((
      block:
        var it {.inject.}: typeof(arr.data[0], typeOfProc);
        op), typeOfProc)

    evalOnceAs(arr2, arr, compiles((let _ = s)))

    var res: Array[OutType, C]
    res.len = arr2.len

    for i in 0..<arr2.len.int:
      var it {.inject, cursor.} = arr2.data[i]
      res.data[i] = op

    res

proc `$`*[T; C: static int](arr {.byref.}: Array[T, C]): string =
  result = "Array[" & $T & ", " & $C & "]("
  for i in 0..<arr.len.int:
    if i > 0:
      result.add ", "
    result.add $arr[i]
  result.add ")"

template toOpenArray*[T; C: static int](arr: Array[T, C]): openArray[T] =
  arr.data.toOpenArray(0, arr.high)

template toOpenArray*[T; C: static int](arr: Array[T, C], first, last: int): openArray[T] =
  arr.data.toOpenArray(first, last)

proc clone*[T: Clone; C: static int](arr: Array[T, C]): Array[T, C] =
  result.len = arr.len
  for i in 0..<arr.len.int:
    result.data[i] = arr.data[i].clone()

func len*[T; C: static int](arr: Array[T, C]): int =
  arr.len.int

func `len=`*[T; C: static int](arr: var Array[T, C], newLen: int) =
  assert newLen <= C
  arr.len = newLen.uint8
