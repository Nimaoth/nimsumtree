
type
  Clone* = concept a, type T
    a.clone() is T

proc clone*[T: bool](x: T): T = x
proc clone*[T: enum](x: T): T = x
proc clone*[T: SomeNumber](x: T): T = x
proc clone*(x: tuple[]): tuple[] = x
proc clone*(x: string): string = x

proc clone*[T](x: openArray[T]): seq[T] =
  result = newSeqOfCap[T](x.len)
  for y in x:
    result.add y.clone()
