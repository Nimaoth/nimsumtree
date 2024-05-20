import std/[unittest, enumerate, strformat, sugar, unicode, os]
import nimsumtree/[sumtree]
import rope

test "Empty":
  check $Rope.new() == "Rope()"
  check $Rope.new("") == "Rope()"

test "One line":
  var a = Rope.new("abcdefghijklmnopqrstuvwxyz")
  check $a == "Rope(abcdefghijklmnopqrstuvwxyz)"
  check a.bytes == 26
  check a.chars == 26
  check a.lines == Point(row: 0, column: 26)

proc testStringSummary(text: string) =
  var point = Point()
  for c in text:
    if c == '\n':
      point.row += 1
      point.column = 0
    else:
      point.column += 1

  var a = Rope.new(text)
  check $a == &"Rope({text})"
  check a.bytes == text.len
  check a.chars == text.runeLen
  check a.lines == point

  echo &"{text.len} bytes, {text.runeLen} runes"
  echo a.tree.stats
  # echo a.tree.pretty

test "Various string summaries":
  testStringSummary("abcdefghijklmnopqrstuvwxyz")

  testStringSummary("""import os
let a = 5
echo a""")

  # const a = staticRead("text_example.txt")
  # testStringSummary(a)

block:
  var a = Rope.new("""import std/[os]
let foo = 5
echo foo""")

  echo a
  echo a.tree.pretty

  echo 0, " -> ", a.offsetToPoint(0)
  echo 5, " -> ", a.offsetToPoint(5)
  echo 10, " -> ", a.offsetToPoint(10)
  echo 15, " -> ", a.offsetToPoint(15)
  echo 20, " -> ", a.offsetToPoint(20)
  echo 25, " -> ", a.offsetToPoint(25)
  echo 30, " -> ", a.offsetToPoint(30)
  echo 35, " -> ", a.offsetToPoint(35)
