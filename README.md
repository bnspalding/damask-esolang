# damask-esolang
a poetic esolang for string transformations

## Introduction
Damask (ˈdæ.məsk) interprets punctuation in a source-poem as a sequence of
encoded transformations to generate a second poem woven from the elements of the
source.

The language is named for damask fabric, which similarly realizes its pattern
directly in the interplay of the single warp and weft. When the warp and weft
are monochrome, the result is a subtle pattern that plays in the light. In a
two-color damask, a pattern in a dominant color is set off against the
interwoven background. In either case, damask is a two-sided (text)ile, and the
inverse of the 'front' side is mirrored in the back.

I'm still figuring out exactly what this esolang is (everything is subject to
change), but I like the metaphor and I'm motivated by the idea of an esolang
that works entirely in string transformations to produce two-sided poems. Tuning
the encoded string operations to produce the correct set of affordances will
come through iterative play.

## Notes

- I think it's more interesting and true to the metaphor 
to make things totally reversible. No operators that
delete letters, make shift work on both sides rather
than only left plus right.
- figure out easier ways to target a specific length
of characters. It should be easy to mark what chars
to operate on.
- add tools to find shift pairs. Like Brainf\*\*\*'s
constant resource, a tool to search a dictionary for
usable shifts would be great
- use property tests to assert inverse functions / reversible operations
(FLIP/UNFLIP), (PUSH, UNPUSH)

## Example
an example of a poem to a poem

## Operators

Certain punctuation marks act as transformations on the text of a source-poem.
These operators and their transformations are detailed below. Because a Damask
poem is reversible, each operation has an inverse.

**comma (,) - FLIP:** swap all to the left of the comma with all to the right

```
world, hello -> hello, world
```

reversed using UNFLIP (a right-associating FLIP). FLIP x UNFLIP = id

? - is UNFLIP necessary? is FLIP its own inverse? this needs to be thought out.

**colon/semicolon (:;) - MOVE:** move the colon and following word to the
location of the associated semicolon. Leave a semicolon at the location of the
move.

Colons and semicolons are associated like parentheses: the first of either
symbol (colon/semicolon) is associated with the last of the other
(semicolon/colon).

MOVE is its own inverse. MOVE x MOVE = id

**em dash (—) SHIFT:** modify the letters to the right of the em dash by 
shifting (positively) each of them by the value of the letter in the word to the 
left side of the dash

```
red— pot -> red—rust

(r+ ) (e+p) (d+u)
19+0   5+16  4+21
  r     u     s

The left side of the above SHIFT is 3 characters (red), and so it operates on
just the first three characters to the right of the em dash ( po).
```

reversed using SHIFT-. SHIFT+ x SHIFT- = id

? - perhaps there is a way to target the right word for shifting instead of the
left.

A space is value 0. Values above 26 wrap around.

The left side of a SHIFT (a word) is any number of letters followed by any
number of spaces. A string of equal length is captured on the right side of the
SHIFT. If there are fewer than *left* characters to the right of the SHIFT, the
right string is right-padded with zeros to the length of the left word.

**breaks (\n \t) - BREAK:** newlines and tabs act as boundaries for operators

For now, let's call these segments of a poem "threads"

In the examples below, parentheses are added for clarity in reading break marks.
```
world, hello(\t) abc -> hello, world abc
```

```
hello(\t)aaa—a(\n)world -> hello(\t)baa(\n)world
```

## Targeting

There should be a better way to modify the selections of operators on a case by
case basis. Something like thread-local punctuation (. ! ? ...) could be used
to modify the selections of operators within that thread


## Tools

### Shift-Finder
Also included will be a tool for searching for valid SHIFTS. Given a dictionary
and an input word (left side) show the word -> word transformations that are
valid for the right side.
