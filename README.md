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

## Example
an example of a poem to a poem

## Operators

**comma (,) - FLIP:** swap all to the left of the comma with all to the right

```
world, hello -> hello, world
```

**colon (:) - PUSH:** push everything after the colon to the end of the poem

```
hello: world(\n)goodbye -> hello(\n)goodbye(\n)world
```

**em dash (—) - SHIFT:** modify the word to the left of the em dash by shifting 
each letter in the word by the value of the letter on the right side of the dash

```
held —   ho world -> hello world

(h+ )(e+ )(l+ )(d+h)( +o)
 8+0  5+0 12+0  4+8  0+15
  h    e    l    l    o

The left side of the above SHIFT is 5 characters (held ), and so it captures
just the five characters following (   ho)
```
A space is value 0. Values above 26 wrap around.

The left side of a SHIFT (a word) is any number of letters followed by any
number of spaces. A string of equal length is captured on the right side of the
SHIFT. If there are fewer than *left* characters to the right of the SHIFT, the
right string is right-padded with zeros to the length of the left word.

**breaks (\n \t) - BREAK:** newlines and tabs act as boundaries for operators

In the examples below, parentheses are added for clarity in reading break marks.
```
world, hello(\t) abc -> hello, world abc
```

```
hello(\t)aaa—a(\n)world -> hello\tbaa\nworld
```
