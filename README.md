Unsupported
===========
Apologies to the millions of the users of this library...it is no longer supported.
I have no plans to support it. It's *dead*. Please use my
[glu-tessellate](https://github.com/orthecreedence/glu-tessellate) library for
triangulation instead.

This library works very well for simpler cases, but for more complicated polygons,
flops with infinite loops and triangulation errors. What's more is it doesn't
even touch polygon holes. [glu-tessellate](https://github.com/orthecreedence/glu-tessellate)
*does* handle holes. If you have access to GLU, please use it instead.

cl-triangulation
================

Library for polygon triangulation in CL. Polygons *must* be simple, and *must*
have no holes. Results are undetermined if your polygon has holes!!

Polygons to NOT have to be in counter-clockwise order, as the library can 
detect this and reverse points as needed.

## Usage

    (cl-triangulation:triangulate #((0 0) (0 1) (1 1) (1 0)))  ->
	    (((1 0) (0 1) (0 0)) ((1 0) (1 1) (0 1)))

Pass it a vector of point pairs and it returns a list of triangles. A triangle
is just a list of three point pairs.

Please note that this library isn't optimized *at all*. It could probably be a
lot faster and have way less code. I built it for OpenGL programming, and speed
doesn't matter to me because I'm triangulating each object only once. Feel free
to make performance adjustmust/cleanup and I'll gladly accept any pull requests.
