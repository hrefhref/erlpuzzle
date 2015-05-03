# ErlPuzzle - libpuzzle NIF for Erlang

> The Puzzle library is designed to quickly find visually similar images (gif, png, jpg), even if they have been resized, recompressed, recolored or slightly modified. The library is free, lightweight yet very fast, configurable, easy to use and it has been designed with security in mind.

[http://www.pureftpd.org/project/libpuzzle](http://www.pureftpd.org/project/libpuzzle)

You'll need:

* erlang!!
* libpuzzle
* gd (+ jpg, + png).

**Warning** to create signatures from files (`cvec_from_file/1`) you'll need to build Erlang with dirty schedulers
support (`--enable-dirty-schedulers`).

```erlang
application:start(puzzle).

{ok,Cvec} = puzzle:cvec_from_file("/path/to/an/image").
{ok,Cvec2} = puzzle:cvec_from_file("/path/to/another/image").

{ok,Distance} = puzzle:compare_cvec(Cvec, Cvec2, fix_for_texts).
%% fix_for_texts is either 0 (disabled) or 1 (enabled).

Distance < puzzle:threshold()
%% if true, images are similar. :)

%% -- Compression
{ok,Ccvec} = puzzle:compress_cvec(Cvec).
{ok,Ccvec2} = puzzle:compress_cvec(Cvec2).

{ok,Ucvec} = puzzle:uncompress_cvec(Ccvec).
Ucvec == Cvec. %% true

%% Compare compressed cvec
{ok,DistanceC} = puzzle:compare_compressed_cvec(Ccvec, Ccvec2).
Distance == DistanceC. %% true

```

Licence: same as libpuzzle, 2-clauses BSD public license

