# ErlPuzzle - libpuzzle NIF for Erlang

> The Puzzle library is designed to quickly find visually similar images (gif, png, jpg), even if they have been resized, recompressed, recolored or slightly modified. The library is free, lightweight yet very fast, configurable, easy to use and it has been designed with security in mind.

[http://www.pureftpd.org/project/libpuzzle](http://www.pureftpd.org/project/libpuzzle)

You'll need:

* erlang!!
* libpuzzle
* gd (+ jpg, + png).

```erlang
application:start(puzzle).

Cvec = puzzle:cvec_from_file("/path/to/an/image").
Cvec2 = puzzle:cvec_from_file("/path/to/another/image").

Distance = puzzle:compare_cvec(Cvec, Cvec2, fix_for_texts).
%% fix_for_texts is either 0 (disabled) or 1 (enabled).


Distance < puzzle:threshold()
%% if true, images are similar. :)
```

It's a work in progress. The distance I get from erlang is different from the distance I get from PHP, but it's still
working fine:

```
(shell)> Cvec = puzzle:cvec_from_file("/Users/j/Desktop/test.jpg").
(shell)> Cvec2 = puzzle:cvec_from_file("/Users/j/Desktop/test2.jpg").
(shell)> Distance = puzzle:compare_cvec(Cvec, Cvec2, 1).
0.30389097843791985
(shell)> Distance < puzzle:threshold().
true
```

Versus PHP:

```
$cvec1 = puzzle_fill_cvec_from_file('/Users/j/Desktop/test.jpg');
$cvec2 = puzzle_fill_cvec_from_file('/Users/j/Desktop/test2.jpg');

$d = puzzle_vector_normalized_distance($cvec1, $cvec2);

echo($d);
echo("\n");
echo($d < PUZZLE_CVEC_SIMILARITY_THRESHOLD);

==> 0.30389097843792
1
```

TODO:
* Support Cvec compression/uncompression
* Find how to compile it properly without setting paths in rebar.config ;)
* Write tests
* ...

Licence: same as libpuzzle, 2-clauses BSD public license

