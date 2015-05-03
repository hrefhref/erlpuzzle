-module(puzzle).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([cvec_from_file/1, compare_cvec/2, compare_cvec/3, compress_cvec/1, uncompress_cvec/1,
         compare_compressed_cvec/2, threshold/0, threshold/1]).

-on_load(on_load/0).

on_load() ->
  BaseDir =
    case code:priv_dir(?MODULE) of
      {error, bad_name} ->
        EbinDir = filename:dirname(code:which(?MODULE)),
        AppPath = filename:dirname(EbinDir),
        filename:join(AppPath, "priv");
      Dir ->
        Dir
      end,
  SoName = filename:join(BaseDir, atom_to_list(?MODULE)),
  erlang:load_nif(SoName, 0).

cvec_from_file(_file) ->
  "NIF library not loaded".

compare_cvec(_cvec1, _cvec2) ->
  "NIF not loaded.".

compare_cvec(_cvec1, _cvec2, _fix_for_texts) ->
  "NIF not loaded.".

compress_cvec(_cvec) ->
  "NIF not loaded.".

uncompress_cvec(_ccvec) ->
  "NIF not loaded.".

compare_compressed_cvec(Ccvec1, Ccvec2) ->
  {ok,Cvec1} = uncompress_cvec(Ccvec1),
  {ok,Cvec2} = uncompress_cvec(Ccvec2),
  compare_cvec(Cvec1, Cvec2).

%% Theses thresholds are defined in libpuzzle but I didn't knew how to query them easily from erlang so... :)
threshold() -> 0.6.
threshold(high) -> 0.7;
threshold(low) -> 0.3;
threshold(lower) -> 0.2.

-ifdef(TEST).

simple_test() ->
  ok = application:start(puzzle),
  {ok, Dir} = file:get_cwd(),

  %% Create Cvec
  {Success1, Cvec1} = puzzle:cvec_from_file(Dir++"/../test/a_1.jpg"),
  io:format("~p~n", [Dir++"/test/a_1.jpg"]),
  io:format("~p~n", [Success1]),
  ?assert(ok == Success1),
  {Success2, Cvec2} = puzzle:cvec_from_file(Dir++"/../test/a_2.jpg"),
  ?assert(ok == Success2),
  {Error, _} = puzzle:cvec_from_file(Dir++"/nope.jpg"),
  ?assert(error == Error),

  %% Compare
  {Success3, Distance} = puzzle:compare_cvec(Cvec1, Cvec2),
  ?assert(ok == Success3),
  ?assert(0.02762894819977688 == Distance),

  %% Compress
  {Success4, Ccvec1} = puzzle:compress_cvec(Cvec1),
  {_, Ccvec2} = puzzle:compress_cvec(Cvec2),
  ?assert(ok == Success4),
  {Success5, Ucvec1} = puzzle:uncompress_cvec(Ccvec1),
  ?assert(ok == Success5),
  ?assert(Ucvec1 == Cvec1),
  {Success6, Distance2} = puzzle:compare_compressed_cvec(Ccvec1, Ccvec2),
  ?assert(ok == Success6),
  ?assert(Distance == Distance2).

-endif.

