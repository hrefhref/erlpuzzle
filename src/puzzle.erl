-module(puzzle).

-export([cvec_from_file/1, compare_cvec/2, compare_cvec/3, threshold/0, threshold/1]).

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

%% Theses thresholds are defined in libpuzzle but I didn't knew how to query them easily from erlang so... :)
threshold() -> 0.6.
threshold(high) -> 0.7;
threshold(low) -> 0.3;
threshold(lower) -> 0.2.

