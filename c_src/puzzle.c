#include "erl_nif.h"
#include <string.h>
#include <puzzle.h>

// from http://d.hatena.ne.jp/vostok92/20091201/1259680319
static int my_enif_get_string(ErlNifEnv *env, ERL_NIF_TERM list, char* buf)
{
  ERL_NIF_TERM head, tail;
  int val;

  while (enif_get_list_cell(env, list, &head, &tail)) {
    if (!enif_get_int(env, head, &val)) {
      return 0;
    }
    *buf = (char)val;
    buf++;
    list = tail;
  }
  *buf = '\0';

  return 1;
}

static ERL_NIF_TERM return_error(ErlNifEnv* env, const char* error_name)
{
  ERL_NIF_TERM atom_error;
  ERL_NIF_TERM atom_error_name;
  atom_error = enif_make_atom(env, "error");
  atom_error_name = enif_make_atom(env, error_name);
  return enif_make_tuple2(env, atom_error, atom_error_name);
}

static ERL_NIF_TERM return_ok(ErlNifEnv* env, ERL_NIF_TERM term)
{
  ERL_NIF_TERM atom_ok;
  atom_ok = enif_make_atom(env, "ok");
  return enif_make_tuple2(env, atom_ok, term);
}

static ERL_NIF_TERM nif_cvec_from_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  //char file;
  char filename[1024];

  memset(filename, 0, sizeof(filename));
  my_enif_get_string(env, argv[0], filename);

  PuzzleContext context;
  PuzzleCvec cvec;

  puzzle_init_context(&context);
  puzzle_init_cvec(&context, &cvec);
  if (puzzle_fill_cvec_from_file(&context, &cvec, filename) != 0) {
    puzzle_free_context(&context);
    puzzle_free_cvec(&context, &cvec);
    return return_error(env, "puzzle_error");
  }

  ERL_NIF_TERM damnerlangterm;

  unsigned char * bin_buf;
  bin_buf = enif_make_new_binary(env, cvec.sizeof_vec, &damnerlangterm);
  memcpy(bin_buf, cvec.vec, cvec.sizeof_vec);

  puzzle_free_cvec(&context, &cvec);
  puzzle_free_context(&context);
  return return_ok(env, damnerlangterm);
}

static ERL_NIF_TERM nif_compare_cvec(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary vec1;
  ErlNifBinary vec2;
  ERL_NIF_TERM result_term;

  PuzzleContext context;
  PuzzleCvec cvec1;
  PuzzleCvec cvec2;

  int fix_for_texts;

  double result;

  if (!enif_inspect_binary(env, argv[0], &vec1) || !enif_inspect_binary(env, argv[1], &vec2))
  {
	return enif_make_badarg(env);
  }

  if (!enif_get_uint(env, argv[2], &fix_for_texts))
  {
	fix_for_texts = 1;
  }

  puzzle_init_cvec(&context, &cvec1);
  puzzle_init_cvec(&context, &cvec2);
  cvec1.vec = vec1.data;
  cvec1.sizeof_vec = (size_t) vec1.size;
  cvec2.vec = vec2.data;
  cvec2.sizeof_vec = (size_t) vec2.size;

  result = puzzle_vector_normalized_distance(&context, &cvec1, &cvec2, fix_for_texts);
  result_term = enif_make_double(env, result);
  //puzzle_free_cvec(&context, &cvec1);
  //puzzle_free_cvec(&context, &cvec2);

  return return_ok(env, result_term);
}

static ERL_NIF_TERM nif_compress_cvec(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary vec;

  PuzzleContext context;
  PuzzleCvec cvec;
  PuzzleCompressedCvec ccvec;

  if (!enif_inspect_binary(env, argv[0], &vec))
  {
	return enif_make_badarg(env);
  }

  puzzle_init_cvec(&context, &cvec);
  cvec.vec = vec.data;
  cvec.sizeof_vec = (size_t) vec.size;
  puzzle_init_compressed_cvec(&context, &ccvec);

  if (puzzle_compress_cvec(&context, &ccvec, &cvec) != 0) {
    puzzle_free_compressed_cvec(&context, &ccvec);
    puzzle_free_cvec(&context, &cvec);
    puzzle_free_context(&context);
    return return_error(env, "puzzle_error");
  }

  ERL_NIF_TERM damnerlangterm;

  unsigned char * bin_buf;
  bin_buf = enif_make_new_binary(env, ccvec.sizeof_compressed_vec, &damnerlangterm);
  memcpy(bin_buf, ccvec.vec, ccvec.sizeof_compressed_vec);

  //puzzle_free_cvec(&context, &cvec);
  //puzzle_free_compressed_cvec(&context, &ccvec);
  //puzzle_free_context(&context);

  return return_ok(env, damnerlangterm);
}

static ERL_NIF_TERM nif_uncompress_cvec(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary vec;

  PuzzleContext context;
  PuzzleCvec cvec;
  PuzzleCompressedCvec ccvec;

  if (!enif_inspect_binary(env, argv[0], &vec))
  {
	return enif_make_badarg(env);
  }

  puzzle_init_compressed_cvec(&context, &ccvec);
  ccvec.vec = vec.data;
  ccvec.sizeof_compressed_vec = (size_t) vec.size;
  puzzle_init_cvec(&context, &cvec);

  if (puzzle_uncompress_cvec(&context, &ccvec, &cvec) != 0) {
    puzzle_free_compressed_cvec(&context, &ccvec);
    puzzle_free_cvec(&context, &cvec);
    puzzle_free_context(&context);
    return return_error(env, "puzzle_error");
  }

  ERL_NIF_TERM damnerlangterm;

  unsigned char * bin_buf;
  bin_buf = enif_make_new_binary(env, cvec.sizeof_vec, &damnerlangterm);
  memcpy(bin_buf, cvec.vec, cvec.sizeof_vec);

  //puzzle_free_cvec(&context, &cvec);
  //puzzle_free_compressed_cvec(&context, &ccvec);
  //puzzle_free_context(&context);

  return return_ok(env, damnerlangterm);
}

static ErlNifFunc nif_funcs[] = {
  {"cvec_from_file", 1, nif_cvec_from_file, ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"compare_cvec", 2, nif_compare_cvec},
  {"compare_cvec", 3, nif_compare_cvec},
  {"compress_cvec", 1, nif_compress_cvec},
  {"uncompress_cvec", 1, nif_uncompress_cvec}
};

ERL_NIF_INIT(puzzle, nif_funcs, NULL, NULL, NULL, NULL);

