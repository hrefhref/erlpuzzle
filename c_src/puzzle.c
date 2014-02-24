#include "erl_nif.h"
#include <puzzle.h>
#include <stdio.h>

//extern char cvec_from_file(int argc, const ERL_NIF_TERM argv);

// from http://d.hatena.ne.jp/vostok92/20091201/1259680319
static int my_enif_get_string(ErlNifEnv *env, ERL_NIF_TERM list, char* buf)
{
  ERL_NIF_TERM cell, head, tail;
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

static int fuck(signed char* pute, int length, char* buf)
{
  int i = 0;
  while(i < length) {
    i++;
    *buf = (char)pute[i];
    buf++;
  }
  *buf = '\0';
  return 1;
}


static ERL_NIF_TERM nif_cvec_from_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  //char file;
  char filename[1024];
  signed char* sig;

  memset(filename, 0, sizeof(filename));
  my_enif_get_string(env, argv[0], filename);

  PuzzleContext context;
  PuzzleCvec cvec;

  puzzle_init_context(&context);
  puzzle_init_cvec(&context, &cvec);
  if (puzzle_fill_cvec_from_file(&context, &cvec, filename) != 0) {
    puzzle_free_context(&context);
    puzzle_free_cvec(&context, &cvec);
    return enif_make_atom(env, "puzzle_error");
  }

  ERL_NIF_TERM damnerlangterm;

  unsigned char * bin_buf;
  bin_buf = enif_make_new_binary(env, cvec.sizeof_vec, &damnerlangterm);
  memcpy(bin_buf, cvec.vec, cvec.sizeof_vec);

  puzzle_free_cvec(&context, &cvec);
  puzzle_free_context(&context);
  return damnerlangterm;
}

static ERL_NIF_TERM nif_compare_cvec(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary vec1;
  ErlNifBinary vec2;

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

  return enif_make_double(env, result);
}

static ErlNifFunc nif_funcs[] = {
    {"cvec_from_file", 1, nif_cvec_from_file},
    {"compare_cvec", 3, nif_compare_cvec}
};

ERL_NIF_INIT(puzzle, nif_funcs, NULL, NULL, NULL, NULL)
