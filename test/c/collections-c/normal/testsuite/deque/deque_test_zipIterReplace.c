#include "deque.h"
#include "owi.h"
#include "utils.h"

static Deque *deque;
static DequeConf conf;
int stat;

void setup_tests() { stat = deque_new(&deque); }

void teardown_tests() { deque_destroy(deque); }

int main() {
  setup_tests();

  int a = owi_i32();
  owi_assume(a > 0);
  owi_assume(a < 127);
  char str_a[] = {a, '\0'};

  int b = owi_i32();
  owi_assume(b > 0);
  owi_assume(b < 127);
  char str_b[] = {b, '\0'};

  int c = owi_i32();
  owi_assume(c > 0);
  owi_assume(c < 127);
  char str_c[] = {c, '\0'};

  int d = owi_i32();
  owi_assume(d > 0);
  owi_assume(d < 127);
  char str_d[] = {d, '\0'};

  int e = owi_i32();
  owi_assume(e > 0);
  owi_assume(e < 127);
  char str_e[] = {e, '\0'};

  int f = owi_i32();
  owi_assume(f > 0);
  owi_assume(f < 127);
  char str_f[] = {f, '\0'};

  int g = owi_i32();
  owi_assume(g > 0);
  owi_assume(g < 127);
  char str_g[] = {g, '\0'};

  owi_assume(b != a && b != c && b != d);

  deque_add(deque, str_a);
  deque_add(deque, str_b);
  deque_add(deque, str_c);
  deque_add(deque, str_d);

  Deque *d2;
  deque_new(&d2);

  deque_add(d2, str_e);
  deque_add(d2, str_f);
  deque_add(d2, str_g);

  symb_str(h);
  symb_str(i);

  DequeZipIter zip;
  deque_zip_iter_init(&zip, deque, d2);

  void *e1, *e2;
  void *r1, *r2;
  while (deque_zip_iter_next(&zip, &e1, &e2) != CC_ITER_END) {
    if (strcmp((char *)e1, str_b) == 0)
      deque_zip_iter_replace(&zip, str_h, str_i, &r1, &r2);
  }

  size_t index;
  deque_index_of(deque, str_h, &index);

  owi_assert(1 == index);

  deque_index_of(deque, str_i, &index);
  owi_assert(1 == index);
  owi_assert(1 == deque_contains(deque, str_h));
  owi_assert(1 == deque_contains(d2, str_i));
  deque_destroy(d2);

  teardown_tests();
  return 0;
}
