#include <owi.h>
#include <stdlib.h>

void arbitrary_pre_align() {
    int s = owi_i32();
    int m = (s < 8) ? s : 7;
    for (int i = 0; i < m; i++) {
        malloc(1);
    }
}

int main() {
    malloc(1024);
    arbitrary_pre_align();
    for (int p = 0; p < 10; p++) {
        int low_pow = (1 << p);
        for (int i = low_pow; i < 2*low_pow; i++) {
            void * ptr = malloc(i);
            if (low_pow < 16) {
                owi_assert((int) ptr % low_pow == 0);
            } else {
                owi_assert((int) ptr % 16 == 0);
            }
        }
    }
}