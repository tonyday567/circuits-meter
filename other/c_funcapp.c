#include <stdio.h>
#include <time.h>

#define N 10000000
#define RUNS 100

static long long nanos(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC_RAW, &ts);
    return (long long)ts.tv_sec * 1000000000LL + ts.tv_nsec;
}

__attribute__((noinline)) int c_const_unit(int x) { return x; }

volatile int sink = 0;

void loop_empty(int n) {
    for (int i = 0; i < n; i++) {
        sink = i;
    }
}

void loop_const(int n) {
    int (*volatile f)(int) = c_const_unit;
    for (int i = 0; i < n; i++) {
        sink = f(i);
    }
}

int main(void) {
    long long total_empty = 0;
    long long total_const = 0;

    for (int r = 0; r < RUNS; r++) {
        long long t0 = nanos();
        loop_empty(N);
        long long t1 = nanos();
        total_empty += (t1 - t0);

        t0 = nanos();
        loop_const(N);
        t1 = nanos();
        total_const += (t1 - t0);
    }

    printf("c empty loop %d x %d: avg %lld ns, %.3f ns/iter\n",
           N, RUNS, total_empty / RUNS, (double)total_empty / RUNS / N);
    printf("c const unit %d x %d: avg %lld ns, %.3f ns/iter\n",
           N, RUNS, total_const / RUNS, (double)total_const / RUNS / N);
    printf("delta per const () call: %.3f ns\n",
           (double)(total_const - total_empty) / RUNS / N);

    return 0;
}
