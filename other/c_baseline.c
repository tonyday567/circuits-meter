#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define N 100000
#define RUNS 100

static long long nanos(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC_RAW, &ts);
    return (long long)ts.tv_sec * 1000000000LL + ts.tv_nsec;
}

int main(void) {
    int *arr = malloc(N * sizeof(int));
    for (int i = 0; i < N; i++) arr[i] = i + 1;

    long long total = 0;
    volatile long long sink = 0;

    for (int r = 0; r < RUNS; r++) {
        long long t0 = nanos();
        long long sum = 0;
        for (int i = 0; i < N; i++) {
            sum += arr[i];
        }
        long long t1 = nanos();
        total += (t1 - t0);
        sink += sum;
    }

    printf("c sum %d ints x %d runs: total avg %lld ns, %.3f ns/element\n",
           N, RUNS, total / RUNS, (double)total / RUNS / N);
    printf("sink: %lld\n", sink);

    free(arr);
    return 0;
}
