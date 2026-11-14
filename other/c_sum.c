#include <stdio.h>
#include <stdlib.h>
#include <time.h>

static long long nanos(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC_RAW, &ts);
    return (long long)ts.tv_sec * 1000000000LL + ts.tv_nsec;
}

__attribute__((noinline)) static long long sum_to(long long n) {
    long long sum = 0;
    for (long long i = 1; i <= n; ++i) {
        sum += i;
        __asm__ volatile("" : "+r"(sum));
    }
    return sum;
}

__attribute__((noinline)) static long long sum_array(const long long *values, long long n) {
    long long sum = 0;
    for (long long i = 0; i < n; ++i) sum += values[i];
    return sum;
}

static int compare_nanos(const void *left, const void *right) {
    long long a = *(const long long *)left;
    long long b = *(const long long *)right;
    return (a > b) - (a < b);
}

int main(int argc, char **argv) {
    if (argc != 3) {
        fprintf(stderr, "usage: %s RUNS LENGTH\n", argv[0]);
        return 2;
    }

    int runs = atoi(argv[1]);
    long long length = atoll(argv[2]);
    if (runs <= 0 || length <= 0) {
        fprintf(stderr, "RUNS and LENGTH must be positive\n");
        return 2;
    }

    long long *samples = malloc((size_t)runs * sizeof(*samples));
    long long *array_samples = malloc((size_t)runs * sizeof(*array_samples));
    long long *values = malloc((size_t)length * sizeof(*values));
    if (samples == NULL || array_samples == NULL || values == NULL) {
        perror("malloc");
        free(samples);
        free(array_samples);
        free(values);
        return 2;
    }
    for (long long i = 0; i < length; ++i) values[i] = i + 1;

    for (int i = 0; i < 1000; ++i) (void)nanos();

    long long total = 0;
    long long checksum = 0;
    for (int r = 0; r < runs; ++r) {
        long long before = nanos();
        checksum = sum_to(length);
        long long after = nanos();
        samples[r] = after - before;
        total += samples[r];
    }

    long long array_total = 0;
    long long array_checksum = 0;
    for (int r = 0; r < runs; ++r) {
        long long before = nanos();
        array_checksum = sum_array(values, length);
        long long after = nanos();
        array_samples[r] = after - before;
        array_total += array_samples[r];
    }

    long long expected = length * (length + 1) / 2;
    if (checksum != expected || array_checksum != expected) {
        fprintf(stderr, "checksum mismatch: %lld and %lld != %lld\n",
                checksum, array_checksum, expected);
        free(samples);
        free(array_samples);
        free(values);
        return 1;
    }

    qsort(samples, (size_t)runs, sizeof(*samples), compare_nanos);
    qsort(array_samples, (size_t)runs, sizeof(*array_samples), compare_nanos);
    printf("c sum 1..%lld x %d: p10=%lldns p50=%lldns p90=%lldns avg=%lldns\n",
           length, runs, samples[runs / 10], samples[runs / 2],
           samples[runs * 9 / 10], total / runs);
    printf("c array sum 1..%lld x %d: p10=%lldns p50=%lldns p90=%lldns avg=%lldns\n",
           length, runs, array_samples[runs / 10], array_samples[runs / 2],
           array_samples[runs * 9 / 10], array_total / runs);
    printf("c sum checksums: result=%lld array=%lld expected=%lld\n",
           checksum, array_checksum, expected);

    free(samples);
    free(array_samples);
    free(values);
    return 0;
}
