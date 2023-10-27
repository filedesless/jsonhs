# Splitting Serializing and Deserializing benchmarks

Turns out serializing was the bottleneck, thanks profiler!

```
Running 1 benchmarks...
Benchmark jsonhs-benchmark: RUNNING...
benchmarking nested arrays/deserialize
time                 16.00 ms   (15.46 ms .. 16.93 ms)
                     0.983 R²   (0.957 R² .. 1.000 R²)
mean                 15.61 ms   (15.39 ms .. 16.18 ms)
std dev              884.3 μs   (226.8 μs .. 1.599 ms)
variance introduced by outliers: 24% (moderately inflated)
                    
benchmarking nested arrays/aeson decode
time                 35.92 ms   (35.73 ms .. 36.09 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 35.77 ms   (35.69 ms .. 35.86 ms)
std dev              213.1 μs   (159.6 μs .. 291.1 μs)
                    
benchmarking nested arrays/json decode
time                 7.085 ms   (7.029 ms .. 7.152 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 7.064 ms   (7.028 ms .. 7.136 ms)
std dev              138.1 μs   (70.08 μs .. 247.0 μs)
                    
benchmarking nested arrays/serialize
time                 4.634 s    (4.438 s .. 4.814 s)
                     1.000 R²   (0.999 R² .. NaN R²)
mean                 4.461 s    (4.369 s .. 4.540 s)
std dev              96.65 ms   (79.87 ms .. 110.4 ms)
variance introduced by outliers: 19% (moderately inflated)
                    
benchmarking nested arrays/aeson encode
time                 3.409 ms   (3.397 ms .. 3.420 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.395 ms   (3.386 ms .. 3.408 ms)
std dev              33.10 μs   (25.27 μs .. 45.69 μs)
                    
benchmarking nested arrays/json encode
time                 673.5 μs   (672.1 μs .. 675.9 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 672.2 μs   (671.0 μs .. 675.0 μs)
std dev              6.058 μs   (3.881 μs .. 8.965 μs)
                    
benchmarking nested objects/deserialize
time                 1.051 μs   (1.049 μs .. 1.052 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.053 μs   (1.051 μs .. 1.055 μs)
std dev              5.564 ns   (4.050 ns .. 7.684 ns)
                    
benchmarking nested objects/aeson decode
time                 1.636 μs   (1.633 μs .. 1.639 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.636 μs   (1.634 μs .. 1.638 μs)
std dev              6.321 ns   (5.137 ns .. 8.325 ns)
                    
benchmarking nested objects/json decode
time                 1.548 μs   (1.545 μs .. 1.552 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.548 μs   (1.546 μs .. 1.553 μs)
std dev              11.16 ns   (6.924 ns .. 18.07 ns)
                    
benchmarking nested objects/serialize
time                 19.13 ns   (19.08 ns .. 19.17 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 19.11 ns   (19.08 ns .. 19.13 ns)
std dev              91.03 ps   (75.19 ps .. 113.3 ps)
                    
benchmarking nested objects/aeson encode
time                 17.54 ns   (17.51 ns .. 17.58 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 17.56 ns   (17.53 ns .. 17.63 ns)
std dev              171.2 ps   (124.9 ps .. 242.9 ps)
                    
benchmarking nested objects/json encode
time                 197.2 ns   (197.0 ns .. 197.4 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 197.6 ns   (197.2 ns .. 198.2 ns)
std dev              1.674 ns   (1.140 ns .. 2.276 ns)
                    
benchmarking long strings/deserialize
time                 14.91 ms   (14.79 ms .. 15.00 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 14.84 ms   (14.76 ms .. 14.92 ms)
std dev              214.4 μs   (131.9 μs .. 325.9 μs)
                    
benchmarking long strings/aeson decode
time                 3.244 ms   (3.218 ms .. 3.274 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 3.279 ms   (3.254 ms .. 3.317 ms)
std dev              89.64 μs   (59.31 μs .. 118.3 μs)
variance introduced by outliers: 12% (moderately inflated)
                    
benchmarking long strings/json decode
time                 1.244 μs   (1.238 μs .. 1.251 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.239 μs   (1.237 μs .. 1.243 μs)
std dev              9.709 ns   (6.405 ns .. 14.13 ns)
                    
benchmarking long strings/serialize
time                 2.772 ms   (2.761 ms .. 2.790 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 2.773 ms   (2.766 ms .. 2.790 ms)
std dev              34.72 μs   (21.30 μs .. 54.09 μs)
                    
benchmarking long strings/aeson encode
time                 1.753 ms   (1.750 ms .. 1.756 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.748 ms   (1.746 ms .. 1.750 ms)
std dev              6.662 μs   (5.111 μs .. 9.944 μs)
                    
benchmarking long strings/json encode
time                 313.9 ns   (312.7 ns .. 315.3 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 313.5 ns   (312.8 ns .. 314.7 ns)
std dev              3.073 ns   (2.015 ns .. 4.360 ns)
                    
Benchmark jsonhs-benchmark: FINISH
```

# First attempt

Good ego check

```
Running 1 benchmarks...
Benchmark jsonhs-benchmark: RUNNING...
benchmarking deserialize/1000 arrays
time                 16.22 ms   (15.81 ms .. 16.56 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 15.90 ms   (15.83 ms .. 16.04 ms)
std dev              255.3 μs   (170.8 μs .. 384.6 μs)
                    
benchmarking deserialize/1000 objects
time                 384.9 ns   (383.9 ns .. 386.2 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 385.6 ns   (384.6 ns .. 387.6 ns)
std dev              4.796 ns   (2.758 ns .. 7.095 ns)
variance introduced by outliers: 11% (moderately inflated)
                    
benchmarking deserialize/long string
time                 10.59 ms   (10.41 ms .. 10.98 ms)
                     0.993 R²   (0.980 R² .. 1.000 R²)
mean                 10.62 ms   (10.54 ms .. 10.84 ms)
std dev              372.6 μs   (133.6 μs .. 727.8 μs)
variance introduced by outliers: 13% (moderately inflated)
                    
benchmarking aeson decode/1000 arrays
time                 144.6 μs   (144.2 μs .. 144.9 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 144.5 μs   (144.4 μs .. 144.8 μs)
std dev              606.8 ns   (386.8 ns .. 1.010 μs)
                    
benchmarking aeson decode/1000 objects
time                 295.8 ns   (293.5 ns .. 299.1 ns)
                     0.997 R²   (0.994 R² .. 1.000 R²)
mean                 300.5 ns   (295.2 ns .. 317.1 ns)
std dev              28.72 ns   (13.88 ns .. 53.26 ns)
variance introduced by outliers: 89% (severely inflated)
                    
benchmarking aeson decode/long string
time                 516.1 μs   (515.2 μs .. 517.4 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 517.0 μs   (516.2 μs .. 519.4 μs)
std dev              4.259 μs   (2.315 μs .. 7.948 μs)
                    
Benchmark jsonhs-benchmark: FINISH
```