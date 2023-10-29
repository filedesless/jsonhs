# Improving Serializing using ShowS

This allows constant time string concatenation using function composition

```
Benchmark jsonhs-benchmark: RUNNING...
benchmarking nested arrays/deserialize
time                 8.460 ms   (8.279 ms .. 8.589 ms)
                     0.989 R²   (0.965 R² .. 0.999 R²)
mean                 8.459 ms   (8.329 ms .. 8.944 ms)
std dev              640.1 μs   (139.2 μs .. 1.303 ms)
variance introduced by outliers: 41% (moderately inflated)
                    
benchmarking nested arrays/aeson decode
time                 4.594 ms   (4.419 ms .. 4.813 ms)
                     0.992 R²   (0.986 R² .. 0.997 R²)
mean                 4.292 ms   (4.234 ms .. 4.395 ms)
std dev              230.4 μs   (170.1 μs .. 328.4 μs)
variance introduced by outliers: 31% (moderately inflated)
                    
benchmarking nested arrays/json decode
time                 1.629 ms   (1.595 ms .. 1.673 ms)
                     0.994 R²   (0.989 R² .. 0.998 R²)
mean                 1.606 ms   (1.580 ms .. 1.633 ms)
std dev              93.86 μs   (75.36 μs .. 117.9 μs)
variance introduced by outliers: 44% (moderately inflated)
                    
benchmarking nested arrays/serialize
time                 635.3 μs   (629.2 μs .. 642.3 μs)
                     0.996 R²   (0.986 R² .. 1.000 R²)
mean                 647.5 μs   (635.8 μs .. 690.2 μs)
std dev              73.95 μs   (12.00 μs .. 155.8 μs)
variance introduced by outliers: 80% (severely inflated)
                    
benchmarking nested arrays/aeson encode
time                 529.8 μs   (527.9 μs .. 532.7 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 530.9 μs   (529.1 μs .. 533.6 μs)
std dev              8.023 μs   (5.168 μs .. 10.94 μs)
                    
benchmarking nested arrays/json encode
time                 214.8 μs   (210.2 μs .. 221.6 μs)
                     0.994 R²   (0.989 R² .. 0.999 R²)
mean                 211.9 μs   (209.7 μs .. 219.6 μs)
std dev              12.21 μs   (8.402 μs .. 21.27 μs)
variance introduced by outliers: 56% (severely inflated)
                    
benchmarking long strings/deserialize
time                 9.484 ms   (9.395 ms .. 9.614 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 9.319 ms   (9.246 ms .. 9.375 ms)
std dev              167.5 μs   (125.3 μs .. 213.0 μs)
                    
benchmarking long strings/aeson decode
time                 1.705 ms   (1.696 ms .. 1.719 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.706 ms   (1.698 ms .. 1.720 ms)
std dev              37.42 μs   (25.14 μs .. 56.72 μs)
                    
benchmarking long strings/json decode
time                 543.3 ns   (542.1 ns .. 544.4 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 542.7 ns   (541.7 ns .. 544.1 ns)
std dev              4.188 ns   (2.518 ns .. 6.721 ns)
                    
benchmarking long strings/serialize
time                 1.943 ms   (1.937 ms .. 1.952 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.941 ms   (1.939 ms .. 1.946 ms)
std dev              12.03 μs   (7.685 μs .. 19.01 μs)
                    
benchmarking long strings/aeson encode
time                 106.0 μs   (105.7 μs .. 106.3 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 106.3 μs   (106.0 μs .. 106.7 μs)
std dev              1.239 μs   (876.1 ns .. 1.621 μs)
                    
benchmarking long strings/json encode
time                 218.3 ns   (217.8 ns .. 218.8 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 217.5 ns   (217.3 ns .. 218.0 ns)
std dev              1.035 ns   (808.0 ps .. 1.298 ns)
                    
Benchmark jsonhs-benchmark: FINISH
```


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
                    
benchmarking aeson decode/long string
time                 516.1 μs   (515.2 μs .. 517.4 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 517.0 μs   (516.2 μs .. 519.4 μs)
std dev              4.259 μs   (2.315 μs .. 7.948 μs)
                    
Benchmark jsonhs-benchmark: FINISH
```