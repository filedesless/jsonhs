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
                    
benchmarking aeson decode/100 arrays
time                 144.6 μs   (144.2 μs .. 144.9 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 144.5 μs   (144.4 μs .. 144.8 μs)
std dev              606.8 ns   (386.8 ns .. 1.010 μs)
                    
benchmarking aeson decode/100 objects
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