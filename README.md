# OFDM

This repository contains some blocks useful for making OFDM transceivers.
This includes:

* Autocorrelator
* CORDIC (pipelined and iterative)
* Divider
* Filters
* NCO

as well as some higher level blocks that combine these to do useful OFDM things.

`src/main/resources` contains some traces from the ADI board that are useful for testing.

## Running
To generate an autocorrelator, run
```
sbt "runMain ofdm.GenerateSyncVerilog"
```


## Testing
To run a test of the autocorrelator and generate a nice plot, waveforms, etc., run
```
sbt "testOnly ofdm.SyncSpec
```

The waveform will be located at `test_run_dir/ofdm.SyncSpec{RandomNumber}/Sync.vcd`.
