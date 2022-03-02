# Translator

`cabal new-run translator ../ProgramFlow/FibonacciSeries/FibonacciSeries.vm`

Below is a little script in fish shell that I use to test all the .tst files that don't end in VME.tst
```fish
for file in ../*/*/*.tst
    if string match -v -r '.*VME\.tst' $file;
        ../../../tools/CPUEmulator.sh $file
    end
end
```

```bash
cabal new-run translator ../FunctionCalls/FibonacciElement/
cabal new-run translator ../FunctionCalls/NestedCall/
cabal new-run translator ../FunctionCalls/SimpleFunction/SimpleFunction.vm
cabal new-run translator ../FunctionCalls/StaticsTest/
cabal new-run translator ../ProgramFlow/BasicLoop/BasicLoop.vm
cabal new-run translator ../ProgramFlow/FibonacciSeries/FibonacciSeries.vm
```
