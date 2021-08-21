# hsbf
![build](https://github.com/leonardoarroyo/hsbf/actions/workflows/test.yml/badge.svg)
[![codecov](https://codecov.io/gh/leonardoarroyo/hsbf/branch/master/graph/badge.svg?token=D6I4MOV4US)](https://codecov.io/gh/leonardoarroyo/hsbf)
----
hsbf is a small brainfuck interpreter written in Haskell.

## Usage
```
Usage: hsbf [-v|--version] [FILES]
  Interpret brainfuck source code for FILES

Available options:
  -v,--version             Print interpreter version
  -h,--help                Show this help text
```

## Download
You can find binary releases for linux at the [releases page](https://github.com/leonardoarroyo/hsbf/releases). There is no macOS release for now, but you should be able to build from source with `stack build`.

## Status
The current hsbf release is a fully functional brainfuck interpreter, but still under development. Here are some of the stuff in the todo list:
* Improve test suite coverage
* Add debugging capabilities
* Maybe turn this into a compiler for fun
