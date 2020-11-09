# metastrip

Metastrip is a tool for stripping metadata for files. It supports the following file formats:

* PNG
* JPG
* BMP
* TIFF

Metastrip can do the following:

* Remove any file metadata
* Subtly randomize pixel values to combat image hashing/steganography
* Generate random filenames

It's fairly fast and can use multiple cores.

Usage is as follows:

```
> metastrip --help
Usage: metastrip ([-r|--random-name] | [-s|--same-name] |
                 [-b|--before-extension PREFIX]) [-o|--output-dir DIR]
                 ([-a|--aggressive] | [-d|--dont-randomize])
                 [-j|--jpeg-quality ARG] [-n|--num-threads INT] FILES...

Available options:
  -r,--random-name         Write output files with a random name
  -s,--same-name           Write output files with the same name as the input
                           files, overwriting if necessary
  -b,--before-extension PREFIX
                           Output a scrubbed copy of a.jpg at
                           a.PREFIX.jpg (default: "scrubbed")
  -o,--output-dir DIR      Output directory (default: Same directory)
  -a,--aggressive          Aggressively randomize pixel values (default: Slight
                           randomization)
  -d,--dont-randomize      Do not randomize pixel values at all
  -j,--jpeg-quality ARG    JPEG save quality (1-100) (default: 50)
  -n,--num-threads INT     Specify number of threads (default: # of cores
                           available)
  -h,--help                Show this help text
```

## Installation

The best way to install from source is to install [The Haskell Stack](https://docs.haskellstack.org/en/stable/README/) and then run `stack install` in this directory. It will usually take a few minutes to build the dependencies and the binary.

## Limitations

Right now, metastrip always decodes and re-encodes the image. This means that if you only want to strip metadata (and not randomize pixel values), metastrip is doing a lot of extra work (the slowest part by far is re-encoding the image) and also introducing a bit of extra error in lossy formats like jpg. Perhaps at some point `--dont-randomize` will avoid actually decoding and re-encoding the image data.

## Security

Parsing complex media formats is extremely fraught, especially for untrusted input. A very large portion of practical RCE vulnerabilities have involved unsafe multimedia parsers/codecs. metastrip is written in Haskell, which can help prevent the construction of such parsers/codecs. However, the codec library used here does use some unsafe constructs for the purpose of performance, so we don't get Haskell's usual "free" guarantee of memory safety. At some point, it would be nice to A) have a non-randomizing option which only used safe-by-construction parsers and didn't touch any (potentially unsafe) codec code and/or B) formally verify the memory safety of the codecs used, e.g. with LiquidHaskell. In any case, this is probably not your top concern.

For fast generation of random data, metastrip uses ChaCha seeded by a strong system-provided entropy source.