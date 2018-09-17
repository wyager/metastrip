# metastrip

Metastrip is a tool for stripping metadata for files. It supports the following file formats:

* PNG
* JPG
* BMP
* TIFF

Metastrip can do the following:

* Remove any file metadata
* Slightly randomize pixel values to fight image hashing/steganography
* Generate random filenames

It's reasonably fast and can use multiple cores.

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