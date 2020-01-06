cl-wavelets
===========
[![Build Status](https://travis-ci.com/shamazmazum/cl-wavelets.svg?branch=master)](https://travis-ci.com/shamazmazum/cl-wavelets)

**cl-wavelets** is a library with a set of algorithms related to
various kinds of wavelet transform. Currently they all work with
arrays of type `(simple-array (signed-byte 32) (*))` (i.e. they are
1D). This makes this library suitable for audio processing and
compression.

Currently supported algorithms:
* DWT
* Frequency analysis using PWT.

Currently supported wavelets:
* Haar wavelet
* CDF (2,2) wavelet
* CDF (4,2) wavelet

### Usage

In examples:
~~~~
(wavelets:dwt (make-array 8
                          :element-type     '(signed-byte 32)
                          :initial-contents '(0 1 2 3 4 5 7 8))
              :wavelet        :cdf-2-2
              :boundary-style :mirror)
#(2 4 0 3 0 0 0 1)

(wavelets:dwt-inverse *
                      :wavelet        :cdf-2-2
                      :boundary-style :mirror)
#(0 1 2 3 4 5 7 8)
~~~~

Generally, there are two kinds of functions: with `!` at the end and without
`!`. Whose with `!` are in-place functions, in other words they modify their
first argument. Whose without `!` do not modify their first argument.

For more info, generate a documentation with **codex** like so:
`codex:document :cl-wavelets :skip-unsocumented t`.

### Examples package

You can load `cl-wavelets/examples` system which contains
`cl-wavelets-examples` package which demonstrates some components of
this library. For example, you can build a spectrogram of an
uncompressed WAV file using `wavelets:frequency-domain` function. To
build a spectrogram, try this:
`wavelets-examples:spectrogram "/path/to/audio.wav" "/path/to/spectrogram.jpg"`
This will produce `spectrogram.jpg` image with the spectrogram. Note,
that the time axis is the vertical one, with the time going
up-down. This is a spectrogram rotated by 90 degrees clockwise,
actually.