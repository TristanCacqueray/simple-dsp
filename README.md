# simple-dsp

This library provides DSP primitives along with a simple audio player.

## Overview and scope

The goal of simple-dsp is to implement IIR filters for sound processing, for example, to remove the low frequencies from an input sound.

Learning resources:

- https://ocw.mit.edu/courses/6-003-signals-and-systems-fall-2011/ (complete)
- https://dspguru.com/dsp/howtos/implement-iir-filters/ (presentation)
- https://en.wikipedia.org/wiki/Digital_biquad_filter (reference)

## Usage

Use the [SimpleDSP.IIR](./src/SimpleDSP/IIR.hs) in your application.

Start the audio player by passing a sound file:

```ShellSession
cabal run -O0 exe:simple-dsp-player -- ~/86quantoc.flac
```

![simple-dsp](https://github.com/TristanCacqueray/simple-dsp/assets/154392/57207ef1-6efe-4b64-9574-d118d1d61471)


## SDL_GL_CreateContext error and nix

If the program fails with:

```ShellSession
simple-dsp-player: SDLCallFailed {sdlExceptionCaller = "SDL.Video.glCreateContext", sdlFunction = "SDL_GL_CreateContext", sdlExceptionError = "Invalid window"}
```

You need to use `nixGL`. To install the right version (see hspkgs input to match the nixpkgs pin):

```ShellSession
nix profile install --override-input nixpkgs github:NixOS/nixpkgs/22c5bd85d8478e24874ff2b80875506f5c3711a6 --impure github:guibou/nixGL
```

Then run:

```ShellSession
nixGL cabal run exe:simple-dsp-player -- $SOUND_FILE_PATH
```
