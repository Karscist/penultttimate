> this file is more implementer oriented than user oriented, pardon
> it is closer to a "lay of the land"
> what I would have liked to have read when I first got into this project
> and less of an `INSTALL.md`
> but it's far closer to an `INSTALL.md` than to a `README.md` so...

# Notes on build process
## Libraries Used
We use [raylib](https://github.com/raysan5/raylib) and [raygui](https://github.com/raysan5/raygui) for the game graphics and [claw-raylib](https://github.com/bohonghuang/claw-raylib/) to use those two libraries from common lisp code.

The goal of `claw-raylib` is generating c wrappers and lisp files to expose raylib's api to lisp code.
Its backend, `claw`, unfortunately uses [libresect](https://github.com/borodust/libresect) to parse raylib's c source and obtain the information needed to generate these headers and lisp code.  
And `libresect` depends on the author's own fork of `libclang`.  
And ain't nobody got time to build fucking `libclang`.

So we use the [prebuild](https://github.com/bohonghuang/claw-raylib/tree/prebuild) branch of `claw-raylib` that's just `claw-raylib` but all the files and headers have been already created, for a rather wide array of targets, so you don't need to build your own `libclang` to generate them.

## Other Dependencies
We use `sbcl` as our common lisp implementation of choice, the code should be implementation independant but we haven't tested it, and `sbcl` is the default parameter for anything that takes a lisp.

You'll also need a c compiler to build the claw generated wrappers, `clang` and `gcc` should both work but we only tested `gcc` and it's the default one in all our build scripts.

To install the lisp libraries used in this project you'll also need `quicklisp`

## Navigating our Makefiles
We have a main `Makefile` in the root that calls raylib's own `Makefile`, a `build.lisp` script for the generated bindings, and a makefile we made for `raygui`, even if `raygui` doesn't have a makefile.

The flags we pass to raylib's `Makefile`, which determine among other things "do we build the wayland for this?", are currently hardcoded, so we always build the wayland for that, if you wanna change that you'll need to edit the `Makefile`, or call `make` with some parameters that I don't really know.

`rayguiMakefile` implements the one-two liner build procedures from the [raygui readme](https://github.com/raysan5/raygui?tab=readme-ov-file#building) as a 30 line makefile, but has an `uninstall` target, which the readme doesn't have.

`build.lisp` is used to compile the pre-generated raylib binding source code.
The compilation target for the bindings (in this case `x86_64-pc-linux-gnu`) is currently hardcoded inside `build.lisp`, you will have to edit the file if you're compiling them for another target.

## Actually Building It
ha

# Docker Image
For the author to deal with how shit it is to manage cl installations, and mostly because bushek is doing this from wsl, we have a docker image with the basics required to set up a development environment for this thing with wayland  
We don't have it on dockerhub or anything yet, there's just a `Dockerfile` in the project's root directory, copy that somewhere and do your docker things.  

To run the container in such a way that you may run a wayland application on the container and have it actually show up on your desktop there's also a `run-wayland-container.sh` in the project root  

installation through docker image would then amount to
```sh
# wherever it is you copied the dockerfile
mkdir some/directory/whatever
mv Dockerfile some/directory/whatever
cd some/directory/whatever
docker build -t name-you-wanna:give-the-image .

# wherever it is you copied the run-wayland-container.sh script
chmod +x run-wayland-container.sh # it should already be executable but just in case
./run-wayland-container.sh name-you-wanna:give-the-image
```

# Local Install
