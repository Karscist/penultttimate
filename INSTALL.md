> this file is more implementer oriented than user oriented, pardon
> it is closer to a "lay of the land"
> it's what I would have liked to have read when I first got into this project
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
You will also need `make` to run the build scripts in question.

To install the lisp libraries used in this project you'll also need `quicklisp`

## Navigating our Makefiles
We have a main `Makefile` in the root that calls raylib's own `Makefile`, a `build.lisp` script for the generated bindings, and a makefile we made for `raygui`, even if `raygui` doesn't have a makefile.

The flags we pass to raylib's `Makefile`, which determine among other things "do we build the wayland for this?", are currently hardcoded, so we always build the wayland for that, if you wanna change that you'll need to edit the `Makefile`, or call `make` with some parameters that I don't really know.

`rayguiMakefile` implements the one-two liner build procedures from the [raygui readme](https://github.com/raysan5/raygui?tab=readme-ov-file#building) as a 30 line makefile, but has an `uninstall` target, which the readme doesn't have.

`build.lisp` is used to compile the pre-generated raylib binding source code.
The compilation target for the bindings (in this case `x86_64-pc-linux-gnu`) is currently hardcoded inside `build.lisp`, you will have to edit the file if you're compiling them for another target.

# Docker Image
For the author to deal with how shit it is to manage cl installations, and mostly because bushek is doing this from wsl, we have a docker image with the basics required to set up a development environment for this thing with wayland  
We don't have it on dockerhub or anything yet, there's just a `Dockerfile` in the project's root directory, copy that somewhere and do your docker things.  

To run the container in such a way that you may run a wayland application on the container and have it actually show up on your desktop there's also a `run-wayland-container.sh` (mostly adapated from [https://github.com/hemashushu/docker-archlinux-gui]) in the project root.  

Installation through docker image would then amount to
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

`run-wayland-container.sh` currently does a `docker run --rm` (that is to say, it deletes the container the moment you exit) as I'm iterating on the image quite a lot and would rather not keep corpses around.

# Local Install
> NOTE: these instruction are currently only for linux
> idk how other systems work
To install on your os instead of using docker - assuming you have sbcl, quicklisp, gcc, make&Co. installed - you'll first of all want to install the dependencies to compile raylib itself (as we will do it later)

On redhat and similar (that is, on fedora)
```sh
# for raylib
sudo dnf install alsa-lib-devel mesa-libGL-devel libX11-devel libXrandr-devel libXi-devel libXcursor-devel libXinerama-devel libatomic
# if you also want wayland
RUN apt install -y wayland-protocols libwayland-dev libxkbcommon-dev
```

On debian and similar (that is, on mint or ubuntu)
```sh
# for raylib
sudo apt install libasound2-dev libx11-dev libxrandr-dev libxi-dev libgl1-mesa-dev libglu1-mesa-dev libxcursor-dev libxinerama-dev libwayland-dev libxkbcommon-dev
# if you also want wayland
sudo dnf install wayland-devel libxkbcommon-devel wayland-protocols-devel
```
for further info/details see [raylib's own docs](https://github.com/raysan5/raylib/wiki/Working-on-GNU-Linux)

You'll then want to install `claw-raylib`, so go to your `quicklisp/local-projects` and
```sh
git clone --depth=1 https://github.com/bohonghuang/claw-raylib/
git clone --depth=1 https://github.com/bohonghuang/cffi-object
git clone --depth=1 https://github.com/bohonghuang/cffi-ops
```
for further info/details, once more, [see their own docs](https://github.com/bohonghuang/claw-raylib)

Then actually clone `penultttimate`, we have `raylib`, `raygui`, and `claw-raylib` as submodules so we clone recursively, for asdf not to shit itself and die you'll want to clone `penultttimate` somewhere asdf knows about, such as in `~/common-lisp` or in another directory you added to your asdf registry

```sh
git clone --recursive --depth=1 \
          https://github.com/Karscist/penultttimate \

cd penultttimate
make
make install
ldconfig # idk if it's needed but why not
```

# Quick Sanity Check
To test your install, be it local or to check wether your container works or not, you can try running the following hello world script to see if it dispalys a simple window (as it should do) or crashes down in fiery glory (as it probably should not do).
```lisp
(ql:quickload :claw-raylib)

(defun basic-window ()
  (let ((screen-width 800)
        (screen-height 450))
    (raylib:with-window ("raylib [core] example - basic window" (screen-width screen-height))
      (raylib:set-target-fps 60)
      (loop :until (raylib:window-should-close)
            :do (raylib:with-drawing
                    (raylib:clear-background raylib:+raywhite+)
                  (raylib:draw-text "Congrats! You created your first window!" 190 200 20 raylib:+lightgray+))))))

(basic-window)
```
