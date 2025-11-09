FROM debian:trixie-slim

# this is a very bovine approach
# and I should probably be using an entrypoint.sh
RUN apt update -y

# build dependencies for raylib and some other basics
# (we probably don't need pkg-config or cmake, but just in case)
# NOTE: cmake will fail to build raylib with wayland support
# if pkg-config isn't installed)
RUN apt install -y build-essential git curl vim emacs pkg-config cmake

# raylib readme lists dependecies for ubuntu but not for debian
# I'm just gonna assume the ubuntu ones also work on debian
RUN apt install -y libasound2-dev libx11-dev libxrandr-dev libxi-dev \
		   libgl1-mesa-dev libglu1-mesa-dev libxcursor-dev \
                   libxinerama-dev libwayland-dev libxkbcommon-dev

# they also only lists wayland packages needed to compile on fedora
# sudo dnf install wayland-devel libxkbcommon-devel wayland-protocols-devel
# the ones below are the debian equivalents
# (partly taken from glfw docs)
# https://www.glfw.org/docs/latest/compile_guide.html#compile_deps_wayland
RUN apt install -y wayland-protocols libwayland-dev libxkbcommon-dev

# install common lisp (sbcl)
RUN apt install sbcl

# install quicklisp
RUN curl -LO https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --non-interactive \
         --load 'quicklisp.lisp' \
         --eval '(quicklisp-quickstart:install)' \
         --eval '(ql-util:without-prompting (ql:add-to-init-file))'

# create lisp development directory thingies
RUN mkdir -p /root/common-lisp && mkdir -p /root/quicklisp/local-projects

# add claw-raylib&Co. to quicklisp
RUN git clone --depth=1 https://github.com/bohonghuang/claw-raylib \
              /root/quicklisp/local-projects/claw-raylib
RUN git clone --depth=1 https://github.com/bohonghuang/cffi-object \
              /root/quicklisp/local-projects/cffi-object
RUN git clone --depth=1 https://github.com/bohonghuang/cffi-ops \
              /root/quicklisp/local-projects/cffi-ops

# clone penultttimate somewhere asdf can see it (~/common-lisp)
RUN git clone --recursive --depth=1 \
              https://github.com/Karscist/penultttimate \
              /root/common-lisp/penultttimate

# make it
RUN make --directory=/root/common-lisp/penultttimate

