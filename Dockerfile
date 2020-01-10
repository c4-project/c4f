##
## Stage 1: Building Herdtools, Memalloy, and ACT
##

# To build the various OCaml dependencies, we need OPAM.  There is already an
# OPAM image set in Docker Hub, so it's easiest to use that.
#
# The Linux distro used here should line up with the one used in stage 2.
FROM ocaml/opam2:4.08 AS builder

# Copying across parts of the builder stage to the runner stage requires that
# we know where OPAM has put its built binaries and shared files.  For now,
# we hardcode the path to the specific switch below; this is fairly inelegant,
# so any better ideas on how to do this are appreciated.
ENV opam_root /home/opam/.opam/4.08

#
# Installing build dependencies from the package manager
#

# Most of these are for the Memalloy build.
# At this stage, we don't (directly) need any C compilers.
USER root
RUN apt-get update && apt-get -y install \
  ant \
  graphviz \
  m4 \
  python

#
# Dropping to a less privileged user
#

USER opam
WORKDIR /home/opam

# The opam2 image keeps its OPAM repository in a Git clone in the opam user's
# directory.
# At time of writing (2019-11-28), this clone is too outdated to pick up act
# dependencies like dune2.  The various references to git-cloning OPAM below
# are a fairly hacky way of fixing this.

#
# Memalloy
#

# These are needed to make Memalloy build.
ENV JAVA_HEAP_SIZE 3g
ENV JAVA_TOOL_OPTIONS -Dfile.encoding=UTF8

# Install Memalloy's dependencies *before* fetching Memalloy, since they
# probably change on a slower pace.
RUN (cd /home/opam/opam-repository && git pull) && opam update && opam install xml-light ocamlfind ocamlbuild
# Fetch and build Memalloy.
RUN git clone git://github.com/JohnWickerson/memalloy
WORKDIR /home/opam/memalloy
# As of 2019-11-18, the `dev` branch of Memalloy is much further along than
# `master`, and includes fixes we need.
RUN git checkout dev
RUN eval $(opam env) && make install

#
# Herdtools7
#

# TODO (@CaptainHayashi): as of 2019-11-28, the version of herdtools in
# OPAM doesn't work with Dune 2.0.  As such, we have to install a version from
# master.
WORKDIR /home/opam
RUN git clone git://github.com/herd/herdtools7
WORKDIR /home/opam/herdtools7

# Note that the `herdtools7` binaries, `herd7` and `litmus7`, will have
# hardcoded references to ${opam_root}
RUN (cd /home/opam/opam-repository && git pull) && opam update && opam install .

#
# ACT
#

RUN mkdir /home/opam/act
WORKDIR /home/opam/act

# First, acquire the dependencies for the ACT OCaml binaries.
# Do this _before_ gulping down the source trees, so that a cache invalidation
# on the source doesn't force a rebuild of all of ACT's dependencies.
COPY --chown=opam act.opam dune-project Makefile /home/opam/act/
RUN (cd /home/opam/opam-repository && git pull) && opam update && opam install --deps-only .

# Then, get the source trees, in rough increasing order of likelihood of
# change.
COPY --chown=opam bin /home/opam/act/bin
COPY --chown=opam regress_tests /home/opam/act/regress_tests
COPY --chown=opam lib /home/opam/act/lib

# Now, build the ACT OCaml binaries.
RUN (cd /home/opam/opam-repository && git pull) && opam update && opam install .

##
## Stage 2: Building the running environment
##

FROM debian

USER root
# Installing runtime dependencies:
# bash and python3 for ACT scripts, default-jdk and graphviz for Memalloy.
RUN apt-get update && apt-get -y install \
  bash \
  default-jdk \
  graphviz \
  python3

# Installing the compilers we want to test.
RUN apt-get install -y build-essential gcc clang

# We need to make the unprivileged user _before_ pointing symlinks into its
# home directory.
RUN useradd -ms /bin/bash act

# `herdtools7` binaries expect various files in `${opam_root}/share/herdtools7`.
# There doesn't seem to be an elegant way to fix this, so what we do is point a
# symlink into `/home/act/share/herdtools7`, and later copy the files into
# there.
ENV opam_root /home/opam/.opam/4.08
RUN mkdir -p ${opam_root}/share && \
    mkdir -p /home/act/share/herdtools7 && \
    ln -s /home/act/share/herdtools7 ${opam_root}/share/herdtools7

# We can now step down to an unprivileged user; ACT shouldn't need root!
USER act
WORKDIR /home/act

# Copy over the memalloy directory.
# Memalloy should change infrequently compared to ACT, so we copy it earlier.
COPY --from=builder --chown=act /home/opam/memalloy memalloy/
# This is required by Memalloy, and may need changing if we move away from X64.
ENV OS amd64-linux

# Copy over herdtools and their run-time data.
COPY --from=builder --chown=act \
 ${opam_root}/bin/herd7 \
 ${opam_root}/bin/litmus7 \
 bin/
COPY --from=builder --chown=act ${opam_root}/share/herdtools7 share/herdtools7

# Copy over the ACT binaries, which should *hopefully* work.
COPY --from=builder --chown=act ${opam_root}/bin/act-* bin/
ENV PATH "/home/act/bin:${PATH}"

# The scripts (Python3 and bash) don't need any building, so we can just copy
# them from the build context.
COPY --chown=act:act scripts scripts

# Compiler setup
COPY --chown=act:act c11_lahav.cat c11_lahav.cat
RUN ./scripts/make_conf --c-model "/home/act/c11_lahav.cat" > act.conf

CMD [ "./scripts/do_memalloy_test", "-v", "-m", "is_local", "-c", "(id (has_tag x64))", "-D", "c_litmus_indirect", "memalloy" ]
