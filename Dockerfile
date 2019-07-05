FROM fpco/stack-build:lts-13.26

ADD ./ /work
WORKDIR /work
RUN echo "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-8 main" >> /etc/apt/sources.list
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN apt-get update
RUN apt-get install -y --no-install-recommends llvm-8-dev
RUN stack build
RUN stack install --local-bin-path=/usr/local/bin

FROM ubuntu:xenial
RUN apt-get update
RUN apt-get install -y --no-install-recommends wget
RUN echo "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-8 main" >> /etc/apt/sources.list
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN apt-get update
RUN apt-get install -y --no-install-recommends libllvm-8
COPY --from=0 /usr/local/bin/faber-exe /usr/local/bin/faber
CMD ["/usr/local/bin/faber"]
