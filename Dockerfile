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

RUN apt-get update \
  && apt-get install -y --no-install-recommends wget libgmp10 \
  && echo "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-8 main" >> /etc/apt/sources.list \
  && wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - \
  && apt-get update \
  && apt-get install -y --no-install-recommends libllvm-8 \
  && apt-get -y remove wget \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

COPY --from=0 /usr/local/bin/faber-exe /usr/local/bin/faber
CMD ["/usr/local/bin/faber"]
