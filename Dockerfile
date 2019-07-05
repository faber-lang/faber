FROM fpco/stack-build:lts-13.26

RUN echo "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-8 main" >> /etc/apt/sources.list
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN apt-get update
RUN apt-get install -y --no-install-recommends llvm-8-dev

ADD ./ /work
WORKDIR /work

RUN stack build
RUN stack install --local-bin-path=/usr/local/bin

FROM ubuntu:xenial

RUN apt-get update \
  && apt-get install -y --no-install-recommends wget libgmp10 ca-certificates gcc libc6-dev \
  && echo "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-8 main" >> /etc/apt/sources.list \
  && wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - \
  && apt-get update \
  && apt-get install -y --no-install-recommends llvm-8 \
  && update-alternatives --install /usr/local/bin/llc llc `which llc-8` 10 \
  && apt-get -y remove wget \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

COPY --from=0 /usr/local/bin/faber-exe /usr/local/bin/faber
COPY bin/fabc /usr/local/bin/
COPY bin/fabrun /usr/local/bin/

VOLUME /data
WORKDIR /data

CMD ["/usr/local/bin/faber"]
