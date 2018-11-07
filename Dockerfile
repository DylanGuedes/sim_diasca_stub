FROM erlang:20

ENV USER root

RUN apt update && apt install -y \
  make \
  uuid-runtime && \
  mkdir -p /sim-diasca 

WORKDIR /sim-diasca
COPY . /sim-diasca

RUN make clean && make all
