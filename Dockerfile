FROM debian:unstable

RUN apt update && apt install make erlang -qy
ADD . /src
RUN cd /src && make all

RUN echo "docker.simulator" > /etc/hostname
RUN echo "127.0.0.1 docker.simulator localhost" > /etc/hosts
#RUN /etc/init.d/hostname.sh start

CMD [ "make", "smart_city_run", "CMD_LINE_OPT='--batch'" ]
