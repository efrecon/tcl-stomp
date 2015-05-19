FROM efrecon/mini-tcl
MAINTAINER Emmanuel Frecon <emmanuel@sics.se>

RUN apk add --update-cache git

WORKDIR /tmp
RUN git clone https://github.com/efrecon/tcl-stomp
RUN mv /tmp/tcl-stomp /opt/stomp
RUN rm -rf /var/cache/apk/*

EXPOSE 61613
VOLUME /data

ENTRYPOINT ["tclsh8.6", "/opt/stomp/stompserver.tcl"]
