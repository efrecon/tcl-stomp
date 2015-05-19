FROM efrecon/mini-tcl
MAINTAINER Emmanuel Frecon <emmanuel@sics.se>

# Install git so we can install tcl-stomp
RUN apk add --update-cache git

# Install tcl-stomp into /tmp and move it to a proper location
WORKDIR /tmp
RUN git clone https://github.com/efrecon/tcl-stomp
RUN mv /tmp/tcl-stomp /opt/stomp
RUN rm -rf /var/cache/apk/*
WORKDIR /opt/stomp

# Expose STOMP port and data volume
EXPOSE 61613
VOLUME /data

# New entrypoint! the server itself, nothing else so we can give it
# options through CMD (or from compose or similar tool)
ENTRYPOINT ["tclsh8.6", "/opt/stomp/stompserver.tcl"]
