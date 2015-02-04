FROM efrecon/tcl
MAINTAINER Emmanuel Frecon <emmanuel@sics.se>

# Set the env variable DEBIAN_FRONTEND to noninteractive to get
# apt-get working without error output.
ENV DEBIAN_FRONTEND noninteractive


RUN apt-get update
RUN apt-get install -y subversion

RUN svn checkout https://github.com/efrecon/tcl-stomp/trunk /opt/stomp

EXPOSE 61613

ENTRYPOINT ["tclsh8.6", "/opt/stomp/stompserver.tcl"]