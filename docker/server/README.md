# stompserver

This is a simple STOMP server.

To build:

    docker build -t efrecon/stomp .

To run with high verbosity:

    docker run -it --rm -p 61613:61613 efrecon/stomp -v 5