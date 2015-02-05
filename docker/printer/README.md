# stomp printer

This is a simple STOMP topic dumper

To build:

    docker build -t efrecon/stompout .

To print out on `stdout` every message that is sent to the topic
`/test` and is of type `text/plain`, issue the following.

    docker run -it efrecon/stompout -topic /test -type text/plain