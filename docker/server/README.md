# stompserver

This is a simple STOMP server.

To build:

    docker build -t efrecon/stomp .

To run with high verbosity:

    docker run -it --rm -p 61613:61613 efrecon/stomp -v 5

The server has simple support for the following features.  To bring up
the full list of options, call it with `-h`:

* Authentication of users through the `-users` option, which should
  contain a list of colon separated user names and password,
  e.g. `user1:mysecret user2:anotherpass`.

* Encryption using TLS.  Encryption is turned on and controlled using
  the option `tls`.  When set to a negative boolean (`false`, `0` or
  `off` will work), traffic is not encrypted.  When set to a positive
  boolean, traffic is encrypted.  When set to `strict`, the server
  requires the peers to have maching certificates.

To get you going, self-signed certificates can be created using
OpenSSL and the following commands (adapt to your own needs):

    openssl genrsa -out server-private.pem 2048

    openssl req -new -x509 \
    	    -key server-private.pem -out server-public.pem \
	    -batch -days 3650 \
	    -subj "/countryName=SE/stateOrProvinceName=Stockholm/localityName=Kista/organizationName=SICS/commonName=stompserver/emailAddress=emmanuel@sics.se"

The `Dockerfile` creates a volume called `/data` so you can try
mounting this volume onto a local directory, so as to ensure access to
certificates, for example.
