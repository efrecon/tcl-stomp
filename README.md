# stomp-tcl

This project is a near-complete implementation of the [STOMP
protocol][1] in Tcl.  The project provides a library for both the
implementation of servers and clients.  In addition, three utilities
are provided, highlighting the possibilities of the library.

  [1]: https://stomp.github.io/ 


# Library

The Tcl STOMP library implements the an almost complete STOMP v1.2
client library, together with all the necessary buiding blocks for
sending and/or receiving STOMP messages in clients and server.  A sub
package implement all messaging (in and out) support, while the main
package provides for a utility layer allowing callers to send and
receive STOMP messages to/from servers with little effort.

The messaging layer provides the following core facilities:

* Procedures to create messages, set and get their various parameters
  and components.  Messages are composed of a command (as specified in
  STOMP), a set of headers (loosely similar to HTTP headers) and a
  body.  It is possible to strictly follow the message constraints as
  imposed by the protocol, i.e. control which header are mandatory and
  optional for a given command.

* Procedures to send and receive messages to and from a STOMP server.
  Reception is implemented via a "reader" context as messages can
  arrive at any time from a server on a socket.  The reader context
  contains a handler that will be called once incoming messages have
  been parsed and validated.

* Procedures "entertain" a connection towards a server.  One procedure
  ensures that keep-alive (almost) empty data is sent at regular
  intervals. Another watches that a remote servers continuously sends
  heart-beats at regular intervals, and provide a callback whenever it
  is deemed to have disconnected.

The STOMP layer encapsulates all traffic to a remote server into a
single object.  The layer provides an implementation that follows the
v1.2 of the specification, except that it does not directly offers
support for transactions.  The library offers the following core
facilities:

* Opening of connection to a remote server, while respecting the
  initial connection hand-shake that will establish how heart-beats
  will be exchange between the client and the server.

* Facilities to keep the connection alive at all time and as most as
  possible by continuously attempting to reconnect whenever the
  connection has been lost.

* Ability to provide callbacks to follow the current state of the
  connection (in progress, connected, disconnecting, etc.).

* Ability to be notified of the reception of any message.

* Ability to send messages to the remote end, thus providing a
  high-level wrapper around the messaging library, a wrapper that will
  take care of reconnections, header assignments, etc.

* Ability to subscribe to data coming from the server and to arrange
  for callbacks to be delivered each time a message matching the
  subscription is received from the server.  Subscriptions can be
  ended at all time, in cooperation with the server.

* Ability to request for receipts and to be called back on reception
  of the receipt.

* Perform graceful disconnections from the server, i.e. disconnections
  in cooperation with the server.


# Utilities and Examples

## stompserver

The stompserver is a simple STOMP server, highlighting the
capabilities of the server implementation of the library.  The server
has simple support for:

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

## stomper

Stomper will listen on stdin and send any non empty line as a STOMP
message onto a topic which path is specified by the option `-topic`

## printer

Printer performs the opposite of stomper, i.e. it subscribes to a
given topic and prints outs messages sent onto that topic onto stdout.
Printer is able to filter only messages of a givven MIME type using
the option `-type`.

