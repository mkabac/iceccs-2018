# bigraphs4wsn

Server for runtime analysis of Wireless Sensor Networks based on bigraphs.

## Compiling this repo
Install the following dependancies with OPAM:

* jbuilder >= 1.0+beta13
* lwt >= 3.1.0
* ANSITerminal >= 0.7
* bigrapher >= 1.6.1

Follow the instructions at http://www.dcs.gla.ac.uk/~michele/bigrapher.html#inst to install BigraphER. Once all the dependancies are installed, compile the repo with 
```
make build
```

## Usage

Run the sever with

```
_build/install/default/bin/big4wsn_server
```
A full synopsis of the command can be obtained with

```
big4wsn_server --help
```

The terminal supports the following commands:

```
dump          Save current trace to file
help          Show this help
print_state   Print the current state
stats         Show stats
```

For testing purposes a client can be launched with

```
telnet localhost 9000
```

Some example events that can be issued in the client are `new_node`, `join`, and `reading`. Refer to `Event.parse_event` and `Event.parse_event_binary` for more events.
