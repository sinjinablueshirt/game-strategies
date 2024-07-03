# Game Strategies

This project will teach you about adversarial games and game AIs to implement bots that play tic-tac-toe and gomoku!

## Getting Started

Fork this repository by visiting [this page](https://github.com/jane-street-immersion-program/game-strategies/fork) and clicking the green "Create form" button.

Next, clone the form locally. Remember, you can clone the repo by doing `git clone git@github.com:$USER/game-strategies.git`.

You can build the codebase by 
```
$ cd game-strategies/
$ dune build
$ dune runtest
$ dune exec ./bin/game_strategies.exe --
```

## Async

Before we embark on studying games, we will first become familiar with Async, an Ocaml library and framework we use for concurrency. These async exercises are not present in this repo but have already been pushed to your machines under the directory "async_exercises". Enter this directory for the exercises in this section.

### Exercise 0, Task 0: Concurrency - Solving puzzles

Build and show the available concurrency exercises with
```
dune build
dune runtest
$ dune exec ./bin/async_exercises.exe --
```

Inspect the `do_a_thing` function near the top of `src/concurrency_exercises.ml`. What does this function do?

There are several "puzzle" exercises which you can run via `dune exec ./bin/async_exercises.exe -- concurrency -?`. Before running each one, read the source code and try to predict the behavior. Then, execute that puzzle (with eg `dune exec ./bin/async_exercises.exe -- concurrency 1`). Does the actual behavior match your prediction?

### Exercise 0, Task 1: Concurrency - Write your own puzzles

For this task, we will be authoring new puzzles. Follow the existing template and create each new puzzle in its own module inside of `src/concurrency_exercises.ml`. Remember, too, to edit the `command` value to reference any new puzzles.

1. Write and run a puzzle that takes a total of 4 seconds and calls `do_a_thing 2` at least 3 times.
1. Write and run a puzzle that calls `do_a_thing 2` but completes in less than 2 seconds.
1. Write and run a puzzle that calls `do_a_thing` three times with three different "duration" values, and completes as soon as the _earliest_ one finishes.

### Exercise 1, Task 0: RPC - Run Ping

In this exercise, we will start an RPC server and connect to it with a client. Recall the terminology: we say that the client connects to the server and sends a _query_ to it over the network; the server then runs a function which takes that _query_, generates a _response_, and then returns that _response_ over the connection to the client.

The code for this exercise is in `src/rpc_exercises.ml`.  This file contains several modules. None of these modules are strictly necessary but they organize the code nicely.

##### Module _Ping_

This module contains two sub-modules - _Query_ and _Response_ - each of which has a _type t_. These are the types of the messages which will be exchanged between the server and the client. The client will send a _Query_ to the server and the server will reply with a _Response_.

##### Module _Protocol_

It's possible for a server to be able to handle multiple kind of queries (each of which will have a type of response). Each of these _query_/_response_ exchanges constitutes a single RPC ("Remote Procedure Call"). The `ping_rpc` value in this module has a type of `(Ping.Query.t, Ping.Response.t) Rpc.Rpc.t`, that is, it is an RPC which takes a `Ping.Query.t` and replies with a `Ping.Response.t`.

##### Module _Server_

This module contains the `command` value which takes command line parameters and starts the server parts of the program. Make sure you thoroughly understand the other items in this module:
* `handle_ping` is a function which takes a _client_ (_unit_ in this case) and a _query_; it returns a _response_.
* `implementations` is a value of which defines the implementation of all the RPCs which this server implements; it uses `handle_ping` to define that implementation.

##### Module _Client_

This module contains on the `command` value. Inside the function it runs, it constructs a _query_, connects to the server, and sends that _query_. The call to `Rpc.Connection.with_client` returns a Deferred, so we need to bind on it to block until that value has become determined. Once that's done, it prints out the response it received.

Make sure the `async_exercises` directory is built properly:
```
dune build
dune runtest
```

To run both the client and the server, we'll need to ssh to our EC2 box twice. From one terminal, run `dune exec ./bin/async_exercises.exe -- rpc server -port $PORT` - replace $PORT with any integer between 1025 and 65535, it's customary to choose something between 10000 and 20000. From the other terminal, run `dune exec ./bin/async_exercises.exe -- rpc client -server 'localhost:$PORT'` and make sure that (1) the server is running and (2) the port numbers you specify for the server and client are identical.

### Exercise 1, Task 1: RPC - Delayed Ping

To convince ourselves that the server can service multiple clients concurrently, let's add a 10 second delay on the server side. (Refer back to `do_a_thing` from the earlier exercise if you need to remind yourself how to perform this delay.)

To test this, prepare *three* terminals. From one, run the server. From the second, run the client. From the third, *quickly* after running the client in the second terminal, run the client again. Look at the time in the response method and insert new _print_ statements to verify that the server is handling both clients concurrently.

### Exercise 1, Task 2: RPC - Richer query and response

In this task, we'll improve the existing `Ping.Query.t` and `Ping.Response.t` and additionally create new query/response pairs which our client and server understand how to send/receive. The existing `Ping.Query.t` type is currently a _unit_. Let's enhance the query type by making it into a string, which we take as an argument on the command line. Let's also enhance the _response_ record to echo that same message back to the client in addition to the time.


### Exercise 1, Task 3: RPC - Additional queries and responses

A server can understand how to handle more than one kind of query. Create a new module called `RainbowColor` that contains a `type t` that expresses the seven ROYGBIV colors. Create a query module/type that permits clients to send the server a specfic color; the response to this should be a random color in ROYGBIV which is _not_ the color in the query. Our server should be able to handle this new query/response as well as the existing "Ping" query/response. Our client should take something on the command line that indiciates whether it should send a Ping query or a RainbowColor query.