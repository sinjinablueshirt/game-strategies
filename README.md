# Game Strategies

## Getting Started

TODO
Write cloning and building instructions

## Async Basics - Concurrency With Deferreds

Async is the Ocaml library we'll use to implement concurrent programs. Async can be
difficult to understand and tricky to write.

Before you run <insert dune command>, reason through the five puzzles in
src/concurrency_exercises.ml. For each one, predict the behavior you expect. Run the
program to verify your understanding.

## Async Basics - RPCs

One common and useful part of Async is the RPC: Remote Procedure Call. This is a network
protocol which allows a caller (commonly referred to as the client) to call a procedure
on the callee (commonly referred to as the server).

The Async RPC framework allows us to write both clients and servers.

Read the rpc_exercises.ml file and build and run the program via <insert dune command>.

TODO
Figure out if the JSIPeres can handle making two connections to their EC2 boxes in two
terminals.

TODO
More instructions/coaching for adding new query/response types.

TODO
Confirm that the EC2 machines can see each other on the network
