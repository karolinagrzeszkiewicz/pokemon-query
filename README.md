# holmusk-coding-task
holmusk-coding-task


Required packages (all to be installed with opam):
- async_ssl (opam install async_ssl) (note: this requires OpenSSL, which can be installed with: brew install openssl)
- Cohttp
- Cohttp_async

utop:
#require "cohttp,async,cohttp-async,yojson,async_ssl";;
open Cohttp;;
open Async;;
open Cohttp_async;;
open Async_ssl;;

To run execute the following commands from the root of the repository:
make
./bin/pokemon

or:
dune exec -- ./main/main.exe
