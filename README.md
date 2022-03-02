# Pokemon 

This project contains an application that enables a user to query the PokeAPI (https://pokeapi.co/) from the console, and it is written in OCaml using the Async library.


### Required OCaml libraries:
- async_ssl: ```opam install async_ssl``` (note: this requires OpenSSL, which can be installed with: ``` brew install openssl```)
- Cohttp: ```opam install cohttp```
- Async: ```opam install async```
- Cohttp_async: ```opam install cohttp-async```
- Yojson: ```opam install yojson```

### Running the project
To run execute the following commands from the root of the repository:
```
make
./bin/pokemon
```

### Sample program output

```
> Enter a Pokemon name: 
ditto
name: ditto 
 
is_legendary: false 
 
habitat: urban 
 
description: Capable of copying
an enemy's genetic
code to instantly
                 transform itself
into a duplicate
of the enemy. 
 
height: 3 
 
types: [normal,] 
```

```
> Enter a Pokemon name: 
karolina
Invalid Pokemon name!
```

### Part 1: IO Monad type in OCaml

In monads.ml we define the IO Monad type which can be used to represent effectful computations (e.g. side effects, like printing to the console, which occur outside of the runtime environment of the program) without performing them.

One candidate for such IO type is:
```
type 'a io = IO of 'a
```

However, as demonstrated in the file, such IO type does not yield the desired behaviour because OCaml is a right-to-left call by value language which means that when values of this type are passed as arguments to a function, they will always be evaluated, and the order of evaluation of the arguments of IO type will be from right to left.

Hence we define IO using OCaml's inbuilt Lazy.t type which can be used to suspend computations by
wrapping them in a Lazy constructor and hence simulate the behavior of call-by-need languages like Haskell:
```
type 'a io_better = IOBetter of 'a Lazy.t
```
Note that this is a polymorphic type and it can be used to represent any OCaml expression (including functions) without evaluating it.


### Part 2: Querying APIs with Async

Now we would like to take user input (Pokemon name) to query the PokeAPI and print some information about the pokemon to the console. Both taking user input and printing to the console are effectful computations happening outside of the runtime environment of our program, and so we would like to have a way to represent their possibilities of failure without executing them. The IO monad described above allows us to achieve precisely that, but it has certain limitations – for instance, we can't have a function that takes an effectful computation and wraps it in the IO Monad without evaluating it because OCaml, being a call-by-value language, evaluates anything that is passed as an argument to a function. 

However, OCaml's Async library allows us to simulate the behavior of an IO Monad without the above-mentioned shortcoming, and it also provides support for wait-free concurrency. Async's Deferred.t type acts as a container that will eventually be filled with the result of evaluating an expression. The functions that return a value of this type (e.g. the ask_for_input () function in main/main.ml) never block, and once the container is filled with the result (here: pokemon name) they can be composed with other functions performing effectful or non-effectful computations using the bind (>>=) and map (>>|) operators. Such composition will necessarily produce a value of the Deferred.t type but with its "content" modified accordingly. We can "fire" these computations by starting a scheduler and calling the result of the function.

### Discussion

One of the design decisions behind this project was to print the pokemon information sequentially – the run () function takes user input and applies a chain of functions to it via the bind and map operators such that the user input i.e pokemon name, and the corresponding json file containg its information are passed through the chain as Deferred.t values. However, the results of queries are printed separately by relevant functions instead of being combined and printed in the end as one string. Such design is appropriate in case we are okay with some queries failing (and catch the exceptions) and can simply print e.g. "habitat: NA" when the habitat query fails. If we prefer to instead print either full or no information then we would rather pass an "accumulator" string or a buffer through the sequence of functions appending the results of queries to it as we go and only printing the accumulated string (or buffer) to the console at the end. Note that instead of accumulating a string we are "accumulating" print functions.

This leads us to another possible shortcoming of this design – we are repeatedly passing huge strings of json data as arguments and returning them. This is because we want to avoid making the same API call more than once bur rather store its result once and for all, and only 'forget it' once we make a different API call to collect the data that could not be extracted from the previous API.
