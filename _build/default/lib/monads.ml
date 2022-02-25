(* ***** Holmusk SWE Assignment: Monads in OCaml ***** *)

(* In order to do this test, you require to have
 * - OCaml 4.02 or higher installed on your system
 * - A text editor which allows you view your code and run the Ocaml REPL side-by-side
 *
 *)

(* Holmusk's tech stack consists primarily of functional programming languages: we use
 * Haskell on the backend, and Elm on the frontend, the latter being largely based on
 * Haskell. Haskell can often involve high levels of abstraction, but Elm is generally
 * more rooted in basic functional programming principles. Some background is
 * functional programming is usually good enough to get started with these
 * languages, and appreciate their utility in a tech stack.
 *
 * This test assumes you have worked before with OCaml, and are familiar with
 * functional programming principles.
 *
 *)

(*
 * The term monad appears in various contexts across computer science, mathematics, logic, and linguistics. For
 * this assignment, we will assume that the term refers to the following interface that exposes a parameterized
 * type, and two operations on values of this type:
 *)

module type MONAD =
sig
  type 'a m
  val return : 'a -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
end
;;

(* Next, let us make a digression to understand effectful computations and IO in Haskell:

 * Generally speaking, for any programming language to be really useful in a software engineering
context, it needs to support side-effects. Side-effects refer to computations that occur
/outside/ the context of a program's runtime environment. Examples of effectful computations
include printing to the console, querying a database, reading from and writing to files, making HTTP calls etc.

Since effectful computations take place /outside/ the runtime environment of the program,
the programming language cannot provide any guarantees about the success of these
computations. Thus consider, for example, the following OCaml program:

@
foo :: Int -> Int
let foo x = printf ("%d", x) ; x + 1
@

This program prints the integer it receives, and then returns the successor of the integer.
However, there is no guarantee that @foo@ will execute as expected and always return the
successor of the integer it is passed. This is because it must first print the integer to
console-- this is a side-effect outside the runtime environment of the program, and might
very well fail.

In computer science parlance, we thus say that @foo@ is not referentially transparent. Let
us take a moment to consider the definition of referential transparency from the Haskell
wiki:

"While there is no single formal definition[1], it usually means that an expression always
evaluates to the same result in any context."

Thus, since
(a) @foo@ has a side-effect that is outside the OCaml runtime context, and
(b) we cannot therefore guarantee that it will /always/ evaluate to the same value,
we therefore say @foo@ is not referentially transparent.

Haskell, however, provides /both/ effectful computations and referential transparency. It
does so by differentiating pure Haskell /values/ from effectful /actions/ (like printing to
console). The latter are represented as an ADT (Abstract Data Type) called @IO@
(see: https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:IO).

The @IO Char@ type can be thought of as a /representation/ of the actual action of fetching
a character from @stdin@. This is Haskell's way of saying: we know that this computation
results in a @Char@ value, but we don't actually perform it (since it is performed outside
the runtime environment). Thus, we will /represent/ this action using the @IO@ context. This is
the key point to note for @IO@.

The following metaphor also tends to be helpful in understanding @IO@:
Consider a recipe to bake a cake. It outlines how one bakes a cake, and the end results.
However, the recipe itself does not bake the cake. In this sense, the recipe /represents/
the baking of the cake, and is not the baking of the cake itself. Likewise, @IO Char@ can be
thought of as a recipe to fetch a character from @stdin@.

So what happens when I follow this recipe and attempt to bake the cake, but my oven
malfunctions halfway through? My /action/ to bake the cake has clearly failed. But this
failed /action/ is still represented by the recipe, because I followed the recipe. Likewise,
my actions of messing up the proprtions of the ingredients, or setting the oven to the wrong
temperature are still represented by the same recipe, /even if/ the resultant overly sweet
or burned cake is not what I was hoping to get. The key point to note here is that even the
possibilities of failure in the baking process are /implicitly/ represented by the recipe.

Likewise with the @IO@ type-- the possibilities of failures occuring outside the Haskell
runtime environment are also implicity represented by @IO@. Thus, @getChar@ will have type
@IO Char@ in /all contexts/, since the possibilities of failure outside the Hasklel runtime
context are also reprented by the type @IO Char@. This is precisely how Haskell preserves
referential transparency while also allowing for effectful computations.
 *)

(* As discussed above, OCaml does not provide any equivalent IO type. Implementing an equivalent type in OCaml
 * would be far too complicated, and is out of the scope of this assignment. However, we can define a dummy
 * IO type in OCaml as follows:
 **)

 type 'a io = IO of 'a
 ;;

 (* There's nothing special about this type; all it does is wraps a value of type 'a in a constructor that we
  * have named IO. It contains none of the magic under the hood that makes Haskell's IO type so powerful.
  *
  * However, we can still take advantage of this dummy type to better structure our code w.r.t. effectful
  * computations. This will be the focus of this assignment.
  * **)

 (* Q1. Write an implementaion of the MONAD sig for 'a io type *)

module IOMonad =
  struct
    type 'a m = 'a io
    let return (a : 'a) : 'a m = IO a
    let bind (ma : 'a io) (amb : 'a -> 'b io) : 'b m = 
      IO (begin match ma with
		| IO a -> begin match amb a with
				| IO b -> b
			  end
	  end)	    
  end
;;
(* comment: to avoid evaluating a, though OCaml is call by value so it will be evaluated anyway *)

(* We now define a convenient notation for the bind operation, taking inspiration from Haskell: *)

let ( >>= ) ma amb = IOMonad.bind ma amb
;;

(* The bind operation assumes that we are actually interested in the value of type 'a contained in the
 * context m. However, this is not always the case. For example, consider the following function: *)

let print_io str = IO (Printf.printf str)
;;

(* This function invokes the print function on the string passed to it, and then wraps the result of the function
 * (the unit value) in the IO constructor.
 *
 * Now suppose we want to print 4 strings using print_io. Each invokation of print_io is of type unit io, and we
 * don't care about the unit value that is returned when we invoke print_io to print the next string.
 * *)

(* Q2. Implement an operator ( >> ) that takes two IO operations (effectful computations like print_io) op1 and
 * op2 such that op1 >> op2 runs op1 first, ignores the result of this computation, and then runs op2, returning
 * the result of this computation. Use the ( >>= ) operator in your solution.
 * *)

let ( >> ) (io_a : 'a io) (io_b : 'b io) : 'b io =
  io_a >>= (fun _ -> io_b);;

(* Test your definition: *)

let chain_io_actions =  print_io "Au revoir \n" >> print_io "Auf Wiedersehen \n" >> print_io "Sayonara \n";;


(* Q3. If you run chain_io_actions multiple times, you will find that the order of the printed strings is
 * inconsistent. Why do you think this is the case? What OCaml operator can you use in place of ( >> ) to fix
 * this issue?

OCaml is call by value and the function arguments are evaluated from right to left, so that's why Sayonara is printed first, then Auf Wiedersehen, then Au revoir

let ( >> ) (io_a : 'a io) (io_b : 'b io) : 'b io =
  let _ = io_a in 
  io_b;;

this is a simple way to evaluate io_a but disregard the result of the computation,
and then evaluate io_b returning the result of the computation: let ... in ... imposes a strict order on these evaluations


 *
 **)

(* ***** *)

(* ********** End of file ********** *)
