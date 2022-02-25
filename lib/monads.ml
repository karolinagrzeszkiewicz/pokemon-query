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
     let bind (ma : 'a m) (amb : 'a -> 'b m) : 'b m = 
       begin match ma with
       | IO a -> amb a
       end
   end
 ;;
 
 (* Comment:
 
 We could also do:
 
 let bind (ma : 'a m) (amb : 'a -> 'b m) : 'b m = 
       IO (begin match ma with
     | IO a -> begin match amb a with
         | IO b -> b
         end
     end)
 
 - wrap the entire computation in the IO context which is more compatible with the
 description of monads provided above (i.e. if IO context suspends the wrapped computation), 
 but in OCaml the two expressions will yield the same computation (since IO is not suspending anything)
 *)
 
 
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
 
 let chain_io_actions = print_io "Au revoir \n" >> print_io "Auf Wiedersehen \n" >> print_io "Sayonara \n"
 ;;
 
 
 (* Q3. You may find that no matter how hard you try, you cannot get the operations
  * in chain_io_actions to execute from left to right. Can you explain why this is the
  * case?
 
 OCaml is call by value and the function arguments are evaluated from right to left, 
 so that's why Sayonara is printed first, then Auf Wiedersehen, then Au revoir.
 
 With impure expressions, like the ones above, passed as arguments they will be evaluated first
 (from right to left) before the body of the function is evaluated – then it doesn't really matter
 what we write in the body because the order of arguments already determines the order of the printed statements
 
 Conclusion: we need a different IO type, one that suspends the wrapped computations, and that would allow
 us to imitate the behaviour of some call-by-need programming languages
  *
  **)
 
 (* Q4. In order to fix the issue in 3, we need a slightly modified definition of our
  * IO type. Come up with this definition (hint: define the type in such a way that
  * a different argument evaluation strategy is simulated):
  * *)
 
 open Lazy 
 
 type 'a io_better = IOBetter of 'a Lazy.t
 ;;
 
 (* Q5. Write an implementaion of the MONAD sig for 'a io_better type *)
 
 module IOBetterMonad =
   struct
     type 'a m = 'a io_better
     let return (a : 'a) : 'a m = IOBetter (lazy a)
     let bind (ma : 'a m) (amb : 'a -> 'b m) : 'b m = 
       IOBetter (lazy 
       (begin match ma with
             | IOBetter lazy_a -> 
                 (begin match amb (Lazy.force lazy_a) with 
                       | IOBetter lazy_b -> Lazy.force lazy_b
                 end)
       end))
   end
   ;;
 
 (* note: return a triggers a (producing a side effect in case a is impure),
 bind should not trigger anything since am is a monad i.e. suspended and amb is a function *)
 
 (* Q6. As in the case of 'a io, define an operator for the bind operation: *)
 
 let ( >>>= ) (ma : 'a io_better) (amb : 'a -> 'b io_better) : 'b io_better = 
   IOBetterMonad.bind ma amb;;
 
 
 (* Q7. Likewise, define the corresponding ( >> ) operator for io_better values: *)
 
 let ( >>> ) (ma : 'a io_better) (mb : 'b io_better) : 'b io_better = 
   ma >>>= (fun _ -> mb);;
 
 (* Q8. Define a print function wrapper for our 'a io_better type: *)
 
 let print_io_better str = IOBetter (lazy (Printf.printf str))
 ;;
 
 (* Test your definition: *)
 
 let chain_io_better_actions =
     print_io_better "Au revoir \n" >>>
     print_io_better "Auf Wiedersehen \n" >>>
     print_io_better "Sayonara \n"
 ;;
 
 let chain_io_better_actions_triggered = 
   match chain_io_better_actions with 
   | IOBetter lazy_x -> Lazy.force lazy_x
 ;;
 
 (* Comment: 
 
 applying Lazy.force to a suspended computation will trigger it
 and then the result of that computation will be memoized and the computation
 will no longer be suspended e.g. calling Lazy.force on the computation wrapped in
 IOBetter in chain_io_better_actions for the first time will run the computation,
 but the second time it will just return an empty container i.e. IOBetter (lazy ())
 
 So Lazy.force removes the side effect from the monad 
 (however, the result of the computation, in this case the unit value, stays in the container)
 
 let chain_io_better_actions =
     print_io_better "Au revoir \n" >>>
     print_io_better "Auf Wiedersehen \n" >>>
     print_io_better "Sayonara \n"
 ;;
 val chain_io_better_actions : unit io_better = IOBetter <lazy>
 
 let chain_io_better_actions_triggered = 
   match chain_io_better_actions with 
   | IOBetter lazy_x -> Lazy.force lazy_x
 ;;
 Au revoir 
 Auf Wiedersehen 
 Sayonara 
 val chain_io_better_actions_triggered : unit = ()
 
 let chain_io_better_actions_triggered = 
   match chain_io_better_actions with 
   | IOBetter lazy_x -> Lazy.force lazy_x
 ;;
 val chain_io_better_actions_triggered : unit = ()
 
 *)
 
 (* one more test case: a combination of impure and pure expressions*)
 
 let succ x = 
   Printf.printf "computing the successor of %d...\n" x; 
   x+1
 
 let chain_io_better_actions_v2 =
   (IOBetter (lazy (succ 0))) >>>
   (IOBetter (lazy (succ 1))) >>>
   (IOBetter (lazy (succ 2)))
 ;;
 
 let chain_io_better_actions_triggered_v2 = 
   match chain_io_better_actions_v2 with 
   | IOBetter lazy_x -> Lazy.force lazy_x
 ;;
 
 (* 3 is memoized as the result *)
 
 (* Take home message *)
 
 (* If you definied io_better correctly, the (>>>) operator should now evaluate operations
  * sequentially. The (>>>=) and (>>>) operators can now be used to chain effectful
  * computations, as long as the effectful computations take place in the io_better
  * monad. As shown with print_io_better, we can define wrapper functions around
  * an effectful function to ensure that the effect runs in the io_better monadic
  * context.
  *
  * This could possibly be useful when dealing with effectful computations in the
  * second part of the assignment
  **)
 
 (* ***** *)
 
 (* ********** End of file ********** *)
