(* https://atom.io/packages/ocaml-repl *)
(* #use "/Users/bond/Dev/ocamlClass/test.ml";; *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/toplevel.html *)

(* repl stuff
 #trace <function name>

*)
let x = 2 in x;;
(fun x -> x + 1) 2;;

let add x y = x + y;;


let prepend x xs = match xs with
  | [] -> x :: []
  | xs -> x :: xs
  ;;
(* explicit parens for function calls *)

type 'a option =
  | None
  | Some of 'a ;;


(* ADT *)

type expr_adt =
  | Int of int
  | Bool of bool

(* GADT *)
type 'a expr_adt =
  | IntExp : int -> int expr_adt
  | BoolExp : bool -> bool expr_adt

(*
term level natural numbers
data Nat = Z | S Nat
*)

(* type level nats*)
type z = Z : z
(* data Z = Z *)
type 'n s = S : 'n -> 'n s
(* data S n = S n *)

type 'a list_adt =
  | Nil
  | Cons of 'a * 'a list_adt (* curried constructors *)

(*
data List a s where
  Nil :: List a Z
  Conz :: a -> List a n -> List a (S n)
*)

type ('a, _) list_gadt =
  | Nill : ('a , z) list_gadt
  | Conz : 'a * (('a , 'n) list_gadt) -> ('a , 'n s) list_gadt


module type Functor = sig
  type 'a f
  val map : ('a -> 'b) -> 'a f -> 'b f
end

module Option_Functor : (Functor with type 'a f = 'a option) = struct
  type 'a f = 'a option
  let map f fa = match fa with
                    | None -> None
                    | Some(a) -> Some(f(a))

end

open Option_Functor
let ex = map (fun x -> x + 2) (Some 7)

module type Monad = sig
  type 'a f
  val pure : 'a -> 'a f`
  val bind : ('a -> 'b f) -> 'a f -> 'b f
end

module Option_Monad : (Monad with type 'a f = 'a option) = struct
  type 'a f = 'a option
  let pure a = Some(a)
  let bind f fa = match fa with
                  | None -> None
                  | Some(a) -> f(a)
end

open Option_Monad
let ex2 = pure 4
let ex3 = bind (fun x -> Some(x+2)) (Some(8))


(* if ocaml modular implicits are merged into main,
then typeclass encoding is possible
https://tycon.github.io/modular-implicits.html
however the branch seems to be dead
 *)
