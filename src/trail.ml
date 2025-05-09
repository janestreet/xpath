open! Core

module Specifier = struct
  type t =
    | Default_namespace
    | Prefixed_namespace of string
    | Attribute of int
    | Element_child of int
  [@@deriving sexp_of, compare]

  include Comparable.Make_plain (struct
      type nonrec t = t [@@deriving sexp_of, compare]
    end)
end

(* The parents of the node in reverse order *)
type t = (Specifier.t * Node.t) list [@@deriving sexp_of]

let rev_map_local =
  let rec rmap_f f accu = function
    | [] -> accu
    | a :: l -> rmap_f f (f a :: accu) l
  in
  fun l ~f -> rmap_f f [] l
;;

let compare a b =
  let a = rev_map_local a ~f:Modes.Global.wrap in
  let b = rev_map_local b ~f:Modes.Global.wrap in
  List.compare__local
    (fun { global = a, _ } { global = b, _ } -> Specifier.compare a b)
    a
    b [@nontail]
;;

include Comparable.Make_plain (struct
    type nonrec t = t [@@deriving sexp_of, compare]
  end)
