module CharSet = Set.Make(Char)

type state = int
type direction = L | R
type echar = Blank | Char of char
type tape = echar list * echar * echar list

type t = CharSet.t                                     (* Alphabet *)
       * string array                                  (* State Mapping *)
       * state                                         (* Current State *)
       * (state * echar) -> (direction * echar * state)(* Transition Function *)
       * state                                         (* Accept State *)
       * state                                         (* Reject State *)
       * tape                                          (* Infinite Tape *)

let move_head = function
	| (([],c,r), L) -> ([],Blank,c::r)
	| ((c'::l,c,r), L) -> (l,c',c::r)
	| ((l,c,[]), R) -> (c::l,Blank,[])
	| ((l,c,c'::r), R) -> (c::l,c',r)

let write_char (l,_,r) c' = (l,c',r)

let step = function
(*	| (alpha,labels,s,d,a,r,t) -> raise (Failure "Not Yet Implemented")*)
	| _ -> raise (Failure "Not Yet Implemented")

let name () = Printf.printf "UTM\n"
