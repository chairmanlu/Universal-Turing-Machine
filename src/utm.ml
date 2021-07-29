module CharSet = Set.Make(Char)

type state = int
type direction = L | R
type echar = Blank | Char of char
type tape = echar list * echar * echar list
type status = Running | Accepted | Rejected

type t = CharSet.t                                     (* Alphabet *)
       * string array                                  (* State Mapping *)
       * state                                         (* Current State *)
       * (state * echar) -> (direction * echar * state)(* Transition Function *)
       * state                                         (* Accept State *)
       * state                                         (* Reject State *)
       * tape                                          (* Infinite Tape *)


(* Possible optimization - Prune Blanks from ends of list *)
let moveHead = function
	| (([],c,r), L) -> ([],Blank,c::r)
	| ((c'::l,c,r), L) -> (l,c',c::r)
	| ((l,c,[]), R) -> (c::l,Blank,[])
	| ((l,c,c'::r), R) -> (c::l,c',r)

let writeChar (l,_,r) c' = (l,c',r)

(* step: t -> t * status
 * step t evaluates one step of the turing machine specified by t. It outputs 
 * the resulting turing machine and status of the machine after the step.
 * If the initial state is an accepting or rejecting state nothing is evaluated
 * and the status is immediately returned.
 *)
let step = function
	| ((alpha,labels,s,delta,a,r,((_,c,_) as t)) as tm) ->
		if s=a then (tm,Accepted) else if s=r then (tm,Rejected) else
		let (dir,c',s') = delta (s,c) in
		let t' = moveHead ((writeChar t c'),dir) in
		let res = if s'= a then Accepted else if s'=r then Rejected else Running in
		((alpha,labels,s',delta,a,r,t'),res)
