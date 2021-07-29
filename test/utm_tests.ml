open OUnit2

let blank = ([],Utm.Blank,[])
let c x = Utm.Char(x)

let rec move = function
	| (t,_,0) -> t
	| (t,d,x) -> move (Utm.moveHead (t,d),d,x-1)

let testWrite _ =
	let tape = Utm.writeChar blank (c 'a') in
	let tape' = ([],c 'a',[]) in
	assert_equal tape tape'

let testWrite2 _ =
	let tape = Utm.writeChar blank (c 'a') in
	let tape' = Utm.moveHead (tape,L) in
	let tape'' = ([],Utm.Blank,[c 'a']) in
	assert_equal tape' tape''

let testMove _ =
	let tape = ([c 'a'],c 'b',[]) in
	let tape' = Utm.moveHead (tape,R) in
	let tape'' = ([c 'b';c 'a'],Utm.Blank,[]) in
	assert_equal tape' tape''

let testMove2 _ =
	let tape = ([],Utm.Blank,[]) in
	let tape' = Utm.moveHead (tape,R) in
	let tape'' = ([Utm.Blank], Utm.Blank, []) in
	assert_equal tape' tape''

let testMove3 _ =
	let tape = ([c 'a';c 'b';c 'c';c 'd';c 'e'], Utm.Blank, [c 'w';c 'x';c 'y';c 'z';c 'f']) in
	let tape' = move (tape,L,5) in
	let tape'' = move(tape',R,5) in
	assert_equal tape tape''

let testStep _ =
	let d = function
		| (0,_) -> (Utm.R,Utm.Char '1',1)
		| _ -> raise (Failure "Invalid Transition Input") in
	let s0 = 0 in
	let a = 1 in
	let r = 2 in
	let t = blank in
	let sigma = Utm.CharSet.of_list ['1'] in
	let labels = Array.of_list["INIT";"HALT";"REJ"] in
	let tm = (sigma,labels,s0,d,a,r,t) in
	let (tm',res) = Utm.step tm in
	let (_,_,s,_,_,_,t') = tm' in
	let tape = ([c '1'],Utm.Blank,[]) in
	assert_equal t' tape;
	assert_equal s a;
	assert_equal res Utm.Accepted


let testStep2 _ =
	let d = function
		| (0,Utm.Blank) -> (Utm.R,Utm.Char '1',1)
		| (0,Utm.Char(_)) -> (Utm.L,Utm.Char '1',2)
		| (1,Utm.Blank) -> (Utm.L,Utm.Char '1',0)
		| (1,Utm.Char(_)) -> (Utm.R,Utm.Char '1',1)
		| (2,Utm.Blank) -> (Utm.L,Utm.Char '1',1)
		| (2,Utm.Char(_)) -> (Utm.R,Utm.Char '1',3)
		| _ -> raise (Failure "Invalid Transition Input") in
	let s0 = 0 in
	let a = 4 in
	let r = 3 in
	let t = blank in
	let sigma = Utm.CharSet.of_list ['1'] in
	let labels = Array.of_list["A";"B";"C";"HALT";"ACC"] in
	let machine = (sigma,labels,s0,d,a,r,t) in
	let rec count = function
		| (tm,x) ->
			let (tm',res) = Utm.step tm in
			begin
				match res with
				| Utm.Running -> count (tm',x+1)
				| _ -> (x+1,res,tm')
			end in
	let (tot,res,machine') = count (machine,0) in
	let (_,_,s,_,_,_,t') = machine' in
	let tape = ([c '1';c '1';c '1';c '1'],c '1',[c '1']) in
	let echar_to_string = function
		| Utm.Blank -> "_"
		| Utm.Char(c) -> String.make 1 c in
	let list_to_string l = "[" ^ (String.concat ";" (List.map echar_to_string l)) ^ "]" in
	let printTape (left,c,right) = list_to_string left ^ "," ^ echar_to_string c ^ "," ^ list_to_string right in
	assert_equal res Utm.Rejected;
	assert_equal ~printer:string_of_int 13 tot;
	assert_equal ~printer:string_of_int s r;
	assert_equal ~printer:printTape tape t'

let suite =
	"TapeTest" >:::[
		"Basic Write" >:: testWrite;
		"Write-Move" >:: testWrite2;
		"Basic Move" >:: testMove;
		"Blank Move" >:: testMove2;
		"Multiple Move" >:: testMove3;
		"Basic Step" >:: testStep;
		"Busy Beaver" >:: testStep2
	]

let () =
	run_test_tt_main suite
