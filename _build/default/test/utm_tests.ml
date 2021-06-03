open OUnit2

let blank_tape = ([],Utm.Blank,[])
let c x = Utm.Char(x)

let tape1 = ([],c 'a',[])
let tape2 = ([c 'a'],c 'b',[])
let tape3 = ([c 'b';c 'a'],Utm.Blank,[])

let test_write _ =
	let tape_a = Utm.write_char blank_tape (c 'a') in
	assert_equal tape_a tape1

let test_move1 _ =
	let tape = Utm.move_head (tape2,R) in
	assert_equal tape tape3

let suite =
	"TapeTest" >:::[
		"test_write" >:: test_write;
		"test_move1" >:: test_move1
	]

let () =
	run_test_tt_main suite
