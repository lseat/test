type 'a stream = Stream of 'a * (unit -> 'a stream)

(* NOTE: You may add the rec keyword as you wish for this file. *)


let rec take n (Stream (h,t)) = 
       match n with
       |i when i<=0 -> []       (*should it be i<=0*)
       |i  ->  h::(take (n-1) (t()))


let rec repeat   x = 
     Stream (x, fun() -> repeat x)

let rec map f s = 
     match s with
     |Stream(h,t) -> Stream(f h, fun() -> map f (t()));;


let rec nele (n:int) (s:'a stream) : 'a =
	  match n,s with
      |0, Stream(h,t)  -> h
      |i,Stream(h,t)   -> nele (i-1) (t())

let rec haha acc (Stream(h,t)) =
	Stream ((acc,h),fun() -> (haha (acc+1) (t())))


let rec diag s =
	let sstream = (haha 0 s) in
	map (fun ss -> nele (fst ss)(snd ss)) sstream
	


let rec suffixes s = 
    match s with
    |Stream(h,t) -> Stream(Stream(h,t),fun()-> suffixes (t()) )


let rec interleave s s' = 
	match s,s' with
	|Stream(h1,t1),Stream(h2,t2) -> Stream(h1,fun()-> Stream (h2,fun() -> interleave (t1()) (t2())))


let fibs () = 
     let rec fib_help a b =
          Stream(a,fun()->Stream(b,fun()->fib_help (a+b) (a+b+b)))
      in fib_help 0 1

let rec cal n = 
     match n with
     |0. -> 4.
     |i   -> (((-1.)**i) /. (2.*.i +. 1.) *. 4.) +. (cal (n-.1.))  

let pi () =
    let rec pi_help a = 
        Stream(cal a, fun() -> pi_help (a+.1.))
        in pi_help 0.


        

let rec look_help acc lst =
	match lst with
	| [] -> [1]
	| h::[] -> acc::[h]
	| h::x::t  -> if (h=x) then (look_help (acc+1) (x::t))
	                       else acc::h::(look_help 1 (x::t)) 


let look_and_say () = 
	let rec look_help2 acc lst =
	    Stream(look_help acc lst,fun()->look_help2 acc (look_help acc lst))
	in look_help2 1 []
