open Ast

(******************************************************************************)
(** types (see .mli) **********************************************************)
(******************************************************************************)

type value =
  | VUnit | VInt of int | VBool of bool | VString of string
  | VClosure of var * expr * environment
  | VVariant of constructor * value
  | VPair of value * value
  | VError of string
and environment = (var * value ref) list

let rec find_match (p : pattern) (v : value) : environment option =
    (*idea: if p=v then build environment else none *)
    match p,v with
    |PUnit,VUnit -> Some []
    |PInt i,VInt j when i=j -> Some []
    |PBool a,VBool b when a=b -> Some []
    |PString s, VString t when s=t -> Some []
    |PVar id , _ -> Some[(id , ref v )]
    |PPair (p1,p2) , VPair(v1,v2) -> 
        begin match (find_match p1 v1),(find_match p2 v2) with
        |None,_   -> None
        |_,None   -> None
        |Some a, Some b -> Some (a@b)
      end
    |PVariant(c1,p1) , VVariant(c2,v2) -> 
       find_match p1 v2 
    |_,_   -> None 
    

(** apply the given operator to the given arguments *)
let rec eval_operator (op : operator) (v1 : value) (v2 : value) : value =
  failwith "I never could bear the idea of anyone expecting something from me.
            It always made me want to do just the opposite."

(** Format a value for printing. *)
let rec format_value (f : Format.formatter) (v : value) : unit =
  failwith "I'm going to smile, and my smile will sink down into your pupils,
            and heaven knows what it will become."

(** use format_value to print a value to the console *)
let print_value = Printer.make_printer format_value

(** use format_value to convert a value to a string *)
let string_of_value = Printer.make_string_of format_value

(******************************************************************************)
(** eval **********************************************************************)
(******************************************************************************)

let rec eval env e =
  match e with 
  |Unit -> VUnit
  |Int i -> VInt i
  |Bool b -> VBool b
  |String s -> VString s 
  |BinOp (o,e1,e2) -> begin match (eval env e1), (eval env e2) with
                      |VInt m,VInt n -> if (o=Concat) then VError "error"
                                                      else eval_operator o (VInt m) (VInt n)
                      |VString m, VString n -> if (o=Concat) then eval_operator o (VString m) (VString n)
                                                             else VError "error"
                      |_,_ -> VError "error"
                    end
  |If (e1,e2,e3) -> 
        let ve1 = eval env e1 in 
           begin match ve1 with
                    |VBool true -> eval env e2
                    |VBool false -> eval env e3
                    |_ -> VError "error"
            end
  |Var v -> VError "error"
  |Fun (v,e1) -> VClosure(v,e1,env)
  |Pair (e1,e2) -> VPair((eval env e1), (eval env e2))
  |Variant (c,e1) -> VVariant (c,(eval env e1))

  |Let (v,e1,e2) ->  let envp = ((v,ref (eval env e1)) :: env ) in
                     (eval envp e2) 

  |App (e1,e2) -> let inp = eval env e1 in
                  begin
                  match inp with
                  |VClosure (va,e11,envv) -> eval envv e2
                  |_ -> VError "error"
                end
                  (* need double check *)

  |LetRec(f,e1,e2) -> let env_new = (f,ref (VError "error")) :: env in
                      let v1 = eval env_new e1 in
                      begin match v1 with
                      |VClosure (f, somee , env_new2) -> 
                       (*     let ((snd (List.hd(env_new2))) := v1) in  *)
                            (eval (env_new2 ) e2)
                      |_ -> VError "error"
                      end  


  |Match (e1,lst) -> 
          List.fold_right(fun b a -> match (find_match (fst b) (eval env e1)) with
                                         |Some m -> eval m (snd b) 
                                         |None -> a ) lst (VError "error") 


      


   