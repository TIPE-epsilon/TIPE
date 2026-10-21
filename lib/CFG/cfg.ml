type bloc = {
  code : tac list;
  condition : int option;
  tbloc : bloc option;
  ebloc : bloc option;
}

type op = Deref | Adress | Access | Or | And | Eq | Neq | Gr | Ls | Geq | Leq | Not | Add | Sub | Mul | Pow | Div | Mod | PostIncr | PreIncr | PostDec | Predec | Neg 
          
type addressable =
  Var of int
| Access of lval_expr * expression
| Deref of lval_expr
and lval_expr =
  Add of lval_expr * int
| Addr of addressable
| Var of int
| Access of lval_expr * expression
| Deref of lval_expr
         
and lval =
  Var of int
| Access of lval_expr * expression
| Deref of lval_expr
         
and expression =
  | Fcall of (int * expression list)
  | Assign of (lval * expression)
  | Halloc of expression
  | Free of lval
  | Print of expression
  | BinOp of (op * expression * expression)
  | UnOp of (op * expression)
  | Param of expression  
  | lval
    
and ast =
  | Bloc of ast list
  | While of (expression * ast * ast) (* trois enfants : condition, corps, suite du code*)
  | If of (expression * ast * ast)
  | Fcall of (int * expression list)
  | Assign of (lval * expression)
  | Swap of (lval * lval)
          
type tac = Add of (int * int * int) | Assign of (int * int)

let* o f = o f

let generate_tac_op op d s1 s2 =
  match op with
  |"'" -> Deref (d,s1)
  |"&" -> Adress (d,s1)
  |"[]" -> Access (d,s1,s2)
  |"||" -> Or (d,s1,s2)
  |"&&" -> And (d,s1,s2)
  |"=" -> Eq (d,s1,s2)
  |"!=" -> Neq (d,s1,s2)
  |">" -> Gr (d,s1,s2)
  |"<" -> Ls (d,s1,s2)
  |"<=" ->Leq (d,s1,s2)
  |">=" -> Geq (d,s1,s2)
  |"!" -> Not (d,s1)
  |"+" -> Add (d,s1,s2)
  |"-" -> Sub (d,s1,s2)
  |"^" -> Pow (d,s1,s2)
  |"*" -> Mul (d,s1,s2)
  |"/" -> Div (d,s1,s2)
  |"%" -> Mod (d,s1,s2)
  |"PostIncr" -> PostIncr (d,s1)
  |"PreIncr" -> PreIncr (d,s1)
  |"PostDec" -> PostDec (d,s1)
  |"PreDec" -> PreDec (d,s1)
  |"Neg" -> Neg (d,s1)
  |"<-" -> Assign (d,s1,s2)
  |"halloc" -> Halloc (d,s1)
  |"free" -> Free (d,s1)
  |"print" -> Print (d,s1)
         
       
let generate_expression (e l k : expression -> tac list ->
                                 (int * tac_list -> int * tac_list)
                                 -> int * tac_list) =
  match e with
  | Fcall (id,pl) ->

     
  | BinOp (op,e1,e2) ->
     let* id1,c1 = generate_expression e1 l in
     let* id2,c2 = generate_expression e2 c1 in
     let t = new_temp () in 
     k (t,(generate_tac_op op t id1 id2)::c2)
     
  | UnOp (op,e) ->
     let* id,c = generate_expression e l in
     let t = new_temp () in
     k (t, (generate_tac_op op t id 0)::c) 
     
  | Var x -> k (x, l)
           
and generate_bloc tree (k: bloc -> bloc) =
  match tree with
  | Bloc l -> failwith "todo"
            
  | While (c,t,e) ->
     let* tbloc = generate_bloc t in     (*comment dire à tbloc que son tbloc est le bloc qui vient d'être créé*)
     let* ebloc = generate_bloc e in
     let c_id, code = generate_expression c [] Fun.id in
     {
       code;
       condition = Some c_id;
       tbloc = Some tbloc;
       ebloc = Some ebloc;
     } |> k
     
  | If (c, t, e) ->
     let* tbloc = generate_bloc t in
     let* ebloc = generate_bloc e in

     let c_id, code = generate_expression c [] Fun.id in
     {
       code;
       condition = Some c_id;
       tbloc = Some tbloc;
       ebloc = Some ebloc;
     } |> k

  | Fcall (id, l) -> 
                   
  | Assign (x, e) ->
     k {code = let source, code = generate_expression e [] Fun.id
               in (Assign (x, source))::code;
                  condition = None;
                  tbloc = None;
                  ebloc = None;
       }
  | Swap (e1,e2) ->
     let id1, code1 = generate_expression e1 [] Fun.id
     and id2, code2 = generate_expression e2 [] Fun.id in
     k {
         code = (Swap (id1,id2)::[])@code1@code2;
         condition = None;
         tbloc = None;
         ebloc = None;
       }
       
       
