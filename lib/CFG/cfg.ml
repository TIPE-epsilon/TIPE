open Type

let (let*) o f = o f
   
let new_temp =
  let tmp = ref (-1) in   (*trouver un moyen de mettre la bonne valeur*)
  fun () -> incr tmp;
            (*le mettre dans la table des symboles*)
            !tmp
  
            
let generate_tac_bin_op (op:binOp) d s1 s2 =
  match op with
  |Access -> Access (d,s1,s2)
  |Or -> Or (d,s1,s2)
  |And -> And (d,s1,s2)
  |Eq -> Eq (d,s1,s2)
  |Neq -> Neq (d,s1,s2)
  |Gr -> Gr (d,s1,s2)
  |Ls -> Ls (d,s1,s2)
  |Leq ->Leq (d,s1,s2)
  |Geq -> Geq (d,s1,s2)
  |Add -> Add (d,s1,s2)
  |Sub -> Sub (d,s1,s2)
  |Pow -> Pow (d,s1,s2)
  |Mul -> Mul (d,s1,s2)
  |Div -> Div (d,s1,s2)
  |Mod -> Mod (d,s1,s2)
        
let generate_tac_un_op (op:unOp) d s1 =
  match op with
  |Deref-> Deref (d,s1)
  |Adress -> Adress (d,s1)
  |Not -> Not (d,s1)
  |PostIncr -> PostIncr (d,s1)
  |PreIncr -> PreIncr (d,s1)
  |PostDec -> PostDec (d,s1)
  |PreDec -> PreDec (d,s1)
  |Neg -> Neg (d,s1)

let rec generate_adressable (adr:addressable) l k =
  match adr with
  |Var x -> k (x,l)
  |Access (lve,e) ->
    let id1,c1 = generate_lval_expr lve l Fun.id in
    let id2,c2 = generate_expression e c1 Fun.id in
    let t = new_temp () in
    (t,Access(t,id1,id2)::c2) |> k
  |Deref lve ->
    let id,code = generate_lval_expr lve l Fun.id in
    let t = new_temp () in
    (t,Deref(t,id)::code) |> k

and generate_lval_expr (lve:lval_expr) l k =
  match lve with
  |Add (le,x) ->
    let* id,code = generate_lval_expr le l in
    let t = new_temp () in
    (t,Add (t,id,x)::code) |> k

  |Addr adr ->
    let id,code = generate_adressable adr l Fun.id in
    let t = new_temp () in
    (t,Adress(t,id)::code) |> k

  |Access (le,e)->
    let* id1,c1 = generate_lval_expr le l in
    let id2,c2 = generate_expression e c1 Fun.id in
    let t = new_temp () in
    (t,Access (t,id1,id2)::c2) |> k
    
  |Deref le ->
    let* id,c = generate_lval_expr le l in
    let t = new_temp () in
    (t,Deref (t,id)::c) |> k
    
  |Var x -> k (x,l) 
  
        
and generate_left_value (lv:lval) l k =
  match lv with
  |Access (le,e) ->
     let id1,c1 = generate_lval_expr le l Fun.id in
     let id2,c2 = generate_expression e c1 Fun.id in
     let t = new_temp () in
     (t,Access (t,id1,id2)::c2) |> k

  |Deref le ->
    let id,c = generate_lval_expr le l Fun.id in
    let t = new_temp () in
    (t,Deref (t,id)::c) |> k
    
  |Var x -> k (x,l) 
                                  

and generate_expression (e:expression) l k =
  match e with
  | Fcall (id,pl) -> failwith "todo"
           
  | Assign (lv,e)->
     let* id1,c1 = generate_expression e l in
     let id2,c2 = generate_left_value lv c1 Fun.id in
     let t = new_temp () in
     (t,Assign(t,id2,id1)::c2) |> k
     
  | Swap (lv1,lv2) ->
     let* id1,c1 = generate_left_value lv1 l in
     let* id2,c2 = generate_left_value lv2 c1 in
     (id2,Swap (id1,id2)::c2) |> k
     
  | Halloc e ->
     let* id,code = generate_expression e l in
     let t = new_temp () in
     (t,Halloc(t,id)::code) |> k
     
  | Free lv ->
     let id,code = generate_left_value lv l Fun.id in
     let t = new_temp () in
     (t,Free(t,id)::code) |> k
     
  |Print e ->
    let* id,code = generate_expression e l in
    let t = new_temp () in
    (t,Print(t,id)::code) |> k
    
  | BinOp (op,e1,e2) ->
     let* id1,c1 = generate_expression e1 l in
     let* id2,c2 = generate_expression e2 c1 in
     let t = new_temp () in 
     (t,(generate_tac_bin_op op t id1 id2)::c2) |> k
     
  | UnOp (op,e) ->
     let* id,c = generate_expression e l in
     let t = new_temp () in
     (t, (generate_tac_un_op op t id)::c) |> k 
     
  | Var x -> k (x, l)
        
and generate_bloc (tree:ast) (k: bloc -> bloc) =
  match tree with
  | Bloc l -> failwith "todo"
            
  | While (c,t,e) ->
     let* tbloc = generate_bloc t in     (*comment dire à tbloc que son tbloc est le bloc qui vient d'être créé*)
     begin
     match e with 
     |Some e' ->
       let* ebloc = generate_bloc e' in
       let c_id, code = generate_expression c [] Fun.id in
       {
         code;
         condition = Some c_id;
         tbloc = Some tbloc;
         ebloc = Some ebloc;
       } |> k
     |None -> let c_id,code = generate_expression c [] Fun.id in
              {
                code;
                condition = Some c_id;
                tbloc = Some tbloc;
                ebloc = None;
              } |> k 
     end              
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

  | Fcall (id, l) -> failwith "todo"
                                                                                                                                                   
                   
  | Assign (x, e) ->
     k {code = (let source, code = generate_expression e [] Fun.id in
               let id,c2 = generate_left_value x code Fun.id in
               let t = new_temp () in
               (Assign (t,id,source))::c2);
               condition = None;
               tbloc = None;
               ebloc = None;
       }
  | Swap (e1,e2) ->
     let id1, code1 = generate_left_value e1 [] Fun.id in
     let id2, code2 = generate_left_value e2 code1 Fun.id in
     {
       code = Swap (id1,id2)::code2;
       condition = None;
       tbloc = None;
       ebloc = None;
     } |> k 
       
       
