(*TODO mli *)

type void = Int of int | String of string | Float of float | Bool of bool | None

type symbol_type = {
			name : string;
			s_type: string;
			t_id : int;
			mutable value : void;
			ligne : int;
			colonne : int;
			}
			
(*===================fonctions pour symbol_type===================*)

let create_symbol (name:string) (s_type:string) (t_id:int) (value:void) (ligne:int) (colonne:int) = 
	{name = name;s_type = s_type; t_id = t_id; value = value; ligne = ligne; colonne = colonne}

let get_name_symbol (s:symbol_type) = s.name

let get_type_symbol (s:symbol_type) = s.s_type

let get_t_id_symbol (s:symbol_type) = s.t_id

let get_value_symbol (s:symbol_type) = s.value

let get_ligne_symbol (s:symbol_type) = s.ligne

let get_colonne_symbol (s:symbol_type) = s.colonne

let set_value_symbol (s:symbol_type) (v:void) = s.value <- v

let get_value_string_symbol (s:symbol_type) = match s.value with
											| Int v -> string_of_int v
											| Float v -> string_of_float v
											| Bool v -> if v then "true" else "false"
											| String v -> v
											| None -> "None"

let print_symbol (s:symbol_type) = print_string ("[ name : "^s.name^" ; type : "^s.s_type^" ; t_id : "^(string_of_int s.t_id)^" ; value : "^(get_value_string_symbol s)^" ; ligne : "^(string_of_int s.ligne)^" ; colonne : "^(string_of_int s.colonne)^"]\n")

(*================================================================*)

type lexeme_table_type = symbol_type Dynarray.t

(*================fonctions pour lexeme_table_type================*)

let create_lexeme_table = Dynarray.create 

let add_lexeme (t:lexeme_table_type) (x:symbol_type) = Dynarray.add_last t x

let get_lexeme (t:lexeme_table_type) (i:int) = Dynarray.get t i

let len_lexeme (t:lexeme_table_type) = Dynarray.length t

let pop_lexeme (t:lexeme_table_type) = Dynarray.pop_last t

let clear_lexeme_table (t:lexeme_table_type) = Dynarray.clear t

let iter_lexeme_table (t:lexeme_table_type) (f:symbol_type -> unit) = Dynarray.iter f t

let print_lexeme_table (t:lexeme_table_type) = print_string "[|\n";
												iter_lexeme_table t (fun x -> print_string "    "; print_symbol x);
												print_string "|]\n"


(*================================================================*)

type symbol_table_type = (string, symbol_type) Hashtbl.t (*key = name vu qu'ils ne sont qu'une seule fois dedans*)

(*================fonctions pour symbol_table_type================*)

let create_symbol_table = Hashtbl.create

let mem_symbol_table (t:symbol_table_type) (key:string) = Hashtbl.mem t key

let find_opt_symbol_table (t:symbol_table_type) (key:string) = Hashtbl.find_opt t key

let len_symbol_table (t:symbol_table_type) = Hashtbl.length t

let find_symbol_table (t:symbol_table_type) (key:string) = Hashtbl.find t key

let add_symbol_table (t:symbol_table_type) (key:string) (value:symbol_type) = Hashtbl.replace t key value

let clear_symbol_table (t:symbol_table_type) = Hashtbl.reset t

let iter_symbol_table (t:symbol_table_type) (f:string -> symbol_type -> unit) = Hashtbl.iter f t

let print_symbol_table (t:symbol_table_type) = print_string "{ \n";
												iter_symbol_table t (fun x y -> print_string "    "; print_symbol y);
												print_string " }\n"

(*================================================================*)

type tac_lign_type = {op : string;
				s1 : int;
				s2 : int;
				s3 : int;
				}
				
let print_tac_lign (t:tac_lign_type) = print_string ("{ op : "^t.op^"; s1 : "^(string_of_int t.s1)^"; s2 : "^(string_of_int t.s2)^"; s3 : "^(string_of_int t.s3)^" }\n");

type tac_type = tac_lign_type Dynarray.t

(*====================fonctions pour tac_type=====================*)

let create_tac = Dynarray.create 

let create_tac_lign (op:string) (s1:int) (s2:int) (s3:int) = {op=op; s1=s1; s2=s2; s3=s3}

let get_tac (t:tac_type) (i:int) = Dynarray.get t i

let add_tac (t:tac_type) (x:tac_lign_type) = Dynarray.add_last t x

let len_tac (t:tac_type) = Dynarray.length t

let pop_tac (t:tac_type) = Dynarray.pop_last t

let clear_tac (t:tac_type) = Dynarray.clear t

let iter_tac (t:tac_type) (f:tac_lign_type -> unit) = Dynarray.iter f t

let print_tac (t:tac_type) = print_string "[\n";
							iter_tac t (fun x -> print_string "    "; print_tac_lign x);
							print_string "]\n"

(*================================================================*)

type symbol_tree_type = {
					node : symbol_table_type;
					mutable parent : symbol_tree_type option;
					children : symbol_tree_type Dynarray.t;
					tac : tac_type;
					(*jai poubelle les ind je pense ça sert à rien*)
					}

(*================fonctions pour symbol_tree_type=================*)

exception TropDeParent

let create_sy (node:symbol_table_type) (parent:symbol_tree_type option) (children:symbol_tree_type Dynarray.t) (tac:tac_type) =
	{node = node; parent = parent; children = children; tac = tac}
	
let add_child_sy (t:symbol_tree_type) (c:symbol_tree_type) = if c.parent != None then raise TropDeParent else c.parent <- Some(t); Dynarray.add_last t.children c

let get_child_sy (t:symbol_tree_type) (i:int) = Dynarray.get (t.children) i

let get_symbol_table_sy (t:symbol_tree_type) = t.node

let get_parent_sy (t:symbol_tree_type) = t.parent

let rec pre_iter_on_table_sy (t:symbol_tree_type) (f:symbol_table_type -> unit) = 
	f t.node;
	if not (Dynarray.is_empty t.children) then
		begin
			for i=0 to (Dynarray.length t.children)-1 do
				pre_iter_on_table_sy (get_child_sy t i) f
			done
		end

let rec post_iter_on_table_sy (t:symbol_tree_type) (f:symbol_table_type -> unit) = 
	if not (Dynarray.is_empty t.children) then
		begin
			for i=0 to (Dynarray.length t.children)-1 do
				post_iter_on_table_sy (get_child_sy t i) f
			done
		end;
	f t.node
		
let rec pre_iter_on_tac_sy (t:symbol_tree_type) (f:tac_type -> unit) = 
	f t.tac;
	if not (Dynarray.is_empty t.children) then
		begin
			for i=0 to (Dynarray.length t.children)-1 do
				pre_iter_on_tac_sy (get_child_sy t i) f
			done
		end
		
let rec post_iter_on_tac_sy (t:symbol_tree_type) (f:tac_type -> unit) = 
	if not (Dynarray.is_empty t.children) then
		begin
			for i=0 to (Dynarray.length t.children)-1 do
				post_iter_on_tac_sy (get_child_sy t i) f
			done
		end;
	f t.tac
	
let print_sy (t:symbol_tree_type) = (*pre*)
	print_string "###\n";
	let rec p h =
		print_symbol_table h.node; print_tac h.tac;
		if not (Dynarray.is_empty h.children) then
			begin
				for i=0 to (Dynarray.length h.children)-1 do
					p (get_child_sy h i)
				done
			end
	in p t;
	print_string "###\n"

(*================================================================*)



(*==============================tests=============================*)

(*
let main () =
	let fleche_gauche = create_symbol "<-" "" 0 None 0 0 in
	let fleche_droite = create_symbol "->" "" 0 None 0 0 in
	let plus = create_symbol "+" "" 0 None 0 0 in
	let moins = create_symbol "-" "" 0 None 0 0 in
	let etoile = create_symbol "*" "" 0 None 0 0 in
	let slash = create_symbol "/" "" 0 None 0 0 in

	(*let _ = print_symbol fleche_gauche*)

	let lexemes = create_lexeme_table () in

	add_lexeme lexemes fleche_gauche;
	add_lexeme lexemes fleche_droite;
	add_lexeme lexemes plus;
	add_lexeme lexemes moins;
	add_lexeme lexemes etoile;
	add_lexeme lexemes slash;
	
	let table = create_symbol_table 6 in
	
	add_symbol_table table "<-" fleche_gauche;
	add_symbol_table table "->" fleche_droite;
	add_symbol_table table "+" plus;
	add_symbol_table table "-" moins;
	add_symbol_table table "*" etoile;
	add_symbol_table table "/" slash;
	
	let tac = create_tac () in
	
	let l1 = create_tac_lign "<-" 1 2 3 in
	let l2 = create_tac_lign "->" 3 2 1 in
	
	add_tac tac l1;
	add_tac tac l2;
	
	let sy = create_sy table None (Dynarray.create ()) tac in
	
	let sy2 = create_sy table None (Dynarray.create ())  tac in
	
	add_child_sy sy sy2;
	
	
	print_lexeme_table lexemes;
	print_symbol_table table;
	print_tac tac;
	print_sy sy
	
let _ = main ()
*)
(*================================================================*)

































