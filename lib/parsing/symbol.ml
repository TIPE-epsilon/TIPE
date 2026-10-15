(*TODO toutes les fonctions pour tous les types *)

type void = Int of int | String of string | Float of float | Bool of bool

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

(* TODO print *)

(*================================================================*)

type lexeme_table_type = symbol_type Dynarray.t

(*================fonctions pour lexeme_table_type================*)

let create_lexeme_table = Dynarray.create 

let add_lexeme (t:lexeme_table_type) (x:symbol_type) = Dynarray.add_last t x

let get_lexeme (t:lexeme_table_type) (i:int) = Dynarray.get t i

(*TODO len pop clear iter print*)





(*================================================================*)

type symbol_table_type = (string, symbol_type) Hashtbl.t

(*================fonctions pour symbol_table_type================*)

(*================================================================*)

type tac_type = (int Dynarray.t) Dynarray.t

(*=====================fonctions pour tac_type====================*)

(*================================================================*)

type symbol_tree_type = {
					node : symbol_table_type;
					parent : symbol_tree_type option;
					children : symbol_tree_type Dynarray.t;
					tac : tac_type;
					global_last : int;
					last_ind : int;
					first_ind : int;
					} 

(*================fonctions pour symbol_tree_type=================*)

(*================================================================*)









































