
type void = Int of int | String of string | Float of float | Bool of bool | None

type symbol_type

val create_symbol : string -> string -> int -> void -> int -> int -> symbol_type

val get_name_symbol : symbol_type -> string

val get_type_symbol : symbol_type -> string

val get_t_id_symbol : symbol_type -> int

val get_value_symbol : symbol_type -> void

val get_ligne_symbol : symbol_type -> int

val get_colonne_symbol : symbol_type -> int

val set_value_symbol : symbol_type -> void -> unit

val get_value_string_symbol : symbol_type -> string

val print_symbol : symbol_type -> unit



type lexeme_table_type

val create_lexeme_table : unit -> lexeme_table_type

val add_lexeme : lexeme_table_type -> symbol_type -> unit

val get_lexeme : lexeme_table_type -> int -> symbol_type

val len_lexeme : lexeme_table_type -> int

val pop_lexeme : lexeme_table_type -> symbol_type

val clear_lexeme_table : lexeme_table_type -> unit

val iter_lexeme_table : lexeme_table_type -> (symbol_type -> unit) -> unit

val print_lexeme_table : lexeme_table_type -> unit



type symbol_table_type

val create_symbol_table : int -> symbol_table_type

val mem_symbol_table : symbol_table_type -> string -> bool

val find_opt_symbol_table : symbol_table_type -> string -> symbol_type option

val len_symbol_table : symbol_table_type -> int

val find_symbol_table : symbol_table_type -> string -> symbol_type

val add_symbol_table : symbol_table_type -> string -> symbol_type -> unit

val clear_symbol_table : symbol_table_type -> unit

val iter_symbol_table : symbol_table_type -> (symbol_type -> unit) -> unit

val print_symbol_table : symbol_table_type -> unit



type tac_lign_type

val print_tac_lign : tac_lign_type -> unit

type tac_type

val create_tac : string -> int -> int -> int -> tac_type

val get_tac : tac_type -> int -> tac_lign_type

val add_tac : tac_type -> tac_lign_type -> unit

val len_tac : tac_type -> int

val pop_tac : tac_type -> tac_lign_type

val clear_tac : tac_type -> unit

val iter_tac : tac_type -> (tac_lign_type -> unit) -> unit

val print_tac : tac_type -> unit



type symbol_tree_type

exception TropDeParent

val create_sy : symbol_table_type -> symbol_tree_type option -> symbol_tree_type Dynarray.t -> tac_type -> symbol_tree_type

val add_child_sy : symbol_tree_type -> symbol_tree_type -> unit

val get_child_sy : symbol_tree_type -> int -> symbol_tree_type

val get_symbol_table_sy : symbol_tree_type -> symbol_table_type

val get_parent_sy : symbol_tree_type -> symbol_tree_type option

val pre_iter_on_table_sy : -> symbol_tree_type -> (symbol_table_type -> unit) -> unit

val post_iter_on_table_sy : -> symbol_tree_type -> (symbol_table_type -> unit) -> unit

val pre_iter_on_tac_sy : -> symbol_tree_type -> (tac_type -> unit) -> unit

val post_iter_on_tac_sy : -> symbol_tree_type -> (tac_type -> unit) -> unit

val print_sy : symbol_tree_type -> unit































