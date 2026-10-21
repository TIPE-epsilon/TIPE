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
          
type tac =
  |Swap of (int*int)
  |Assign of (int*int*int)
  |Halloc of (int*int)
  |Free of (int*int)
  |Print of (int*int)
  |Fcall of (int*int)
  |Param of int
  |Deref of (int*int)
  |Adress of (int*int)
  |Access of (int*int*int)
  |Or of (int*int*int)
  |And of (int*int*int)
  |Eq of x
  |Neq of x
  |Gr of x
  |Ls of x
  |Geq of x
  |Leq of x
  |Not of y
  |Add of x
  |Sub of x
  |Mul of x
  |Pow of x
  |Div of x
  |Mod of x
  |PostIncr of y
  |PostDec of y
  |PreIncr of y
  |PreDec of y
  |Neg of y

    Deref | Adress | Access | Or | And | Eq | Neq | Gr | Ls | Geq | Leq | Not | Add | Sub | Mul | Pow | Div | Mod | PostIncr | PreIncr | PostDec | Predec | Neg 
