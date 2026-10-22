
type binOp = Access | Or | And | Eq | Neq | Gr | Ls | Geq | Leq | Add | Sub | Mul | Pow | Div | Mod 
type unOp = Deref | Adress | Not | PostIncr | PreIncr | PostDec | PreDec | Neg 
                                               
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
  | Swap of (lval*lval)
  | Halloc of expression
  | Free of lval
  | Print of expression
  | BinOp of (binOp * expression * expression)
  | UnOp of (unOp * expression)
  | Var of int
    
type ast =
  | Bloc of ast list
  | While of (expression * ast *(ast option)) (* trois enfants : condition, corps, suite du code*)
  | If of (expression * ast * ast)
  | Fcall of (int * expression list)
  | Assign of (lval * expression)
  | Swap of (lval * lval)
          
type tac =
  |Assign of (int*int*int)
  |Swap of (int*int)
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
  |Eq of (int*int*int)
  |Neq of (int*int*int)
  |Gr of (int*int*int)
  |Ls of (int*int*int)
  |Geq of (int*int*int)
  |Leq of (int*int*int)
  |Not of (int*int)
  |Add of (int*int*int)
  |Sub of (int*int*int)
  |Mul of (int*int*int)
  |Pow of (int*int*int)
  |Div of (int*int*int)
  |Mod of (int*int*int)
  |PostIncr of (int*int)
  |PostDec of (int*int)
  |PreIncr of (int*int)
  |PreDec of (int*int)
  |Neg of (int*int)

type bloc = {
  code : tac list;
  condition : int option;
  tbloc : bloc option;
  ebloc : bloc option;
}
