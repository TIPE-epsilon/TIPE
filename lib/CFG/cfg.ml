type vertex = {
    tac:int;(*temp*)
    symbols:int;
  }
type edge = {
    mutable top:int;
    mutable bot:int;
  }

type cfg = {size:int;
            vertices:vertex array;
            edges:edge array}

let rec generate_cfg cfg (s,d) tree= (*s:source de l'arete,d:destination*)
  match tree with
  |Nil -> (s,d)
  |Bloc l -> List.fold_left (generate_cfg cfg) (s,d) l
  |While (c,b) -> cfg.edges.(d).bot <- snd (generate_cfg cfg (generate_cfg cfg (s,d) c) b); (s,cfg.edges.(s).bot)
  |Ifb l -> let l' = ref [] in
            let ncord = List.fold_left
                          (fun (x,y) (c,b) -> let coord = (generate_cfg cfg (generate_cfg cfg (x,y) c) b) in l' := (fst coord)::(!l');cfg.edges.(y).bot<-snd coord; coord) (s,d) l in 
            List.iter (fun x -> cfg.edges.(x).top<-snd coord; cfg.edges.(x).bot <- snd coord) (!l');
            ncord
  |Expr l -> cfg.vertices.(d) <- generate_tac tree; cfg.edges.(d).top <- d+1; (d,d+1) 

let fill_cfg code =
  let result = syntax code in
  let Root l = code in
  List.map (fun Fdef (_,param,b) -> 
      generate_cfg {size:result.size; vertices:Array.make (b.size) (generate_tac param) ; edges:Array.make (b.size) {top:-1;bot:-1} } (0,1) b ) l

