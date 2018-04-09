
type expr =
  Vrai
| Faux
| X of int
| Non of expr
| Ou of expr * expr
| Et of expr * expr
| Oux of expr * expr
| Implique of expr * expr
| Equivaut of expr * expr


let neg e = match e with
| Vrai -> Faux 
| Faux -> Vrai
| _ -> assert false

let disj e = match e with 
| (Vrai,Faux) -> Vrai 
| (Faux,Vrai) -> Vrai
| (Vrai,Vrai) -> Vrai
| (Faux,Faux) -> Faux
| _ -> assert false

let conj e = match e with
| (Vrai,Faux) -> Faux 
| (Faux,Vrai) -> Faux
| (Vrai,Vrai) -> Vrai
| (Faux,Faux) -> Faux
| _ -> assert false

let exel e = match e with
| (Vrai,Faux) -> Vrai 
| (Faux,Vrai) -> Vrai
| (Vrai,Vrai) -> Faux
| (Faux,Faux) -> Faux
| _ -> assert false

let imp e = match e with
| (Vrai,Faux) -> Faux 
| (Faux,Vrai) -> Vrai
| (Vrai,Vrai) -> Vrai
| (Faux,Faux) -> Vrai
| _ -> assert false

let equiv e = match e with
| (Vrai,Faux) -> Faux 
| (Faux,Vrai) -> Faux
| (Vrai,Vrai) -> Vrai
| (Faux,Faux) -> Vrai
| _ -> assert false

 
let rec evalue e = match e with
|	Vrai -> Vrai
| Faux -> Faux
| Non a -> neg (evalue a)
| Ou (a,b) -> disj (evalue a,evalue b)
| Et (a,b) -> conj (evalue a, evalue b)
| Oux (a,b) -> exel (evalue a, evalue b)
| Implique (a,b) -> imp (evalue a, evalue b)
| Equivaut (a,b) -> equiv (evalue a, evalue b)

