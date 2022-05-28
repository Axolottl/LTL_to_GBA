module Make(Ord:Set.OrderedType) = struct
  module S =
    Set.Make(Ord)
  include S
  
  let of_option = 
   function
    | Some(item) 
    	-> singleton item
    | None 
    	-> empty
    
  let of_list list1 = List.fold_left (fun element1 element2 -> add element2 element1) empty list1
    
  let pop element =
    let item = choose element in
    (item, remove item element)
    
  let unions = List.fold_left union empty
      
  let map_list key valeur = List.map key (elements valeur)

  let map key valeur = of_list (map_list key valeur)
  
end
