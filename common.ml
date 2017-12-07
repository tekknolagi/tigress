module L = struct
  let assoc_opt n h =
    try Some (List.assoc n h)
    with Not_found -> None

  let map_concat f l =
    List.concat @@ List.map f l
end

module S = struct
  let map_concat s f l =
    String.concat s @@ List.map f l
end
