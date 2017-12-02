module L = struct
  let assoc_opt n h =
    try Some (List.assoc n h)
    with Not_found -> None
end
