module String = struct
  include String

  let starting ~w:prefix s =
    if String.length prefix <= String.length s then
      try
        for i = 0 to String.length prefix - 1 do
          if unsafe_get prefix i <> unsafe_get s i then raise Exit
        done;
        true
      with Exit -> false
    else false

  let ending ~w:postfix s =
    let ofs = String.length s - String.length postfix in
    if ofs >= 0 then
      try
        for i = 0 to String.length postfix - 1 do
          if unsafe_get postfix i <> unsafe_get s (ofs + i) then raise Exit
        done;
        true
      with Exit -> false
    else false
end

module List = struct
  include List

  let rec remove_consecutive_duplicates equal = function
    | ([] | [_]) as l -> l
    | x :: ((y :: _) as tl) ->
       if equal x y then remove_consecutive_duplicates equal tl
       else x :: remove_consecutive_duplicates equal tl

  let rec filter_map f = function
    | [] -> []
    | x :: tl -> match f x with
                 | Some y -> y :: filter_map f tl
                 | None -> filter_map f tl
end
