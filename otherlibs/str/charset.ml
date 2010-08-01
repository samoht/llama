    type t = string (* of length 32 *)

    (*let empty = String.make 32 '\000'*)
    let full = String.make 32 '\255'

    let make_empty () = String.make 32 '\000'

    let add s c =
      let i = Char.code c in
      s.[i lsr 3] <- Char.chr(Char.code s.[i lsr 3] lor (1 lsl (i land 7)))

    let add_range s c1 c2 =
      for i = Char.code c1 to Char.code c2 do add s (Char.chr i) done

    let singleton c =
      let s = make_empty () in add s c; s

    (*let range c1 c2 =
      let s = make_empty () in add_range s c1 c2; s
    *)
    let complement s =
      let r = String.create 32 in
      for i = 0 to 31 do
        r.[i] <- Char.chr(Char.code s.[i] lxor 0xFF)
      done;
      r

    let union s1 s2 =
      let r = String.create 32 in
      for i = 0 to 31 do
        r.[i] <- Char.chr(Char.code s1.[i] lor Char.code s2.[i])
      done;
      r

    let disjoint s1 s2 =
      try
        for i = 0 to 31 do
          if Char.code s1.[i] land Char.code s2.[i] <> 0 then raise Exit
        done;
        true
      with Exit ->
        false

    let iter fn s =
      for i = 0 to 31 do
        let c = Char.code s.[i] in
        if c <> 0 then
          for j = 0 to 7 do
            if c land (1 lsl j) <> 0 then fn (Char.chr ((i lsl 3) + j))
          done
      done

    let expand s =
      let r = String.make 256 '\000' in
      iter (fun c -> r.[Char.code c] <- '\001') s;
      r

    let fold_case s =
      let r = make_empty() in
      iter (fun c -> add r (Char.lowercase c); add r (Char.uppercase c)) s;
      r
