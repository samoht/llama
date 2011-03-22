external random_seed: unit -> int = "llama_sys_random_seed";;

  type t = { st : int array; mutable idx : int };;

  let new_state () = { st = Array.make 55 0; idx = 0 };;
  let assign st1 st2 =
    Array.blit st2.st 0 st1.st 0 55;
    st1.idx <- st2.idx;
  ;;

  let full_init s seed =
    let combine accu x = Digest.string (accu ^ string_of_int x) in
    let extract d =
      Char.code d.[0] + (Char.code d.[1] lsl 8) + (Char.code d.[2] lsl 16)
      + (Char.code d.[3] lsl 24)
    in
    let seed = if seed = [| |] then [| 0 |] else seed in
    let l = Array.length seed in
    for i = 0 to 54 do
      s.st.(i) <- i;
    done;
    let accu = ref "x" in
    for i = 0 to 54 + max 55 l do
      let j = i mod 55 in
      let k = i mod l in
      accu := combine !accu seed.(k);
      s.st.(j) <- s.st.(j) lxor extract !accu;
    done;
    s.idx <- 0;
  ;;

  let make seed =
    let result = new_state () in
    full_init result seed;
    result
  ;;

  let make_self_init () = make [| random_seed () |];;

  let copy s =
    let result = new_state () in
    assign result s;
    result
  ;;

  (* Returns 30 random bits as an integer 0 <= x < 1073741824 *)
  let bits s =
    s.idx <- (s.idx + 1) mod 55;
    let newval = s.st.((s.idx + 24) mod 55)
                 + (s.st.(s.idx) lxor ((s.st.(s.idx) lsr 25) land 31)) in
    s.st.(s.idx) <- newval;
    newval land 0x3FFFFFFF   (* land is needed for 64-bit arch *)
  ;;

  let rec intaux s n =
    let r = bits s in
    let v = r mod n in
    if r - v > 0x3FFFFFFF - n + 1 then intaux s n else v
  ;;
  let int s bound =
    if bound > 0x3FFFFFFF || bound <= 0
    then invalid_arg "Random.int"
    else intaux s bound
  ;;

  let rec int32aux s n =
    let b1 = Int32.of_int (bits s) in
    let b2 = Int32.shift_left (Int32.of_int (bits s land 1)) 30 in
    let r = Int32.logor b1 b2 in
    let v = Int32.rem r n in
    if Int32.sub r v > Int32.add (Int32.sub Int32.max_int n) 1l
    then int32aux s n
    else v
  ;;
  let int32 s bound =
    if bound <= 0l
    then invalid_arg "Random.int32"
    else int32aux s bound
  ;;

  let rec int64aux s n =
    let b1 = Int64.of_int (bits s) in
    let b2 = Int64.shift_left (Int64.of_int (bits s)) 30 in
    let b3 = Int64.shift_left (Int64.of_int (bits s land 7)) 60 in
    let r = Int64.logor b1 (Int64.logor b2 b3) in
    let v = Int64.rem r n in
    if Int64.sub r v > Int64.add (Int64.sub Int64.max_int n) 1L
    then int64aux s n
    else v
  ;;
  let int64 s bound =
    if bound <= 0L
    then invalid_arg "Random.int64"
    else int64aux s bound
  ;;

  let nativeint =
    if Nativeint.size = 32
    then fun s bound -> Nativeint.of_int32 (int32 s (Nativeint.to_int32 bound))
    else fun s bound -> Int64.to_nativeint (int64 s (Int64.of_nativeint bound))
  ;;

  (* Returns a float 0 <= x < 1 with at most 90 bits of precision. *)
  let rawfloat s =
    let scale = 1073741824.0
    and r0 = Pervasives.float (bits s)
    and r1 = Pervasives.float (bits s)
    and r2 = Pervasives.float (bits s)
    in ((r0 /. scale +. r1) /. scale +. r2) /. scale
  ;;

  let float s bound = rawfloat s *. bound;;

  let bool s = (bits s land 1 = 0);;
