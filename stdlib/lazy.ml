type 'a repr =
  | Suspension of (unit -> 'a)
  | Value of 'a

type 'a t = 'a repr ref

let force r =
  begin match !r with
    | Value x -> x
    | Suspension f -> let x = f () in r := Value x; x
  end
