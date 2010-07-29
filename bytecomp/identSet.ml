module S = Set.Make(struct
                      type t = Ident.t
                      let compare = compare
                    end)

include S
