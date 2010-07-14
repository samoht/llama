(* Opcodes for the simple primitives. *)

open Misc;;
open Prim;;
open Opcodes;;

let opcode_for_primitive = function
    Pupdate -> opUPDATE
  | Praise -> opRAISE
  | Pnot -> opBOOLNOT
  | Ptag_of -> opTAGOF
  | Pnegint -> opNEGINT
  | Psuccint -> opSUCCINT
  | Ppredint -> opPREDINT
  | Paddint -> opADDINT
  | Psubint -> opSUBINT
  | Pmulint -> opMULINT
  | Pdivint -> opDIVINT
  | Pmodint -> opMODINT
  | Pandint -> opANDINT
  | Porint -> opORINT
  | Pxorint -> opXORINT
  | Pshiftleftint -> opSHIFTLEFTINT
  | Pshiftrightintsigned -> opSHIFTRIGHTINTSIGNED
  | Pshiftrightintunsigned -> opSHIFTRIGHTINTUNSIGNED
  | Pincr -> opINCR
  | Pdecr -> opDECR
  | Pintoffloat -> opINTOFFLOAT
  | Pstringlength -> opSTRINGLENGTH
  | Pgetstringchar -> opGETSTRINGCHAR
  | Psetstringchar -> opSETSTRINGCHAR
  | Pmakevector -> opMAKEVECTOR
  | Pvectlength -> opVECTLENGTH
  | Pgetvectitem -> opGETVECTITEM
  | Psetvectitem -> opSETVECTITEM
  | _ -> fatal_error "opcode_for_primitive"
;;

let opcode_for_float_primitive = function
    Pfloatofint -> opFLOATOFINT
  | Pnegfloat -> opNEGFLOAT
  | Paddfloat -> opADDFLOAT
  | Psubfloat -> opSUBFLOAT
  | Pmulfloat -> opMULFLOAT
  | Pdivfloat -> opDIVFLOAT
;;
