(* Generation of bytecode for .zo files *)

open Misc;;
open Asttypes;;
open Lambda;;
open Instruct;;
open Prim;;
open Opcodes;;
open Prim_opc;;
open Buffcode;;
open Config;;
open Labels;;
open Types;; (* for qualified_ident *)

let out_bool_test tst =
  function PTeq -> out tst
      |    PTnoteq -> out (tst + 1)
      |    PTlt -> out (tst + 2)
      |    PTgt -> out (tst + 3)
      |    PTle -> out (tst + 4)
      |    PTge -> out (tst + 5)
      |    _ -> fatal_error "out_bool_test"
;;

let out_int_const i =
  if i <= (maxint_byte-1)/2 && i >= (minint_byte-1)/2 then begin
    out opCONSTBYTE; out (i+i+1)
  end else if i <= (maxint_short-1)/2 && i >= (minint_short-1)/2 then begin
    out opCONSTSHORT; out_short (i+i+1)
  end else begin
    out opGETGLOBAL; Reloc.slot_for_literal(SCatom(Const_int i))
  end
;;

let out_tag = function
    ConstrRegular(t,_) ->
      out t
  | ConstrExtensible(name, stamp) ->
      Reloc.slot_for_tag name stamp
;;

let out_header (n, tag) =
  out_tag tag;
  out (n lsl 2);
  out (n lsr 6);
  out (n lsr 14)
;;

let rec emit = function
      [] -> ()
    | Kquote(SCatom(Const_int i)) :: code ->
        out_int_const i;
        emit code
    | Kquote(SCatom(Const_char c)) :: code ->
        out_int_const (int_of_char c);
        emit code
    | Kquote(SCblock(tag,[])) :: code ->
        begin match tag with
          ConstrRegular(t, _) ->
            if t < 10 then out (opATOM0 + t) else (out opATOM; out t)
        | ConstrExtensible(name, stamp) ->
            out opATOM; Reloc.slot_for_tag name stamp
        end;
        emit code
    | Kquote(sc) :: code ->
        out opGETGLOBAL;
        Reloc.slot_for_literal sc;
        emit code
    | Kget_global qualid :: code ->
        out opGETGLOBAL;
        Reloc.slot_for_get_global qualid;
        emit code
    | Kset_global qualid :: code ->
        out opSETGLOBAL;
        Reloc.slot_for_set_global qualid;
        emit code
    | Kaccess n :: code ->
        if n < 6 then out(opACC0 + n) else (out opACCESS; out n);
        emit code
    | Kendlet n :: Kendlet p :: code ->
        emit(Kendlet(n+p) :: code)
    | Kendlet 1 :: code ->
        out opENDLET1; emit code
    | Kendlet n :: code ->
        out opENDLET; out n; emit code
    | Kletrec1 lbl :: code ->
        out opLETREC1; out_label lbl; emit code
    | Kmakeblock(tag,n) :: code ->
        if n <= 0 then
          fatal_error "emit : Kmakeblock"
        else if n < 5 then begin
          out (opMAKEBLOCK1 + n - 1);
          out_tag tag
        end else begin
          out opMAKEBLOCK;
          out_header(n, tag)
        end;
        emit code
    | Klabel lbl :: code ->
        if lbl == nolabel then fatal_error "emit: undefined label" else
          (define_label lbl; emit code)
    | Kclosure lbl :: code ->
        out opCUR; out_label lbl; emit code
    | Kpushtrap lbl :: code ->
        out opPUSHTRAP; out_label lbl; emit code
    | Kbranch lbl :: code ->
        out opBRANCH; out_label lbl; emit code
    | Kbranchif lbl :: code ->
        out opBRANCHIF; out_label lbl; emit code
    | Kbranchifnot lbl :: code ->
        out opBRANCHIFNOT; out_label lbl; emit code
    | Kstrictbranchif lbl :: code ->
        out opBRANCHIF; out_label lbl; emit code
    | Kstrictbranchifnot lbl :: code ->
        out opBRANCHIFNOT; out_label lbl; emit code
    | Kswitch lblvect :: code ->
        out opSWITCH;
        out (Array.length lblvect);
        let orig = !out_position in
        Array.iter (out_label_with_orig orig) lblvect;
        emit code
    | Ktest(tst,lbl) :: code ->
        begin match tst with
            Peq_test ->
              out opBRANCHIFEQ; out_label lbl
          | Pnoteq_test ->
              out opBRANCHIFNEQ; out_label lbl
          | Pint_test(PTnoteqimm i) ->
              out opPUSH; out opPUSH; out_int_const i;
              out opEQ; out opPOPBRANCHIFNOT; out_label lbl
          | Pint_test x ->
              out_bool_test opBRANCHIFEQ x; out_label lbl
          | Pfloat_test(PTnoteqimm f) ->
              out opPUSH; out opPUSH; out opGETGLOBAL;
              Reloc.slot_for_literal (SCatom(Const_float (string_of_float f)));
              out opEQFLOAT; out opPOPBRANCHIFNOT; out_label lbl
          | Pfloat_test x ->
              out_bool_test opEQFLOAT x; out opBRANCHIF; out_label lbl
          | Pstring_test(PTnoteqimm s) ->
              out opPUSH; out opPUSH; out opGETGLOBAL;
              Reloc.slot_for_literal (SCatom(Const_string s));
              out opEQSTRING; out opPOPBRANCHIFNOT; out_label lbl
          | Pstring_test x ->
              out_bool_test opEQSTRING x; out opBRANCHIF; out_label lbl
          | Pnoteqtag_test tag ->
              out opBRANCHIFNEQTAG; out_tag tag; out_label lbl
        end;
        emit code
    | Kbranchinterval(low, high, lbl_low, lbl_high) :: code ->
        out opPUSH; out_int_const low; out opPUSH;
        if low != high then out_int_const high;
        out opBRANCHINTERVAL;
        out_label lbl_low;
        out_label lbl_high;
        emit code
    | Kprim Pidentity :: code ->
        emit code
    | Kprim p :: code ->
        (match p with
            Pdummy n ->
              out opDUMMY; out n
          | Ptest tst ->
              (match tst with
                  Peq_test -> out opEQ
                | Pnoteq_test -> out opNEQ
                | Pint_test tst -> out_bool_test opEQ tst
                | Pfloat_test tst -> out_bool_test opEQFLOAT tst
                | Pstring_test tst -> out_bool_test opEQSTRING tst
                | _ -> fatal_error "emit : Kprim, Ptest")
          | Pfield n ->
              if n < 4 then out (opGETFIELD0 + n) else (out opGETFIELD; out n)
          | Psetfield n ->
              if n < 4 then out (opSETFIELD0 + n) else (out opSETFIELD; out n)
          | Pccall(name, arity) ->
              if arity <= 5 then
                out (opC_CALL1 + arity - 1)
              else
                (out opC_CALLN; out arity);
              Reloc.slot_for_c_prim name
          | Pfloatprim p ->
              out opFLOATOP;
              out(opcode_for_float_primitive p)
          | Pmakeblock_mutable ->
              out opMAKEBLOCK1;
              out_tag (ConstrRegular(0,1))
          | p ->
              out(opcode_for_primitive p));
        emit code
    | Kpush :: Kget_global qualid :: Kapply :: code ->
        out opPUSH_GETGLOBAL_APPLY;
        Reloc.slot_for_get_global qualid;
        emit code
    | Kpush :: Kget_global qualid :: Ktermapply :: code ->
        out opPUSH_GETGLOBAL_APPTERM;
        Reloc.slot_for_get_global qualid;
        emit code
    | Kevent ev :: code ->
        ev.ev_pos <- !out_position;
        Event.enter ev;
        emit code
    | instr :: code ->
        out(match instr with
           Kreturn -> opRETURN
        |  Kgrab -> opGRAB
        |  Kpush -> opPUSH
        |  Kpushmark -> opPUSHMARK
        |  Klet -> opLET
        |  Kapply -> opAPPLY
        |  Ktermapply -> opAPPTERM
        |  Kpoptrap -> opPOPTRAP
        |  Kcheck_signals -> opCHECK_SIGNALS
        |  _  -> fatal_error "emit: should not happen");
        emit code
;;

