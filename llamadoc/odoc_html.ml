(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: odoc_html.ml 10493 2010-06-04 05:37:50Z guesdon $ *)

(** Generation of html documentation.*)

let print_DEBUG s = print_string s ; print_newline ()

open Odoc_parameter
open Odoc_value
open Odoc_type
open Odoc_exception
open Odoc_module


(** The functions used for naming files and html marks.*)
    (** The prefix for types marks. *)
    let naming_mark_type = "TYPE"

    (** The prefix for functions marks. *)
    let naming_mark_function = "FUN"

    (** The prefix for exceptions marks. *)
    let naming_mark_exception = "EXCEPTION"

    (** The prefix for values marks. *)
    let naming_mark_value = "VAL"

    (** The prefix for attributes marks. *)
    let naming_mark_attribute = "ATT"

    (** The prefix for methods marks. *)
    let naming_mark_method = "METHOD"

    (** The prefix for code files.. *)
    let naming_code_prefix = "code_"

    (** The prefix for type files.. *)
    let naming_type_prefix = "type_"

    (** Return the two html files names for the given module or class name.*)
    let naming_html_files name =
      let qual =
        try
          let i = String.rindex name '.' in
          match name.[i + 1] with
          | 'A'..'Z' -> ""
          | _ -> "-c"
        with Not_found -> ""
      in
      let prefix = name^qual in
      let html_file = prefix^".html" in
      let html_frame_file = prefix^"-frame.html" in
      (html_file, html_frame_file)

    (** Return the target for the given prefix and simple name. *)
    let naming_target pref simple_name = pref^simple_name

    (** Return the complete link target (file#target) for the given prefix string and complete name.*)
    let naming_complete_target pref complete_name =
      let simple_name = Odoc_name.simple complete_name in
      let module_name =
        let s = Odoc_name.father complete_name in
        if s = "" then simple_name else s
      in
      let (html_file, _) = naming_html_files module_name in
      html_file^"#"^(naming_target pref simple_name)

    (** Return the link target for the given type. *)
    let naming_type_target t = naming_target naming_mark_type (Odoc_name.simple t.ty_name)

    (** Return the complete link target for the given type. *)
    let naming_complete_type_target t = naming_complete_target naming_mark_type t.ty_name

    (** Return the link target for the given exception. *)
    let naming_exception_target e = naming_target naming_mark_exception (Odoc_name.simple e.ex_name)

    (** Return the complete link target for the given exception. *)
    let naming_complete_exception_target e = naming_complete_target naming_mark_exception e.ex_name

    (** Return the link target for the given value. *)
    let naming_value_target v = naming_target naming_mark_value (Odoc_name.simple v.val_name)

    (** Return the given value name where symbols accepted in infix values
       are replaced by strings, to avoid clashes with the filesystem.*)
    let naming_subst_infix_symbols name =
      let len = String.length name in
      let buf = Buffer.create len in
      let ch c = Buffer.add_char buf c in
      let st s = Buffer.add_string buf s in
      for i = 0 to len - 1 do
        match name.[i] with
        | '|' -> st "_pipe_"
        | '<' -> st "_lt_"
        | '>' -> st "_gt_"
        | '@' -> st "_at_"
        | '^' -> st "_exp_"
        | '&' -> st "_amp_"
        | '+' -> st "_plus_"
        | '-' -> st "_minus_"
        | '*' -> st "_star_"
        | '/' -> st "_slash_"
        | '$' -> st "_dollar_"
        | '%' -> st "_percent_"
        | '=' -> st "_equal_"
        | ':' -> st "_column_"
        | '~' -> st "_tilde_"
        | '!' -> st "_bang_"
        | '?' -> st "_questionmark_"
        | c -> ch c
      done;
      Buffer.contents buf

    (** Return the complete link target for the given value. *)
    let naming_complete_value_target v = naming_complete_target naming_mark_value v.val_name

    (** Return the complete filename for the code of the given value. *)
    let naming_file_code_value_complete_target v =
      let f = naming_code_prefix^naming_mark_value^(naming_subst_infix_symbols v.val_name)^".html" in
      f

    (** Return the link target for the given attribute. *)
    let naming_attribute_target a = naming_target naming_mark_attribute (Odoc_name.simple a.att_value.val_name)

    (** Return the complete link target for the given attribute. *)
    let naming_complete_attribute_target a = naming_complete_target naming_mark_attribute a.att_value.val_name

    (** Return the complete filename for the code of the given attribute. *)
    let naming_file_code_attribute_complete_target a =
      let f = naming_code_prefix^naming_mark_attribute^a.att_value.val_name^".html" in
      f

    (** Return the link target for the given method. *)
    let naming_method_target m = naming_target naming_mark_method (Odoc_name.simple m.met_value.val_name)

    (** Return the complete link target for the given method. *)
    let naming_complete_method_target m = naming_complete_target naming_mark_method m.met_value.val_name

    (** Return the complete filename for the code of the given method. *)
    let naming_file_code_method_complete_target m =
      let f = naming_code_prefix^naming_mark_method^m.met_value.val_name^".html" in
      f

    (** Return the link target for the given label section. *)
    let naming_label_target l = naming_target "" l

    (** Return the complete link target for the given section label. *)
    let naming_complete_label_target l = naming_complete_target "" l

    (** Return the complete filename for the code of the type of the
       given module or module type name. *)
    let naming_file_type_module_complete_target name =
      let f = naming_type_prefix^name^".html" in
      f

    (** Return the complete filename for the code of the
       given module name. *)
    let naming_file_code_module_complete_target name =
      let f = naming_code_prefix^name^".html" in
      f

    (** Return the complete filename for the code of the type of the
       given class or class type name. *)
    let naming_file_type_class_complete_target name =
      let f = naming_type_prefix^name^".html" in
      f

let new_buf () = Buffer.create 1024
let bp = Printf.bprintf
let bs = Buffer.add_string


    (** Escape the strings which would clash with html syntax, and
       make some replacements (double newlines replaced by <br>). *)
    let escape s = Odoc_llamahtml.escape_base s

    let keep_alpha_num s =
      let len = String.length s in
      let buf = Buffer.create len in
      for i = 0 to len - 1 do
        match s.[i] with
          'a'..'z' | 'A'..'Z' | '0'..'9' -> Buffer.add_char buf s.[i]
        | _ -> ()
      done;
      Buffer.contents buf

    (** Return a label created from the first sentence of a text. *)
    let label_of_text t=
      let t2 = Odoc_misc.first_sentence_of_text t in
      let s = Odoc_misc.string_of_text t2 in
      let s2 = keep_alpha_num s in
      s2

    (** Create a label for the associated title.
       Return the label specified by the user or a label created
       from the title level and the first sentence of the title. *)
    let create_title_label (n,label_opt,t) =
      match label_opt with
        Some s -> s
      | None -> Printf.sprintf "%d_%s" n (label_of_text t)

    let list_attributes = ref ([] : Odoc_value.t_attribute list)
    let list_methods = ref ([] : Odoc_value.t_method list)
    let list_values = ref ([] : Odoc_value.t_value list)
    let list_exceptions = ref ([] : Odoc_exception.t_exception list)
    let list_types = ref ([] : Odoc_type.t_type list)
    let list_modules = ref ([] : Odoc_module.t_module list)
    let list_module_types = ref ([] : Odoc_module.t_module_type list)


    let index_prefix =
      if !Odoc_args.out_file = Odoc_messages.default_out_file then
        "index"
      else
        Filename.basename !Odoc_args.out_file

    (** The main file. *)
    let index =
      let p = index_prefix in
      Printf.sprintf "%s.html" p

    (** The file for the index of values. *)
    let index_values = Printf.sprintf "%s_values.html" index_prefix
    (** The file for the index of types. *)
    let index_types = Printf.sprintf "%s_types.html" index_prefix
    (** The file for the index of exceptions. *)
    let index_exceptions = Printf.sprintf "%s_exceptions.html" index_prefix
    (** The file for the index of attributes. *)
    let index_attributes = Printf.sprintf "%s_attributes.html" index_prefix
    (** The file for the index of methods. *)
    let index_methods = Printf.sprintf "%s_methods.html" index_prefix
    (** The file for the index of classes. *)
    let index_classes = Printf.sprintf "%s_classes.html" index_prefix
    (** The file for the index of class types. *)
    let index_class_types = Printf.sprintf "%s_class_types.html" index_prefix
    (** The file for the index of modules. *)
    let index_modules = Printf.sprintf "%s_modules.html" index_prefix
    (** The file for the index of module types. *)
    let index_module_types = Printf.sprintf "%s_module_types.html" index_prefix


    let html_of_code b with_pre code = (* ?(with_pre=true) *)
      Odoc_llamahtml.html_of_code b with_pre code

      let index_if_not_empty b l url m =
        match l with
          [] -> ()
        | _ -> bp b "<a href=\"%s\">%s</a><br>\n" url m

    (** Print the html code corresponding to the [text] parameter. *)
    let rec html_of_text b t =
      List.iter (html_of_text_element b) t

    (** Print the html code for the [text_element] in parameter. *)
    and html_of_text_element b te =
      print_DEBUG "text::html_of_text_element";
      match te with
      | Odoc_types.Raw s -> html_of_Raw b s
      | Odoc_types.Code s -> html_of_Code b s
      | Odoc_types.CodePre s -> html_of_CodePre b s
      | Odoc_types.Verbatim s -> html_of_Verbatim b s
      | Odoc_types.Bold t -> html_of_Bold b t
      | Odoc_types.Italic t -> html_of_Italic b t
      | Odoc_types.Emphasize t -> html_of_Emphasize b t
      | Odoc_types.Center t -> html_of_Center b t
      | Odoc_types.Left t -> html_of_Left b t
      | Odoc_types.Right t -> html_of_Right b t
      | Odoc_types.List tl -> html_of_List b tl
      | Odoc_types.Enum tl -> html_of_Enum b tl
      | Odoc_types.Newline -> html_of_Newline b
      | Odoc_types.Block t -> html_of_Block b t
      | Odoc_types.Title (n, l_opt, t) -> html_of_Title b n l_opt t
      | Odoc_types.Latex s -> html_of_Latex b s
      | Odoc_types.Link (s, t) -> html_of_Link b s t
      | Odoc_types.Ref (name, ref_opt, text_opt) ->
          html_of_Ref b name ref_opt text_opt
      | Odoc_types.Superscript t -> html_of_Superscript b t
      | Odoc_types.Subscript t -> html_of_Subscript b t
      | Odoc_types.Module_list l -> html_of_Module_list b l
      | Odoc_types.Index_list -> html_of_Index_list b
      | Odoc_types.Custom (s,t) -> html_of_custom_text b s t
      | Odoc_types.Target (target, code) -> html_of_Target b target code

    and html_of_custom_text b s t = ()

    and html_of_Target b target code =
      if String.lowercase target = "html" then bs b code else ()

    and html_of_Raw b s = bs b (escape s)

    and html_of_Code b s =
      if !Odoc_args.colorize_code then
        html_of_code b false s
      else
        (
         bs b "<code class=\"";
         bs b Odoc_llamahtml.code_class ;
         bs b "\">";
         bs b (escape s);
         bs b "</code>"
        )

    and html_of_CodePre =
        let remove_useless_newlines s =
          let len = String.length s in
          let rec iter_first n =
            if n >= len then
              None
            else
              match s.[n] with
              | '\n' -> iter_first (n+1)
              | _ -> Some n
          in
          match iter_first 0 with
            None -> ""
          | Some first ->
              let rec iter_last n =
                if n <= first then
                  None
                else
                  match s.[n] with
                    '\t'  -> iter_last (n-1)
                  | _ -> Some n
              in
              match iter_last (len-1) with
                None -> String.sub s first 1
              | Some last -> String.sub s first ((last-first)+1)
        in
        fun b s ->
      if !Odoc_args.colorize_code then
        (
         bs b "<pre></pre>";
         html_of_code b true (remove_useless_newlines s);
         bs b "<pre></pre>"
        )
      else
        (
         bs b "<pre><code class=\"";
         bs b Odoc_llamahtml.code_class;
         bs b "\">" ;
         bs b (escape (remove_useless_newlines s));
         bs b "</code></pre>"
        )

    and html_of_Verbatim b s =
      bs b "<pre>";
      bs b (escape s);
      bs b "</pre>"

    and html_of_Bold b t =
      bs b "<b>";
      html_of_text b t;
      bs b "</b>"

    and html_of_Italic b t =
      bs b "<i>" ;
      html_of_text b t;
      bs b "</i>"

    and html_of_Emphasize b t =
      bs b "<em>" ;
      html_of_text b t ;
      bs b "</em>"

    and html_of_Center b t =
      bs b "<center>";
      html_of_text b t;
      bs b "</center>"

    and html_of_Left b t =
      bs b "<div align=left>";
      html_of_text b t;
      bs b "</div>"

    and html_of_Right b t =
      bs b "<div align=right>";
      html_of_text b t;
      bs b "</div>"

    and html_of_List b tl =
      bs b "<ul>\n";
      List.iter
        (fun t -> bs b "<li>"; html_of_text b t; bs b "</li>\n")
        tl;
      bs b "</ul>\n"

    and html_of_Enum b tl =
      bs b "<OL>\n";
      List.iter
        (fun t -> bs b "<li>"; html_of_text b t; bs b"</li>\n")
        tl;
      bs b "</OL>\n"

    and html_of_Newline b = bs b "\n<p>\n"

    and html_of_Block b t =
      bs b "<blockquote>\n";
      html_of_text b t;
      bs b "</blockquote>\n"

    and html_of_Title b n label_opt t =
      let label1 = create_title_label (n, label_opt, t) in
      bp b "<span id=\"%s\">" (naming_label_target label1);
      let (tag_o, tag_c) =
        if n > 6 then
          (Printf.sprintf "div class=\"h%d\"" n, "div")
        else
          let t = Printf.sprintf "h%d" n in (t, t)
      in
      bs b "<";
      bs b tag_o;
      bs b ">";
      html_of_text b t;
      bs b "</";
      bs b tag_c;
      bs b ">";
      bs b "</span>"

    and html_of_Latex b _ = ()
      (* don't care about LaTeX stuff in HTML. *)

    and html_of_Link b s t =
      bs b "<a href=\"";
      bs b s ;
      bs b "\">";
      html_of_text b t;
      bs b "</a>"

    and html_of_Ref b name ref_opt text_opt =
      match ref_opt with
        None ->
          let text =
            match text_opt with
              None -> [Odoc_types.Code name]
            | Some t -> t
          in
          html_of_text b text
      | Some kind ->
          let h name = Odoc_types.Code (Odoc_info.use_hidden_modules name) in
          let (target, text) =
            match kind with
              Odoc_types.RK_module
            | Odoc_types.RK_module_type
            | Odoc_types.RK_value -> (naming_complete_target naming_mark_value name, h name)
            | Odoc_types.RK_type -> (naming_complete_target naming_mark_type name, h name)
            | Odoc_types.RK_exception -> (naming_complete_target naming_mark_exception name, h name)
            | Odoc_types.RK_attribute -> (naming_complete_target naming_mark_attribute name, h name)
            | Odoc_types.RK_method -> (naming_complete_target naming_mark_method name, h name)
            | Odoc_types.RK_section t -> (naming_complete_label_target name,
                                         Odoc_types.Italic [Odoc_types.Raw (Odoc_misc.string_of_text t)])
          in
          let text =
            match text_opt with
              None -> [text]
            | Some text -> text
          in
          bs b ("<a href=\""^target^"\">");
          html_of_text b text;
          bs b "</a>"

    and html_of_Superscript b t =
      bs b "<sup class=\"superscript\">";
      html_of_text b t;
      bs b "</sup>"

    and html_of_Subscript b t =
      bs b "<sub class=\"subscript\">";
      html_of_text b t;
      bs b "</sub>"

    and html_of_Module_list b l =
      bs b "<br>\n<table class=\"indextable\">\n";
      List.iter
        (fun name ->
          bs b "<tr><td>";
          (
           try
             let m =
               List.find (fun m -> m.m_name = name) !list_modules
             in
             let (html, _) = naming_html_files m.m_name in
             bp b "<a href=\"%s\">%s</a></td>" html m.m_name;
             bs b "<td>";
             html_of_info_first_sentence b m.m_info;
           with
             Not_found ->
               Odoc_messages.pwarning (Odoc_messages.cross_module_not_found name);
               bp b "%s</td><td>" name
          );
          bs b "</td></tr>\n"
        )
        l;
      bs b "</table>\n"

    and html_of_Index_list b =
      index_if_not_empty b !list_types index_types Odoc_messages.index_of_types;
      index_if_not_empty b !list_exceptions index_exceptions Odoc_messages.index_of_exceptions;
      index_if_not_empty b !list_values index_values Odoc_messages.index_of_values;
      index_if_not_empty b !list_attributes index_attributes Odoc_messages.index_of_attributes;
      index_if_not_empty b !list_methods index_methods Odoc_messages.index_of_methods;
      index_if_not_empty b !list_modules index_modules Odoc_messages.index_of_modules;
      index_if_not_empty b !list_module_types index_module_types Odoc_messages.index_of_module_types
    (** Print html code for the first sentence of a description.
       The titles and lists in this first sentence has been removed.*)
    and html_of_info_first_sentence b info_opt =
      match info_opt with
        None -> ()
      | Some info ->
          let dep = info.Odoc_types.i_deprecated <> None in
          bs b "<div class=\"info\">\n";
          if dep then bs b "<font color=\"#CCCCCC\">";
          (
           match info.Odoc_types.i_desc with
             None -> ()
           | Some d when d = [Odoc_types.Raw ""] -> ()
           | Some d ->
               html_of_text b
                 (Odoc_misc.text_no_title_no_list
                    (Odoc_misc.first_sentence_of_text d));
               bs b "\n"
          );
          if dep then bs b "</font>";
          bs b "</div>\n"

    (** The list of pairs [(tag, f)] where [f] is a function taking
       the [text] associated to [tag] and returning html code.
       Add a pair here to handle a tag.*)
    let tag_functions = ref ([] : (string * (Odoc_info.text -> string)) list)

    (** Print html for an author list. *)
    let html_of_author_list b l =
      match l with
        [] -> ()
      | _ ->
          bp b "<b>%s:</b> %s<br>\n"
            Odoc_messages.authors
            (String.concat ", " l)

    (** Print html code for the given optional version information.*)
    let html_of_version_opt b v_opt =
      match v_opt with
        None -> ()
      | Some v ->
           bp b "<b>%s:</b> %s<br>\n" Odoc_messages.version v

    (** Print html code for the given optional since information.*)
    let html_of_since_opt b s_opt =
      match s_opt with
        None -> ()
      | Some s ->
          bp b "<b>%s</b> %s<br>\n" Odoc_messages.since s

    (** Print html code for the given "before" information.*)
    let html_of_before b l =
      let f (v, text) =
        bp b "<b>%s %s </b> " Odoc_messages.before v;
        html_of_text b text;
        bs b "<br>\n"
      in
      List.iter f l

    (** Print html code for the given list of raised exceptions.*)
    let html_of_raised_exceptions b l =
      match l with
        [] -> ()
      | (s, t) :: [] ->
          bp b "<b>%s</b> <code>%s</code> "
            Odoc_messages.raises
            s;
          html_of_text b t;
          bs b "<br>\n"
      | _ ->
          bp b "<b>%s</b><ul>" Odoc_messages.raises;
          List.iter
            (fun (ex, desc) ->
              bp b "<li><code>%s</code> " ex ;
              html_of_text b desc;
              bs b "</li>\n"
            )
            l;
          bs b "</ul>\n"

    (** Print html code for the given "see also" reference. *)
    let html_of_see b (see_ref, t)  =
      let t_ref =
        match see_ref with
          Odoc_types.See_url s -> [ Odoc_types.Link (s, t) ]
        | Odoc_types.See_file s -> (Odoc_types.Code s) :: (Odoc_types.Raw " ") :: t
        | Odoc_types.See_doc s -> (Odoc_types.Italic [Odoc_types.Raw s]) :: (Odoc_types.Raw " ") :: t
      in
      html_of_text b t_ref

    (** Print html code for the given list of "see also" references.*)
    let html_of_sees b l =
      match l with
        [] -> ()
      | see :: [] ->
          bp b "<b>%s</b> " Odoc_messages.see_also;
          html_of_see b see;
          bs b "<br>\n"
      | _ ->
          bp b "<b>%s</b><ul>" Odoc_messages.see_also;
          List.iter
            (fun see ->
              bs b "<li>" ;
              html_of_see b see;
              bs b "</li>\n"
            )
            l;
          bs b "</ul>\n"

    (** Print html code for the given optional return information.*)
    let html_of_return_opt b return_opt =
      match return_opt with
        None -> ()
      | Some s ->
          bp b "<b>%s</b> " Odoc_messages.returns;
          html_of_text b s;
          bs b "<br>\n"

    (** Print html code for the given list of custom tagged texts. *)
    let html_of_custom b l =
      List.iter
        (fun (tag, text) ->
          try
            let f = List.assoc tag !tag_functions in
            Buffer.add_string b (f text)
          with
            Not_found ->
              Odoc_info.warning (Odoc_messages.tag_not_handled tag)
        )
        l

    (** Print html code for a description, except for the [i_params] field.
       @param indent can be specified not to use the style of info comments;
       default is [true].
    *)
    let html_of_info indent b info_opt = (* ?(indent=true) *)
      match info_opt with
        None ->
          ()
      | Some info ->
          if indent then bs b "<div class=\"info\">\n";
          (
           match info.Odoc_types.i_deprecated with
            None -> ()
           | Some d ->
               bs b "<span class=\"warning\">";
               bs b Odoc_messages.deprecated ;
               bs b "</span>" ;
               html_of_text b d;
               bs b "<br>\n"
          );
          (
           match info.Odoc_types.i_desc with
             None -> ()
           | Some d when d = [Odoc_types.Raw ""] -> ()
           | Some d -> html_of_text b d; bs b "<br>\n"
          );
          html_of_author_list b info.Odoc_types.i_authors;
          html_of_version_opt b info.Odoc_types.i_version;
          html_of_before b info.Odoc_types.i_before;
          html_of_since_opt b info.Odoc_types.i_since;
          html_of_raised_exceptions b info.Odoc_types.i_raised_exceptions;
          html_of_return_opt b info.Odoc_types.i_return_value;
          html_of_sees b info.Odoc_types.i_sees;
          html_of_custom b info.Odoc_types.i_custom;
          if indent then bs b "</div>\n"



let opt = Odoc_misc.apply_opt

let print_concat b sep f =
  let rec iter = function
      [] -> ()
    | [c] -> f c
    | c :: q ->
        f c;
        bs b sep;
        iter q
  in
  iter

let newline_to_indented_br s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
      '\n' -> Buffer.add_string b "<br>     "
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b

(** This class is used to create objects which can generate a simple html documentation. *)
    let doctype =
      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n"
    let character_encoding =
      "<meta content=\"text/html; charset=iso-8859-1\" http-equiv=\"Content-Type\">\n"

    (** The default style options. *)
    let default_style_options =
      ["a:visited {color : #416DFF; text-decoration : none; }" ;
        "a:link {color : #416DFF; text-decoration : none;}" ;
        "a:hover {color : Red; text-decoration : none; background-color: #5FFF88}" ;
        "a:active {color : Red; text-decoration : underline; }" ;
        ".keyword { font-weight : bold ; color : Red }" ;
        ".keywordsign { color : #C04600 }" ;
        ".superscript { font-size : 4 }" ;
        ".subscript { font-size : 4 }" ;
        ".comment { color : Green }" ;
        ".constructor { color : Blue }" ;
        ".type { color : #5C6585 }" ;
        ".string { color : Maroon }" ;
        ".warning { color : Red ; font-weight : bold }" ;
        ".info { margin-left : 3em; margin-right : 3em }" ;
        ".param_info { margin-top: 4px; margin-left : 3em; margin-right : 3em }" ;
        ".code { color : #465F91 ; }" ;
        "h1 { font-size : 20pt ; text-align: center; }" ;

        "h2 { font-size : 20pt ; border: 1px solid #000000; "^
        "margin-top: 5px; margin-bottom: 2px;"^
        "text-align: center; background-color: #90BDFF ;"^
        "padding: 2px; }" ;

        "h3 { font-size : 20pt ; border: 1px solid #000000; "^
        "margin-top: 5px; margin-bottom: 2px;"^
        "text-align: center; background-color: #90DDFF ;"^
        "padding: 2px; }" ;

        "h4 { font-size : 20pt ; border: 1px solid #000000; "^
        "margin-top: 5px; margin-bottom: 2px;"^
        "text-align: center; background-color: #90EDFF ;"^
        "padding: 2px; }" ;

        "h5 { font-size : 20pt ; border: 1px solid #000000; "^
        "margin-top: 5px; margin-bottom: 2px;"^
        "text-align: center; background-color: #90FDFF ;"^
        "padding: 2px; }" ;

        "h6 { font-size : 20pt ; border: 1px solid #000000; "^
        "margin-top: 5px; margin-bottom: 2px;"^
        "text-align: center; background-color: #C0FFFF ; "^
        "padding: 2px; }" ;

        "div.h7 { font-size : 20pt ; border: 1px solid #000000; "^
        "margin-top: 5px; margin-bottom: 2px;"^
        "text-align: center; background-color: #E0FFFF ; "^
        "padding: 2px; }" ;

        "div.h8 { font-size : 20pt ; border: 1px solid #000000; "^
        "margin-top: 5px; margin-bottom: 2px;"^
        "text-align: center; background-color: #F0FFFF ; "^
        "padding: 2px; }" ;

        "div.h9 { font-size : 20pt ; border: 1px solid #000000; "^
        "margin-top: 5px; margin-bottom: 2px;"^
        "text-align: center; background-color: #FFFFFF ; "^
        "padding: 2px; }" ;

        ".typetable { border-style : hidden }" ;
        ".indextable { border-style : hidden }" ;
        ".paramstable { border-style : hidden ; padding: 5pt 5pt}" ;
        "body { background-color : White }" ;
        "tr { background-color : White }" ;
        "td.typefieldcomment { background-color : #FFFFFF ; font-size: smaller ;}" ;
        "pre { margin-bottom: 4px }" ;

        "div.sig_block {margin-left: 2em}" ;

        "*:target { background: yellow; } " ;
      ]

    (** The style file for all pages. *)
    let style_file = ref "style.css"

    (** The code to import the style. Initialized in [init_style]. *)
    let style = ref ""

    (** The known types names.
       Used to know if we must create a link to a type
       when printing a type. *)
    let known_types_names = ref (Set.empty : string Set.t)

    (** The known class and class type names.
       Used to know if we must create a link to a class
       or class type or not when printing a type. *)
    let known_classes_names = ref (Set.empty : string Set.t)

    (** The known modules and module types names.
       Used to know if we must create a link to a type or not
       when printing a module type. *)
    let known_modules_names = ref (Set.empty : string Set.t)



    (** The header of pages. Must be prepared by the [prepare_header] let.*)
(* ?(nav=None) ?(comments=[]) *)
    let header = ref (fun (b:Buffer.t) (nav:(string option * string option * Odoc_name.t) option)
                        (comments:Odoc_types.text list) (_:string) -> ())

    (** Init the style. *)
    let init_style =
      (match !Odoc_args.css_style with
        None ->
          let default_style = String.concat "\n" default_style_options in
          (
           try
             let file = Filename.concat !Odoc_args.target_dir !style_file in
             if Sys.file_exists file then
               Odoc_info.verbose (Odoc_messages.file_exists_dont_generate file)
             else
               (
                let chanout = open_out file in
                output_string chanout default_style ;
                flush chanout ;
                close_out chanout;
                Odoc_info.verbose (Odoc_messages.file_generated file)
               )
           with
             Sys_error s ->
               prerr_endline s ;
               incr Odoc_global.errors ;
          )
      | Some f ->
          style_file := f
      );
      style := "<link rel=\"stylesheet\" href=\""^ !style_file^"\" type=\"text/css\">\n"

    (** Get the title given by the user *)
    let title = match !Odoc_args.title with None -> "" | Some t -> escape t

    (** Get the title given by the user completed with the given subtitle. *)
    let inner_title s =
      (match title with "" -> "" | t -> t^" : ")^
      (escape s)

    (** Get the page header. *)
    let print_header b nav comments title = !header b nav comments title

    (** Build the html code for the link tags in the header, defining section and
       subsections for the titles found in the given comments.*)
    let html_sections_links b comments =
      let titles = List.flatten (List.map Odoc_misc.get_titles_in_text comments) in
      let levels =
        let rec iter acc l =
          match l with
            [] -> acc
          | (n,_,_) :: q ->
              if List.mem n acc
              then iter acc q
              else iter (n::acc) q
        in
        iter [] titles
      in
      let sorted_levels = List.sort compare levels in
      let (section_level, subsection_level) =
        match sorted_levels with
          [] -> (None, None)
        | [n] -> (Some n, None)
        | n :: m :: _ -> (Some n, Some m)
      in
      let titles_per_level level_opt =
        match level_opt with
          None -> []
        | Some n -> List.filter (fun (m,_,_) -> m = n) titles
      in
      let section_titles = titles_per_level section_level in
      let subsection_titles = titles_per_level subsection_level in
      let print_lines s_rel titles =
        List.iter
          (fun (n,lopt,t) ->
            let s = Odoc_misc.string_of_text t in
            let label = create_title_label (n,lopt,t) in
            bp b "<link title=\"%s\" rel=\"%s\" href=\"#%s\">\n" s s_rel label
          )
          titles
      in
      print_lines "Section" section_titles ;
      print_lines "Subsection" subsection_titles

        let link_if_not_empty b l m url =
          match l with
            [] -> ()
          | _ ->
              bp b "<link title=\"%s\" rel=Appendix href=\"%s\">\n" m url

    (** A function to build the header of pages. *)
    let prepare_header module_list =
      let f b nav comments t = (* ?(nav=None) ?(comments=[]) *)
        bs b "<head>\n";
        bs b !style;
        bs b character_encoding ;
        bs b "<link rel=\"Start\" href=\"";
        bs b index;
        bs b "\">\n" ;
        (
         match nav with
           None -> ()
         | Some (pre_opt, post_opt, name) ->
             (match pre_opt with
               None -> ()
             | Some name ->
                 bp b "<link rel=\"previous\" href=\"%s\">\n"
                   (fst (naming_html_files name));
             );
             (match post_opt with
               None -> ()
             | Some name ->
                 bp b "<link rel=\"next\" href=\"%s\">\n"
                   (fst (naming_html_files name));
             );
             (
              let father = Odoc_name.father name in
              let href = if father = "" then index else fst (naming_html_files father) in
              bp b "<link rel=\"Up\" href=\"%s\">\n" href
             )
        );
        link_if_not_empty b !list_types Odoc_messages.index_of_types index_types;
        link_if_not_empty b !list_exceptions Odoc_messages.index_of_exceptions index_exceptions;
        link_if_not_empty b !list_values Odoc_messages.index_of_values index_values;
        link_if_not_empty b !list_attributes Odoc_messages.index_of_attributes index_attributes;
        link_if_not_empty b !list_methods Odoc_messages.index_of_methods index_methods;
        link_if_not_empty b !list_modules Odoc_messages.index_of_modules index_modules;
        link_if_not_empty b !list_module_types Odoc_messages.index_of_module_types index_module_types;
        let print_one m =
          let html_file = fst (naming_html_files m.m_name) in
          bp b "<link title=\"%s\" rel=\"Chapter\" href=\"%s\">"
            m.m_name html_file
        in
        print_concat b "\n" print_one module_list;
        html_sections_links b comments;
        bs b "<title>";
        bs b t ;
        bs b "</title>\n</head>\n"
      in
      header := f

    (** Html code for navigation bar.
       @param pre optional name for optional previous module/class
       @param post optional name for optional next module/class
       @param name name of current module/class *)
    let print_navbar b pre post name =
      bs b "<div class=\"navbar\">";
      (
       match pre with
         None -> ()
       | Some name ->
           bp b "<a href=\"%s\">%s</a>\n"
             (fst (naming_html_files name))
             Odoc_messages.previous
      );
      bs b "&nbsp;";
      let father = Odoc_name.father name in
      let href = if father = "" then index else fst (naming_html_files father) in
      bp b "<a href=\"%s\">%s</a>\n" href Odoc_messages.up;
      bs b "&nbsp;";
      (
       match post with
         None -> ()
       | Some name ->
           bp b "<a href=\"%s\">%s</a>\n"
             (fst (naming_html_files name))
             Odoc_messages.next
      );
      bs b "</div>\n"

    (** Return html code with the given string in the keyword style.*)
    let keyword s =
      "<span class=\"keyword\">"^s^"</span>"

    (** Return html code with the given string in the constructor style. *)
    let constructor s = "<span class=\"constructor\">"^s^"</span>"

    (** Output the given ocaml code to the given file name. *)
    let output_code in_title file code =
      try
        let chanout = open_out file in
        let b = new_buf () in
        bs b "<html>";
        print_header b None [] (inner_title in_title);
        bs b"<body>\n";
        html_of_code b true code;
        bs b "</body></html>";
        Buffer.output_buffer chanout b;
        close_out chanout
      with
        Sys_error s ->
          incr Odoc_global.errors ;
          prerr_endline s

    (** Take a string and return the string where fully qualified
       type (or class or class type) idents
       have been replaced by links to the type referenced by the ident.*)
    let create_fully_qualified_idents_links m_name s =
      let f str_t =
        let match_s = Str.matched_string str_t in
        let rel = Odoc_name.get_relative m_name match_s in
        let s_final = Odoc_info.apply_if_equal
            Odoc_info.use_hidden_modules
            match_s
            rel
        in
        if Set.mem match_s !known_types_names then
           "<a href=\""^(naming_complete_target naming_mark_type match_s)^"\">"^
           s_final^
           "</a>"
        else
          if Set.mem match_s !known_classes_names then
            let (html_file, _) = naming_html_files match_s in
            "<a href=\""^html_file^"\">"^s_final^"</a>"
          else
            s_final
      in
      let s2 = Str.global_substitute
          (Str.regexp "\\([A-Z]\\([a-zA-Z_'0-9]\\)*\\.\\)+\\([a-z][a-zA-Z_'0-9]*\\)")
          f
          s
      in
      s2

    (** Take a string and return the string where fully qualified module idents
       have been replaced by links to the module referenced by the ident.*)
    let create_fully_qualified_module_idents_links m_name s =
      let f str_t =
        let match_s = Str.matched_string str_t in
        let rel = Odoc_name.get_relative m_name match_s in
        let s_final = Odoc_info.apply_if_equal
            Odoc_info.use_hidden_modules
            match_s
            rel
        in
        if Set.mem match_s !known_modules_names then
          let (html_file, _) = naming_html_files match_s in
          "<a href=\""^html_file^"\">"^s_final^"</a>"
        else
          s_final
      in
      let s2 = Str.global_substitute
          (Str.regexp "\\([A-Z]\\([a-zA-Z_'0-9]\\)*\\.\\)+\\([A-Z][a-zA-Z_'0-9]*\\)")
          f
          s
      in
      s2

    (** Print html code to display a [Types.type_expr]. *)
    let html_of_type_expr b m_name t =
      let s = Odoc_misc.remove_ending_newline (Odoc_print.string_of_type_expr t) in
      let s2 = newline_to_indented_br s in
      bs b "<code class=\"type\">";
      bs b (create_fully_qualified_idents_links m_name s2);
      bs b "</code>"

    (** Print html code to display a [Types.type_expr list]. *)
    let html_of_type_expr_list par b m_name sep l = (* ?par *)
      print_DEBUG "html#html_of_type_expr_list";
      let s = Odoc_str.string_of_type_list par sep l in
      print_DEBUG "html#html_of_type_expr_list: 1";
      let s2 = newline_to_indented_br s in
      print_DEBUG "html#html_of_type_expr_list: 2";
      bs b "<code class=\"type\">";
      bs b (create_fully_qualified_idents_links m_name s2);
      bs b "</code>"

    (** Print html code to display a list of type parameters for the given type.*)
    let html_of_type_expr_param_list b m_name t =
      let s = Odoc_str.string_of_type_param_list t in
      let s2 = newline_to_indented_br s in
      bs b "<code class=\"type\">";
      bs b (create_fully_qualified_idents_links m_name s2);
      bs b "</code>"

    (** Print html code to display a [Types.module_type]. *)
    let html_of_module_type b code m_name t = (* ?code *)
      assert false
(*
      let s = Odoc_misc.remove_ending_newline (Odoc_print.string_of_module_type code t) in
      bs b "<code class=\"type\">";
      bs b (create_fully_qualified_module_idents_links m_name s);
      bs b "</code>"
*)

    (** Print html code for the description of a function parameter. *)
    let html_of_parameter_description b p =
      match Odoc_parameter.names p with
        [] ->
          ()
      | name :: [] ->
          (
           (* Only one name, no need for label for the description. *)
           match Odoc_parameter.desc_by_name p name with
             None -> ()
           | Some t -> html_of_text b t
          )
      | l ->
          (*  A list of names, we display those with a description. *)
          let l2 = List.filter
              (fun n -> (Odoc_parameter.desc_by_name p n) <> None)
              l
          in
          let print_one n =
            match Odoc_parameter.desc_by_name p n with
              None -> ()
            | Some t ->
                bs b "<code>";
                bs b n;
                bs b "</code> : ";
                html_of_text b t
          in
          print_concat b "<br>\n" print_one l2

    (** Print html code for a list of parameters. *)
    let html_of_parameter_list b m_name l =
      match l with
        [] -> ()
      | _ ->
          bs b "<div class=\"param_info\">";
          bs b "<table border=\"0\" cellpadding=\"3\" width=\"100%\">\n";
          bs b "<tr>\n<td align=\"left\" valign=\"top\" width=\"1%\">";
          bs b "<b>";
          bs b Odoc_messages.parameters;
          bs b ": </b></td>\n" ;
          bs b "<td>\n<table class=\"paramstable\">\n";
          let print_one p =
            bs b "<tr>\n<td align=\"center\" valign=\"top\" width=\"15%\" class=\"code\">\n";
            bs b
              (
               match Odoc_parameter.complete_name p with
                 "" -> "?"
               | s -> s
              );
            bs b "</td>\n<td align=\"center\" valign=\"top\">:</td>\n";
            bs b "<td>";
            html_of_type_expr b m_name (Odoc_parameter.typ p);
            bs b "<br>\n";
            html_of_parameter_description b p;
            bs b "\n</tr>\n";
          in
          List.iter print_one l;
          bs b "</table>\n</td>\n</tr>\n</table></div>\n"

    (** Print html code for the parameters which have a name and description. *)
    let html_of_described_parameter_list b m_name l =
      (* get the params which have a name, and at least one name described. *)
      let l2 = List.filter
          (fun p ->
            List.exists
              (fun n -> (Odoc_parameter.desc_by_name p n) <> None)
              (Odoc_parameter.names p))
          l
      in
      let f p =
        bs b "<div class=\"param_info\"><code class=\"code\">";
        bs b (Odoc_parameter.complete_name p);
        bs b "</code> : " ;
        html_of_parameter_description b p;
        bs b "</div>\n"
      in
      List.iter f l2

    (** Print html code for a value. *)
    let html_of_value b v =
      Printtyp.reset_names ();
      bs b "<pre>" ;
      bp b "<span id=\"%s\">" (naming_value_target v);
      bs b (keyword "val");
      bs b " ";
      (
       match v.val_code with
         None -> bs b (escape (Odoc_name.simple v.val_name))
       | Some c ->
           let file = naming_file_code_value_complete_target v in
           output_code v.val_name (Filename.concat !Odoc_args.target_dir file) c;
           bp b "<a href=\"%s\">%s</a>" file (escape (Odoc_name.simple v.val_name))
      );
      bs b "</span>";
      bs b " : ";
      html_of_type_expr b (Odoc_name.father v.val_name) v.val_type;
      bs b "</pre>";
      html_of_info true b v.val_info;
      (
       if !Odoc_args.with_parameter_list then
         html_of_parameter_list b (Odoc_name.father v.val_name) v.val_parameters
       else
         html_of_described_parameter_list b (Odoc_name.father v.val_name) v.val_parameters
      )

    (** Print html code for an exception. *)
    let html_of_exception b e =
      Printtyp.reset_names ();
      bs b "<pre>";
      bp b "<span id=\"%s\">" (naming_exception_target e);
      bs b (keyword "exception");
      bs b " ";
      bs b (Odoc_name.simple e.ex_name);
      bs b "</span>";
      (
       match e.ex_args with
         [] -> ()
       | _ ->
           bs b (" "^(keyword "of")^" ");
           html_of_type_expr_list
             (Some false) b (Odoc_name.father e.ex_name) " * " e.ex_args
      );
      (
       match e.ex_alias with
         None -> ()
       | Some ea ->
           bs b " = ";
           (
            match ea.ea_ex with
              None -> bs b ea.ea_name
            | Some e ->
                bp b "<a href=\"%s\">%s</a>" (naming_complete_exception_target e) e.ex_name
           )
      );
      bs b "</pre>\n";
      html_of_info true b e.ex_info

    (** Print html code for a type. *)
    let html_of_type b t =
      Printtyp.reset_names ();
      let father = Odoc_name.father t.ty_name in
      bs b
        (match t.ty_manifest, t.ty_kind with
          None, Tcs_abstract -> "<pre>"
        | None, Tcs_sum _
        | None, Tcs_record _ -> "<br><code>"
        | Some _, Tcs_abstract -> "<pre>"
        | Some _, Tcs_sum _
        | Some _, Tcs_record _ -> "<pre>"
        );
      bp b "<span id=\"%s\">" (naming_type_target t);
      bs b ((keyword "type")^" ");
      html_of_type_expr_param_list b father t;
      (match t.ty_parameters with [] -> () | _ -> bs b " ");
      bs b (Odoc_name.simple t.ty_name);
      bs b "</span> ";
      (
       match t.ty_manifest with
         None -> ()
       | Some typ ->
           bs b "= ";
           html_of_type_expr b father typ;
           bs b " "
      );
      (match t.ty_kind with
        Tcs_abstract -> bs b "</pre>"
      | Tcs_sum l ->
          bs b "= ";
          bs b
            (
             match t.ty_manifest with
               None -> "</code>"
             | Some _ -> "</pre>"
            );
          bs b "<table class=\"typetable\">\n";
          let print_one constr =
            bs b "<tr>\n<td align=\"left\" valign=\"top\" >\n";
            bs b "<code>";
            bs b (keyword "|");
            bs b "</code></td>\n<td align=\"left\" valign=\"top\" >\n";
            bs b "<code>";
            bs b (constructor constr.vc_name);
            (
             match constr.vc_args with
               [] -> ()
             | l ->
                 bs b (" " ^ (keyword "of") ^ " ");
                 html_of_type_expr_list (Some false) b father " * " l;
            );
            bs b "</code></td>\n";
            (
             match constr.vc_text with
               None -> ()
             | Some t ->
                 bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >";
                 bs b "<code>";
                 bs b "(*";
                 bs b "</code></td>";
                 bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >";
                 html_of_text b t;
                 bs b "</td>";
                 bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"bottom\" >";
                 bs b "<code>";
                 bs b "*)";
                 bs b "</code></td>";
            );
            bs b "\n</tr>"
          in
          print_concat b "\n" print_one l;
          bs b "</table>\n"

      | Tcs_record l ->
          bs b "= ";
          bs b "{";
          bs b
            (
             match t.ty_manifest with
               None -> "</code>"
             | Some _ -> "</pre>"
            );
          bs b "<table class=\"typetable\">\n" ;
          let print_one r =
            bs b "<tr>\n<td align=\"left\" valign=\"top\" >\n";
            bs b "<code>&nbsp;&nbsp;</code>";
            bs b "</td>\n<td align=\"left\" valign=\"top\" >\n";
            bs b "<code>";
            if r.rf_mutable then bs b (keyword "mutable&nbsp;") ;
            bs b (r.rf_name ^ "&nbsp;: ") ;
            html_of_type_expr b father r.rf_type;
            bs b ";</code></td>\n";
            (
             match r.rf_text with
               None -> ()
             | Some t ->
                 bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >";
                 bs b "<code>";
                 bs b "(*";
                 bs b "</code></td>";
                 bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >";
                 html_of_text b t;
                 bs b "</td><td class=\"typefieldcomment\" align=\"left\" valign=\"bottom\" >";
                 bs b "<code>*)</code></td>";
            );
            bs b "\n</tr>"
          in
          print_concat b "\n" print_one l;
          bs b "</table>\n}\n"
      );
      bs b "\n";
      html_of_info true b t.ty_info;
      bs b "\n"

    (** Print html code for a module comment.*)
    let html_of_module_comment b text =
      bs b "<br>\n";
      html_of_text b text;
      bs b "<br>\n"

    let html_of_module_element b father ele =
      match ele with
      | Element_value v ->
          html_of_value b v
      | Element_exception e ->
          html_of_exception b e
      | Element_type t ->
          html_of_type b t
      | Element_module_comment text ->
          html_of_module_comment b text

    (** Print html code to display the given module kind. *)
    let html_of_module_kind b father modu kind = (* ?modu *)
      match kind with
        Module_struct eles ->
          html_of_text b [Odoc_types.Code "sig"];
          (
           match modu with
             None ->
               bs b "<div class=\"sig_block\">";
               List.iter (html_of_module_element b father) eles;
               bs b "</div>"
           | Some m ->
               let (html_file, _) = naming_html_files m.m_name in
               bp b " <a href=\"%s\">..</a> " html_file
          );
          html_of_text b [Odoc_types.Code "end"]
      | Module_alias a ->
          bs b "<code class=\"type\">";
          bs b (create_fully_qualified_module_idents_links father a.ma_name);
          bs b "</code>"

    (** Print html code to display the given module type kind. *)
    let html_of_module_type_kind b father modu mt kind = (* ?modu ?mt *)
      match kind with
        Module_type_struct eles ->
          html_of_text b [Odoc_types.Code "sig"];
          (
           match mt with
             None ->
               (
                match modu with
                  None ->
                    bs b "<div class=\"sig_block\">";
                    List.iter (html_of_module_element b father) eles;
                    bs b "</div>"
                | Some m ->
                    let (html_file, _) = naming_html_files m.m_name in
                    bp b " <a href=\"%s\">..</a> " html_file
               )
           | Some mt ->
               let (html_file, _) = naming_html_files mt.mt_name in
               bp b " <a href=\"%s\">..</a> " html_file
          );
          html_of_text b [Odoc_types.Code "end"]
      | Module_type_alias a ->
          bs b "<code class=\"type\">";
          bs b (create_fully_qualified_module_idents_links father a.mta_name);
          bs b "</code>"

    (** Generate a file containing the module type in the given file name. *)
    let output_module_type in_title file mtyp =
      assert false
(*
      let s = Odoc_misc.remove_ending_newline (Odoc_info.string_of_module_type true mtyp) in
      output_code in_title file s
*)

    (** Print html code for a module. *)
    let html_of_module b info complete with_link m = (* ?(info=true) ?(complete=true) ?(with_link=true) *)
      let (html_file, _) = naming_html_files m.m_name in
      let father = Odoc_name.father m.m_name in
      bs b "<pre>";
      bs b ((keyword "module")^" ");
      (
       if with_link then
         bp b "<a href=\"%s\">%s</a>" html_file (Odoc_name.simple m.m_name)
       else
         bs b (Odoc_name.simple m.m_name)
      );
      bs b ": ";
      html_of_module_kind b father (Some m) m.m_kind;
      bs b "</pre>";
      if info then
        (
         if complete then
           html_of_info false
         else
           html_of_info_first_sentence
        ) b m.m_info
      else
        ()

    (** Print html code for a module type. *)
    let html_of_modtype b info complete with_link mt = (* ?(info=true) ?(complete=true) ?(with_link=true) *)
      let (html_file, _) = naming_html_files mt.mt_name in
      let father = Odoc_name.father mt.mt_name in
      bs b "<pre>";
      bs b ((keyword "module type")^" ");
      (
       if with_link then
         bp b "<a href=\"%s\">%s</a>" html_file (Odoc_name.simple mt.mt_name)
         else
         bs b (Odoc_name.simple mt.mt_name)
      );
      (match mt.mt_kind with
        None -> ()
      | Some k ->
          bs b " = ";
          html_of_module_type_kind b father None (Some mt) k
      );
      bs b "</pre>";
      if info then
        (
         if complete then
           html_of_info false
         else
           html_of_info_first_sentence
        ) b mt.mt_info
      else
        ()

    (** Print html code for an included module. *)
    let html_of_included_module b im =
      bs b "<pre>";
      bs b ((keyword "include")^" ");
      (
       match im.im_module with
         None ->
           bs b im.im_name
       | Some mmt ->
           let (file, name) =
             match mmt with
               Mod m ->
                 let (html_file, _) = naming_html_files m.m_name in
                 (html_file, m.m_name)
             | Modtype mt ->
                 let (html_file, _) = naming_html_files mt.mt_name in
                 (html_file, mt.mt_name)
           in
           bp b "<a href=\"%s\">%s</a>" file name
      );
      bs b "</pre>\n";
      html_of_info true b im.im_info

    (** A method to create index files. *)
    let generate_elements_index :
        'a list ->
          ('a -> Odoc_name.t) ->
            ('a -> Odoc_types.info option) ->
              ('a -> string) -> string -> string -> unit =
    fun elements name info target title simple_file ->
      try
        let chanout = open_out (Filename.concat !Odoc_args.target_dir simple_file) in
        let b = new_buf () in
        bs b "<html>\n";
        print_header b None [] (inner_title title);
        bs b "<body>\n<center><h1>";
        bs b title;
        bs b "</h1></center>\n" ;

        let sorted_elements = List.sort
            (fun e1 e2 -> compare (Odoc_name.simple (name e1)) (Odoc_name.simple (name e2)))
            elements
        in
        let groups = Odoc_misc.create_index_lists sorted_elements (fun e -> Odoc_name.simple (name e)) in
        let f_ele e =
          let simple_name = Odoc_name.simple (name e) in
          let father_name = Odoc_name.father (name e) in
          bp b "<tr><td><a href=\"%s\">%s</a> " (target e) (escape simple_name);
          if simple_name <> father_name && father_name <> "" then
            bp b "[<a href=\"%s\">%s</a>]" (fst (naming_html_files father_name)) father_name;
          bs b "</td>\n<td>";
          html_of_info_first_sentence b (info e);
          bs b "</td></tr>\n";
        in
        let f_group l =
          match l with
            [] -> ()
          | e :: _ ->
              let s =
                match (Char.uppercase (Odoc_name.simple (name e)).[0]) with
                  'A'..'Z' as c -> String.make 1 c
                | _ -> ""
              in
              bs b "<tr><td align=\"left\"><br>";
              bs b s ;
              bs b "</td></tr>\n" ;
              List.iter f_ele l
        in
        bs b "<table>\n";
        List.iter f_group groups ;
        bs b "</table><br>\n" ;
        bs b "</body>\n</html>";
        Buffer.output_buffer chanout b;
        close_out chanout
      with
        Sys_error s ->
          raise (Failure s)

    (** A let to generate a list of module/class files. *)
    let generate_elements :
        ('a option -> 'a option -> 'a -> unit) -> 'a list -> unit =
      fun f_generate l ->
        let rec iter pre_opt = function
            [] -> ()
          | ele :: [] -> f_generate pre_opt None ele
          | ele1 :: ele2 :: q ->
              f_generate pre_opt (Some ele2) ele1 ;
              iter (Some ele1) (ele2 :: q)
        in
        iter None l

    (** Generate the html file for the given module type.
       @raise Failure if an error occurs.*)
    let generate_for_module_type pre post mt =
      try
        let (html_file, _) = naming_html_files mt.mt_name in
(*        let type_file = naming_file_type_module_complete_target mt.mt_name in*)
        let chanout = open_out (Filename.concat !Odoc_args.target_dir html_file) in
        let b = new_buf () in
        let pre_name = opt (fun mt -> mt.mt_name) pre in
        let post_name = opt (fun mt -> mt.mt_name) post in
        bs b doctype ;
        bs b "<html>\n";
        print_header b
          (Some (pre_name, post_name, mt.mt_name))
           (Odoc_module.module_type_comments true mt)
          (inner_title mt.mt_name);
        bs b "<body>\n";
        print_navbar b pre_name post_name mt.mt_name;
        bp b "<center><h1>";
        bs b (Odoc_messages.module_type^" ");
        bs b mt.mt_name;
        bs b "</h1></center>\n<br>\n" ;
        html_of_modtype b false true true mt;

        (* a horizontal line *)
        bs b "<hr width=\"100%\">\n";

        bs b "</body></html>";
        Buffer.output_buffer chanout b;
        close_out chanout;

        (* generate the file with the complete module type *)
        assert false
(*
        (
         match mt.mt_type with
           None -> ()
         | Some mty ->
             output_module_type
               mt.mt_name
               (Filename.concat !Odoc_args.target_dir type_file)
               mty
        )
*)
      with
        Sys_error s ->
          raise (Failure s)

    (** Generate the html file for the given module.
       @raise Failure if an error occurs.*)
    let generate_for_module pre post modu =
      try
        Odoc_info.verbose ("Generate for module "^modu.m_name);
        let (html_file, _) = naming_html_files modu.m_name in
        let type_file = naming_file_type_module_complete_target modu.m_name in
        let code_file = naming_file_code_module_complete_target modu.m_name in
        let chanout = open_out (Filename.concat !Odoc_args.target_dir html_file) in
        let b = new_buf () in
        let pre_name = opt (fun m -> m.m_name) pre in
        let post_name = opt (fun m -> m.m_name) post in
        bs b doctype ;
        bs b "<html>\n";
        print_header b
           (Some (pre_name, post_name, modu.m_name))
           (Odoc_module.module_comments true modu)
          (inner_title modu.m_name);
        bs b "<body>\n" ;
        print_navbar b pre_name post_name modu.m_name ;
        bs b "<center><h1>";
        if modu.m_text_only then
          bs b modu.m_name
        else
          (
           bs b
             (
              if Odoc_module.module_is_functor modu then
                Odoc_messages.functo
              else
                Odoc_messages.modul
             );
           bp b " <a href=\"%s\">%s</a>" type_file modu.m_name;
           (
            match modu.m_code with
              None -> ()
            | Some _ -> bp b " (<a href=\"%s\">.ml</a>)" code_file
           )
          );
        bs b "</h1></center>\n<br>\n";

        if not modu.m_text_only then html_of_module b false true true modu;

        (* a horizontal line *)
        if not modu.m_text_only then bs b "<hr width=\"100%\">\n";

        (* module elements *)
        List.iter
          (html_of_module_element b (Odoc_name.father modu.m_name))
          (Odoc_module.module_elements true modu);

        bs b "</body></html>";
        Buffer.output_buffer chanout b;
        close_out chanout;

(*
        (* generate the file with the complete module type *)
        output_module_type
          modu.m_name
          (Filename.concat !Odoc_args.target_dir type_file)
          modu.m_type;
*)

        match modu.m_code with
          None -> ()
        | Some code ->
            output_code
              modu.m_name
              (Filename.concat !Odoc_args.target_dir code_file)
              code
      with
        Sys_error s ->
          raise (Failure s)

    (** Generate the [<index_prefix>.html] file corresponding to the given module list.
       @raise Failure if an error occurs.*)
    let generate_index module_list =
      try
        let chanout = open_out (Filename.concat !Odoc_args.target_dir index) in
        let b = new_buf () in
        let title = match !Odoc_args.title with None -> "" | Some t -> escape t in
        bs b doctype ;
        bs b "<html>\n";
        print_header b None [] title;
        bs b "<body>\n";
        bs b "<center><h1>";
        bs b title;
        bs b "</h1></center>\n" ;
        let info = Odoc_misc.apply_opt
            (Odoc_info.info_of_comment_file module_list)
            !Odoc_args.intro_file
        in
        (
         match info with
           None ->
             html_of_Index_list b;
             bs b "<br/>";
             html_of_Module_list b
               (List.map (fun m -> m.m_name) module_list);
         | Some i -> html_of_info false b info
        );
        bs b "</body>\n</html>";
        Buffer.output_buffer chanout b;
        close_out chanout
      with
        Sys_error s ->
          raise (Failure s)

    (** Generate the values index in the file [index_values.html]. *)
    let generate_values_index module_list =
(      generate_elements_index
:
        Odoc_value.t_value list ->
          (Odoc_value.t_value -> Odoc_name.t) ->
            (Odoc_value.t_value -> Odoc_types.info option) ->
              (Odoc_value.t_value -> string) -> string -> string -> unit   )

        !list_values
        (fun v -> v.val_name)
        (fun v -> v.val_info)
        naming_complete_value_target
        Odoc_messages.index_of_values
        index_values

    (** Generate the exceptions index in the file [index_exceptions.html]. *)
    let generate_exceptions_index module_list =
      generate_elements_index
        !list_exceptions
        (fun e -> e.ex_name)
        (fun e -> e.ex_info)
        naming_complete_exception_target
        Odoc_messages.index_of_exceptions
        index_exceptions

    (** Generate the types index in the file [index_types.html]. *)
    let generate_types_index module_list =
      generate_elements_index
        !list_types
        (fun t -> t.ty_name)
        (fun t -> t.ty_info)
        naming_complete_type_target
        Odoc_messages.index_of_types
        index_types

    (** Generate the attributes index in the file [index_attributes.html]. *)
    let generate_attributes_index module_list =
      generate_elements_index
        !list_attributes
        (fun a -> a.att_value.val_name)
        (fun a -> a.att_value.val_info)
        naming_complete_attribute_target
        Odoc_messages.index_of_attributes
        index_attributes

    (** Generate the methods index in the file [index_methods.html]. *)
    let generate_methods_index module_list =
      generate_elements_index
        !list_methods
        (fun m -> m.met_value.val_name)
        (fun m -> m.met_value.val_info)
        naming_complete_method_target
        Odoc_messages.index_of_methods
        index_methods

    (** Generate the modules index in the file [index_modules.html]. *)
    let generate_modules_index module_list =
      generate_elements_index
        !list_modules
        (fun m -> m.m_name)
        (fun m -> m.m_info)
        (fun m -> fst (naming_html_files m.m_name))
        Odoc_messages.index_of_modules
        index_modules

    (** Generate the module types index in the file [index_module_types.html]. *)
    let generate_module_types_index module_list =
      generate_elements_index
        !list_module_types
        (fun mt -> mt.mt_name)
        (fun mt -> mt.mt_info)
        (fun mt -> fst (naming_html_files mt.mt_name))
        Odoc_messages.index_of_module_types
        index_module_types

    (** Generate all the html files from a module list. The main
       file is [<index_prefix>.html]. *)
    let generate module_list =
      (* init the style *)
      init_style ;
      (* init the lists of elements *)
      list_values := Odoc_search.values module_list ;
      list_exceptions := Odoc_search.exceptions module_list ;
      list_types := Odoc_search.types module_list ;
      list_attributes := Odoc_search.attributes module_list ;
      list_methods := Odoc_search.methods module_list ;
      list_modules := Odoc_search.modules module_list ;
      list_module_types := Odoc_search.module_types module_list ;

      (* prepare the page header *)
      prepare_header module_list ;
      (* Get the names of all known types. *)
      let types = Odoc_search.types module_list in
      known_types_names :=
        List.fold_left
          (fun acc t -> Set.add t.ty_name acc)
          !known_types_names
          types ;
      (* Get the names of all known modules and module types. *)
      let module_types = Odoc_search.module_types module_list in
      let modules = Odoc_search.modules module_list in
      known_modules_names :=
        List.fold_left
          (fun acc m -> Set.add m.m_name acc)
          !known_modules_names
          modules ;
      known_modules_names :=
        List.fold_left
          (fun acc mt -> Set.add mt.mt_name acc)
          !known_modules_names
          module_types ;
      (* generate html for each module *)
      if not !Odoc_args.index_only then
        generate_elements generate_for_module module_list ;

      try
        generate_index module_list;
        generate_values_index module_list ;
        generate_exceptions_index module_list ;
        generate_types_index module_list ;
        generate_attributes_index module_list ;
        generate_methods_index module_list ;
        generate_modules_index module_list ;
        generate_module_types_index module_list ;
      with
        Failure s ->
          prerr_endline s ;
          incr Odoc_global.errors

    let _ =
      Odoc_llamahtml.html_of_comment :=
        (fun s ->
           let b = new_buf () in
          html_of_text b (Odoc_text.text_of_string s);
           Buffer.contents b
        )
