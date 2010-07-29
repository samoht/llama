  (* The run-time library for scanf. *)
  type in_channel_name =
    | From_file of string * Pervasives.in_channel
    | From_string
    | From_function
    | From_channel of Pervasives.in_channel
  ;;

  type in_channel = {
    mutable eof : bool;
    mutable current_char : char;
    mutable current_char_is_valid : bool;
    mutable char_count : int;
    mutable line_count : int;
    mutable token_count : int;
    mutable get_next_char : unit -> char;
    tokbuf : Buffer.t;
    input_name : in_channel_name;
  }
  ;;

  type scanbuf = in_channel;;

  let null_char = '\000';;

  (* Reads a new character from input buffer.  Next_char never fails,
     even in case of end of input: it then simply sets the end of file
     condition. *)
  let next_char ib =
    try
      let c = ib.get_next_char () in
      ib.current_char <- c;
      ib.current_char_is_valid <- true;
      ib.char_count <- succ ib.char_count;
      if c = '\n' then ib.line_count <- succ ib.line_count;
      c with
    | End_of_file ->
      let c = null_char in
      ib.current_char <- c;
      ib.current_char_is_valid <- false;
      ib.eof <- true;
      c
  ;;

  let peek_char ib =
    if ib.current_char_is_valid then ib.current_char else next_char ib;;

  (* Returns a valid current char for the input buffer. In particular
     no irrelevant null character (as set by [next_char] in case of end
     of input) is returned, since [End_of_file] is raised when
     [next_char] sets the end of file condition while trying to read a
     new character. *)
  let checked_peek_char ib =
    let c = peek_char ib in
    if ib.eof then raise End_of_file;
    c
  ;;

  let end_of_input ib =
    ignore (peek_char ib);
    ib.eof
  ;;

  let eof ib = ib.eof;;

  let beginning_of_input ib = ib.char_count = 0;;
  let name_of_input ib =
    match ib.input_name with
    | From_file (fname, _ic) -> fname
    | From_string -> "unnamed character string"
    | From_function -> "unnamed function"
    | From_channel _ic -> "unnamed pervasives input channel"
  ;;

  let char_count ib =
    if ib.current_char_is_valid then ib.char_count - 1 else ib.char_count
  ;;
  let line_count ib = ib.line_count;;
  let reset_token ib = Buffer.reset ib.tokbuf;;
  let invalidate_current_char ib = ib.current_char_is_valid <- false;;

  let token ib =
    let tokbuf = ib.tokbuf in
    let tok = Buffer.contents tokbuf in
    Buffer.clear tokbuf;
    ib.token_count <- succ ib.token_count;
    tok
  ;;

  let token_count ib = ib.token_count;;

  let skip_char max ib =
    invalidate_current_char ib;
    max
  ;;

  let ignore_char max ib = skip_char (max - 1) ib;;

  let store_char max ib c =
    Buffer.add_char ib.tokbuf c;
    ignore_char max ib
  ;;

  let default_token_buffer_size = 1024;;

  let create iname next = {
    eof = false;
    current_char = null_char;
    current_char_is_valid = false;
    char_count = 0;
    line_count = 0;
    token_count = 0;
    get_next_char = next;
    tokbuf = Buffer.create default_token_buffer_size;
    input_name = iname;
  }
  ;;

  let from_string s =
    let i = ref 0 in
    let len = String.length s in
    let next () =
      if !i >= len then raise End_of_file else
      let c = s.[!i] in
      incr i;
      c in
    create From_string next
  ;;

  let from_function = create From_function;;

  (* Scanning from an input channel. *)

  (* Position of the problem:

     We cannot prevent the scanning mechanism to use one lookahead character,
     if needed by the semantics of the format string specifications (e.g. a
     trailing ``skip space'' specification in the format string); in this case,
     the mandatory lookahead character is indeed read from the input and not
     used to return the token read. It is thus mandatory to be able to store
     an unused lookahead character somewhere to get it as the first character
     of the next scan.

     To circumvent this problem, all the scanning functions get a low level
     input buffer argument where they store the lookahead character when
     needed; additionally, the input buffer is the only source of character of
     a scanner. The [scanbuf] input buffers are defined in module {!Scanning}.

     Now we understand that it is extremely important that related successive
     calls to scanners indeed read from the same input buffer. In effect, if a
     scanner [scan1] is reading from [ib1] and stores an unused lookahead
     character [c1] into its input buffer [ib1], then another scanner [scan2]
     not reading from the same buffer [ib1] will miss the character [c],
     seemingly vanished in the air from the point of view of [scan2].

     This mechanism works perfectly to read from strings, from files, and from
     functions, since in those cases, allocating two buffers reading from the
     same source is unnatural.

     Still, there is a difficulty in the case of scanning from an input
     channel. In effect, when scanning from an input channel [ic], this channel
     may not have been allocated from within this library. Hence, it may be
     shared (two functions of the user's program may successively read from
     [ic]). This is highly error prone since, one of the function may seek the
     input channel, while the other function has still an unused lookahead
     character in its input buffer. In conclusion, you should never mix direct
     low level reading and high level scanning from the same input channel.

     This phenomenon of reading mess is even worse when one defines more than
     one scanning buffer reading from the same input channel
     [ic]. Unfortunately, we have no simple way to get rid of this problem
     (unless the basic input channel API is modified to offer a ``consider this
     char as unread'' procedure to keep back the unused lookahead character as
     available in the input channel for further reading).

     To prevent some of the confusion the scanning buffer allocation function
     is a memo function that never allocates two different scanning buffers for
     the same input channel. This way, the user can naively perform successive
     call to [fscanf] below, without allocating a new scanning buffer at each
     invocation and hence preserving the expected semantics.

     As mentioned above, a more ambitious fix could be to change the input
     channel API to allow arbitrary mixing of direct and formatted reading from
     input channels. *)

  (* Perform bufferized input to improve efficiency. *)
  let file_buffer_size = ref 1024;;

  (* The scanner closes the input channel at end of input. *)
  let scan_close_at_end ic = close_in ic; raise End_of_file;;

  (* The scanner does not close the input channel at end of input:
     it just raises [End_of_file]. *)
  let scan_raise_at_end _ic = raise End_of_file;;

  let from_ic scan_close_ic iname ic =
    let len = !file_buffer_size in
    let buf = String.create len in
    let i = ref 0 in
    let lim = ref 0 in
    let eof = ref false in
    let next () =
      if !i < !lim then begin let c = buf.[!i] in incr i; c end else
      if !eof then raise End_of_file else begin
        lim := input ic buf 0 len;
        if !lim = 0 then begin eof := true; scan_close_ic ic end else begin
          i := 1;
          buf.[0]
        end
      end in
    create iname next
  ;;

  let from_ic_close_at_end = from_ic scan_close_at_end;;

  (* The scanning buffer reading from [Pervasives.stdin].
     One could try to define [stdib] as a scanning buffer reading a character at a
     time (no bufferization at all), but unfortunately the top-level
     interaction would be wrong.
     This is due to some kind of ``race condition'' when reading from [Pervasives.stdin],
     since the interactive compiler and [scanf] will simultaneously read the
     material they need from [Pervasives.stdin]; then, confusion will result from what should
     be read by the top-level and what should be read by [scanf].
     This is even more complicated by the one character lookahead that [scanf]
     is sometimes obliged to maintain: the lookahead character will be available
     for the next ([scanf]) entry, seemingly coming from nowhere.
     Also no [End_of_file] is raised when reading from stdin: if not enough
     characters have been read, we simply ask to read more. *)
  let stdin =
    from_ic scan_raise_at_end
      (From_file ("-", Pervasives.stdin)) Pervasives.stdin
  ;;

  let stdib = stdin;;

  let open_in fname =
    match fname with
    | "-" -> stdin
    | fname ->
      let ic = open_in fname in
      from_ic_close_at_end (From_file (fname, ic)) ic
  ;;

  let open_in_bin fname =
    match fname with
    | "-" -> stdin
    | fname ->
      let ic = open_in_bin fname in
      from_ic_close_at_end (From_file (fname, ic)) ic
  ;;

  let from_file = open_in;;
  let from_file_bin = open_in_bin;;

  let memo_from_ic =
    let memo = ref [] in
    (fun scan_close_ic ic ->
     try List.assq ic !memo with
     | Not_found ->
       let ib = from_ic scan_close_ic (From_channel ic) ic in
       memo := (ic, ib) :: !memo;
       ib)
  ;;

  let from_channel = memo_from_ic scan_raise_at_end;;

  let close_in ib =
    match ib.input_name with
    | From_file (_fname, ic) -> Pervasives.close_in ic
    | From_string | From_function -> ()
    | From_channel ic -> Pervasives.close_in ic
  ;;
