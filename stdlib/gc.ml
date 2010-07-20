open Printf

type stat = {
  minor_words : int;
  promoted_words : int;
  major_words : int;
  minor_collections : int;
  major_collections : int;
  heap_words : int;
  heap_chunks : int;
  live_words : int;
  live_blocks : int;
  free_words : int;
  free_blocks : int;
  largest_words : int;
  fragments : int
};;

type control = {
  mutable minor_heap_size : int;
  mutable major_heap_increment : int;
  mutable space_overhead : int;
  mutable verbose : bool
};;


external stat : unit -> stat = 1 "gc_stat";;
external get : unit -> control = 1 "gc_get";;
external set : control -> unit = 1 "gc_set";;
external minor : unit -> unit = 1 "gc_minor";;
external major : unit -> unit = 1 "gc_major";;
external full_major : unit -> unit = 1 "gc_full_major";;

let print_stat c =
  let st = stat () in
  fprintf c "minor_words: %d\n" st.minor_words;
  fprintf c "promoted_words: %d\n" st.promoted_words;
  fprintf c "major_words: %d\n" st.major_words;
  fprintf c "minor_collections: %d\n" st.minor_collections;
  fprintf c "major_collections: %d\n" st.major_collections;
  fprintf c "heap_words: %d\n" st.heap_words;
  fprintf c "heap_chunks: %d\n" st.heap_chunks;
  fprintf c "live_words: %d\n" st.live_words;
  fprintf c "live_blocks: %d\n" st.live_blocks;
  fprintf c "free_words: %d\n" st.free_words;
  fprintf c "free_blocks: %d\n" st.free_blocks;
  fprintf c "largest_words: %d\n" st.largest_words;
  fprintf c "fragments: %d\n" st.fragments;
;;
