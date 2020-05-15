(* Configuration of extension:
 * Should we geneate C++ code, should we add debug printing, etc. *)
type config =
  { mutable gencpp: bool
  ; mutable destdir: string
  ; mutable ext: string
  ; mutable insert_locks: bool
  ; mutable trace_locks: bool
  }

let config  =
  { gencpp=true
  ; destdir="."
  ; ext="cpp"
  ; insert_locks = true
  ; trace_locks = false
  }
