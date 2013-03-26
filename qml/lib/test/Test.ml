open Printf

let root = S.(build_tree (read_modules "/home/kakadu/.opam/4.00.1/lib/ocaml"))

let selected = ref [-1]

let print() = Tree.print_tree root (fun _ -> "") !selected

let () =
  print_endline "Testing";
  print ();
  try
    while true  do
      printf "Enter x,y:\n%!";
      Scanf.sscanf (read_line()) "%d,%d" (fun x y ->
        printf "You entered (%d,%d)\n" x y;
        let (new_s,changed) = Tree.change_state !selected (x,y) root in
        printf "New selected are: [%s]\n" (String.concat "," (List.map string_of_int new_s));
        printf "To redraw from %d\n" changed;
        selected := new_s;
        print()
      )
    done
  with Scanf.Scan_failure s ->
    print_endline s;
    exit 0
