open Base
open Stdio
module C = Configurator.V1

let write_sexp fn sexp =
  Out_channel.write_all fn ~data:(Sexp.to_string sexp)

let wrap_qmake cfg param =
  let s = Stdlib.String.trim @@
    C.Process.run_capture_exn cfg "qmake" ["-query"; param]
  in
  write_sexp (Printf.sprintf "%s.sexp" param) (sexp_of_string s);
  s

let () =
  C.main ~name:"mylib" (fun c ->
    let default : C.Pkg_config.package_conf =
      { libs   = ["-lblah"]
      ; cflags = []
      }
    in
    let conf =
      match C.Pkg_config.get c with
      | None -> default
      | Some pc ->
        Option.value (C.Pkg_config.query pc ~package:"Qt5Quick") ~default
    in

    let prefix = wrap_qmake c "QT_INSTALL_PREFIX" in
    write_sexp "dummy_path.sexp"      (sexp_of_string @@ Printf.sprintf "%s/mkspecs/features/data/dummy.cpp" prefix);

    write_sexp "c_flags.sexp"         (sexp_of_list sexp_of_string conf.cflags);
    write_sexp "c_library_flags.sexp" (sexp_of_list sexp_of_string conf.libs);
    write_sexp "ocaml_qt_libs.sexp"   (sexp_of_list sexp_of_string @@
                                       List.concat_map ~f:(fun s -> ["-ccopt";s]) @@
                                       conf.libs);
  )
