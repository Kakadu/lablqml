open Format
open Base
open Stdio
module C = Configurator.V1

let write_sexp fn sexp = Out_channel.write_all fn ~data:(Sexp.to_string sexp)

let () =
  C.main ~name:"mylib" (fun c ->
      let default : C.Pkg_config.package_conf = { libs = []; cflags = [] } in
      let conf =
        let pc =
          match C.Pkg_config.get c with
          | None -> C.die "pkg-config is not available"
          | Some pc -> pc
        in
        Option.value (C.Pkg_config.query pc ~package:"Qt5Quick") ~default
      in
      let check_which s =
        if Stdlib.Sys.command (Printf.sprintf "which %s-qt5" s) = 0
        then sprintf "%s-qt5" s
        else s
      in
      let run_qmake ?prefix spec =
        let ans =
          C.Process.run_capture_exn c "qmake" [ "-query"; spec ] |> Base.String.strip
        in
        let filename, cnts =
          match prefix with
          | Some (name, opt) -> sprintf "%s%s" name spec, sprintf "%s%s" opt ans
          | None -> spec, ans
        in
        write_sexp (sprintf "%s.sexp" filename) (sexp_of_string cnts);
        ans
      in
      let _ = run_qmake "QT_VERSION" in
      let _ = run_qmake "QT_INSTALL_HEADERS" in
      let libs_Qt = run_qmake "QT_INSTALL_LIBS" in
      let _ = run_qmake ~prefix:("I_", "-I") "QT_INSTALL_HEADERS" in
      write_sexp "c_flags.sexp" (sexp_of_list sexp_of_string conf.cflags);
      write_sexp "c_library_flags.sexp" (sexp_of_list sexp_of_string conf.libs);
      write_sexp
        "ocaml_qt_libs.sexp"
        (sexp_of_list sexp_of_string
        @@ List.concat_map ~f:(fun s -> [ "-ccopt"; s ])
        @@ conf.libs);
      write_sexp "moc.sexp" (sexp_of_string @@ check_which "moc");
      write_sexp "rcc.sexp" (sexp_of_string @@ check_which "rcc");
      let () =
        let files =
          C.Process.run_capture_exn c "ls" [ "-1"; sprintf "%s/metatypes/*.json" libs_Qt ]
          |> Base.String.strip
          |> C.Flags.extract_comma_space_separated_words
        in
        let filename = "qml_foreign_types.sexp" in
        let contents = sprintf "--foreign-types=%s" (String.concat ~sep:"," files) in
        write_sexp filename (sexp_of_string contents)
      in
      ())
;;
