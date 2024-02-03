open Format
open Base
open Stdio
module C = Configurator.V1

module Ver = struct
  let cmp_lists a b =
    let rec helper = function
      | [], [] -> 0
      | _, [] -> 1
      | [], _ -> -1
      | h1 :: _, h2 :: _ when Stdlib.compare h1 h2 <> 0 -> Int.compare h1 h2
      | _ :: t1, _ :: t2 -> helper (t1, t2)
    in
    helper (a, b)

  let cmp l r =
    let l = String.split l ~on:'.' |> List.map ~f:Stdlib.int_of_string in
    let r = String.split r ~on:'.' |> List.map ~f:Stdlib.int_of_string in
    cmp_lists l r
end

let write_sexp fn sexp = Out_channel.write_all fn ~data:(Sexp.to_string sexp)

let qtquick_pkg,rcc, moc = 
  match Sys.getenv "QT" with 
  | Some "6" -> "Qt6Quick", "/usr/lib/qt6/libexec/rcc", "/usr/lib/qt6/libexec/moc"
  | _ -> "Qt5Quick", "rcc", "moc"

let () =
  C.main ~name:"mylib" (fun c ->
      let default : C.Pkg_config.package_conf = { libs = []; cflags = [] } in
      let conf =
        let pc =
          match C.Pkg_config.get c with
          | None -> C.die "pkg-config is not available"
          | Some pc -> pc
        in
        Option.value (C.Pkg_config.query pc ~package:qtquick_pkg) ~default
      in
      let check_which s =
        if Stdlib.Sys.command (Printf.sprintf "which %s-qt5" s) = 0 then
          sprintf "%s-qt5" s
        else s
      in
      let qmake_bin = check_which (match Sys.getenv "QT" with 
        | Some "6" -> "qmake6"
        | _ -> "qmake") 
      in
      write_sexp "moc.sexp" (sexp_of_string @@ check_which moc);
      write_sexp "rcc.sexp" (sexp_of_string @@ check_which rcc);
      write_sexp "qmake.sexp" (sexp_of_string qmake_bin);

      let run_qmake ?prefix spec =
        let ans =
          C.Process.run_capture_exn c qmake_bin [ "-query"; spec ]
          |> Base.String.strip
        in
        let filename, cnts =
          match prefix with
          | Some (name, opt) ->
              (sprintf "%s%s" name spec, sprintf "%s%s" opt ans)
          | None -> (spec, ans)
        in
        write_sexp (sprintf "%s.sexp" filename) (sexp_of_string cnts);
        ans
      in
      let ver_Qt = run_qmake "QT_VERSION" in
      let _ = run_qmake "QT_INSTALL_HEADERS" in
      let libs_Qt = run_qmake "QT_INSTALL_LIBS" in
      let bins_Qt = run_qmake "QT_INSTALL_BINS" in
      let _ = run_qmake ~prefix:("I_", "-I") "QT_INSTALL_HEADERS" in
      write_sexp "c_flags.sexp" (sexp_of_list sexp_of_string conf.cflags);
      write_sexp "c_library_flags.sexp" (sexp_of_list sexp_of_string conf.libs);
      write_sexp "ocaml_qt_libs.sexp"
        (sexp_of_list sexp_of_string
        @@ List.concat_map ~f:(fun s -> [ "-ccopt"; s ])
        @@ conf.libs);
      write_sexp "qmltyperegistrar.sexp"
        (sexp_of_string @@ check_which @@ sprintf "%s/qmltyperegistrar" bins_Qt);
      let () =
        let filename = "qml_foreign_types.sexp" in
        let contents =
          if Ver.cmp ver_Qt "5.15" > 0 then
            let files =
              C.Process.run_capture_exn c "ls"
                [ "-1"; sprintf "%s/metatypes/*.json" libs_Qt ]
              |> Base.String.strip
              |> C.Flags.extract_comma_space_separated_words
            in
            sprintf "--foreign-types=%s" (String.concat ~sep:"," files)
          else sprintf ""
        in
        write_sexp filename (sexp_of_string contents)
      in
      ())
