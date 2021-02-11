open Base
open Printf
open PpxQtCfg
open Gencpp

let fprintfn ch fmt = ksprintf (Format.fprintf ch "%s\n%!") fmt

exception ErrorMsg of string * Location.t

open Gencpp

let () =
  Location.register_error_of_exn (fun exn ->
      match exn with
      | ErrorMsg (msg, loc) -> Some (Location.error ~loc msg)
      | _ -> None)
;;

let string_suits_prop s =
  match s with
  | "int" -> Result.return Arg.Int
  | "bool" -> Result.return Arg.Bool
  | "string" -> Result.return Arg.QString
  | "variant" -> Result.return Arg.QVariant
  | _ -> Result.fail (Printf.sprintf "Type '%s' is unknown" s)
;;

open Myparser
open Myparser.Testdemo

module GenProp = struct
  open Gencpp

  module Mangling = struct
    let signal_stub ~classname ~signal_name =
      sprintf "caml_%s_%s_cppmeth_wrapper" classname signal_name
    ;;

    let getter_stub ~classname ~getter_name =
      [%string "register_$(classname)_$(getter_name)_getter"]
    ;;
  end

  module CamlSidePreRegistered = struct
    let run ~classname ~propname (typ : Gencpp.Arg.default Arg.t) pinfo =
      let sgnl_name = Names.signal_of_prop propname in
      let getter_name = Option.value_exn pinfo.p_read in
      let cpptyp_name = cpptyp_of_proptyp @@ wrap_typ_simple typ in
      let cpp () =
        let ppf, prints_ = get_smart_ppf get_source_ch ~classname in
        let println fmt = fprintfn ppf fmt in
        let typ = Arg.default_plus_model typ in
        (* There we only call registered in OCaml functions *)
        let () =
          let registration_f =
            Mangling.getter_stub ~classname ~getter_name:(Option.value_exn pinfo.p_read)
          in
          println "%s %s::%s() {" cpptyp_name classname getter_name;
          println "  CAMLparam0();";
          println "  CAMLlocal1(_ans);";
          (* println "  value *callback = nullptr;"; *)
          prints_
            [%string
              {|
            static value *closure = nullptr;
            if (closure == nullptr) {
              closure = (value*) caml_named_value("$registration_f") ;
              Q_ASSERT_X(closure, Q_FUNC_INFO, "Value $registration_f is not created on OCaml side");
            }
            _ans = caml_callback(*closure, Val_unit);
            |}];
          let cppvar = "ans" in
          println "  %s %s;" (Gencpp.cpptyp_of_typ (typ, ai_empty)) cppvar;
          Gencpp.cpp_value_of_ocaml
            ~cppvar
            ~ocamlvar:"_ans"
            ppf
            (Gencpp.vars_triplet [ "_ans" ])
            typ;
          println "  CAMLreturnT(%s,ans);" (cpptyp_of_typ (typ, ai_empty));
          println "}\n"
        in
        let _setter =
          let setter_name = Option.value_exn pinfo.p_write in
          let registration_f = [%string "register_$(classname)_$(propname)_setter"] in
          let ocaml_var = "_val" in
          println "void %s::%s(%s newVal) {" classname setter_name cpptyp_name;
          println "  CAMLparam0();";
          println "  CAMLlocal1(%s);" ocaml_var;
          (* println "  value *callback = nullptr;"; *)
          Gencpp.ocaml_value_of_cpp
            ppf
            (Gencpp.vars_triplet [ ocaml_var ])
            ~ocamlvar:ocaml_var
            ~cppvar:"newVal"
            typ;
          prints_
            [%string
              {|
            static value *closure = nullptr;
            closure = (value*) caml_named_value("$registration_f");
            Q_ASSERT_X(closure, Q_FUNC_INFO, "Value $registration_f is not created on OCaml side");
            caml_callback(*closure, $(ocaml_var));
            CAMLreturn0;
            |}];
          println "\n}\n"
        in
        let _notifier =
          let stubname = Mangling.signal_stub ~classname ~signal_name:sgnl_name in
          Gencpp.gen_stub_cpp
            ~classname
            ~methname:sgnl_name
            ~stubname
            ppf
            [ typ, ai_empty; Arg.default_plus_model Unit, ai_empty ]
        in
        ()
      in
      let hdr () =
        let ppf = get_header_ppf ~classname in
        let println fmt = fprintfn ppf fmt in
        fprintfn ppf "public:";
        println
          "  Q_PROPERTY(%s %s READ %s %s NOTIFY %s)"
          cpptyp_name
          propname
          (Option.value_exn pinfo.p_read)
          (Option.value_map pinfo.p_write ~default:"" ~f:(sprintf "WRITE %s"))
          sgnl_name;
        println "  %s %s();" cpptyp_name getter_name;
        Option.iter pinfo.p_write ~f:(fun setter_name ->
            println "  void %s(%s);" setter_name cpptyp_name);
        println "signals:";
        println "  void %s(%s %s);" sgnl_name cpptyp_name propname;
        ()
      in
      let ml () = () in
      cpp ();
      hdr ();
      ml ()
    ;;
  end
end

module OnSingleton = struct
  let on_header ~loc ~classname info =
    Format.printf "on_singleton\n%!";
    let ppf, _ = Gencpp.get_smart_ppf Gencpp.get_header_ch ~classname in
    let println fmt = fprintfn ppf fmt in
    println "class %s : public %s {" classname "QObject";
    println "  Q_OBJECT";
    println "  QML_NAMED_ELEMENT(%s)" classname;
    println "  QML_SINGLETON";
    println "public:";
    println "  %s();" classname;
    List.iter info.props ~f:(fun (propname, typ, pinfo) ->
        match string_suits_prop typ with
        | Error msg ->
          raise @@ ErrorMsg (sprintf "Can't wrap property '%s': %s" propname msg, loc)
        | Ok typ ->
          ();
          GenProp.CamlSidePreRegistered.run ~classname ~propname typ pinfo
        (* Gencpp.gen_prop ~classname ~propname typ *)
        (*let signal_name = Names.signal_of_prop propname in
       ref_append ~set:heading (make_stub_for_signal ~classname ~loc ~typ:core_typ signal_name);
       let open Ast_helper in
       let e = Exp.(poly (apply
                            (ident (mkloc (Lident ("stub_"^signal_name)) loc) )
                            [Nolabel, [%expr self#handler]
                            ]
                         )
                         None) in

       [ (Cf.method_ (mkloc ("emit_" ^ signal_name) loc)
                   Public (Cfk_concrete (Fresh,e)) ).pcf_desc
       ; Pcf_method (Located.mk ~loc (Gencpp.Names.getter_of_prop propname),
                     flag,
                     Cfk_virtual Ast_helper.Typ.(arrow Nolabel (unit_coretyp loc) core_typ) )
       ]*))
  ;;

  let creation_callback ~classname = [%string "save_freshly_created_$classname"]

  let on_src ~loc:_ ~classname info =
    let ppf, prints_ = Gencpp.get_smart_ppf Gencpp.get_source_ch ~classname in
    ignore prints_;
    let println fmt = fprintfn ppf fmt in
    println "#include <QtQml/QQmlEngine>";
    println "#ifdef __cplusplus";
    println "extern \"C\" {";
    println "#endif";
    println "#include <caml/alloc.h>";
    println "#include <caml/mlvalues.h>";
    println "#include <caml/callback.h>";
    println "#include <caml/memory.h>";
    println "#include <caml/threads.h>";
    println "#ifdef __cplusplus";
    println "}";
    println "#endif";
    let _namespace = Option.value info.namespace ~default:"Qt.example.qobjectSingleton" in
    let registration_f = creation_callback ~classname in
    let obj = "this" in
    Format.pp_print_string
      ppf
      [%string
        {|
      $classname::$classname() {
            CAMLparam0();
            CAMLlocal1(_ans);

            // store this shit in OCaml
            static value *closure = nullptr;
            if (closure == nullptr) {
              closure = (value*) caml_named_value("$registration_f") ;
              Q_ASSERT_X(closure, Q_FUNC_INFO, "Value $registration_f is not created on OCaml side");
            }   |}];
    Gencpp.alloc_and_store ppf ~classname ~obj ~where:"_ans";
    Gencpp.leave_blocking_section ppf;
    println "  caml_callback(*closure, _ans);";
    Gencpp.enter_blocking_section ppf;
    println "  CAMLreturn0;";
    println "}\n%!";
    ()
  ;;

  let on_ml ~loc ~classname (name, stru, sign) info =
    let open Ppxlib in
    let open Ast_builder.Default in
    let extra_stru ?(creation = false) ?(notifiers = false) ?(getters = false) () =
      List.concat
        [ (if creation
          then
            [ [%stri type t = Lablqml.cppobj]
            ; [%stri
                let self_container : (t, [< `Noninitialized ]) Result.t ref =
                  ref (Result.error `Noninitialized)
                ;;]
            ; (let stub_name = creation_callback ~classname in
               [%stri
                 let () =
                   Callback.register
                     [%e pexp_constant ~loc @@ Pconst_string (stub_name, loc, None)]
                     (fun cppobj ->
                       Format.printf "initialized %s %d\n%!" __FILE__ __LINE__;
                       self_container := Result.ok cppobj)
                 ;;])
            ]
          else [])
        ; List.concat_map info.props ~f:(fun (_, ptyp, info) ->
              Stdlib.List.flatten
                [ (match info.p_notify with
                  | Some notifier when notifiers ->
                    let stub_name =
                      GenProp.Mangling.signal_stub ~classname ~signal_name:notifier
                    in
                    let open Ppxlib.Ast_builder.Default in
                    List.return
                    @@ pstr_primitive ~loc
                    @@ value_description
                         ~loc
                         ~name:(Located.mk ~loc notifier)
                         ~type_:
                           [%type:
                             Lablqml.cppobj
                             -> [%t ptyp_constr ~loc (Located.mk ~loc @@ Lident ptyp) []]
                             -> unit]
                         ~prim:[ stub_name ]
                  | _ -> [])
                ; (match info.p_read with
                  | Some getter when getters ->
                    let stub_name =
                      GenProp.Mangling.getter_stub ~classname ~getter_name:getter
                    in
                    List.return
                      [%stri
                        let () =
                          Callback.register
                            [%e
                              pexp_constant ~loc @@ Pconst_string (stub_name, loc, None)]
                            (fun () ->
                              [%e pexp_ident ~loc (Located.mk ~loc (Lident getter))] ())
                        ;;]
                  | _ -> [])
                ])
        ]
    in
    let extra_sign =
      Stdlib.List.flatten
        [ [ [%sigi: type t] ]
        ; [ [%sigi: val self_container : (t, [ `Noninitialized ]) Result.t ref] ]
        ; List.concat_map info.props ~f:(fun (_, ptyp, info) ->
              Stdlib.List.flatten
                [ (match info.p_notify with
                  | None -> []
                  | Some notifier ->
                    let stub_name =
                      GenProp.Mangling.signal_stub ~classname ~signal_name:notifier
                    in
                    let open Ppxlib.Ast_builder.Default in
                    List.return
                    @@ psig_value ~loc
                    @@ value_description
                         ~loc
                         ~name:(Located.mk ~loc notifier)
                         ~type_:
                           [%type:
                             t
                             -> [%t ptyp_constr ~loc (Located.mk ~loc @@ Lident ptyp) []]
                             -> unit]
                         ~prim:[ stub_name ])
                ])
        ]
    in
    pstr_module ~loc
    @@ module_binding
         ~loc
         ~name:(Located.mk ~loc (Option.some name))
         ~expr:
           (pmod_constraint
              ~loc
              (pmod_structure ~loc
              @@ extra_stru ~creation:true ~notifiers:true ()
              @ stru
              @ extra_stru ~getters:true ())
              (pmty_signature ~loc @@ sign @ extra_sign))
    |> List.return
  ;;

  let run ~loc ~classname mb info =
    if PpxQtCfg.config.gencpp
    then (
      on_src ~loc ~classname info;
      on_header ~loc ~classname info);
    on_ml ~loc ~classname mb info
  ;;
end

let wrap_module_decl ~loc name stru sign info =
  Format.printf "%s %d\n%!" __FILE__ __LINE__;
  print_endline @@ Testdemo.show_info info;
  let classname =
    match info.name with
    | Some s -> s
    | None -> failwith "class has no name"
  in
  Gencpp.only_open ~classname;
  Gencpp.print_header_preamble ~classname;
  Gencpp.print_source_preamble ~classname;
  let ans =
    if info.is_singleton
    then OnSingleton.run ~loc ~classname (name, stru, sign) info
    else failwith "not implemented"
  in
  let () = Gencpp.close_files ~caml_owner:false ~options:[] () in
  ans
;;
