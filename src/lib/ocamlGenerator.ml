open Core
open Core.Common
open Core.Std
open Parser
open Generators
open Printf
open SuperIndex

let ocaml_class_name classname =
  if classname.[0] = 'Q' then
    let s = String.copy classname in
    s.[0] <- 'q';
    s
  else
    classname

exception Break2File of string
let break2file s = raise (Break2File s)

class ocamlGenerator graph dir index = object (self)
  method private prefix = dir ^/ "ml"

  (* low_level means, for example, to generate [`qobject] obj instead of qObject *)
  method toOcamlType ?(forcePattern=None) ~low_level (arg: Parser.func_arg) : castResult =
    let {arg_type=t; arg_default=default; _} = arg in
    let patt = match forcePattern with
      | None   -> pattern index arg
      | Some x  -> x in
    match patt with
    | InvalidPattern -> CastError (sprintf "Cant cast: %s" (string_of_type t) )
    | PrimitivePattern "int"    -> Success "int"
    | PrimitivePattern "double" -> CastError "double value!"
    | PrimitivePattern "bool"   -> Success "bool"
    | PrimitivePattern "QString"-> Success "string"
    | PrimitivePattern "void"   -> Success "unit"
    | PrimitivePattern "char*"  -> Success "string"
    | PrimitivePattern "QModelIndex" -> Success "QModelIndex.t"
    | PrimitivePattern "QVariant"   -> Success "QVariant.t"
    | PrimitivePattern x        -> CastError (sprintf "Unknown primitive `%s`" x)
    | ObjectPattern ->
      Success (if low_level then "[> `qobject] obj" else ocaml_class_name t.t_name)
    | ObjectDefaultPattern ->
      Success (
        sprintf "%s option" (if low_level then "[> `qobject] obj" else ocaml_class_name t.t_name)
      )
    | EnumPattern ({e_items;_},key) ->
      let s = String.concat ~sep:" | " (List.map e_items (fun x -> "`"^x)) in
      Success (sprintf "[%s]" s)

  method toOcamlType_exn ?forcePattern ~low_level arg : string =
    let f: pattern option = match forcePattern with x -> x in
    match self#toOcamlType ~forcePattern:f ~low_level arg with
      | Success s -> s
      | CastError err -> failwithf "CastError: %s" err ()

  method gen_constr ~classname ~is_tween h i argslist =
    (* TODO: maybe disable copy constructors *)
    try
      if List.length argslist > 5 then break2file "more then 5 parameters";
      fprintf h "\n(* constructor %s *)\n" (string_of_constr ~classname argslist);
      let argnames = List.mapi argslist ~f:(fun i _ -> "x"^(string_of_int i)) in

      let cpp_func_name classname =
        let native_name = cpp_stub_name ~classname ?res_n_name:None ~is_byte:false `Public argslist in
        if List.length argslist > 5 then
          sprintf "%s\" \"%s" (cpp_stub_name ~classname ?res_n_name:None ~is_byte:true `Public argslist)
            native_name
        else
          native_name
      in
      let cpp_func_name = cpp_func_name (if is_tween then classname^"_twin" else classname) in

      let types = List.map argslist ~f:(self#toOcamlType_exn ~low_level:true) in
      let no_args = (List.length types = 0) in

      let ocaml_func_name =
        if not is_tween then sprintf "create_%s_%d" classname i
        else sprintf "create_%s_twin_%d" classname i in

      let print_stub caml_name stub_name =
        fprintf h "external %s' : %s -> [>`qobject] obj = \"%s\"\n" caml_name
          (if no_args then "unit" else ( String.concat ~sep:"->" types) )
          stub_name
      in
      print_stub ocaml_func_name cpp_func_name;

      let types = List.map argslist ~f:(self#toOcamlType_exn ~low_level:false) in

      let args2 = List.map3_exn types argslist argnames ~f:(fun _ arg name ->
        match pattern index arg with
          | InvalidPattern -> assert false
          | EnumPattern _
          | PrimitivePattern _ -> name
          | ObjectPattern    -> sprintf " (%s#handler) " name
          | ObjectDefaultPattern -> sprintf "(wrap_handler \"%s\" \"%s\" %s)" cpp_func_name name name
      ) in
      let args1 = List.map3_exn types argslist argnames ~f:(fun ttt ({arg_type=start;_} as arg) name ->
        match pattern index arg with
          | InvalidPattern -> assert false
          | EnumPattern _
          | PrimitivePattern _ -> sprintf "(%s: %s)" name ttt
          | ObjectDefaultPattern -> sprintf "(%s: %s option)" name (ocaml_class_name start.t_name)
          | ObjectPattern -> sprintf "(%s: %s)" name (ocaml_class_name start.t_name)
      ) in(*
      let print_main_creator ocaml_func_name =
        let s1 = sprintf "let %s %s = " ocaml_func_name
          (if no_args then "()" else String.concat ~sep:" " args1) in
        fprintf h "%s" s1;
        if String.length s1 > 60 then fprintf h "\n    ";

        fprintf h "%s' %s %s\n" ocaml_func_name (if no_args then "()" else String.concat ~sep:" " args2)
          (sprintf "\n    |> new %s" (ocaml_class_name classname))
      in
      print_main_creator ocaml_func_name;*)
      ()
    with BreakSilent -> ()
      | Break2File s -> fprintf h "(* %s *)\n" s

  method private gen_meth_stubs ~isQObject ~classname h meth =
    fprintf h "  (* method %s *)\n" (string_of_meth meth);
    assert(meth.m_access <> `Private);
    let ocaml_classname = ocaml_class_name classname in

      let cpp_func_name =
        (* Some hack. If this meth has < 5 parameters that string format is similiar to [a-z]+
           But if it needs to functions  for stubs format is [a-z]+" "[a-z]+
        *)
        let classname = if isQObject then classname^"_twin" else meth.m_declared in
	    let access = if isQObject then `Public else meth.m_access in
        let res_n_name = (meth.m_res, meth.m_name) in
        let helper is_byte = cpp_stub_name ~classname ~res_n_name ~is_byte access meth.m_args in
        let native_name = helper false in
        if List.length meth.m_args > 4 then
          sprintf "%s\" \"%s" (helper true) native_name
        else
          native_name
      in

      let types = List.map meth.m_args ~f:(fun arg -> self#toOcamlType ~low_level:true arg ) in
      let types = List.map types ~f:(function Success s -> s | _ -> assert false) in

      let res_arg = simple_arg meth.m_res in
      let res_t = (match pattern index res_arg with
        | ObjectPattern -> self#toOcamlType ~forcePattern:(Some ObjectDefaultPattern) ~low_level:true res_arg
        | ObjectDefaultPattern -> assert false
        | _ -> self#toOcamlType ~low_level:true res_arg)
      |> (function Success s -> s | _ -> raise BreakSilent) in

      fprintf h "external %s%s_%s': 'a->%s\n\t\t= \"%s\"\n" ocaml_classname
        (if isQObject then "_twin" else "") meth.m_out_name
        (String.concat (types @ [res_t]) ~sep:"->") cpp_func_name
  (**
   * [get_params_helper ocamltypes cpp_arguments arg_names method_out_name]
   * returns two string lists. 1st for method arguments
   * 2nd - ....
   * *)
  method get_params_helper types args argnames m_out_name =
    let args2 = List.map3_exn types args argnames ~f:(fun _ arg name -> match pattern index arg with
      | InvalidPattern -> assert false
      | EnumPattern _ | PrimitivePattern _ -> name
      | ObjectDefaultPattern -> sprintf "(wrap_handler \"%s\" \"%s\" %s)" m_out_name name name
      | ObjectPattern -> sprintf "(%s#handler)" name
    ) in
    (* List of strings for method arguments signature*)
    let args1 = List.map3_exn types args argnames ~f:(fun ttt ({arg_type=start;_} as arg) name ->
      match pattern index arg with
        | InvalidPattern -> assert false
        | EnumPattern _ | PrimitivePattern _ -> sprintf "(%s: %s)" name ttt
        | ObjectDefaultPattern -> sprintf "(%s: %s option)" name (ocaml_class_name start.t_name)
        | ObjectPattern -> sprintf "(%s: %s )" name (ocaml_class_name start.t_name)
    ) in
	(args1,args2)

  method postconvert res h =
    match pattern index (simple_arg res) with
      | ObjectPattern -> begin
        let res_classname = ocaml_class_name res.t_name in
           let isQObject =
             let key = NameKey.key_of_fullname res.t_name in
             isQObject ~key graph
           in
        fprintf h "\n";
        fprintf h "    |> (function\n";
        fprintf h "       | None -> None\n";
        (
        match classify2_exn ~index ~graph res.t_name with
          | `NonQObject -> begin
            (* non QObject classes does not have twin class and we have not place to store OCaml object *)
            (* abstract classes will not be marked `virtual` in OCaml. We will just not provide *)
            (* constructors for them *)
            fprintf h "       | Some o -> Some (new %s o)" res_classname
          end
          | `QObject `Normal ->
              (* this class can be twin or non-twin *)
              fprintf h "       | Some o -> Some(match Qtstubs.get_caml_object o with\n";
              fprintf h "                       | Some x -> (x:>%s)\n" res_classname;
              fprintf h "                       | None -> new %s_non_twin o\n" res_classname
          | `QObject `SemiAbstract ->
              (* also can be twin or non-twin *)
              fprintf h "       | Some o -> Some(match Qtstubs.get_caml_object o with\n";
              fprintf h "                       | Some x -> (x:>%s_twin)\n" res_classname;
              fprintf h "                       | None -> new %s_non_twin o\n" res_classname
          | `QObject `Abstract ->
              (* will be created only outside and non-inheritable *)
              fprintf h "       | Some o -> Some (new %s o)" res_classname

        );
          (*
        fprintf h "       |  Some o -> Some(match Qtstubs.get_caml_object o with\n";
        fprintf h "                     | Some x -> (x:>%s)\n" res_classname;
        fprintf h "                     | None -> new %s o)\n"; *)
        fprintf h "  )\n"
      end
      | _ -> ()

  (* generates member code
   *  ~is_abstract true if method is pure virtual
   * *)
  method gen_meth ?new_name ~isQObject ~is_abstract ~classname h (meth: meth) =
    let (res,methname,args) = (meth.m_res,meth.m_name,meth.m_args) in
    try
      let () = match meth.m_access with `Private -> raise BreakSilent | _ -> () in

      if List.length args + 1 > 10 then
        break2file (sprintf "skipped meth %s: too many arguments\n" (string_of_meth meth) );

      fprintf h "  (* method %s *)\n" (string_of_meth meth);
      let res_type =
        let res_arg = simple_arg res in
        match pattern index res_arg with
          | ObjectPattern -> self#toOcamlType_exn ~forcePattern:ObjectDefaultPattern ~low_level:false res_arg
          | ObjectDefaultPattern -> assert false
          | _ -> self#toOcamlType_exn ~low_level:false res_arg
      in
      let types = List.map args ~f:(self#toOcamlType_exn ~low_level:false) in
      let argnames = List.mapi args ~f:(fun i _ -> "x"^(string_of_int i)) in

      let getparams () = self#get_params_helper types args argnames meth.m_out_name in
      let postconvert () = self#postconvert res h  in
      let meth_out_name = match new_name with None -> meth.m_out_name | Some x -> x in
      let () =
        match (isQObject,is_abstract) with
	      | (false,true) -> begin (* not QObject, abstract meth *)
(*	        assert (meth.m_access <> `Protected);*)
            fprintf h "  method virtual %s : %s" meth_out_name (String.concat ~sep:"->" types );
            if List.length types <> 0 then fprintf h " -> ";
            match pattern index (simple_arg res) with
              | ObjectPattern -> fprintf h "%s option" (ocaml_class_name res.t_name)
              | _ -> fprintf h "%s" res_type
	      end
	      | (false,false) -> begin (* not QObject, normal meth *)

	        let (args1,args2) = getparams () in
            fprintf h "  method %s %s =\n" meth_out_name (String.concat ~sep:" " args1);
	        fprintf h "    %s_%s' self#handler %s"
              (ocaml_class_name classname) meth.m_out_name (String.concat ~sep:" " args2);
            postconvert ()
	      end
	      | (true,false) -> begin (* QObject, normal meth *)
	        let (args1,args2) = getparams () in
            fprintf h "  method %s %s =\n" meth_out_name (String.concat ~sep:" " args1);
            fprintf h "    (match Qtstubs.get_class_name me with\n";
            fprintf h "    | Some \"%s_twin\" -> %s_twin_call_super_%s' self#handler %s\n" classname
	          (ocaml_class_name classname) meth.m_out_name (String.concat ~sep:" " args2);
            fprintf h "    | Some \"%s\" -> " classname;
	        if meth.m_access = `Protected
	        then fprintf h "print_endline \"Can't call protected method of non-twin object\"; assert false\n"
	        else fprintf h "%s_%s' self#handler %s\n"
              (ocaml_class_name classname) meth.m_out_name (String.concat ~sep:" " args2);
            fprintf h "    | _ -> print_endline \"Some bug in logic\"; assert false\n";
            fprintf h "    )";
	        postconvert ()
	      end
	      | (true,true) -> begin (* QObject, abstract meth *)
            fprintf h "  method virtual %s : %s" meth_out_name (String.concat ~sep:"->" types );
            if List.length types <> 0 then fprintf h " -> ";
            match pattern index (simple_arg res) with
              | ObjectPattern -> fprintf h "%s option" (ocaml_class_name res.t_name)
              | _ -> fprintf h "%s" res_type
    (*	  let (args1,args2) = getparams () in
            fprintf h "  method %s %s =\n" meth_out_name (String.concat ~sep:" " args1);
	        postconvert ();
            fprintf h "    (match Qtstubs.get_class_name me with\n";
            fprintf h "    | Some \"%s_twin\" -> %s_twin_call_super_%s' self#handler %s\n" classname
	        (ocaml_class_name classname) meth.m_out_name (String.concat ~sep:" " args2);
            fprintf h "    | Some \"%s\" -> print_endline \"Calling abstract meth of non-twin obj\"; flush stdout; raise AbstractTwinException \n" classname;
            fprintf h "    | _ -> print_endline \"Some bug in logic\"; assert false\n";
            fprintf h "    ) |> postconvert" *)
	      end
      in
      fprintf h "\n\n";

    with BreakS str -> ( fprintf h "(* %s *)\n" str;
                         print_endline str )
      | BreakSilent -> ()
      | Break2File s -> ( fprintf h "(* %s *)\n" s; print_endline s)

  method gen_slot ~classname h (slot: Parser.MethKey.t) =
    try
      let (_:string) = classname in
      (match slot.m_access with `Public -> () | `Private | `Protected -> raise BreakSilent);
      let (name,args) = (slot.m_name,slot.m_args) in
      let res = simple_arg slot.m_res in
      let types = List.map args ~f:(fun arg -> self#toOcamlType ~low_level:false arg) in
      let types = List.map types ~f:(function Success s -> s | _ -> raise BreakSilent) in
      let res_t = match self#toOcamlType ~low_level:false res with
        | Success s -> s | _ -> raise BreakSilent in
      let argnames = List.mapi args ~f:(fun i _ -> "arg" ^ (string_of_int i)) in
      fprintf h "  method slot_%s = object (_ : (< %s .. >, %s ) #ssslot)\n"
        slot.m_out_name
        (List.map2_exn argnames types ~f:(fun name t -> name^":"^t^";") |> String.concat ~sep:" ")
        (String.concat ~sep:" -> " (types @ [res_t]));
      let wrap_type t = { { t with t_is_const=false } with t_is_ref=false } in
      fprintf h "    method name = \"%s(%s)\"\n" name
        (slot.m_args |> List.map ~f:(fun x -> string_of_type (wrap_type x.arg_type) )
            |> String.concat ~sep:",");
      fprintf h "    method call = self#%s\n" slot.m_out_name;
      fprintf h "  end\n"
    with BreakSilent -> ()
      | BreakS s -> ()

  method gen_signal = fun h (name,args) ->
    try
      let types = List.map args ~f:(self#toOcamlType ~low_level:false) in
      let types = List.map types ~f:(function Success s -> s | _ -> raise BreakSilent) in
      let argnames = List.mapi args ~f:(fun i _ -> "arg" ^ (string_of_int i)) in
      fprintf h "  method signal_%s = object (self : <%s ..> #sssignal)\n"
        name (List.map2_exn argnames types ~f:(fun name t -> name^":"^t^";") |> String.concat ~sep:" ");
      let wrap_type t = {{ t with t_is_ref=false } with t_is_const = false } in
      fprintf h "    method name = \"%s(%s)\"\n" name
        (List.map args ~f:(fun x -> string_of_type (wrap_type x.arg_type) ) |> String.concat ~sep:",");
      fprintf h "  end\n";
    with BreakSilent -> ()
      | BreakS s -> ()

  method generate (queue: [`Single of NameKey.t | `Group of NameKey.t list] Queue.t) =
    self#makefile dir;
    let h1 = open_out (dir ^/ "classes.ml") in
    let h2 = open_out (dir ^/ "stubs.ml") in
    let h3 = open_out (dir ^/ "creators.ml") in
    fprintf h1 "\nopen Stubs\nopen Stub_helpers\nexception AbstractTwinException\n\n";
    fprintf h2 "open Stub_helpers\n\n";
    fprintf h3 "open Stub_helpers\nopen Classes\nopen Stubs\n";

    printf "Queue length is %d\n" (Queue.length queue);
(*    printf "Key in index are: %s\n" (SuperIndex.keys index |> List.to_string snd); *)
    let classes = ref [] in
    let add_class key = Ref.replace classes (fun c -> key::c) in
    Queue.iter queue ~f:(function
      | `Single key -> begin
        match SuperIndex.find index key with
          | Some (Enum e) -> ()
          | Some (Class (c,_)) ->
              add_class key;
              self#gen_single_class ~prefix:[] h1 h2 h3 c
          | None ->
              raise (Bug (sprintf "Can't find class with key `%s`" (NameKey.to_string key) ) )
      end
      | `Group keys ->
          (* this key are all classs keys*)
          let ks = List.map keys ~f:(fun key ->
            match SuperIndex.find_exn index key with
              | Enum  e -> assert false
              | Class (c,_) -> add_class key; c
          ) in
            self#gen_cycle_classes ~prefix:[] h1 h2 h3 ks
    );
    print_endline "end of classes generation";(*
    fprintf h1 " aa = object end\n\n\n\n\n";*)

    fprintf h1 "(* stubs for creating OCaml classes values from C++ *)\n";
    List.iter !classes ~f:(fun ( (lst,_) as key) ->
      match SuperIndex.find_exn index key with
        | Class (c,_) ->
          let is_abstr = is_abstract_class index ~prefix:[] c.c_name in
          let classname = List.hd_exn lst in
          let ocaml_classname = ocaml_class_name classname in
          fprintf h1 "(* classname = %s, isQObject = %b *)\n" classname
            (isQObject ~key:(NameKey.make_key ~prefix:[] ~name:classname) graph);
          fprintf h1 "let () = Callback.register \"make_%s\" (new %s)\n"
            (String.concat ~sep:"." lst)
            (match classify2_exn classname ~index ~graph with
              | `NonQObject -> ocaml_classname
              | `QObject `Normal -> ocaml_classname
              | `QObject `SemiAbstract -> ocaml_classname^"_non_twin"
              | `QObject `Abstract -> ocaml_classname
            ) (* TODO: check code above*)
        | _ -> assert false
    );

    Out_channel.close h3;
    Out_channel.close h2;
    Out_channel.close h1

  method private gen_constr_data ~classname ~index argslist =
    (* TODO: construct argnames using C++ argument names *)
    let argnames = List.mapi argslist ~f:(fun i _ -> "x"^(string_of_int i)) in
    let types = List.map argslist ~f:(fun arg -> self#toOcamlType ~low_level:false arg) in
    (* With_return  ???? *)
    let types = List.map types ~f:(function Success s -> s
      | _ -> assert false (*because we've checked that all constructors are correct in Filter module *)) in
    (* paramters of create* function or class's parameters *)
    let args1 = List.map3_exn types argslist argnames ~f:(fun ttt ({arg_type=start;_} as arg) name ->
        match pattern index arg with
          | InvalidPattern -> assert false
          | EnumPattern _
          | PrimitivePattern _ -> sprintf "(%s: %s)" name ttt
          | ObjectDefaultPattern -> sprintf "(%s: %s option)" name (ocaml_class_name start.t_name)
          | ObjectPattern -> sprintf "(%s: %s)" name (ocaml_class_name start.t_name)
      ) in
    (* arguments for calling external function *)
    let args2 = List.map3_exn types argslist argnames ~f:(fun _ arg name ->
        match pattern index arg with
          | InvalidPattern -> assert false
          | EnumPattern _
              (* We do low_level casts here because  we call low_level creator when class's initializer *)
          | PrimitivePattern _ -> name
          | ObjectPattern    -> sprintf " (%s#handler) " name
          | ObjectDefaultPattern -> sprintf "(wrap_handler \"%s\" \"%s\" %s)" "don't know" name name
      ) in
    (String.concat ~sep:" " args1, String.concat ~sep:" " args2)

  method gen_single_class ~prefix h_classes h_stubs h_constrs c =
    let key = NameKey.make_key ~name:c.c_name ~prefix in
    fprintf h_classes "(* clas %s *)\n" (NameKey.to_string  key);
    fprintf h_classes "class ";
    self#gen_class ~prefix h_classes h_stubs h_constrs c;
    fprintf h_classes " end\n\n"

  method gen_cycle_classes ~prefix h_classes h_stubs h_constrs cs =
    assert (List.length cs <> 0);
    fprintf h_classes "(* clas_cycle %s *)\n"
      (List.map cs ~f:(fun c -> NameKey.make_key ~name:c.c_name ~prefix)
          |> List.map ~f: NameKey.to_string
          |> String.concat ~sep:","
      );
    fprintf h_classes "class ";
    let gen = self#gen_class ~prefix h_classes h_stubs h_constrs in
    let (h,tl) = List.( (hd_exn cs, tl_exn cs) ) in
    gen h;
    List.iter tl ~f:(fun c ->
      fprintf h_classes "\nend and ";
      gen c
    );
    fprintf h_classes " end\n\n"

  method gen_class ~prefix h_classes h_stubs h_constrs c =
    let key = NameKey.make_key ~name:c.c_name ~prefix in
    if not (SuperIndex.mem index key) then
      printf "Skipping class %s - its not in index\n" (NameKey.to_string key)
    else if skipClass ~prefix c then print_endline ("Skipping class " ^ c.c_name)
    else begin
      let classname = c.c_name in
      printf "Generating class %s\n%!" classname;
      let print_c fmt = fprintf h_classes fmt in
      let print_cre fmt = fprintf h_constrs fmt in
      let print_stu fmt = fprintf h_stubs fmt in

      let clas_sort = classify_class_exn classname ~index in
      let is_abstract = is_abstract_class ~prefix index classname in
      let ocaml_classname = ocaml_class_name classname in
      let isQObject = isQObject graph ~key in

      let () =
        let s = sprintf "(* isQObject=%b\t classification=%s *)\n" isQObject
          (match clas_sort with | `Normal -> "normal"
            | `SemiAbstract -> "SemiAbstract" | `Abstract -> "Abstract")
        in
        print_stu "\n(* ********** class %s *********** *)\n" classname;
        print_stu "%s" s;
        print_c "%s" s
      in

      begin match (clas_sort,isQObject) with
        | (`Normal,false) ->
            printf "Normal non QObject class %s\n%!" classname;
            List.iteri c.c_constrs ~f:(self#gen_constr ~is_tween:isQObject ~classname h_constrs);
            print_c " %s me = object(self)\n" ocaml_classname;
            print_c "  method handler  : [`qobject] obj = me \n\n";

            (* we know that there is no almost good methods here.*)
            let good_meths = MethSet.filter c.c_meths ~f:(is_good_meth ~classname ~index) in
            MethSet.iter good_meths ~f:(fun m ->
              let stub ~isQObject m = self#gen_meth_stubs ~isQObject ~classname h_stubs m in
	          if (m.m_access=`Public) then stub false m;
              stub ~isQObject m;
              stub ~isQObject
	            {{m with m_out_name="call_super_"^m.m_out_name} with m_name="call_super_"^m.m_name};
              self#gen_meth ~isQObject ~is_abstract:false ~classname h_classes m
            );
            (* TODO: slots *)
            (* TODO: signals *)

        | (`Normal,true) ->
            printf "Normal class %s\n%!" classname;
            (* twin class *)
            (* We can create both QObject and non QObject classes but will only 1st *)
            List.iteri c.c_constrs ~f:(self#gen_constr ~is_tween:true ~classname h_constrs);
            print_c " %s me = object(self)\n" ocaml_classname;
            print_c "  method handler  : [`qobject] obj = me \n\n";
            print_c "  initializer print_endline \"initializing %s\";\n" ocaml_classname;
            print_c "              Qtstubs.set_caml_object me self\n";
            (* we know that there is no almost good methods here.*)
            let good_meths = MethSet.filter c.c_meths ~f:(is_good_meth ~classname ~index) in
            let do_method m =
              let stub ~isQObject m = self#gen_meth_stubs ~isQObject ~classname h_stubs m in
	          if (m.m_access=`Public) then stub false m;
              stub ~isQObject m;
              stub ~isQObject
	            {{m with m_out_name="call_super_"^m.m_out_name} with m_name="call_super_"^m.m_name};
              self#gen_meth ~isQObject ~is_abstract:false ~classname h_classes m
            in
            MethSet.iter good_meths ~f:do_method;
            (* slots *)
            let good_slots = MethSet.filter c.c_slots ~f:(fun m ->
              not (List.mem m.m_modif `Abstract) &&
                m.m_access = `Public &&
                is_good_meth ~classname ~index m
            ) in
            MethSet.iter good_slots ~f:(fun (m:meth) ->
              self#gen_slot ~classname h_classes m;
              do_method m;
            );
            (* TODO: signals *)
            print_c "end\n";
            print_c "class %s_non_twin me = object(self)\n" ocaml_classname;
            print_c "  method handler : [`qobject] obj = me\n\n";
            let good_meths = MethSet.filter good_meths
              ~f:(fun m -> m.m_access = `Public) in
            let f = fun m ->
              self#gen_meth_stubs ~isQObject:false ~classname h_stubs m;
              self#gen_meth ~isQObject:false ~is_abstract:false ~classname h_classes m
            in
            MethSet.iter good_meths ~f;
            MethSet.iter good_slots ~f;
            (* TODO: slots *)
            (* TODO: signals *)

        | (`SemiAbstract,true) ->
            printf "SemiAbstract class %s\n%!" classname;
            (* Two classes needed.
             * * Virtual twin class where user can implement virtual methods
             * * Non-virtual class for native Qt classes
             * *)
            (* non-twin class *)
            print_c "%s_non_twin me = object (self)\n" ocaml_classname;
            print_c "  method handler  : [`qobject] obj = me \n\n";
            let pub_meths = MethSet.filter c.c_meths
              ~f:(fun m -> (m.m_access=`Public) && is_good_meth ~index ~classname m) in
            MethSet.iter pub_meths ~f:(fun m ->
              let isQObject = false in
              self#gen_meth_stubs ~isQObject ~classname h_stubs m;
              self#gen_meth ~isQObject ~classname ~is_abstract:false h_classes m
            );
            print_c "end\n";
            (* twin class *)
            List.iteri c.c_constrs ~f:(self#gen_constr ~is_tween:true ~classname h_constrs);
            print_c "and virtual %s me = object(self)\n" ocaml_classname;
            print_c "  method handler  : [`qobject] obj = me \n\n";
            print_c "  initializer print_endline \"initializing %s\";\n" ocaml_classname;
            print_c "              Qtstubs.set_caml_object me self\n";

            let (abstrs, normal) = MethSet.partition_tf c.c_meths ~f:(fun m -> List.mem m.m_modif `Abstract)
            in
            let normal = MethSet.filter normal ~f:(is_good_meth ~classname ~index) in
            MethSet.iter normal ~f:(fun m ->
              let stub ~isQObject m = self#gen_meth_stubs ~isQObject ~classname h_stubs m in
	          if (m.m_access=`Public) then stub false m;
              stub ~isQObject m;
              stub ~isQObject
	            {{m with m_out_name="call_super_"^m.m_out_name} with m_name="call_super_"^m.m_name};
              self#gen_meth ~isQObject ~is_abstract:false ~classname h_classes m
            );
            MethSet.iter abstrs ~f:(fun m ->
              print_c "(* %s*)\n" (string_of_meth m);
              let types = List.map m.m_args ~f:(self#toOcamlType_exn ~low_level:false) in
              let argnames = List.mapi m.m_args ~f:(fun i _ -> "x"^(string_of_int i)) in
              let (xs,ys) = self#get_params_helper types m.m_args argnames m.m_out_name in
              print_c "  method virtual %s: %s\n" m.m_out_name (String.concat ~sep:"->" types)

            );
        (*
            print_c "and %s me = object(self)\n" ocaml_classname;
            print_c "  inherit %s_pub_only me as super\n" ocaml_classname;

            MethSet.iter abstrs ~f:(fun m ->
	          self#gen_meth_stubs ~isQObject:true ~classname h_stubs m;

              print_c "(* almost_good abstract meth: %s*)\n%!" (string_of_meth m);
              let types = List.map m.m_args ~f:(self#toOcamlType_exn ~low_level:false) in
              let argnames = List.mapi m.m_args ~f:(fun i _ -> "x"^(string_of_int i)) in
              let (xs,ys) = self#get_params_helper types m.m_args argnames m.m_out_name in
              print_c "method %s %s =\n" m.m_out_name (String.concat ~sep:" " xs);
              print_c "  (match Qtstubs.get_class_name me with\n";
              print_c "   | Some \"%s_twin\" -> %s_twin_%s' self#handler %s\n" classname
                (ocaml_class_name classname) m.m_out_name (String.concat ~sep:" " ys);
              print_c "   | Some \"%s\" -> failwith(Printf.sprintf \"Can't call protected abstract meth\")\n"
                classname;
              print_c "   | ____ -> failwith \"Bug in logic\"\n";
              print_c "  )";
              self#postconvert m.m_res h_classes;

              print_c "\n"
            )
                            *)
        | (`SemiAbstract,false)
            (* We can override methods in non QObject class because we does not know where to store
             * OCaml object value *)
        | (`Abstract,_) ->
            printf "Totally abstract class %s\n%!" classname;
            (* this class will not have twin object because some methods are not translatable to OCaml*)
            (* i.e. it will be like non QObject class *)
            let isQObject = false in
            (* We will not mark it virtual, we just will not provide constructors to create it
             * This class can be a return type in some method, we will use ocaml keyword  `new`
             * to create it *)
            (*fprintf h_constrs "(* sort of class %s is `Abstract *)\n" classname;*)

            print_c " %s me = object(self)\n" ocaml_classname;
            print_c "   method handler : [`qobject] obj = me\n";
            MethSet.iter c.c_meths ~f:(fun m ->
              let is_abstract = List.mem m.m_modif `Abstract in
              if m.m_access = `Public && is_good_meth ~classname ~index m
              then (
                self#gen_meth_stubs ~isQObject ~classname h_stubs m;
                self#gen_meth ~is_abstract:false ~isQObject ~classname h_classes m
              )
            );
            (* TODO: slots *)
            (* TODO: signals *)

      end
(*
      let itermeth_nocheck ~isQObject m =
	    if m.m_access=`Protected && not isQObject then ()
	    else if not need_twin then begin
	      self#gen_meth_stubs ~isQObject ~classname h_stubs m;
          self#gen_meth ~isQObject ~is_abstract:false ~classname h_classes m
	    end else (
	    )
      in
      let itermeth ~isQObject m =
        if is_good_meth ~classname ~index m
        then itermeth_nocheck ~isQObject m
      in
      let iter_meth' = fun m ->
        if is_good_meth ~classname ~index m
        then itermeth_nocheck ~isQObject:need_twin m
        else if (is_almost_good_meth ~classname ~index m) && (List.mem m.m_modif `Abstract)
        then begin
          (* C++ code will be generated to call OCaml's code.
           * in OCaml side this code should be written by the human after sublcassing *)
          (* I really don't know what to generate there *)
	      self#gen_meth_stubs ~isQObject ~classname h_stubs m;
          self#gen_meth ~isQObject ~is_abstract:true ~classname h_classes m
        end
        else () (* Do nothing *)
      in
      MethSet.iter c.c_meths ~f:iter_meth';
      let public_slots = MethSet.filter c.c_slots ~f:(fun m -> m.m_access=`Public) in
      (* TODO: understand what to do with non-public slots *)
      MethSet.iter public_slots ~f:(itermeth ~isQObject:need_twin);
      MethSet.iter public_slots ~f:(fun slot ->
        if is_good_meth ~classname ~index slot then
          self#gen_slot ~classname h_classes slot
      );

      if is_abstract then (
        fprintf h_classes "end and %s_abstrhelper me = object(self)\n" ocaml_classname;
        fprintf h_classes "  method handler : [`qobject] obj = me \n\n";
        MethSet.iter c.c_meths ~f:(fun m ->
          if is_good_meth ~classname ~index m
          then itermeth_nocheck ~isQObject:false m
          else if (is_almost_good_meth ~classname ~index m) && (List.mem m.m_modif `Abstract)
          then begin

(*            self#gen_meth ~isQObject:true ~is_abstract:false ~classname h_classes m *)
          end
        );
        MethSet.iter public_slots ~f:(itermeth ~isQObject:false);
        List.iter c.c_sigs ~f:(self#gen_signal h_classes);
      )
        *)
    end

  method makefile dir =
    ignore (Sys.command ("touch " ^ dir ^/ ".depend"));
    let h = open_out (dir ^ "/Makefile") in
    fprintf h "ML_MODULES=stubs.cmo classes.cmo creators.cmo\n";
    fprintf h "ML_MODULES_OPT=stubs.cmx classes.cmx creators.cmx\n\n";
    fprintf h "OCAMLC=ocamlc.opt -g\nOCAMLOPT=ocamlopt.opt -g\n\n";
    fprintf h "INC=-I ./../../test_gen \n";
    fprintf h ".SUFFIXES: .ml .mli .cmi .cmx .cmo \n\n";
    fprintf h ".ml.cmo:\n\t$(OCAMLC)   $(INC) -c $<\n\n";
    fprintf h ".ml.cmx:\n\t$(OCAMLOPT) $(INC) -c $<\n\n";
    fprintf h ".mli.cmi:\n\t$(OCAMLC)  $(INC) -c $<\n\n";
    fprintf h "all: byte opt\n\n";
(*    fprintf h "depend:\n\tocamldep $(INC) *.ml *.mli > .depend\n\n";*)
    fprintf h "byte: $(ML_MODULES)\n\n";
    fprintf h "opt:  $(ML_MODULES_OPT)\n\n";
    fprintf h ".PHONY: all clean opt byte\n\n";
    fprintf h "include .depend\n\n";
    fprintf h "clean:\n\trm -f *.cm[iox] *.o\n\n";
    Out_channel.close h

end
