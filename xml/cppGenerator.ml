open Core
open Core.Common
open Generators
open SuperIndex
open Parser

open Core.Std

module List = struct
  include List
  (** Like map2_exn but with index parameter *)
  let map2i_exn la lb ~f = 
    let i = ref 0 in
    map2_exn la lb ~f:(fun x y -> let ans = f !i x y in incr i; ans)
end

type comp_target = {
  comp_class : string;
  comp_twin : string option
}

class cppGenerator ~graph ~includes ~bin_prefix dir index = object (self)
  
  method get_name ~classname modif args ?res_n_name is_byte =
    cpp_stub_name ~classname modif args ?res_n_name ~is_byte

  method declare_locals argnames h = 
    let len = List.length argnames in
    if len = 0 then ()
    else if len > 5 then begin
      let l1,l2 = headl 5 argnames in
      fprintf h "  ::CAMLlocal5(%s);\n" (String.concat ~sep:"," l1);
      self#declare_locals l2 h
    end else begin
      fprintf h "  ::CAMLlocal%d(%s);\n" len (String.concat ~sep:"," argnames)
    end

  method declare_params h names =
    let len = List.length names in
    assert (len <= 10);
    if len>5 then (
      let l1,l2 = headl 5 names in
      fprintf h "  CAMLparam5(%s);\n"           (String.concat ~sep:"," l1);
      fprintf h "  CAMLxparam%d(%s);\n" (len-5) (String.concat ~sep:"," l2)
    ) else
      fprintf h "  CAMLparam%d(%s);\n" len (String.concat ~sep:"," names)

  method gen_stub ~prefix ~isQObject classname modif args ?res_n_name h =
    try
      let classname = if isQObject then classname^"_twin" else classname in
      let is_constr = Option.is_none res_n_name in
      let get_name is_byte = self#get_name ~classname modif args ?res_n_name is_byte in 
      let native_func_name = get_name false in
      let () = 
        let args_str = args |> List.map ~f:string_of_arg |> String.concat ~sep:"," in
        match res_n_name with
        | Some (res_type,name) -> 
          fprintf h "// method %s %s(%s)\n" (string_of_type res_type) name args_str
        | None ->
          fprintf h "// constructor %s(%s)\n" classname args_str
      in
      let argslen = List.length args + (if is_constr then 0 else 1) in
      if argslen > 10 then raise BreakSilent;

      let (arg_casts,argnames) =
        let argnames = List.mapi args ~f:(fun i _ -> sprintf "arg%d" i) in
        let arg_casts = List.map2_exn argnames args ~f:(fun name arg ->
          fromCamlCast index {arg with arg_type=unreference arg.arg_type} name
        ) in
        let arg_casts = List.map2_exn args arg_casts ~f:(fun arg -> function
          | Success s  -> s
          | CastError _ | CastValueType _
          | CastTemplate _  -> 
              (*printf "arg_cast failed: %s" (string_of_arg arg);*)
              raise BreakSilent
        ) in

        if not is_constr then begin
          let self_cast =
            sprintf "assert(Tag_val(self)==Abstract_tag);\n  %s *_self = ((%s*) Field(self,0));"
              classname classname in
          ( self_cast :: arg_casts, "self" :: argnames)
        end else
          (arg_casts, argnames)
      in
      assert (List.length argnames = (List.length arg_casts));
      let res_type = match res_n_name with
        | None -> ptrtype_of_classname classname
        | Some (res,_) -> res
      in
      let is_proc = res_type=void_type in
      let () = 
        if not is_proc then
          let res_arg = { arg_type = unreference res_type; arg_default=None; arg_name=None } in
          match fromCamlCast index res_arg "resname" with
            | Success _ -> ()
            | _ -> 
                (*print_endline "failed resCast"; *)
                raise BreakSilent
      in

      fprintf h "value %s(" native_func_name;
      List.map argnames ~f:(fun x -> "value "^x) |> String.concat ~sep:"," |> fprintf h "%s";
      fprintf h ") {\n";

      self#declare_params h argnames;
      if not is_proc then
        self#declare_locals ["_ans"] h;

      List.iter arg_casts ~f:(fun s -> fprintf h "  %s\n" s);
      let argsCall = 
        let lst = if not is_constr then (
          assert (List.length argnames >0);
          List.tl_exn argnames
        ) else argnames in
        List.map lst ~f:((^)"_") |> String.concat ~sep:", " in
      let full_classname = match prefix with
        | [] -> classname
        | lst -> sprintf "%s::%s" (String.concat ~sep:"::" lst) classname in
      if is_constr then (
        fprintf h "  %s* ans = new %s(%s);\n" full_classname full_classname argsCall;
        fprintf h "  printf (\"created new %s: %%p\\n\",ans);\n" full_classname;
        fprintf h "  _ans = caml_alloc_small(1, Abstract_tag);\n";
        fprintf h "  (*((%s **) &Field(_ans, 0))) = ans;\n" full_classname;
        fprintf h "  printf (\"abstract %s: %%ld\\n\", _ans);\n" full_classname;
        fprintf h "  CAMLreturn(_ans);\n"
      ) else begin
        let methname = match res_n_name with Some (_,x) -> x | None -> assert false in
        fprintf h "  printf(\"Calling method %s::%s of object %%p\\n\", _self);\n"
          full_classname methname;
        if is_proc then (
          fprintf h "  _self -> %s(%s);\n" methname argsCall;
          fprintf h "  CAMLreturn(Val_unit);\n"
        ) else begin
          let res_arg = simple_arg (unreference res_type) in
          let ans_type_str = match pattern index res_arg with
            | EnumPattern (e,key) -> snd key
            | _ -> string_of_type res_arg.arg_type
          in
          let resCast = match toCamlCast index res_arg "ans" "_ans" with
            | Success s -> s
            | CastError _ | CastValueType _ | CastTemplate _ ->
              print_endline "resCast failed";raise (BreakSilent)
          in
          fprintf h "  %s ans = _self -> %s(%s);\n" ans_type_str methname argsCall;
          fprintf h "  %s\n" resCast;
          fprintf h "  CAMLreturn(%s);\n"
              (match pattern index res_arg with
                | ObjectPattern -> sprintf " (ans) ? Val_some(%s) : Val_none" "_ans"
                | _ -> "_ans")
        end
      end;
      fprintf h "}\n";
      if argslen > 5 then begin
        (* additional stub for  bytecode function when args count > 5 *)
        let stub_name = get_name true in
        fprintf h "value %s(value *argv, int) {\n" stub_name;
        fprintf h "  return %s(\n    %s);\n" (get_name false)
          (List.init argslen ~f:(fun x -> x)
              |> List.map ~f:(fun i-> sprintf "argv[%d]" i)
              |> String.concat ~sep:",");
        fprintf h "}\n\n"
      end
      else fprintf h "\n"
    with
      | BreakSilent -> ()
      | BreakS str -> ( fprintf h "// %s\n" str; print_endline str )

  method makefile dir ~twins lst = 
    let lst = List.stable_sort ~cmp:String.compare lst in
    let h = open_out (dir ^/ "Makefile") in
    fprintf h "INCLUDES=-I./../../ -I. ";
    List.iter includes ~f:(fun s -> fprintf h " %s" s);
    fprintf h "\n";
    fprintf h "GCC=g++ -c -pipe -g -Wall -W $(INCLUDES) \n\n";
    let f tl x = x |> List.map ~f:(fun s -> sprintf "%s%s" s tl) |> String.concat ~sep:" " in
    fprintf h "HEADERS=%s\n" (f ".h" twins);
    fprintf h "HEADERS2=$(addprefix moc_,$(HEADERS))\n";
    fprintf h "MOC_EXE=%smoc\n" (if bin_prefix="" then "" else bin_prefix^"/");    
    fprintf h "MOCS=$(HEADERS2:.h=.o)\n";
    fprintf h "C_QTOBJS=%s %s\n\n" (f ".o" twins) (f ".o" lst);
    fprintf h ".SUFFIXES: .h .cpp .o\n\n";
    fprintf h "all: step1 step2\n\n";
    fprintf h "step2: $(MOCS)\n\n";
    fprintf h "step1: $(C_QTOBJS) $(HEADERS)\n\n";
    fprintf h "moc_%%.cpp: %%.h\n";
    fprintf h "\t\t$(MOC_EXE) $< > $@\n\n";
    fprintf h ".cpp.o:\n\t$(GCC) -c -I`ocamlc -where` -I.. $(COPTS) -fpic $<\n\n";
    fprintf h ".cpp.h:\n\tmoc $@ > moc_$<\n\n";
    fprintf h "clean: \n\trm -f *.o\n\n";
    fprintf h ".PHONY: all clean\n\n"; 
    Out_channel.close h

  method private prefix = dir ^/ "cpp"
  
  method is_abstr_class c =
    let ans = ref false in
    let f m = match m.m_modif with `Abstract -> ans:=true | _ -> () in
    MethSet.iter c.c_meths ~f;
    MethSet.iter c.c_slots ~f;
    !ans
 
  method gen_class ~prefix ~dir c =
    let key = NameKey.make_key ~name:c.c_name ~prefix in
    if not (SuperIndex.mem index key) then begin
      printf "Skipping class %s - its not in index\n" (NameKey.to_string key);
      None
    end else if skipClass ~prefix c then begin 
      printf "skipping class %s\n" c.c_name;
      None
    end else begin
      let classname = c.c_name in
      let subdir = String.concat ~sep:"/" (List.rev prefix) in
      let dir = dir ^/ subdir in
      ignore (Sys.command ("mkdir -p " ^ dir ));
      let isQObject = isQObject graph ~key in
      printf "Generating C++ for class %s: isQObject==%b\n%!" (NameKey.to_string key) isQObject;
      let (stubs_filename, twin_cppname, twin_hname) = 
        let d = dir ^/ classname in
        (d ^ ".cpp", d ^ "_twin.cpp", d^ "_twin.h") 
        (* I'll find some problems with adding include files for every class. 
           Maybe fix makefile to cut directory from cpp and add it to includes of gcc
        *)
        (* What each files will contain?
           stubs_filename = stubs for calling public method of a class 
                            (this class can be subclass or QObject or not)
           twin_hname     = header file for twin-class (class should be a descedant of QObject)
           twin_cppname   = source file for implementation twin-class's methods and for stubs for
                            calling this methods
         *)
      in
      let h = open_out stubs_filename in
      fprintf h "#include <QtWidgets/QtWidgets>\n";
      fprintf h "#include \"headers.h\"\n";
      fprintf h "extern \"C\" {\n";
      fprintf h "#include \"enum_headers.h\"\n";
      fprintf h "#define %s_val(v) ((%s *) Field(v,0))\n\n" classname classname;
      let isAbstract = self#is_abstr_class c in
      printf "class %s: is abstract=%b, is QObject=%b\n" c.c_name isAbstract isQObject;
      if isAbstract then
        fprintf h "//class has pure virtual members - no constructors\n"
      else begin
        let f args =
          let m = meth_of_constr ~classname args in
          (* in stubs file we generate stubs for calling native (not twin) implementation of methods.
             So we can suppose that all classes aren't descedants of QObject *)
          if (m.m_access = `Public) && (is_good_meth ~index ~classname m) then 
            self#gen_stub ~prefix ~isQObject:false classname m.m_access args ?res_n_name:None h
        in
        (* we don't publish protected constructors --- in OCaml we will not override them *)
        List.iter ~f c.c_constrs 
      end;
      
      let f m = 
        (* when class is descedant QObject we should generate stubs for protected meths,
           no QObject --- no stubs for proteced methods
        *)
        fprintf h "// %s, declared in `%s`, classname=`%s`\n" (string_of_meth m) m.m_declared classname;
        if (is_good_meth ~classname ~index m) && (m.m_declared = classname) && (m.m_access = `Public)
	      && (m.m_modif <> `Abstract) then
          self#gen_stub ~prefix ~isQObject:false classname m.m_access m.m_args 
            ?res_n_name:(Some (m.m_res, m.m_name)) h
      in
      MethSet.iter ~f c.c_meths;
      MethSet.iter ~f c.c_slots; 

      fprintf h "}  // extern \"C\"\n";
      Out_channel.close h;
      let need_twin = does_need_twin ~isQObject classname ~index in
      let ans =
        let comp_class = (if subdir = "" then classname else subdir ^/ classname) in
        let comp_twin = if need_twin then Some (comp_class ^ "_twin") else None in
        { comp_class; comp_twin }
      in
      (* Now let's write twin class *)
      let () = if need_twin then (
        let h = open_out twin_cppname in
        self#gen_twin_source ~prefix c h;
        Out_channel.close h;
        let h = open_out  twin_hname in
        self#gen_twin_header ~prefix c h;
        Out_channel.close h
      ) in
      Some ans
    end
  
  method twin_methname cpp_methname modif = match modif with
    | `Private -> raise (Bug "we don't generate private methods in twin header")
    | `Public -> cpp_methname
    | `Protected -> sprintf "prot_%s" cpp_methname

  method gen_twin_source ~prefix c h = 
    let classname  = c.c_name in
    fprintf h "#include <QtWidgets/QtWidgets>\n"; (* TODO: include QtGui, OpenGL is not needed *)
    fprintf h "#include \"headers.h\"\n";
    fprintf h "#include \"enum_headers.h\"\n";
    fprintf h "#include <stdio.h>\n\n";
    fprintf h "#include \"%s_twin.h\"\n" classname;
    fprintf h "extern \"C\" {\n";

    List.iter c.c_constrs ~f:(fun args -> (* TODO: maybe use gen_stubs value? *)
      let classname = c.c_name ^ "_twin" in
      let native_func_name = self#get_name ~classname `Public args ?res_n_name:None false in
      fprintf h "value %s" native_func_name;
      let caml_argnames = self#gen_stub_arguments ~locals:["ans"] args h in (* they has type `value` *)
      let cpp_argnames = List.map caml_argnames ~f:(fun s -> "_"^s) in
      fprintf h "  %s *_ans = new %s(%s);\n" classname classname (String.concat ~sep:"," cpp_argnames);
      fprintf h "  setAbstrClass<%s>(ans,_ans);\n" classname;
      fprintf h "  printf(\"created %s = %%p, asbtr = %%ld\\n\",_ans,ans);\n" classname;
      fprintf h "  CAMLreturn(ans);\n}\n\n"
    );
    let new_meths =
      MethSet.fold c.c_meths ~init:MethSet.empty ~f:(fun a m -> match m.m_access with
        | `Public -> MethSet.add a m
        | `Private -> a
        | `Protected -> MethSet.add a {m with m_access=`Public} ) in
    (* Also we have to generate caller stubs for meths of twin object *)
    let iter_meth m =
      (*printf "generating a meth %s\n" (string_of_meth m);*)
      let f name =
        self#gen_stub ~prefix ~isQObject:true classname m.m_access
          m.m_args ?res_n_name:(Some (m.m_res, (name) )) h in
      f m.m_name;
      if m.m_modif <> `Abstract then
	    f ("call_super_"^m.m_name)
    in
    let public_slots = MethSet.filter c.c_slots ~f:(fun s -> s.m_access=`Public) in
    MethSet.iter new_meths ~f:iter_meth;
    MethSet.iter public_slots ~f:iter_meth; 
    fprintf h "\n}\n";
    ()

  method gen_twin_header ~prefix c h = 
    let classname  = c.c_name in
    fprintf h "#include <QtWidgets/QtWidgets>\n"; (* TODO: include QtGui, OpenGL is not needed *)
    fprintf h "#include \"headers.h\"\n";
    fprintf h "#include \"enum_headers.h\"\n";
    fprintf h "#include <stdio.h>\n\n";
    fprintf h "#define  %s_twin_val(v) ((%s_twin*) Field(v,0))\n\n" classname classname;
    fprintf h "class %s_twin : public %s {\nQ_OBJECT\npublic:\n" classname classname;
    fprintf h "  virtual ~%s_twin() {}\n" classname;
(*
    let () = 
      MethSet.to_list c.c_meths |> List.map ~f:(fun x -> x.m_out_name) |> String.concat ~sep:","
    |> (fun s -> printf "meths of class %s: %s\n" classname s)
    in *)
    let (pub_meths,prot_meths) = 
      MethSet.fold c.c_meths ~init:(MethSet.empty, MethSet.empty) ~f:(fun (a,b) m -> match m.m_access with
        | `Public -> (MethSet.add a m,b)
        | `Private -> (a,b)
        | `Protected -> (a,MethSet.add b m) ) in
    let (++) = MethSet.union in
    let new_meths = pub_meths ++ (MethSet.map prot_meths ~f:(fun m -> {m with m_access=`Public})) 
      ++ (MethSet.filter c.c_slots ~f:(fun s -> s.m_access=`Public)) in
    MethSet.elements new_meths |> List.find ~f:(fun m -> m.m_out_name ="paintEvent") 
    |> (function Some m ->
      (m |> sexp_of_meth |> Sexplib.Sexp.to_string_hum |> print_endline );
      printf "`is_good_meth = %b\n" (is_good_meth ~index ~classname m)
	   | None -> ());
    let new_meths = MethSet.map new_meths ~f:(fun m -> {m with m_modif = `Normal}) in
    MethSet.filter new_meths ~f:(is_good_meth ~index ~classname) |> MethSet.iter ~f:(fun m ->
(*      printf "generating twin for %s\n" (string_of_meth m); flush stdout; *)
      fprintf h "//%s declared in %s\n" (string_of_meth m) m.m_declared;
      let twin_methname = self#twin_methname m.m_name m.m_access in
      fprintf h "%s %s(" (string_of_type m.m_res) twin_methname;

      let argslen = List.length m.m_args in
      let argnames = List.mapi m.m_args ~f:(fun i _ -> sprintf "arg%d" i) in
      let argsstr = List.map2_exn argnames m.m_args ~f:(fun name arg ->
        string_of_arg {arg with arg_name=Some name}) |> String.concat ~sep:", " in
      fprintf h "%s) {\n" argsstr;
      fprintf h "  CAMLparam0();\n";
      if not (is_void_type m.m_res) then 
        self#declare_locals ["camlobj";"_ans";"meth"] h
      else
        self#declare_locals ["camlobj";"meth"] h;
      fprintf h "  printf(\"Calling %s::%s of object = %%p\\n\",this);\n" classname m.m_name;
      fprintf h "  GET_CAML_OBJECT(this,the_caml_object)\n";
      fprintf h "  camlobj = (::value) the_caml_object;\n";
      fprintf h "  meth = caml_get_public_method( camlobj, caml_hash_variant(\"%s\"));\n" m.m_out_name;
      fprintf h "  assert(meth!=0);\n";

      let call_closure_str = match argslen with 
        | 0 -> "caml_callback(meth, camlobj);"
        | _ -> begin
          (* TODO: use CAMLlocalN macros *)
          fprintf h "  ::value *args = new ::value[%d]; // TODO: use CAMLlocalN macros\n" (argslen+1);
          fprintf h "  args[0] = camlobj;\n";
          let arg_casts = List.map2i_exn m.m_args argnames ~f:(fun i arg name ->
            let arg_name = sprintf "args[%d]" (i+1) in
            let s = toCamlCast index arg name arg_name in
            match (s,pattern index arg) with
              | (CastError _,_) | (CastValueType _,_) | (CastTemplate _,_) -> s
              | (Success s,ObjectPattern) ->
                let cp = Parser.string_split ~on:"::" arg.arg_type.t_name in
                let buf = Bigbuffer.create 50 in
                let open Bigbuffer.Printf in
                bprintf buf "  { setAbstrClass<%s %s>(%s,%s);\n" 
                  (if arg.arg_type.t_is_const then "const" else "") arg.arg_type.t_name arg_name name;
                bprintf buf "    ::value *call_helper=caml_named_value(\"make_%s\");\n"
                  (String.concat ~sep:"." cp);
                bprintf buf "    assert(call_helper != 0);\n";
                bprintf buf "    %s=caml_callback(*call_helper,%s); }\n" arg_name arg_name;
                Success (Bigbuffer.contents buf)
              | (Success _,_) -> s
          ) |> List.map ~f:(function Success s -> s | _ -> assert false) in
          List.iter arg_casts  ~f:(fun s -> fprintf h "  %s;\n" s);
          fprintf h "    // delete args or not?\n";
          sprintf "caml_callbackN(meth, %d, args);" (argslen+1)
        end
      in
      let isProcedure = is_void_type m.m_res in
      if isProcedure then begin
        fprintf h "  %s;\n" call_closure_str;
        fprintf h "  CAMLreturn0;\n"

      end else begin
        let res = { arg_type=m.m_res; arg_name=None; arg_default=None} in
        fprintf h "  _ans = %s;\n" call_closure_str;
        let cast = fromCamlCast index res ~cpp_argname:(Some "ans") "_ans" 
          |> (function Success s -> s | _ -> assert false) in
        fprintf h "  %s;\n" cast;
        fprintf h "  CAMLreturnT(%s,ans);\n" (string_of_type res.arg_type);
      end;
      fprintf h "}\n";
      fprintf h "%s call_super_%s(%s) {\n" (string_of_type m.m_res) twin_methname argsstr;
      fprintf h "  %s %s::%s(%s);\n}\n" (if isProcedure then "" else "return")
        classname twin_methname (String.concat ~sep:"," argnames);
    );
    (* Now generate constructors*)
    List.iter c.c_constrs ~f:(fun lst ->
      let argnames = List.mapi lst ~f:(fun i _ -> sprintf "x%d" i) in
      fprintf h "  %s_twin(%s) : %s(%s) {}\n" 
        classname
        (List.map2_exn argnames lst  ~f:(fun name arg ->
          string_of_arg {arg with arg_name = Some name})
            |> String.concat ~sep:",")
        classname
        (String.concat ~sep:"," argnames)
    );
    fprintf h "};\n\n";
    ()

  method gen_stub_arguments ?locals:(locals=[]) args h =
    let argnames = List.mapi args ~f:(fun i _ -> sprintf "arg%d" i) in
    fprintf h "(%s) {\n" (String.concat ~sep:"," (List.map argnames ~f:(fun s -> "value "^s)));
    self#declare_params h argnames;
    let arg_casts = List.map2_exn argnames args ~f:(fun name ({arg_type=t;arg_default=default;_} as arg) ->
      fromCamlCast index {arg with arg_type=unreference t} name
    ) in
    let arg_casts = List.map arg_casts ~f:(function Success s -> s | _ -> assert false) in
    self#declare_locals locals h;
    List.iter arg_casts ~f:(fun s -> fprintf h "  %s\n" s);
    argnames
  
  method gen_enum_in_ns ~key ~dir:string {e_name;e_items;e_access;e_flag_name} = 
    let (_: index_data SuperIndex.key_) = key in
    let _f () = 
      let (x:index_data) = SuperIndex.find_exn index key in
      ignore x
    in
    if not (SuperIndex.mem index key) then None
    else if not (is_public e_access) then None
    else begin
      let prefix = fst key |> List.tl_exn in
      let subdir = (String.concat ~sep:"/" (List.rev prefix)) in
      let dir = self#prefix ^/ subdir in
      ignore (Sys.command ("mkdir -p " ^ dir ));
      let filename = String.concat ~sep:"_" ("enum" :: (fst key|> List.rev)) in
      let h = open_out (dir ^/ filename ^".cpp") in
      fprintf h "//enum %s %s\n\n" e_name (List.to_string (fun x -> x) e_items);

      fprintf h "#include <QtWidgets/QtWidgets>\n";
      fprintf h "#pragma GCC diagnostic ignored \"-Wswitch\"\n";
      fprintf h "#include \"headers.h\"\nextern \"C\" {\n";
      let (fname1,fname2) = enum_conv_func_names key in
      let s =  List.tl_exn (fst key) |> List.rev |> String.concat ~sep:"::" in
      fprintf h "%s %s(value v) {\n" (snd key) fname1;

      List.iter e_items ~f:(fun e ->
        fprintf h "  if (v==caml_hash_variant(\"%s\")) return %s::%s;\n" e s e
      );
      fprintf h "  printf(\"%s\");\n" "if u see this line, the thereis a bug in enum generation";
      fprintf h "  return %s::%s;\n" s (List.hd_exn e_items);
      fprintf h "}\n\n";

      fprintf h "value %s(%s e) {\n" fname2 (snd key);
      fprintf h "  switch (e) {\n";

      List.iter e_items ~f:(fun e ->
        fprintf h "    case %s::%s: return hash_variant(\"%s\");\n" s e e
      );
      fprintf h "  }\n";
      fprintf h "  printf(\"%s\");\n" "if u see this line, the thereis a bug in enum generation";
      fprintf h "  return %s::%s;\n" s (List.hd_exn e_items);
      fprintf h "\n}\n\n}\n";
      Out_channel.close h;
      Some (if subdir = "" then filename else subdir ^/ filename)
    end

  method generate_q (q: NameKey.t Queue.t) =
    ignore (Sys.command ("rm -rf " ^ self#prefix ^"/*") );
    let classes = ref [] in
    let twin_classes = ref [] in
    let enums = ref [] in
    Queue.iter q ~f:(fun key -> 
      match SuperIndex.find_exn index key with
      | Enum e -> begin
        match self#gen_enum_in_ns ~key ~dir:(self#prefix) e with
          | Some s -> Ref.replace enums (fun lst -> (key,s)::lst)
          | None -> ()
      end
      | Class (c,_) ->  begin 
        match self#gen_class ~prefix:(fst key |> List.tl_exn) ~dir:(self#prefix) c with
          | Some {comp_class;comp_twin} ->
            classes := comp_class :: !classes;
            (match comp_twin with None -> () | Some x -> twin_classes := x :: !twin_classes)
          | None -> ()
      end    
    );
    print_endline "Generating makefile";
    let classnames = !classes in
    self#makefile self#prefix ~twins: (!twin_classes) (List.map ~f:snd !enums @ classnames );
    let enums_h = open_out (self#prefix ^/ "enum_headers.h") in
    List.iter !enums ~f:(fun ((_,fullname) as key,_) ->
      let (fname1,fname2) = enum_conv_func_names key in
      fprintf enums_h "extern \"C\" %s %s(value);\n" fullname fname1;
      fprintf enums_h "extern \"C\" value %s(%s);\n\n" fname2 fullname
    );
    Out_channel.close enums_h

end
