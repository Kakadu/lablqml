open Format
open Base
open Stdio
module C = Configurator.V1

let write_sexp fn sexp = Out_channel.write_all fn ~data:(Sexp.to_string sexp)
let namespace = "com.mycompany.qmlcomponents"

let () =
  C.main ~name:"mylib" (fun c ->
      write_sexp "my_namespace.sexp" (sexp_of_string namespace);
      write_sexp
        "import_name_my_namespace.sexp"
        (sexp_of_string @@ sprintf "--import-name=%s" namespace))
;;
