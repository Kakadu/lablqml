type +'a obj

external create_app : string array -> [`qobject] obj = "ml_qapp_create"
external exec : unit -> int = "ml_qapp_exec"

