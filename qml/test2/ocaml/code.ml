let gamemap = object
  val mutable title = "mytitle"
  method title () = title
  method setTitle s = title <- s
  val mutable width = 640
  method width () = width
  method setWidth s = width <- s
 
end

let () = Callback.register "prop_Gamemap_title_get_string" gamemap#title
let () = Callback.register "prop_Gamemap_title_set_string" gamemap#setTitle

let () = Callback.register "prop_Gamemap_width_get_int" gamemap#width
let () = Callback.register "prop_Gamemap_width_set_int" gamemap#setWidth


