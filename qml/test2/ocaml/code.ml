let gamemap = object
  val mutable title = "mytitle"
  method title () = title
  method setTitle s = title <- s
  val mutable width = 640
  method width () = width
  method setWidth s = width <- s
 
end

let () = Callback.register "prop_Gamemap_title_get" gamemap#title
let () = Callback.register "prop_Gamemap_title_set" gamemap#setTitle

let () = Callback.register "prop_Gamemap_width_get" gamemap#width
let () = Callback.register "prop_Gamemap_width_set" gamemap#setWidth


