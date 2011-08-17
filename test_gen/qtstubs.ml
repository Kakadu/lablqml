type 'a obj

let (|>) x f = f x 


external make_root_widget : unit -> [`qobject] obj = "make_root_widget"



