open Ppxlib

let loc  = Location.none

let wrap_qml _ x = x


let%test _ =
  wrap_qml [%expr qml singleton ~name:"asdf" ]
    true

let%test _ =
  wrap_qml [%expr qml singleton]
    true

let%test _ =
  wrap_qml
    [%expr qml singleton
      ((author: string) Read getAuthor Write setAuthor Notify authorChanged)
      ((author: string) Read getAuthor Write setAuthor Notify authorChanged)
    ]
    true

let%test _ =
  wrap_qml
    [%expr qml singleton
      ((author: string) Read getAuthor Write setAuthor Notify authorChanged)
      ((author: string) Read getAuthor Write setAuthor Notify authorChanged)
    ]
    true


let%test _ =
  wrap_qml
    [%expr qml singleton
       ~name:"MyApi"
       ( (someProperty:int) Read somePropery Write setSomeProperty Notify somePropertyChanged)
    ]
    true
