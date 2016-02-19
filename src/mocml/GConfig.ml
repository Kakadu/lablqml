open Core_kernel.Std

module MethOptions = struct
  type t =
    [ `PrintMethCalls
    | `DebugBlockingSections
    | `Ext of [`C | `CPP ]
    | `AbstractItemModel of string option ] list

end

module TypeOptions = struct
  type t = [`AbstractItemModel of string option ] list
  let of_meth_options (config: MethOptions.t) : t =
    List.fold_left ~init:[] config ~f:(fun acc -> function
    | `Ext _
    | `PrintMethCalls
    | `DebugBlockingSections -> acc
    | `AbstractItemModel _ as x -> x::acc
    )
end
