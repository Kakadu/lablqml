open OUnit2

let test_ppx_getenv _ =
  (* set in myocamlbuild.ml *)
  assert_equal "42" [%getenv "PPX_GETENV_CHECK"]

let suite = "Test ppx_getenv" >::: [
    "test_ppx_getenv" >:: test_ppx_getenv;
  ]

let _ =
  run_test_tt_main suite
