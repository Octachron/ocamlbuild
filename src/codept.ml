open Tags.Operators
open Command
module U = Ocaml_utils
module T = Tools
module Outcome = My_std.Outcome

let codept' mode tags =
  let tags' = tags++"ocaml"++"ocamldep" ++ "codept" in
    S [ A "codept"; T tags'; U.ocaml_ppflags (tags++"pp:dep"); mode]

let codept mode arg out env _build =
  let arg = env arg and out = env out in
  let tags = T.tags_of_pathname arg in
  Cmd(S[codept' mode tags; P arg; Sh ">"; Px out])


let codept_dep mode arg deps out env build =
  let arg = env arg and out = env out and deps = env deps in
  let tags = T.tags_of_pathname arg in
  let approx_deps = U.string_list_of_file deps in
  let include_dirs = Pathname.(include_dirs_of @@ dirname arg ) in
  let sigs = List.map (fun m -> U.expand_module include_dirs m ["sig"])
      approx_deps in
  let outsigs = build sigs in
  let sigs =
    List.map Outcome.good
    @@ List.filter Outcome.(function Good _ -> true | Bad _ -> false )
    @@ outsigs in
  Cmd( S[ codept' mode tags; P arg; Command.atomize_paths sigs;
          Sh ">"; Px out])
