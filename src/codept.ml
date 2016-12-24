open Tags.Operators
open Command
module U = Ocaml_utils
module T = Tools
module Outcome = My_std.Outcome

let is_pflag_included root s =
  let predicate t =
    match String.split_on_char '(' t with
    | a :: _ -> a = root
    | _ -> false in
  List.exists predicate @@ Tags.elements s

let codept' ?(approx=true) mode tags =
  let tags' = tags++"ocaml"++"ocamldep" ++ "codept" in
  let tags' = let open Tags in
    (* when computing dependencies for packed modules, cyclic alias
       references within the packed modules becomes problematic when
       packing. To avoid creating packed cmo/cmx with invalid elements,
       we disallow the simultaneous use of no_alias_deps and for-pack(…). *)
    if is_pflag_included "for-pack" tags' then
      tags' -- "no_alias_deps"
    else
      tags' in
  let k = if approx then S [ A"-k"] else S [] in
  S [ A "codept";k; T tags'; U.ocaml_ppflags (tags++"pp:dep"); mode]

let codept ?(approx=true) mode arg out env _build =
  let arg = env arg and out = env out in
  let tags = T.tags_of_pathname arg in
  Cmd(S[codept' ~approx mode tags; P arg; Sh ">"; Px out])


let codept_dep ?(approx=false) mode arg deps out env build =
  let arg = env arg and out = env out and deps = env deps in
  let tags = T.tags_of_pathname arg in
  let approx_deps = U.string_list_of_file deps in
  (** eliminate self-dependency from approximated dependencies *)
  let approx_deps =
      List.filter (fun x -> x <> U.module_name_of_pathname arg) approx_deps in
  let include_dirs = Pathname.(include_dirs_of @@ dirname arg ) in
  let sigs = List.map (fun m -> U.expand_module include_dirs m ["sig"])
      approx_deps in
  (** use codept "-k" to not fail on apparent self-reference *)
  let outsigs = build sigs in
  let sigs =
    List.map Outcome.good
    @@ List.filter Outcome.(function Good _ -> true | Bad _ -> false )
    @@ outsigs in
  Cmd( S[ codept' ~approx mode tags; P arg; Command.atomize_paths sigs;
          Sh ">"; Px out])
