open Tags.Operators
open Command
module U = Ocaml_utils
module T = Tools
module Outcome = My_std.Outcome

let o = A "-o"

let is_pflag_included root s =
  let predicate t =
    match String.split_on_char '(' t with
    | a :: _ -> a = root
    | _ -> false in
  List.exists predicate @@ Tags.elements s

let codept' port ?(approx=true) mode tags =
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
  let k = if approx then S [ A"-k"; A "-silent-fault-level"; A "warning" ]
    else S [] in
  S [  A "codept-client"; A "-port"; A port; k; T tags';
       U.ocaml_ppflags (tags++"pp:dep"); mode]

let codept port ?(approx=true) arg outs env _build =
  let arg = env arg in
  let tags = T.tags_of_pathname arg in
  let out = env @@ snd @@ List.hd outs in
  Configuration.tag_file out (Tags.elements tags);
  let outs = List.map (fun (mode, name) -> S [o; Px (env name); mode ] ) outs in
  Cmd(S[codept' port ~approx N tags; P arg; S outs])


let codept_dep port ?(approx=false) arg deps outs env build =
  let arg = env arg and deps = env deps in
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
  let outs = List.map (fun (mode, name) -> S [o; Px (env name); mode ] ) outs in
  Cmd( S[ codept' port ~approx N tags; P arg; Command.atomize_paths sigs;  S outs])
