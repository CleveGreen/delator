type directive = { prefix : string; prefix_length : int; level : int }

let default_level = ref (Level.to_int Level.Info)
let directives = ref [||]

let level_of_int = function
  | 0 -> Level.Trace
  | 1 -> Level.Debug
  | 2 -> Level.Info
  | 3 -> Level.Warn
  | _ -> Level.Error

let set_default level =
  (* OCaml 5.2's bounded-data-race memory model guarantees no tearing for
     immediate values. A concurrent stale filter is harmless; callers that
     need immediate visibility set the level before spawning domains. *)
  default_level := Level.to_int level

let default () =
  (* As in [is_enabled], an immediate filter read may observe an older value
     under OCaml 5.2's bounded-data-race model, which is acceptable here. *)
  level_of_int !default_level

let rec prefix_equal value prefix prefix_length index =
  index = prefix_length
  || (String.unsafe_get value index = String.unsafe_get prefix index
     && prefix_equal value prefix prefix_length (index + 1))

let[@inline always] starts_with value value_length prefix prefix_length =
  value_length >= prefix_length
  && prefix_equal value prefix prefix_length 0

let rec select configured target target_length index best_length best_level =
  if index = Array.length configured then best_level
  else
    let directive = Array.unsafe_get configured index in
    if
      directive.prefix_length > best_length
      && starts_with target target_length directive.prefix directive.prefix_length
    then
      select configured target target_length (index + 1)
        directive.prefix_length directive.level
    else
      select configured target target_length (index + 1) best_length best_level

let[@inline always] target_level target =
  let configured = !directives in
  (* This is the same deliberately-racy immediate filter read documented by
     [is_enabled]; stale filtering is safe and integer reads cannot tear. *)
  let fallback = !default_level in
  match Array.length configured with
  | 0 -> fallback
  | 1 ->
      let directive = Array.unsafe_get configured 0 in
      if
        starts_with target (String.length target) directive.prefix
          directive.prefix_length
      then directive.level
      else fallback
  | _ ->
      select configured target (String.length target) 0 (-1) fallback

let[@inline always] is_enabled ~level ~target =
  (* This deliberately-racy immediate read has no tearing under OCaml 5.2's
     bounded-data-race memory model. Eventual propagation is sufficient for a
     runtime log filter, so observing an earlier configured level is safe. *)
  Level.to_int level >= target_level target

let parse_entry entry =
  match String.split_on_char '=' (String.trim entry) with
  | [ raw_level ] -> (
      match Level.of_string raw_level with
      | Ok level -> `Default level
      | Error message -> invalid_arg ("DELATOR_LOG: " ^ message))
  | [ prefix; raw_level ] -> (
      match Level.of_string raw_level with
      | Ok level ->
          let prefix = String.trim prefix in
          `Directive
            { prefix; prefix_length = String.length prefix;
              level = Level.to_int level }
      | Error message -> invalid_arg ("DELATOR_LOG: " ^ message))
  | _ -> invalid_arg (Printf.sprintf "DELATOR_LOG: invalid directive %S" entry)

let configure_from_env () =
  match Sys.getenv_opt "DELATOR_LOG" with
  | None | Some "" -> ()
  | Some specification ->
      let parsed =
        String.split_on_char ',' specification
        |> List.filter (fun entry -> String.trim entry <> "")
        |> List.map parse_entry
      in
      let configured = ref [] in
      List.iter
        (function
          | `Default level -> set_default level
          | `Directive directive -> configured := directive :: !configured)
        parsed;
      directives := Array.of_list (List.rev !configured)
