type t = {
  id : int;
  parent : int option;
  name : string;
  target : string;
  level : Level.t;
  fields : Field.t list;
  created_on : Domain.id;
  mutable started_at : int64 option;
}

let next_id = Atomic.make 1
let stack = Domain.DLS.new_key (fun () -> [])

let[@inline always] current_id () =
  match Domain.DLS.get stack with span :: _ -> Some span.id | [] -> None

let depth () = List.length (Domain.DLS.get stack)

let create ~name ~target ~level fields =
  (* Uniqueness across domains requires an atomic read-modify-write. The SC
     ordering of [fetch_and_add] adds no extra cost over the RMW on amd64. *)
  let id = Atomic.fetch_and_add next_id 1 in
  { id; parent = current_id (); name; target; level; fields;
    created_on = Domain.self (); started_at = None }

let id span = span.id
let parent span = span.parent
let name span = span.name
let target span = span.target
let level span = span.level
let fields span = span.fields

let check_domain span =
  if span.created_on <> Domain.self () then
    invalid_arg "Delator span entered or exited on a different domain"

let enter span =
  check_domain span;
  match span.started_at with
  | Some _ -> invalid_arg "Delator span entered more than once"
  | None ->
      span.started_at <- Some (Clock.now_ns ());
      Domain.DLS.set stack (span :: Domain.DLS.get stack)

let exit span =
  check_domain span;
  match (span.started_at, Domain.DLS.get stack) with
  | Some started_at, current :: rest when current.id = span.id ->
      Domain.DLS.set stack rest;
      span.started_at <- None;
      Int64.sub (Clock.now_ns ()) started_at
  | None, _ -> invalid_arg "Delator span exited before it was entered"
  | Some _, _ -> invalid_arg "Delator spans must exit in stack order"
