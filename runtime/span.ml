(* The stack and every mutation of it belong to the current domain. Low-level
   spans also reject entry or exit from a domain other than their creator. *)
[@@@alert "-unsafe_multidomain"]

type local_state = {
  mutable ids : int array;
  mutable samples : Clock.sample array;
  mutable depth : int;
  mutable next_id : int;
  mutable id_limit : int;
  owner : Domain.id;
}

type context = local_state

type t = {
  id : int;
  mutable parent_state : int;
  local : local_state;
}

let initial_stack_capacity = 16
let id_block_size = 4_096
let lifecycle_mask = 3
let fresh = 0
let active = 1
let finished = 2
let maximum_span_id = max_int lsr 2

(* IDs need global uniqueness but not a global order. Reserving a block keeps
   the Atomic RMW, and therefore its required SC uniqueness guarantee, off the
   per-span path. Each block is consumed by one domain only. *)
let next_id_block = Atomic.make 1

let make_local () =
  { ids = Array.make initial_stack_capacity 0;
    samples = Array.make initial_stack_capacity Clock.zero_sample;
    depth = 0;
    next_id = 0;
    id_limit = 0;
    owner = Domain.self () }

let local = Domain.DLS.new_key make_local

let fresh_id state =
  if state.next_id = state.id_limit then begin
    let first = Atomic.fetch_and_add next_id_block id_block_size in
    if first <= 0 || first > maximum_span_id - id_block_size then
      failwith "Delator exhausted the span ID space";
    state.next_id <- first;
    state.id_limit <- first + id_block_size
  end;
  let id = state.next_id in
  state.next_id <- id + 1;
  id

let ensure_stack_capacity state =
  if state.depth = Array.length state.ids then begin
    let capacity = state.depth * 2 in
    let ids = Array.make capacity 0 in
    let samples = Array.make capacity Clock.zero_sample in
    Array.blit state.ids 0 ids 0 state.depth;
    Array.blit state.samples 0 samples 0 state.depth;
    state.ids <- ids;
    state.samples <- samples
  end

let current_context () = Domain.DLS.get local

let[@inline always] current_id_in state =
  if state.depth = 0 then None else Some state.ids.(state.depth - 1)

let[@inline always] current_id () = current_id_in (current_context ())

let depth () = (Domain.DLS.get local).depth

let push state id =
  ensure_stack_capacity state;
  let depth = state.depth in
  state.ids.(depth) <- id;
  if Clock.is_enabled () then state.samples.(depth) <- Clock.sample ();
  state.depth <- depth + 1

let pop state id =
  let depth = state.depth in
  if depth = 0 || state.ids.(depth - 1) <> id then
    invalid_arg "Delator spans must exit in stack order";
  state.depth <- depth - 1;
  if Clock.is_enabled () then Clock.elapsed_ns state.samples.(depth - 1) else 0L

let create () =
  let local = Domain.DLS.get local in
  let parent_id =
    if local.depth = 0 then 0 else local.ids.(local.depth - 1)
  in
  { id = fresh_id local;
    (* The low two bits are lifecycle state; the remaining bits retain the
       parent ID so [parent] stays valid after exit without another field. *)
    parent_state = parent_id lsl 2;
    local }

let id span = span.id

let parent span =
  let parent_id = span.parent_state lsr 2 in
  if parent_id = 0 then None else Some parent_id

let check_domain span =
  if span.local.owner <> Domain.self () then
    invalid_arg "Delator span entered or exited on a different domain"

let enter span =
  check_domain span;
  if span.parent_state land lifecycle_mask = fresh then begin
    push span.local span.id;
    span.parent_state <- span.parent_state lor active
  end else
    invalid_arg "Delator span entered more than once"

let exit span =
  check_domain span;
  match span.parent_state land lifecycle_mask with
  | state when state = active ->
    let elapsed = pop span.local span.id in
    span.parent_state <- (span.parent_state land lnot lifecycle_mask) lor finished;
    elapsed
  | state when state = fresh ->
      invalid_arg "Delator span exited before it was entered"
  | _ -> invalid_arg "Delator span exited more than once"

(* These are used by [Delator.in_span]. They retain the frame in reusable
   domain-local arrays instead of allocating the low-level lifecycle object. *)
let start state = fresh_id state

let enter_started state id = push state id

let finish id =
  let state = Domain.DLS.get local in
  pop state id
