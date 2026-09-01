module F (Input : sig
  val value : int
end) = struct
  let value = Input.value
end
