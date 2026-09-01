module type Value = sig
  val value : int
end

let read packed =
  let module Value = (val packed : Value) in
  Value.value

let packed =
  (module struct
    let value = 7
  end : Value)

let value = read packed
