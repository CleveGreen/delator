let consume ~metadata:_ () = ()
let value = 1

type record_payload = {
  stable : int;
  metadata : int;
}
type variant_payload = Payload of int * int
