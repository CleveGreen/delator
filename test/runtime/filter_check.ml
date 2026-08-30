let expect name expected actual =
  if Bool.equal expected actual |> not then failwith ("unexpected filter result: " ^ name)

let () =
  expect "parser info" true
    (Delator.Runtime.is_enabled ~level:Info ~target:"parser");
  expect "parser debug" false
    (Delator.Runtime.is_enabled ~level:Debug ~target:"parser");
  expect "special trace" true
    (Delator.Runtime.is_enabled ~level:Trace ~target:"lowering.special");
  expect "other trace" false
    (Delator.Runtime.is_enabled ~level:Trace ~target:"lowering.other");
  expect "other debug" true
    (Delator.Runtime.is_enabled ~level:Debug ~target:"lowering.other")
