[@@@alert "-unsafe_multidomain"]
[@@@alert "-do_not_spawn_domains"]

let emit domain =
  Delator.in_span ~level:Info ~target:"multi" ~name:("domain" ^ string_of_int domain)
    (fun () ->
      for index = 1 to 50 do
        if Delator.Runtime.is_enabled ~level:Info ~target:"multi" then
          Delator.Runtime.event ~target:"multi" ~level:Info ~msg:"event"
            ~fields:[ ("domain", Delator.Field.int domain); ("index", Delator.Field.int index) ]
      done)

let () =
  let child =
    Domain.spawn (fun () ->
        emit 1;
        Delator.Runtime.event ~target:"multi" ~level:Info
          ~msg:"domain-exit-tail" ~fields:[])
  in
  emit 0;
  Domain.join child
