module Defaults = struct
  let default_threads_with ~max_domains ~affinity ~topology =
    max_domains + affinity + topology
end

let default_threads_with = Defaults.default_threads_with
