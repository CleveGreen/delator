type packet = { metadata : int [@log_value.trace] }

let read packet = packet.metadata
