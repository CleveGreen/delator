type packet = { metadata : int [@log_value.trace] }

let make value = { metadata = (value [@log_value.debug]) }
