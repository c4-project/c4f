let print_bool = Stdio.Out_channel.printf "%b"

let prn (type a) (pp : a Fmt.t) : a -> unit = Fmt.pr "%a@." pp
