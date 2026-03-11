let parse_stream stream =
  let parsable = Procq.Parsable.make stream in
  Parser.parse_all_ast parsable

let parse_file (filepath : string) =
  let in_chan = open_in filepath in
  parse_stream @@ Gramlib.Stream.of_channel in_chan

let parse_str (code: string) =
  parse_stream @@ Gramlib.Stream.of_string code
