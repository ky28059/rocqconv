open Rocqconv

let () =
  let file = try Sys.argv.(1) with _ -> "/tmp/query.v" in
  let ast = parse_file file in

  print_endline "Vernac AST:\n";
  List.iter (fun x -> print_endline @@ string_of_vernac_ast x) ast;
