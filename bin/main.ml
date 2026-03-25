open Rocqconv

let () =
  let in_file = try Sys.argv.(1) with _ -> "/tmp/query.v" in
  let ast = parse_file in_file in

  let out_file = try Sys.argv.(2) with _ -> "/tmp/axioms.ml" in

  print_endline "Vernac AST:\n";
  List.iter (fun x -> print_endline @@ string_of_vernac_ast x) ast;

  print_endline "\nAxioms:\n";
  let axs = String.concat "\n" @@ axioms_of_parsed ast in
  print_endline axs;

  let oc = open_out out_file in
  output_string oc axs;
  close_out oc
