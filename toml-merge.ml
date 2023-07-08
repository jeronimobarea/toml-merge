#!/usr/bin/env ocaml

let toml_ext = ".toml"

let dir_is_empty dir =
  Array.length (Sys.readdir dir) = 0

let list_files_by_extension dir ext =
  match dir_is_empty dir with
  | true  -> []
  | false ->
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.extension f = ext)
    |> List.map (fun f -> Printf.sprintf "%s%s" dir f)

let read_file f =
  Printf.printf "reading file: %s" f; print_newline ();
  let ic = open_in f in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content

let create_file_with_extension content dir name ext =
  let f = Printf.sprintf "%s%s%s" dir name ext in
  let oc = open_out f in
  output_string oc content;
  Printf.printf "saving into file: %s" f; print_newline ();
  close_out oc

let merge dir_in dir_out filename =
  match list_files_by_extension dir_in toml_ext with
    | [] -> print_endline "No files found"
    | files ->
      files
      |> List.map read_file
      |> String.concat "\n"
      |> Printf.sprintf "# FILE GENERATED USING toml-merge: github.com/jeronimobarea/toml-merge\n%s"
      |> (fun c -> create_file_with_extension c dir_out filename toml_ext)

let dir_in = ref ""
let dir_out = ref ""
let output_file = ref ""

let speclist =
  [
    ("-d", Arg.Set_string dir_in, "Directory to fetch the files from. Default: current");
    ("-o", Arg.Set_string dir_out, "Directory to save the merged file. Default: current");
    ("-n", Arg.Set_string output_file, "Filename of the merged file. Default: config");
  ]

let usage_msg = "toml-merge [-d] <./input_dir> [-o] <./output_dir> [-n] <filename>"
 
let () = 
  Arg.parse speclist (fun _ -> ()) usage_msg;
  if !dir_in = "" then
    dir_in := "./";

  if !dir_out = "" then
    dir_out := "./";

  if !output_file = "" then
    output_file := "config";

  merge !dir_in !dir_out !output_file
