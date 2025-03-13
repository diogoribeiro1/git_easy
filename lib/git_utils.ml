(* git_utils.ml *)

let exec_command_in_repo repo_dir cmd =
  let full_cmd = Printf.sprintf "cd %s && %s" repo_dir cmd in
  let ic = Unix.open_process_in full_cmd in
  let rec read_lines acc =
    match input_line ic with
    | line -> read_lines (line :: acc)
    | exception End_of_file ->
        close_in ic;
        List.rev acc
  in
  try
    let lines = read_lines [] in
    (* Depuração: exibir o comando e a saída *)
    Printf.eprintf "Comando: %s\nSaída: %s\n" full_cmd (String.concat "; " lines);
    lines
  with
  | Unix.Unix_error (err, _, _) ->
      Printf.eprintf "Erro ao executar comando Git: %s\n" (Unix.error_message err);
      []

let get_branches repo_dir =
  let branches = exec_command_in_repo repo_dir "git branch --all" in
  List.map (fun b -> String.trim b) branches
  |> List.filter (fun b -> b <> "") (* Remove apenas linhas vazias *)

let get_git_log repo_dir =
  let log = exec_command_in_repo repo_dir "git log --graph --pretty=format:'%C(auto)%h%d %s %C(green)(%an, %ar)' --all" in
  List.map String.trim log
  |> List.filter (fun l -> l <> "")

let merge_branch repo_dir branch =
  exec_command_in_repo repo_dir (Printf.sprintf "git merge %s" branch)

let get_current_branch repo_dir =
  match exec_command_in_repo repo_dir "git branch --show-current" with
  | [] -> "unknown"
  | head :: _ -> String.trim head

let rec find_git_repo dir =
  let git_dir = Filename.concat dir ".git" in
  if Sys.file_exists git_dir && Sys.is_directory git_dir then
    Some dir
  else if dir = "/" then
    None
  else
    find_git_repo (Filename.dirname dir)