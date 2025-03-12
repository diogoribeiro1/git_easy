(* open Unix *)

(* Executa um comando e retorna a saída como uma lista de strings *)
let exec_command cmd =
  let ic = Unix.open_process_in cmd in
  let rec read_lines acc =
    match input_line ic with
    | line -> read_lines (line :: acc)
    | exception End_of_file ->
        close_in ic;
        List.rev acc
  in
  read_lines []

(* Retorna todas as branches do repositório atual *)
let get_branches () = exec_command "git branch --all"

(* Retorna o histórico de commits em formato de gráfico *)
let get_git_log () = exec_command "git log --graph --oneline --all"
