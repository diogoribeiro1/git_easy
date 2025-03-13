open Notty
open Notty_unix
open Git_easy.Git_utils

let list_dirs path =
  try
    Array.fold_right (fun f acc ->
      let full = Filename.concat path f in
      if Sys.is_directory full then f :: acc else acc)
      (Sys.readdir path) []
  with Sys_error msg ->
    Printf.eprintf "Erro ao listar diretórios: %s\n" msg;
    []

let navigate_dirs term path =
  let dirs = list_dirs path in
  let options = ".." :: dirs in
  let rec loop selected =
    Term.image term (I.vcat (List.mapi (fun i dir ->
      let color = if i = selected then A.(fg lightblue ++ bg black)
                  else A.(fg white) in
      I.string color (" > " ^ dir)
    ) options));
    match Term.event term with
    | `Key (`Arrow `Up, _) ->
        loop (max 0 (selected - 1))
    | `Key (`Arrow `Down, _) ->
        loop (min (List.length options - 1) (selected + 1))
    | `Key (`Enter, _) ->
        let chosen = List.nth options selected in
        if chosen = ".." then
          let parent = Filename.dirname path in
          if parent = path then path else parent
        else
          Filename.concat path chosen
    | `Key (`ASCII 'q', _) -> exit 0
    | _ -> loop selected
  in
  loop 0

let select_merge_branch term repo_dir =
  let branches = get_branches repo_dir in
  let rec loop selected =
    Term.image term (I.vcat (List.mapi (fun i branch ->
      let color = if i = selected then A.(fg lightblue ++ bg black)
                  else A.(fg white) in
      I.string color (" > " ^ branch)
    ) branches));
    match Term.event term with
    | `Key (`Arrow `Up, _) -> loop (max 0 (selected - 1))
    | `Key (`Arrow `Down, _) -> loop (min (List.length branches - 1) (selected + 1))
    | `Key (`Enter, _) -> List.nth branches selected
    | `Key (`ASCII 'q', _) -> exit 0
    | _ -> loop selected
  in
  loop 0

let _render_git_log term repo_dir =
  let log_lines = get_git_log repo_dir in
  if log_lines = [] then
    Term.image term (I.string A.(fg red) "Nenhum log disponível.")
  else
    let images = List.map (fun line ->
      if String.contains line '*' then I.string A.(fg green) ("  " ^ line)
      else if String.contains line '|' then I.string A.(fg yellow) ("  " ^ line)
      else I.string A.(fg white) ("  " ^ line)
    ) log_lines in
    Term.image term (I.vcat (I.string A.(fg lightmagenta) "Git Log:" :: images))

let _render_branches term repo_dir =
  let branches = get_branches repo_dir in
  if branches = [] then
    Term.image term (I.string A.(fg red) "Nenhuma branch encontrada.")
  else
    let current_branch = get_current_branch repo_dir in
    let images = List.map (fun b ->
      let color = if String.contains b '*' || String.trim b = current_branch then A.(fg green)
      else if String.length b >= 7 && String.sub b 0 7 = "remotes" then A.(fg yellow)                  else A.(fg white) in
      I.string color ("  " ^ b)
    ) branches in
    Term.image term (I.vcat (I.string A.(fg lightmagenta) "Branches:" :: images))

let merge_interface term repo_dir =
  Term.image term (I.string A.(fg lightmagenta) "Selecione a branch para merge:");
  let branch = select_merge_branch term repo_dir in
  let result = merge_branch repo_dir branch in
  Term.image term (I.string A.(fg lightmagenta) ("Merge result:\n" ^ String.concat "\n" result));
  Term.image term (I.string A.(fg white) "Pressione Enter para continuar...");
  match Term.event term with
  | `Key (`Enter, _) -> ()
  | `Key (`ASCII 'q', _) -> exit 0
  | _ -> ()

let rec main_loop term path =
  let chosen_dir = navigate_dirs term path in
  match find_git_repo chosen_dir with
  | Some repo_dir ->
      (* Depuração: verificar o diretório *)
      Printf.eprintf "Repo encontrado: %s\n" repo_dir;
      
      (* Obter branches e log *)
      let branches = get_branches repo_dir in
      let log_lines = get_git_log repo_dir in
      let current_branch = get_current_branch repo_dir in
      
      (* Depuração: verificar dados *)
      Printf.eprintf "Branches: %s\n" (String.concat "; " branches);
      Printf.eprintf "Log: %s\n" (String.concat "; " log_lines);
      
      (* Construir imagens *)
      let repo_msg = I.string A.(fg lightmagenta) ("Repository: " ^ repo_dir) in
      
      let branch_images = 
        if branches = [] then
          [I.string A.(fg red) "Nenhuma branch encontrada."]
        else
          I.string A.(fg lightmagenta) "Branches:" ::
          List.map (fun b ->
            let color = if String.contains b '*' || String.trim b = current_branch then A.(fg green)
                        else if String.length b >= 7 && String.sub b 0 7 = "remotes" then A.(fg yellow)
                        else A.(fg white) in
            I.string color ("  " ^ b)
          ) branches
      in
      
      let log_images =
        if log_lines = [] then
          [I.string A.(fg red) "Nenhum log disponível."]
        else
          I.string A.(fg lightmagenta) "Git Log:" ::
          List.map (fun line ->
            if String.contains line '*' then I.string A.(fg green) ("  " ^ line)
            else if String.contains line '|' then I.string A.(fg yellow) ("  " ^ line)
            else I.string A.(fg white) ("  " ^ line)
          ) log_lines
      in
      
      let instructions = I.string A.(fg white) "Pressione 'm' para merge, Enter para voltar ou 'q' para sair." in
      
      (* Combinar tudo em uma única imagem *)
      let full_image = I.vcat ([repo_msg] @ branch_images @ log_images @ [instructions]) in
      Term.image term full_image;
      
      (* Processar eventos *)
      (match Term.event term with
        | `Key (`ASCII 'm', _) -> merge_interface term repo_dir; main_loop term path
        | `Key (`Enter, _) -> main_loop term path
        | `Key (`ASCII 'q', _) -> exit 0
        | _ -> main_loop term path)
  | None ->
      Term.image term (I.string A.(fg red) "Nenhum repositório Git encontrado.");
      main_loop term chosen_dir

let () =
  let term = Term.create () in
  ignore (main_loop term (Sys.getcwd ()));
  Term.release term