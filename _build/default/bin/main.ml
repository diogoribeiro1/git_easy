open Notty
open Notty_unix
open Git_easy

(* Função para renderizar a linha do tempo *)
let render_git_log term =
  let log_lines = Git_utils.get_git_log () in
  let images = List.map (fun line -> I.string A.(fg lightcyan) line) log_lines in
  let full_image = I.vcat images in
  Term.image term full_image

let () =
  let term = Term.create () in

  (* Exibir branches *)
  let branches = Git_utils.get_branches () in
  let branch_list = List.map (fun b -> I.string A.(fg lightgreen) b) branches in
  let branches_image = I.vcat branch_list in
  Term.image term branches_image;

  (* Adicionar um espaço e mostrar o log *)
  Term.image term (I.string A.empty " ");
  render_git_log term;

  (* Esperar o usuário pressionar ENTER para sair *)
  ignore (input_line stdin);
  Term.release term
