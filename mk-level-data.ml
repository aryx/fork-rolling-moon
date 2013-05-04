#! /usr/bin/env ocaml
#load "str.cma"
#directory "+site-lib/xml-light"
#directory "+xml-light"
#load "xml-light.cma"

let float_of_string str =
  try float_of_string str
  with Failure "float_of_string" ->
    invalid_arg str
;;

let output_custom_int ic d =
  let d = d + 32768 in
  let b0 = d land 255
  and b1 = d lsr 8 in
  output_byte ic b0;
  output_byte ic b1;
;;


type transform =
  | Translate of (float * float)
  | Matrix of (float * float * float * float * float * float)
  | No_t

let parse_transform args =
  try
    let tr = (List.assoc "transform" args) in
    try
      Scanf.sscanf tr "translate(%f,%f)" (fun x y -> Translate(x, y))
    with Scanf.Scan_failure _ ->
      Scanf.sscanf tr "matrix(%f,%f,%f,%f,%f,%f)"
        (fun a b c d e f -> Matrix(a,b,c,d,e,f))
  with
    Not_found -> No_t
;;

let apply_trans t x y =
  match t with
  | No_t -> (x, -.y)
  | Translate(tx, ty) -> (x +. tx, -. (y +. ty))
  | Matrix(a,b,c,d,e,f) ->
      let x = x *. a +. y *. c +. e
      and y = x *. b +. y *. d +. f in
      (x, -.y)
;;


let output_path args =
  let str = (List.assoc "d" args) in
  output_byte stdout 1;  (* target type *)

  let sp = Str.split (Str.regexp "[ \r\n\t]*[LMz][ \r\n\t]*") str in

  let nb = List.length sp in
  output_binary_int stdout nb;

  let t = parse_transform args in

  List.iter (fun str ->
      let coord = Str.split (Str.regexp ",") str in
      let x, y =
        match coord with
        | x::y::[] -> x, y
        | _ -> invalid_arg str
      in
      let x = (float_of_string x)
      and y = (float_of_string y) in
      let x, y = apply_trans t x y in
      output_custom_int stdout (int_of_float(x *. 1.0));
      output_custom_int stdout (int_of_float(y *. 1.0));
    ) sp;
;;


let output_ball args =
  let cx = float_of_string(List.assoc "sodipodi:cx" args)
  and cy = float_of_string(List.assoc "sodipodi:cy" args)
  and rx = float_of_string(List.assoc "sodipodi:rx" args)
  and ry = float_of_string(List.assoc "sodipodi:ry" args) in
  let t = parse_transform args in
  let x, y = apply_trans t cx cy in
  let r =
    if rx <> ry
    then invalid_arg(Printf.sprintf " different radius : %g %g" rx ry)
    else rx
  in
  output_custom_int stdout (int_of_float(x *. 1.0));
  output_custom_int stdout (int_of_float(y *. 1.0));
  output_custom_int stdout (int_of_float(r *. 1.0));
;;

let output_target args =
  output_byte stdout 2;  (* target type *)
  output_ball args;
;;

let output_avatar args =
  output_byte stdout 3;  (* avatar type *)
  output_ball args;
;;


let is_param param args value =
  try (List.assoc param args) = value
  with Not_found -> false
;;

let level_of_svg() =
  let xml = Xml.parse_in stdin in
  let rec dig = function
    | [] -> () (* invalid_arg "input" *)
    | Xml.PCData _ ::tail -> dig tail
    | Xml.Element(tag, args, childs) :: tail ->

        if is_param "class" args "level"
        then output_path args;

        if is_param "class" args "target"
        then output_target args;

        if is_param "class" args "avatar"
        then output_avatar args;

        dig (List.rev_append childs tail)
  in
  dig [xml]
;;

let () =
  level_of_svg();
;;

(* vim: sw=2 sts=2 ts=2 et fdm=marker
 *)
