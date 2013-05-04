
(* {{{ COPYING *(

  This file is a simple arcade game that uses the Chipmunk library.
  Copyright (C) 2008  Florent Monnier  <fmonnier@linux-nantes.org>

  This program is free software: you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation, either version 3
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>

)* }}} *)
(* {{{ Modules *)





open GL
open Glu
open Glut



open Chipmunk ;;
open Low_level ;;
open OO ;;

(* }}} *)

(* {{{ load level *)

type level =
  | Path of (float * float) array
  | Target of (float * float * float)
  | Avatar of (float * float * float)
;;

let input_custom_int ic =
  let b0 = input_byte ic in
  let b1 = input_byte ic in
  (b0 lor (b1 lsl 8)) - 32768
;;


let read_path ic =
  let n = input_binary_int ic in
  let arr =
    Array.init n (fun _ ->
      let x = input_custom_int ic in
      let y = input_custom_int ic in
      let x = (float x) /. 1.0
      and y = (float y) /. 1.0 in
      (x,y))
  in
  Path(arr)
;;


let read_target ic =
  let x = input_custom_int ic in
  let y = input_custom_int ic in
  let r = input_custom_int ic in
  let x = (float x) /. 1.0
  and y = (float y) /. 1.0
  and r = (float r) /. 1.0 in
  Target(x,y,r)
;;


let read_avatar ic =
  let x = input_custom_int ic in
  let y = input_custom_int ic in
  let r = input_custom_int ic in
  let x = (float x) /. 1.0
  and y = (float y) /. 1.0
  and r = (float r) /. 1.0 in
  Avatar(x,y,r)
;;


let read_datas ic =
  let rec aux acc =
    try
      let kind =
        try input_byte ic
        with End_of_file -> raise Exit
      in
      try match kind with
        | 1 -> aux (read_path ic :: acc)
        | 2 -> aux (read_target ic :: acc)
        | 3 -> aux (read_avatar ic :: acc)
        | x -> invalid_arg(Printf.sprintf "Unknown data type: %d" x)
      with
        End_of_file -> failwith("uncomplete data")
    with
      Exit -> acc
  in
  aux []
;;


let load_datas file =
  let ic = open_in_bin file in
  let datas = read_datas ic in
  close_in ic;
  (datas)
;;

let load_level file =
  let datas = load_datas file in

  let rec filter paths targets s = function
    | Path p :: tl -> filter (p::paths) targets s tl
    | Target i :: tl -> filter paths (i::targets) s tl
    | Avatar a::tl -> filter paths targets (Some a) tl
    | [] ->
        let x, y, r =
          match s with Some s -> s
          | None -> (150.0, 0.0, 18.0)  (* default start position, and radius *)
         in
        (paths, targets, x, y, r)
  in
  filter [] [] None datas
;;

(* }}} *)

(* {{{ global vars *)

let do_tess = ref true ;;

let field = ref 400. ;;

let sleep_ticks = 16 ;;
let texd = ref false ;;
let do_bg = ref false ;;

let pi = 4.0 *. atan 1.0 ;;

let init_global() = ref None ;;
let get_global g = match !g with Some v -> v | None -> raise Not_found ;;

let _space = init_global()
let _staticBody = init_global()
let _level_paths = init_global()

let _ball = init_global()

let active_shapes_li = ref [] ;;
let active_bodies_li = ref [] ;;

let targets_shapes_li = ref [] ;;

let _ball_id = init_global()
let targets_id_arr = ref [| |] ;;

let _stop = ref false ;;
let _finished = ref false ;;

let cpv x y = {cp_x=x; cp_y=y}
let cpvzero = cpvzero()

let gravity = ref 600. ;;

let key_left_down = ref false ;;
let key_right_down = ref false ;;

let spent_time = ref 0 ;;
let angle_z = ref (0.)
let angle_step = ref 2.0 ;;

let rad_to_deg =
  let f = (pi /. 180.) in
  (function v -> v *. f)
;;

let level_files = ref [] ;;
let texture_file = ref "" ;;

(* default window size *)
(*
let win_width, win_height = ref 800, ref 600 ;;
let win_width, win_height = ref 700, ref 525 ;;
let win_width, win_height = ref 640, ref 480 ;;
let win_width, win_height = ref 600, ref 450 ;;
let win_width, win_height = ref 500, ref 375 ;;
let win_width, win_height = ref 400, ref 300 ;;
let win_width, win_height = ref 300, ref 225 ;;
let win_width, win_height = ref 280, ref 210 ;;
*)
let win_width, win_height = ref 640, ref 480 ;;


let factor = ref 0. ;;

let tess_id = ref None ;;
let outline_id = ref None ;;
let bg_id = ref None ;;


(* }}} *)
(* {{{ restart the level *)

let reset_avatar, avatar_start_pos =
  let x = ref 0.0 and y = ref 0.0 in
  (fun () ->
     let ball = get_global _ball in
     ball#reset_forces;
     ball#set_torque ~t:0.0;
     ball#set_force ~f:cpvzero;
     ball#set_a_vel ~w:0.0;
     ball#set_angle ~a:0.0;
     ball#set_vel ~v:cpvzero;
     ball#set_rot ~rot:cpvzero;
     ball#set_pos ~p:(cpv !x !y);
     ),
  (fun _x _y ->
     x := _x;
     y := _y)
;;

(* }}} *)

(* {{{ init the physic *)

let init_rolling_ball level =
  let level_paths, level_targets, start_x, start_y, ball_radius =
    load_level level
  in
  _level_paths := Some level_paths;

  let space = new cp_space in
  _space := Some space;

  space#set_gravity (cpv 0.0 (-. !gravity));
  space#set_elastic_iterations 10;

  space#resize_static_hash 30.0 4000;
  space#resize_active_hash 30.0 20;

  (* {{{ static body *)

  let staticBody = new cp_body infinity infinity in
  _staticBody := Some staticBody;

  List.iter (fun path ->
      let n = Array.length path in
      let rec terrain_loop i a =
        if i >= n then () else begin
          let x, y = path.(i) in
          let b = cpv x y in

          let seg = new cp_shape staticBody (SEGMENT_SHAPE(a, b, 0.0)) in

          seg#set_layers 0b1111111_11111111_11111111_11111111;

          seg#set_friction 1.0;
          seg#set_elasticity 0.6;

          space#add_static_shape seg;

          terrain_loop (succ i) b
        end
      in
      let x, y = path.(0) in
      let a = cpv x y in
      terrain_loop 1 a;
  ) level_paths;

  (* }}} *)
  (* {{{ avatar ball *)

  let ball_mass = 2.0 in
  let ball_moment = moment_for_circle ball_mass ball_radius 0.0 cpvzero in

  let ball = new cp_body ball_mass ball_moment in
  _ball := Some ball;

  active_bodies_li := ball :: !active_bodies_li;

  avatar_start_pos start_x start_y;
  ball#set_pos (cpv start_x start_y);
  space#add_body ball;

  let shape = new cp_shape ball (CIRCLE_SHAPE(ball_radius, cpvzero)) in
  shape#set_friction 0.2;
  shape#set_elasticity 0.8;
  space#add_shape shape;
  active_shapes_li := shape :: !active_shapes_li;

  _ball_id := Some(shape#get_hashid);

  shape#set_layers 0b0000000_00000000_11111111_00000000;

  (* Twin mode:

  let ball_mass = 2.0 in
  let ball_radius = 18.0 in
  let ball_moment = moment_for_circle ball_mass ball_radius 0.0 cpvzero in

  let ball = new cp_body ball_mass ball_moment in
  _ball := Some ball;

  active_bodies_li := ball :: !active_bodies_li;

  ball#set_pos (cpv 200.0 0.0);
  space#add_body ball;

  let shape = new cp_shape ball (CIRCLE_SHAPE(ball_radius, cpvzero)) in
  shape#set_friction 0.2;
  shape#set_elasticity 0.8;
  space#add_shape shape;
  active_shapes_li := shape :: !active_shapes_li;

  shape#set_layers 0b0000000_00000000_00000000_11111111;
  *)

  (* }}} *)
  (* {{{ game targets *)

  let add_target (x, y, r) =
    let target = new cp_body infinity infinity in
    target#set_pos (cpv x y);

    let shape = new cp_shape target (CIRCLE_SHAPE(r, cpvzero)) in
    shape#set_friction 0.6;
    shape#set_elasticity 0.6;

    space#add_static_shape shape;
    targets_shapes_li := shape :: !targets_shapes_li;

    (shape#get_hashid, false)
  in
  targets_id_arr := Array.of_list (List.map add_target level_targets);

  (* }}} *)
;;

(* }}} *)

(* {{{ free level *)

let free_display_list should_be dl =
  match !dl with
  | None ->
      if should_be then
        Printf.eprintf "Warning: empty display list\n%!"
  | Some gl_list ->
      glDeleteLists ~gl_list ~range:1;
      dl := None;
;;


let globals_init_vals() =
  _stop := false;
  _finished := false;

  angle_z := 0.;
  spent_time := 0;

  factor := 0.;

  key_left_down := false;
  key_right_down := false;
;;


let re_init_globals() =
  active_shapes_li := [];
  active_bodies_li := [];
  targets_shapes_li := [];

  globals_init_vals();

  free_display_list (!do_tess) tess_id;
  free_display_list (true) outline_id;
  free_display_list (!do_bg) bg_id;
;;


let free_level() =
  let space = get_global _space in
  space#free_children;
  space#free;
  let staticBody = get_global _staticBody in
  staticBody#free;
;;

(* }}} *)
(* {{{ next level *)

(*
let pop_level() =
  try
    let hd = List.hd !level_files in
    level_files := List.tl !level_files;
    (hd)
  with
  | Failure "hd" -> failwith "no level"
  | Failure "tl" -> failwith "bug"
;;
*)

let pop_level() =
  match !level_files with
  | hd::tl -> level_files := tl; (hd)
  | [] -> failwith "no level"
;;

let next_level() =
  re_init_globals();
  free_level();
  let level =
    try pop_level()
    with Failure "no level" ->
      print_endline "goodbye"; exit 0
  in
  init_rolling_ball level;
;;

let is_last_level() =
  match !level_files with
  | [] -> true
  | _ -> false
;;

(* timer *)
let goto_next = ref false ;;
let goto_next_level ~value = if !goto_next then next_level() ;;

(* }}} *)

(* {{{ handle collisions *)

let prop_targets() =
  let len = Array.length !targets_id_arr in
  let n = Array.fold_left (fun n (_,b) -> if b then succ n else n) 0 !targets_id_arr in
  Printf.sprintf "%d/%d" (succ n) len;
;;


let proc_collisions ~arb =
  let a = cpArbiterGetShapePA arb
  and b = cpArbiterGetShapePB arb in
  let a_id = cpShapeGetHashID a
  and b_id = cpShapeGetHashID b in

  let ball_id = get_global _ball_id in

  let check_target  idx  (it_id, collided) =
    if it_id = a_id || it_id = b_id then
      if ball_id = a_id || ball_id = b_id then begin
        if not(!_stop) && not(snd !targets_id_arr.(idx)) then
            Printf.printf " %s : collided at %g sec.\n%!" (prop_targets()) (float !spent_time /. 10.);

        !targets_id_arr.(idx) <- (it_id, true);
      end
  in

  Array.iteri check_target !targets_id_arr;

  let done_ =
    Array.fold_left (fun x (_,b) -> x && b) true !targets_id_arr
  in

  if done_ && not(!_stop) then begin
    Printf.printf " Completed in %g sec.\n%!" (float !spent_time /. 10.);
    _stop := true;
    _finished := true;
    if not(is_last_level()) then
      (goto_next := true;
       glutTimerFunc ~msecs:5000 ~timer:goto_next_level ~value:0);

    if not(!key_left_down) && not(!key_right_down) then
      if (!spent_time mod 2) = 0
      then key_left_down := true
      else key_right_down := true;
  end;
;;

(* }}} *)
(* {{{ update physics steps *)

let rolling_ball_update() =
  if not(!_stop) then begin
    let ball = get_global _ball
    and space = get_global _space in

    let y = !gravity *. cos (rad_to_deg !angle_z)
    and x = !gravity *. sin (rad_to_deg !angle_z)
    in
    space#set_gravity (cpv x (-. y));

    let substeps = 4 in
    let dt = (1.0 /. 60.0) /. (float substeps) in

    for i=0 to pred substeps do

      let arbiters = space#get_arbiters  in
      Array.iter (fun arb -> proc_collisions ~arb) arbiters;

      ball#reset_forces;

      space#step ~dt;
    done;
  end;
;;

(* }}} *)

(* {{{ draw Objects *)

let target_circle ~x ~y ~r ~a =
  let segs = 16 in
  let coef = 2.0 *. pi /. (float segs) in

  glBegin GL_LINE_LOOP;
    for n=0 to pred segs do
      let rads = (float n) *. coef in
      glVertex2 (r *. cos(rads +. a) +. x)
                (r *. sin(rads +. a) +. y);
    done;
  glEnd();
;;

let was_touched id arr =
  let len = Array.length arr in
  let rec aux i =
    if i >= len then false else
      let _id, b = arr.(i) in
      if _id = id then b
      else aux(succ i)
  in
  aux 0
;;

let drawTargetShape ~shape =
  let body = shape#body
  and circle = shape#get_circle_shape
  and id = shape#get_hashid
  in
  let c = cpvadd body#get_pos (cpvrotate circle#get_center body#get_rot) in
  if was_touched id !targets_id_arr
  then glColor3 0.0 0.5 0.0
  else glColor3 0.2 1.0 0.7;
  target_circle  c.cp_x  c.cp_y  circle#get_radius  body#get_angle;
;;


(* circle of the avatar ball *)
let drawCircle ~x ~y ~r ~a =
  let segs = 16 in
  let coef = 2.0 *. pi /. (float segs) in

  glColor3 0.8 0.5 0.0;
  glBegin GL_LINE_LOOP;
    for n=0 to pred segs do
      let rads = (float n) *. coef in
      glVertex2 (r *. cos(rads +. a) +. x)
                (r *. sin(rads +. a) +. y);
    done;
  glEnd();

  glColor3 0.9 0.6 0.0;
  glBegin GL_LINES;
    glVertex2 x y;
    glVertex2 (r *. cos(a) +. x)
              (r *. sin(a) +. y);

    glVertex2 x y;
    glVertex2 (r *. cos(a +. (pi *. 0.666666)) +. x)
              (r *. sin(a +. (pi *. 0.666666)) +. y);

    glVertex2 x y;           
    glVertex2 (r *. cos(a +. (pi *. 1.333333)) +. x)
              (r *. sin(a +. (pi *. 1.333333)) +. y);
  glEnd();
;;

let drawCircleShape ~shape =
  let body = shape#body
  and circle = shape#get_circle_shape
  in
  let c = cpvadd body#get_pos (cpvrotate circle#get_center body#get_rot) in
  drawCircle  c.cp_x  c.cp_y  circle#get_radius  body#get_angle;
;;


let drawSegmentShape ~shape =
  let body = shape#body
  and seg = shape#get_segment_shape
  in
  let a = cpvadd body#get_pos (cpvrotate seg#get_a body#get_rot)
  and b = cpvadd body#get_pos (cpvrotate seg#get_b body#get_rot)
  in

  glBegin GL_LINES;
    glVertex2  a.cp_x  a.cp_y;
    glVertex2  b.cp_x  b.cp_y;
  glEnd();
;;


let drawPolyShape ~shape =
  let body = shape#body
  and poly = shape#get_poly_shape in

  let num = poly#get_num_verts
  and verts = poly#get_verts in

  glBegin GL_LINE_LOOP;
  for i=0 to pred num do
    let v = cpvadd body#get_pos (cpvrotate verts.(i) body#get_rot) in
    glVertex2  v.cp_x  v.cp_y;
  done;
  glEnd();
;;


let drawObject ~shape =
  match shape#kind with
  | CP_CIRCLE_SHAPE -> drawCircleShape ~shape;
  | CP_SEGMENT_SHAPE -> drawSegmentShape ~shape;
  | CP_POLY_SHAPE -> drawPolyShape ~shape;
  | _ ->
      Printf.printf "Bad enumeration in drawObject().\n";
;;


let drawBodies ~bodies =
  glBegin GL_POINTS;
    glColor3 0.0 0.8 1.0;
    List.iter
      (fun body ->
        let p = body#get_pos in
        glVertex2  p.cp_x  p.cp_y;
      ) bodies;
  glEnd();
;;


let draw_collisions ~arb:arbiter =
  let cont_arr = (get_arbiter_contacts ~arbiter) in

  glColor3 1.0 0.5 0.0;
  glBegin GL_POINTS;
    Array.iter (fun contact ->
        let v = cpContactGetP contact in
        glVertex2  v.cp_x  v.cp_y;
      ) cont_arr;
  glEnd();
;;

(* }}} *)
(* {{{ display *)

let display =
  function () ->
  let ball = get_global _ball in
  let pos = ball#get_pos in
  let x = -. pos.cp_x
  and y = -. pos.cp_y in

  glClear [GL_COLOR_BUFFER_BIT];
  glLoadIdentity();

  if !_finished then begin
    let f = cos !factor in
    glScale f f 1.0;
    factor := !factor +. 0.008;
  end;

  glRotate ~angle:(!angle_z) ~x:0.0 ~y:0.0 ~z:(-1.0);

  glTranslate x y 0.0;

  (* {{{ draw background *)
  if !texd && !do_bg then
  begin
    match !bg_id with
    | None ->
        let id = glGenLists 1 in
        glNewList id GL_COMPILE_AND_EXECUTE;
          glEnable GL_TEXTURE_2D;
          glColor3 0.8 0.8 0.8;
          glBegin GL_POLYGON;
            glTexCoord2 ( 2000.0) ( 2000.0);  glVertex3 ( 2000.) ( 2000.) (-1.0);
            glTexCoord2 ( 2000.0) (-2000.0);  glVertex3 ( 2000.) (-2000.) (-1.0);
            glTexCoord2 (-2000.0) (-2000.0);  glVertex3 (-2000.) (-2000.) (-1.0);
            glTexCoord2 (-2000.0) ( 2000.0);  glVertex3 (-2000.) ( 2000.) (-1.0);
          glEnd();
          glDisable GL_TEXTURE_2D;
        glEndList();
        bg_id := Some id;
    | Some id ->
        glPushMatrix();
          glTranslate (-.(x/.2.)) (-.(y/.2.)) 0.0;
          glCallList id;
        glPopMatrix();
  end;
  (* }}} *)
  (* {{{ draw the level *)
  begin
    let level_paths = get_global _level_paths in
    (* {{{ fill the shape *)
    if !do_tess then
    begin
      let tess = gluNewTess() in
        gluTessDefaultCallback tess GLU_TESS_VERTEX;
        gluTessDefaultCallback tess GLU_TESS_BEGIN;
        gluTessDefaultCallback tess GLU_TESS_END;
        (*
        gluCallbackTessVertex tess (fun ~x ~y ~z -> glTexCoord2 x y; glVertex3 x y z;);
        gluCallbackTessBegin tess (fun ~prim -> glBegin prim;);
        gluCallbackTessEnd tess (fun _ -> glEnd(););
        *)

        gluTessDefaultCallback tess GLU_TESS_COMBINE;
        gluTessDefaultCallback tess GLU_TESS_ERROR;

        gluGetTessWindingRule tess GLU_TESS_WINDING_POSITIVE;


        let filling() =
          if !texd then begin
            glEnable GL_TEXTURE_2D;

            glMatrixMode GL_TEXTURE;
            let f = 1.0 /. 180. in
            glLoadIdentity();
            glScale f f 1.0;
            (*
            glTranslate (x/.5.) (y/.5.) 0.0;
            *)
            glMatrixMode GL_MODELVIEW;
          end;

          if !texd
          then glColor3 1.0 1.0 1.0
          else glColor3 0.1 0.0 0.3;
        in
        (*
        (* applying here allows to modify the texture mapping,
           while keeping the tesselation in a display list *)
        filling();
        *)

        begin
          match !tess_id with
          | None ->
              let tess_pnts =
                List.map (Array.map (fun (x,y) -> (x,y,0.0))) level_paths
              in
              let id = glGenLists 1 in
              glNewList id GL_COMPILE_AND_EXECUTE;

                filling();

                gluTesselateIter tess tess_pnts;
                (* {{{ equivalent: 
                gluTessBeginPolygon ~tess;
                  List.iter (fun points ->
                      gluTessBeginContour ~tess;
                        Array.iter (fun (x,y,z) -> gluTessVertex ~tess ~x ~y ~z) points;
                      gluTessEndContour ~tess;
                    ) tess_pnts;
                gluTessEndPolygon ~tess;
                }}} *)

              glEndList();
              tess_id := Some id;
          | Some id -> glCallList id;
        end;

        if !texd then
          glDisable GL_TEXTURE_2D;

      gluDeleteTess ~tess;
    end;
    (* }}} *)
    (* {{{ draw outlines *)
    begin
      match !outline_id with
      | None ->
          let id = glGenLists 1 in
          glNewList id GL_COMPILE_AND_EXECUTE;

            glColor3 0.0 0.5 1.0;
            List.iter (fun a ->
                glBegin GL_LINE_LOOP;
                  Array.iter (fun (x,y) -> glVertex2 x y;) a;
                glEnd();
              ) level_paths;

          glEndList();
          outline_id := Some id;
      | Some id -> glCallList id;
    end;
    (* }}} *)
  end;
  (* }}} *)

  List.iter (fun shape -> drawObject ~shape) !active_shapes_li;
  List.iter (fun shape -> drawTargetShape ~shape) !targets_shapes_li;

  (*
  let bodies = !active_bodies_li in
  drawBodies ~bodies;
  *)

  (*
  let space = get_global _space in
  let arbiters = space#get_arbiters  in
  Array.iter (fun arb -> draw_collisions ~arb) arbiters;
  *)

  glutSwapBuffers();

  rolling_ball_update();
;;

(* }}} *)
(* {{{ reshape *)

let reshape ~width:w ~height:h =
  glViewport 0 0 w h;
  glMatrixMode GL_PROJECTION;
  glLoadIdentity();

  if w <= h then
    let p = (float h) /. (float w) in
    glOrtho (-. !field) (!field)  (-. !field *. p) (!field *. p) (-10.0) (10.0);
  else
    let p = (float w) /. (float h) in
    glOrtho (-. !field *. p) (!field *. p)  (-. !field) (!field) (-10.0) (10.0);

  glMatrixMode GL_MODELVIEW;
;;

(* }}} *)

(* {{{ user entries *)

let reset_targets() =
  Array.iteri (fun i (id,_) ->
      !targets_id_arr.(i) <- (id, false);
    ) !targets_id_arr;
;;

let restart_level() =
  reset_avatar();
  reset_targets();
  globals_init_vals();
;;

let toggle_pause() = _stop := not(!_stop) ;;

let keyboard ~key ~x ~y =
  match key with
  | '\027' | 'q' | 'Q' -> free_level(); exit 0;

  | 'z' -> restart_level();

  | 'd' ->
      let ball = get_global _ball in
      let x = (ball#get_pos).cp_x
      and y = (ball#get_pos).cp_y in
      Printf.printf " %g %g\n%!" x y;

  | 'n' -> goto_next := false; next_level();

  | 'b' -> do_bg := not(!do_bg);

  | 't' ->
      Printf.printf " spent time %g sec.\n%!" (float !spent_time /. 10.);

  | 'p' -> toggle_pause();

  | _ -> ()
;;


let special ~key ~x ~y =
  if not(!_stop) then
    match key with
    | GLUT_KEY_LEFT ->  key_left_down := true
    | GLUT_KEY_RIGHT -> key_right_down := true

    | GLUT_KEY_UP   -> angle_step := !angle_step +. 0.1
    | GLUT_KEY_DOWN -> angle_step := !angle_step -. 0.1

    | GLUT_KEY_PAGE_UP   -> gravity := !gravity +. 20.0
    | GLUT_KEY_PAGE_DOWN -> gravity := !gravity -. 20.0
    | _ -> ()
;;

let special_up ~key ~x ~y =
  if not(!_stop) then
    match key with
    | GLUT_KEY_LEFT ->  key_left_down := false
    | GLUT_KEY_RIGHT -> key_right_down := false
    | _ -> ()
;;

(* }}} *)
(* {{{ menu *)

let rb_menu ~value =
  match value with
  | 1 -> toggle_pause()
  | 2 -> restart_level();
  | 3 -> texd := not(!texd);
  | 4 -> do_tess := not(!do_tess);
  | 5 -> do_bg := not(!do_bg);
  | 6 -> glutFullScreen();
  | 7 -> field := !field *. 0.8;
         reshape
           (glutGet GLUT_WINDOW_WIDTH)
           (glutGet GLUT_WINDOW_HEIGHT);
         glutPostRedisplay();
  | 8 -> field := !field /. 0.8;
         reshape
           (glutGet GLUT_WINDOW_WIDTH)
           (glutGet GLUT_WINDOW_HEIGHT);
         glutPostRedisplay();
  | 9 -> next_level();
  | 0 -> exit 0;
  | _ -> ()
;;

let resol_menu ~value =
  match value with
  | 800_600 -> glutReshapeWindow 800 600;
  | 700_525 -> glutReshapeWindow 700 525;
  | 640_480 -> glutReshapeWindow 640 480;
  | 600_450 -> glutReshapeWindow 600 450;
  | 500_375 -> glutReshapeWindow 500 375;
  | 400_300 -> glutReshapeWindow 400 300;
  | 300_225 -> glutReshapeWindow 300 225;
  | 280_210 -> glutReshapeWindow 280 210;
  | _ -> ()
;;

(* }}} *)
(* {{{ parse cmd arg *)

let parse_resolution str =
  Scanf.sscanf str "%dx%d" (fun x y -> x, y)
;;

let parse_args() =
  let argc = Array.length Sys.argv in
  for i=0 to pred argc do
    match Sys.argv.(i) with
    | "-l" | "--level" ->
        level_files := !level_files @ [Sys.argv.(i+1)];
    | "-t" | "--texture" ->
        texture_file := Sys.argv.(i+1);
        texd := true;
        do_bg := true;
    | "-b" | "--no-bg" ->
        do_bg := false;
    | "-r" | "--resolution" ->
        let w, h = parse_resolution Sys.argv.(i+1) in
        win_width := w;
        win_height := h;
    | "-w" | "--wireframe"
    | "--not-tess" ->
        do_tess := false;
    | _ -> ()
  done;
;;

(* }}} *)

(* {{{ timer *)

let rec timer ~value =
  if !key_left_down  then angle_z := !angle_z -. (!angle_step);
  if !key_right_down then angle_z := !angle_z +. (!angle_step);

  glutTimerFunc ~msecs:sleep_ticks ~timer ~value:0;
  glutPostRedisplay();
;;


let rec time_count ~value =
  if not(!_stop) then incr spent_time;
  glutTimerFunc ~msecs:100 ~timer:time_count ~value:0;
;;

(* }}} *)
(* {{{ gl inits *)

let init_gl() =
  glClearColor 0.2 0.0 0.4 0.0;

  (*
  (* antialiasing *)
  glEnable GL_LINE_SMOOTH;
  glEnable GL_POINT_SMOOTH;
  glEnable GL_BLEND;
  glHint GL_LINE_SMOOTH_HINT  GL_DONT_CARE;
  glHint GL_POINT_SMOOTH_HINT  GL_DONT_CARE;
  glLineWidth 0.8;

  glPointSize 2.0;
  *)

  glShadeModel GL_FLAT;
  glDisable GL_DEPTH_TEST;
;;

(* }}} *)
(* {{{ texture *)


let load_texture ~filename =
  let texture, width, height, internal_format, pixel_data_format =
    Jpeg_loader.load_img (Filename filename)
  in

  let texid = glGenTexture() in
  glBindTexture BindTex.GL_TEXTURE_2D texid;

  (* Parameters for applying the textures *)
  glTexParameter TexParam.GL_TEXTURE_2D (TexParam.GL_TEXTURE_MAG_FILTER  Mag.GL_NEAREST);
  glTexParameter TexParam.GL_TEXTURE_2D (TexParam.GL_TEXTURE_MIN_FILTER  Min.GL_NEAREST);

  glTexImage2D TexTarget.GL_TEXTURE_2D  0 internal_format  width height
               pixel_data_format GL_UNSIGNED_BYTE texture;
;;
(* }}} *)
(* {{{ glut inits *)

let init_glut() =
  ignore(glutInit Sys.argv);
  glutInitDisplayMode [GLUT_RGB; GLUT_DOUBLE];
  (*
  glutInitDisplayMode [GLUT_RGBA; GLUT_DOUBLE; GLUT_DEPTH];
  *)
  glutInitWindowSize !win_width !win_height;

  ignore(glutCreateWindow "Rolling Moon");
  (*
  glutPositionWindow 20 20;
  *)

  init_gl();

  if !texd then
    load_texture ~filename:!texture_file;

  glutDisplayFunc ~display;
  glutReshapeFunc ~reshape;
  glutKeyboardFunc ~keyboard;
  glutSpecialFunc ~special;
  glutSpecialUpFunc ~special_up;
  glutTimerFunc ~msecs:sleep_ticks ~timer ~value:0;
  glutTimerFunc ~msecs:100 ~timer:time_count ~value:0;
  glutIdleFunc ~idle:(fun () -> Gc.minor());

  (*
  at_exit (fun () -> Printf.printf " %g sec.\n" (float !spent_time /. 10.));
  *)

  (* resol sub menu *)
  let resol_sub_menu = glutCreateMenu resol_menu in
  glutAddMenuEntry "800 x 600"  800_600;
  glutAddMenuEntry "700 x 525"  700_525;
  glutAddMenuEntry "640 x 480"  640_480;
  glutAddMenuEntry "600 x 450"  600_450;
  glutAddMenuEntry "500 x 375"  500_375;
  glutAddMenuEntry "400 x 300"  400_300;
  glutAddMenuEntry "300 x 225"  300_225;
  glutAddMenuEntry "280 x 210"  280_210;

  (* menu *)
  ignore(glutCreateMenu rb_menu);
  glutAddMenuEntry "pause"  1;
  glutAddMenuEntry "restart level"  2;
  if !texd then
    glutAddMenuEntry "switch texture"  3;
  glutAddMenuEntry "toggle wireframe"  4;
  glutAddMenuEntry "toggle background"  5;
  glutAddMenuEntry "fullscreen"  6;
  glutAddMenuEntry "zoom"  7;
  glutAddMenuEntry "dezoom"  8;
  glutAddMenuEntry "next level"  9;
  glutAddMenuEntry "quit"  0;
  glutAttachMenu GLUT_RIGHT_BUTTON;
  glutAddSubMenu "resolution" resol_sub_menu;

  glutSetCursor GLUT_CURSOR_NONE;

  glutMainLoop();
;;

(* }}} *)
(* {{{ main *)

let () =
  at_exit Gc.full_major;

  init_chipmunk();
  parse_args();
  init_rolling_ball (pop_level());

  init_glut();
;;

(* }}} *)

(* vim: sw=2 sts=2 ts=2 et fdm=marker
 *)
