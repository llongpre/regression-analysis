open Gnuplot

type funct = float list
type dataset = (float * float) list

exception Illegal

(* inc_fname returns the next filename to be used when saving png files.
 * Png file names are integers. *)
let next_fnum = ref 0
let inc_fname () = next_fnum := 1 + !next_fnum;
  "graph" ^ (string_of_int !next_fnum) ^ ".png"

(* PRE: input is a float list.
 * POST: returns max float from list *)
let rec max_list = function
  | [] -> min_float
  | h::t -> max h (max_list t)

(* PRE: input is a float list
 * POST: returns min float from list *)
let rec min_list = function
  | [] -> max_float
  | h::t -> min h (max_list t)

(* PRE: funct is a float list of coefficients of a polonomial function, in
 * decending order.
 * POST: Returns string equation in the format required by gnuplot. *)
let create_funct funct =
  let ord = List.length funct - 1 in
  let f = List.map (fun x -> string_of_float x) funct in
    if ord=0 then (if f=[] then "" else (List.hd f))
    else if ord=1 then (List.hd f)^"*x+"^(List.nth f 1)
    else if ord=2 then (List.hd f)^"*x**2+"^(List.nth f 1)^"*x+"^(List.nth f 2)
    else if ord=3 then (List.hd f)^"*x**3+"^(List.nth f 1)^"*x**2+"^
      (List.nth f 2)^"*x+"^(List.nth f 3)
    else raise Illegal

(* POST: Uses gnuplot to create a plot. Saves the image as a .png file in the
 * user's current directory.
 * PRE: series is a list of gnuplot Series. series can be empty.
 *  x1, x2, y1, and y2 are floats. (x1,x2) is the xrange of the plot.
 *  (y1,y2) is the yrange of the plot. These must be non-empty. *)
let create_plot series (x1,x2) (y1,y2) =
  let filename = inc_fname () in
  let gp = Gp.create () in
  Gp.plot_many gp
    ~range:(Range.XY (x1, x2, y1, y2))
    ~output:(Output.create (`Png filename))
    ((Series.lines_func "0" ~color:`Black)::series);
  Gp.close gp;
  filename

(* POST: Produces a .png image plot given a dataset input.
 * PRE: data is a float * float list. x1,x2,y1,y2 are floats. *)
let plot_data data (x1,x2) (y1,y2) =
  let series = [Series.points_xy data ~color:`Blue] in
  create_plot series (x1,x2) (y1,y2)

(* POST: Produces a .png image plot given a function input.
 * PRE: funct is a float list. x1,x2,y1,y2 are floats. *)
let plot_funct funct (x1,x2) (y1,y2) =
  let equation = create_funct funct in
  let series = [Series.lines_func equation ~color:`Blue] in
  create_plot series (x1,x2) (y1,y2)

(* POST: Produces a .png image plot given a dataset and function input.
 * PRE: data is a float * float list. funct is a float list.
 *  x1,x2,y1,y2 are floats. *)
let plot_model funct data (x1,x2) (y1,y2) =
  let equation = create_funct funct in
  let series = [Series.lines_func equation ~color:`Blue;
                Series.points_xy data ~color:`Blue] in
  create_plot series (x1,x2) (y1,y2)

(* POST: Produces a .png image plot given a dataset input. x and y range are
 * set to be 1 greater/less than the max and min of the datapoints in data.
 * PRE: data is a float * float list. *)
let plot_res data =
  let xs = List.split data |> fst in
  let ys = List.split data |> snd in
  let x1 = max_list xs +. 1. in
  let x2 = min_list xs -. 1. in
  let y1 = max_list ys +. 1. in
  let y2 = min_list ys -. 1. in
  let series = [Series.points_xy data ~color:`Blue] in
  create_plot series (x1,x2) (y1,y2)