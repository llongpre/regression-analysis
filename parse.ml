open Eval
open Plot
open Stats

type funct = float list
type dataset = (float * float) list
type state = {message: string; funct: funct; data: dataset}
type command =
  | LinReg      of dataset
  | PolyReg     of int * dataset
  | GraphFunct  of funct * (float * float) * (float * float)
  | GraphData   of dataset * (float * float) * (float * float)
  | GraphModel  of funct * dataset * (float * float) * (float * float)
  | PlotRes     of dataset
  | PlotResf    of funct * dataset
  | RSqr        of dataset
  | Model       of funct * int * float * (float * float)
  | Poisson     of float * int
  | PoissonC    of float * int
  | Binomial    of int * float * int
  | BinomialC   of int * float * int
  | Variance    of dataset
  | Variancef   of funct * dataset
  | StdErr      of dataset
  | StdErrf     of funct * dataset
  | Stddev      of dataset
  | Stddevf     of funct * dataset
  | Chisq       of dataset
  | Chisqf      of funct * dataset
  | Unknown     of string

(* The following variables contain string messages to be printed to the user
 * in case of errors. *)
let lr_err = "Invalid linreg input. Enter \"linreg help\" for detailed instructions."
let pr_err = "Invalid polyreg input. Enter \"polyreg help\" for detailed instructions."
let gf_err = "Invalid graphf input. Enter \"graphf help\" for detailed instructions."
let gd_err = "Invalid graphd input. Enter \"graphd help\" for detailed instructions."
let gm_err = "Invalid graphm input. Enter \"graphm help\" for detailed instructions."
let plr_err = "Invalid plotres input. Enter \"plotres help\" for detailed instructions."
let rs_err = "Invalid rsquared input. Enter \"rsquared help\" for detailed instructions."
let m_err = "Invalid model input. Enter \"model help\" for detailed instructions."
let p_err = "Invalid poisson input. Enter \"poisson help\" for detailed instructions."
let pc_err = "Invalid poissonc input. Enter \"poissonc help\" for detailed instructions."
let b_err = "Invalid binomial input. Enter \"binomial help\" for detailed instructions."
let bc_err = "Invalid binomialc input. Enter \"binomialc help\" for detailed instructions."
let v_err = "Invalid variance input. Enter \"variance help\" for detailed instructions."
let se_err = "Invalid stderr input. Enter \"stderr help\" for detailed instructions."
let sd_err = "Invalid stddev input. Enter \"stddev help\" for detailed instructions."
let c_err = "Invalid chisqr input. Enter \"chisqr help\" for detailed instructions."

(* Helper function for parse_data_from_file.
 * PRE: lst is a list of length 2 containing strings.
 * POST: Returns a float tuple of the contents of the list. *)
let list_to_tuple lst =
  match lst with
  | x::y::[] -> float_of_string x, float_of_string y
  | _ -> failwith "invalid csv format"

(* PRE: file is a string, the file name of a csv in the current directory,
 * contains two columns of x and y values, respectively.
 * POST: Returns tuple list of floats in the format [(x,y);...] *)
let parse_data_from_file file = Csv.load file |> List.map list_to_tuple

(*POST: [add_coeff lst] is a list of strings that alters any string ""
* to "1" in lst to accommodate for the lack of written coefficient
* PRE: lst is a list of strings
*)
let rec add_coeff lst =
  match lst with
  | h::t when h = "" -> "1"::(add_coeff t)
  | h::t when h = "-" -> "-1"::(add_coeff t)
  | h::t -> h::(add_coeff t)
  | [] -> []

(*POST: [float_lst lst] is a float list that contains floats
* of all strings in lst
* PRE: lst is a list of strings
*)
let float_list lst =
  List.rev_map (fun x -> float_of_string x) (List.rev lst)

(*POST: [parse_reg lst poly] is a (float * float) list representing the list
* of (x,y) tuples from the given csv file
* PRE: lst is a list of strings and poly is a boolean representing whether
* or not the user wants a linear or polynomial regression
*)
let parse_reg lst poly =
  if poly=false then
    LinReg (parse_data_from_file (List.hd lst))
  else let ord = int_of_string (List.hd lst) in if ord = 2 || ord = 3 || ord = 1 then 
    PolyReg (ord,parse_data_from_file (List.nth lst 1))
  else raise Illegal

(*POST: [find str] is an int designating where in a string there is a caret
* if there is one, returning 0 if there is no caret
* PRE: str is a string
*)
let find str = if String.contains str '^' = true then
  (String.index str '^')+1 else 0

(*POST: [order str] is an int designating the order of a function
* PRE: str is a string
*)
let order str = if find str != 0 then int_of_string (String.sub str
  (find str) 1) else if String.contains str 'x' then 1 else 0

(*POST: [cat str cha] is a string that contains the substring of
* of str containing everything after cha, not inclusive
* PRE: str is a string and cha is a char
*)
let cat str cha =
  String.sub str ((String.index str cha)+1)
      ((String.length str)-(String.index str cha)-1)

(*POST: [fill func lst ord] is a string list containing all coefficients
* of func, inserting 0s for absent variables
* PRE: func is a string that goes only up to a function order of 3,
* lst is a string list containing the current coefficients, and ord is
* an int representing the order of func
*)
let fill func lst ord =
  let rslt = [(List.hd lst)] in
  let rec filler func lst ord (acc:string list) =
    match ord with
    | 0 -> if String.contains func '+' || String.contains func '-' then
      (acc@[(List.nth lst ((List.length lst)-1))]) else (acc@["0"])
    | 1 -> if String.contains func 'x' then filler (cat func 'x')
      lst 0 (acc@[(List.nth lst (if String.length (cat func 'x')>0 then
        ((List.length lst)-2) else ((List.length lst)-1)))])
      else filler func lst 0 (acc@["0"])
    | 2 -> if String.contains func '^' then filler (cat func '^')
      lst 1 (acc@[List.nth lst 1]) else filler func lst 1 (acc@["0"])
    | _ -> acc
    in
    match ord with
    | 0 -> rslt
    | 1 -> filler (cat func 'x') lst 0 rslt
    | 2 -> filler (cat func '^') lst 1 rslt
    | 3 -> filler (cat func '^') lst 2 rslt
    | _ -> rslt

(*POST: [fixneg func] is a string that replaces all intended subtractions
* with +- in order to properly parse the string when splitting it up and raises
* an exception if the function isn't put in properly
* PRE: func is a string
*)
let fixneg func =
  let fixx = Str.global_replace (Str.regexp
    "[x]\\-") "x+-" func in
  let fixeq = Str.global_replace (Str.regexp
    "\\=\\-\\") "=+-" fixx in
  let fixtwo = Str.global_replace (Str.regexp
    "[2]\\-") "2+-" fixeq in
  let fixthree = Str.global_replace (Str.regexp
    "[3]\\-") "3+-" fixtwo in 
  let rec ord_check func =
    match func with 
    | "" -> true  
    | x when (String.sub x 0 1)="^" -> if order (cat func '^') >= order func then
      false else ord_check (cat x (String.get func 0))
    | x -> ord_check (cat x (String.get func 0))
     in 
  if ord_check func = true then fixthree else raise Illegal

(*POST: [parse_model_func f lst] is a (funct * int * float * (float * float))
* tuple that represents the case when the user is using the previous given
* function with the given number of points, standard error, xmin, and xmax
* PRE: f is a funct and lst is a string containing a function or the word
* function, the desired number of data points, the desired standard error,
* and bounds for the x values min and max in that order
*)
let parse_model_func f lst =
  let n = List.nth lst 1 |> int_of_string in
  let stderr = List.nth lst 2 |> float_of_string in
  let xmin = List.nth lst 3 |> float_of_string in
  let xmax = List.nth lst 4 |> float_of_string in
  Model(f,n,stderr,(xmin,xmax))

(*POST: [parse_model_func f lst] is a (funct * int * float * (float * float))
* that takes in a state and a list and outputs a model command of the given
* line, number of data points, standard error, and xmin and xmax
* PRE: st is a state used for when the user wants to use a previous funct
* lst is a string containing a function or the word
* function, the desired number of data points, the desired standard error,
* and bounds for the x values min and max in that order.
* There are no spaces in the function itself.
*)
let parse_model st lst =
  if List.hd lst = "function" then parse_model_func st.funct lst else
  let n = List.nth lst 1 |> int_of_string in
  let stderr = List.nth lst 2 |> float_of_string in
  let xmin = List.nth lst 3 |> float_of_string in
  let xmax = List.nth lst 4 |> float_of_string in
  let func = Str.split (Str.regexp
    "[y]\\=\\|[x]\\^[0-9]*\\+\\|[x]\\+\\|[x]\\^[0-9]*\\|[x]")
    (fixneg (List.hd lst)) in
  let func2 = add_coeff func in
  let func3 = fill (List.hd lst) func2 (order (List.hd lst)) in
  let line = float_list func3 in
  Model(line,n,stderr,(xmin,xmax))

(*POST: [parse_graph_func st lst] is a funct * (float * float) *
* (float * float) that outputs a function graphing command using either
* the given line or the previously used line and tuples of x and y values
* designating the bounds of the graph
* PRE: st is a state used for when the user wants to use a previous funct
* and lst is a string containing a function (or the word function for
* previous functions), an xmin, xmax, ymin, and ymax in that order all
* separated by spaces. There are no spaces in the function itself.
*)
let parse_graph_func st lst =
  let xtup = (float_of_string (List.nth lst 1),float_of_string
    (List.nth lst 2)) in
  let ytup = (float_of_string (List.nth lst 3),float_of_string
    (List.nth lst 4)) in
  if List.hd lst = "function" then GraphFunct(st.funct,xtup,ytup)
  else
  let func = Str.split (Str.regexp
    "[y]\\=\\|[x]\\^[0-9]*\\+\\|[x]\\+\\|[x]\\^[0-9]*\\|[x]")
    (fixneg (List.hd lst)) in
  let func2 = add_coeff func in
  let func3 = fill (List.hd lst) func2 (order (List.hd lst)) in
  let line = float_list func3 in
  if fst xtup >= snd xtup || fst ytup >= snd ytup then raise Illegal else
  GraphFunct (line,xtup,ytup)

(*POST: [parse_graph_data st lst] is a funct * (float * float) *
* (float * float) that outputs a function graphing command using either
* the given line or the previously used line and tuples of x and y values
* designating the bounds of the graph
* PRE: st is a state used for when the user wants to use a previous dataset
* and lst is a string containing a file name for a csv file (or the word
* dataset for previous datasets), an xmin, xmax, ymin, and ymax in that
* order all separated by spaces.
*)
let parse_graph_data st lst =
  let data = if List.hd lst = "dataset" then st.data
    else parse_data_from_file (List.hd lst) in
  let xtup = (float_of_string (List.nth lst 1),float_of_string
    (List.nth lst 2)) in
  let ytup = (float_of_string (List.nth lst 3),float_of_string
    (List.nth lst 4)) in
  if fst xtup >= snd xtup || fst ytup >= snd ytup then raise Illegal else
  GraphData(data,xtup,ytup)

(*POST: [parse_graph_model st lst] is a funct * dataset * (float * float) *
* (float * float) that outputs a model graphing command using either
* the given line or the previously used line and tuples of x and y values
* designating the bounds of the graph
* PRE: st is a state used for when the user wants to use a previous dataset
* or previous function and lst is a string list containing a function
* (or the word function for previous functions), a file name for a csv file
* (or the word dataset for previous datasets), an xmin, xmax, ymin, and ymax
* in that order all separated by spaces. There are no spaces in the function
* itself.
*)
let parse_graph_model st lst =
  let data = if List.nth lst 1 = "dataset" then st.data else
    parse_data_from_file (List.nth lst 1) in
  let xtup = (float_of_string (List.nth lst 2),float_of_string
    (List.nth lst 3)) in
  let ytup = (float_of_string (List.nth lst 4),float_of_string
    (List.nth lst 5)) in
  if List.hd lst = "function" then GraphModel(st.funct,data,xtup,ytup)
  else
  let func = Str.split (Str.regexp
    "[y]\\=\\|[x]\\^[0-9]*\\+\\|[x]\\+\\|[x]\\^[0-9]*\\|[x]")
    (fixneg (List.hd lst)) in
  let func2 = add_coeff func in
  let func3 = fill (List.hd lst) func2 (order (List.hd lst)) in
  let line = float_list func3 in
  if fst xtup >= snd xtup || fst ytup >= snd ytup then raise Illegal else
  GraphModel(line,data,xtup,ytup)

(*POST: [parse_rsqr lst] is a dataset containg a list of tuples
* representing the x y pairs from the given csv file
* PRE: lst is a string list where the head of the string is
* the name of a csv file
*)
let parse_rsqr lst =
  RSqr (parse_data_from_file (List.hd lst))

(*POST: [parse_residuals lst] is a dataset containg a list of tuples
* representing the x y pairs from the given csv file
* PRE: lst is a string list where the head of the string is
* the name of a csv file
*)
let parse_plotres st lst =
  if List.length lst = 1 then
    if List.hd lst = "dataset" then PlotRes st.data
    else PlotRes (parse_data_from_file (List.hd lst))
  else
  let data = if List.nth lst 1 = "dataset" then st.data
    else parse_data_from_file (List.nth lst 1) in
  if List.hd lst = "function" then PlotResf(st.funct,data)
  else
  let func = Str.split (Str.regexp
    "[y]\\=\\|[x]\\^[0-9]*\\+\\|[x]\\+\\|[x]\\^[0-9]*\\|[x]")
    (fixneg (List.hd lst)) in
  let func2 = add_coeff func in
  let func3 = fill (List.hd lst) func2 (order (List.hd lst)) in
  let line = float_list func3 in
  PlotResf (line, data)

(*POST: [parse_stderr lst] is either a dataset or a funct * dataset
* depending on if the user inputs a function as well as a dataset.
* PRE: st is a state used for when the user wants to use a previous dataset
* or previous function and lst is a string list containing a function
* (or the word function for previous functions) and a file name for a csv file
* (or the word dataset for previous datasets) in that order all separated
* by spaces. There are no spaces in the function itself. The function
* argument is optional.
*)
let parse_stderr st lst =
  if List.length lst = 1 then
    if List.hd lst = "dataset" then StdErr st.data
    else StdErr (parse_data_from_file (List.hd lst))
  else
  let data = if List.nth lst 1 = "dataset" then st.data
    else parse_data_from_file (List.nth lst 1) in
  if List.hd lst = "function" then StdErrf(st.funct,data)
  else
  let func = Str.split (Str.regexp
    "[y]\\=\\|[x]\\^[0-9]*\\+\\|[x]\\+\\|[x]\\^[0-9]*\\|[x]")
    (fixneg (List.hd lst)) in
  let func2 = add_coeff func in
  let func3 = fill (List.hd lst) func2 (order (List.hd lst)) in
  let line = float_list func3 in
  StdErrf (line, data)

(*POST: [parse_poisson lst c] is a Poisson command that contains
* the inputted mean and variable, changing depending on whether
* the user wants the calculation to be cumulative or not
* PRE: lst is a string list containing a mean and a variable in that
* order. c is a boolean that states whether the input was meant to be
* cumulative or not.
*)
let parse_poisson lst c =
  let mean = List.hd lst |> float_of_string in
  let var = List.nth lst 1 |> int_of_string in
  if c = false then
    Poisson (mean,var)
  else
    PoissonC (mean,var)

(*POST: [parse_binom lst c] is a binomial command that contains
* the inputted number of tests, probability of an event happening,
*  and variable, changing depending on whether
* the user wants the calculation to be cumulative or not
* PRE: lst is a string list containing number of tests, probability
* of an event happening, and a variable in that
* order. c is a boolean that states whether the input was meant to be
* cumulative or not.
*)
let parse_binom lst c =
  let num = List.hd lst |> int_of_string in
  let prob = List.nth lst 1 |> float_of_string in
  let var = List.nth lst 2 |> int_of_string in
  if c = false then
    Binomial (num,prob,var)
  else
    BinomialC (num,prob,var)

(*POST: [parse_var st lst sq] is a variance or standard deviation
* command that outputs either a dataset or a function and a dataset
* depending on if the user wants to use a function in the calculation.
* PRE: st is a state used for when the user wants to use a previous dataset
* or previous function and lst is a string list containing a function
* (or the word function for previous functions) and a file name for a csv file
* (or the word dataset for previous datasets) in that order all separated
* by spaces. There are no spaces in the function itself. The function
* argument is optional. sq is a boolean that tells whether the command was
* for variance or standard deviation
*)
let parse_var st lst sq =
  if List.length lst = 1 then
    let data = if List.hd lst = "dataset" then st.data
      else parse_data_from_file (List.hd lst) in
    if sq = false then
      Variance(data)
    else
      Stddev(data)
  else
  let data = if List.nth lst 1 = "dataset" then st.data
    else parse_data_from_file (List.nth lst 1) in
  if List.hd lst = "function" then
    if sq = false then
      Variancef(st.funct,data)
    else
      Stddevf(st.funct,data)
  else
  let func = Str.split (Str.regexp
    "[y]\\=\\|[x]\\^[0-9]*\\+\\|[x]\\+\\|[x]\\^[0-9]*\\|[x]")
    (fixneg (List.hd lst)) in
  let func2 = add_coeff func in
  let func3 = fill (List.hd lst) func2 (order (List.hd lst)) in
  let line = float_list func3 in
    if sq = false then
      Variancef(line,data)
    else
      Stddevf(line,data)

(*POST: [parse_chisq st lst] is a chi squared command
* that outputs either a dataset or a function and a dataset
* depending on if the user wants to use a function in the calculation.
* PRE: st is a state used for when the user wants to use a previous dataset
* or previous function and lst is a string list containing a function
* (or the word function for previous functions) and a file name for a csv file
* (or the word dataset for previous datasets) in that order all separated
* by spaces. There are no spaces in the function itself. The function
* argument is optional.
*)
let parse_chisq st lst =
  if List.length lst = 1 then
    if List.hd lst = "dataset" then Chisq st.data
    else Chisq (parse_data_from_file (List.hd lst))
  else
  let data = if List.nth lst 1 = "dataset" then st.data
    else parse_data_from_file (List.nth lst 1) in
  if List.hd lst = "function" then Chisqf(st.funct,data)
  else
  let func = Str.split (Str.regexp
    "[y]\\=\\|[x]\\^[0-9]*\\+\\|[x]\\+\\|[x]\\^[0-9]*\\|[x]")
    (fixneg (List.hd lst)) in
  let func2 = add_coeff func in
  let func3 = fill (List.hd lst) func2 (order (List.hd lst)) in
  let line = float_list func3 in
  Chisqf (line, data)

(* PRE: st has type state. cmd is a string.
 * POST: Parses a string input by the user into variant type command.
 * Cases of invalid commands produce Unknown which contains the corresponding
 * error message to output to the user. *)
let parse_command st cmd =
  let lst_cmd = Str.split (Str.regexp "[\t\r ]+") cmd in
  if lst_cmd = [] then Unknown "Command not recognized, please try again." else
  let cmd_type = String.lowercase_ascii (List.hd lst_cmd) in
  match cmd_type with
  | "linreg"  | "lr"  ->
    if List.nth lst_cmd 1 = "dataset" then LinReg st.data
    else (try parse_reg (List.tl lst_cmd) false with _ -> Unknown lr_err)
  | "polyreg" | "pr"  ->
    if List.nth lst_cmd 2 = "dataset" then
    let n = List.nth lst_cmd 1 |> int_of_string in PolyReg(n,st.data)
    else (try parse_reg (List.tl lst_cmd) true with _ -> Unknown pr_err)
  | "model"   | "mod" ->
    (try parse_model st (List.tl lst_cmd) with _ -> Unknown m_err)
  | "graphf"  | "gf"  ->
    (try parse_graph_func st (List.tl lst_cmd) with _ -> Unknown gf_err)
  | "graphd"  | "gd"  ->
    (try parse_graph_data st (List.tl lst_cmd) with _ -> Unknown gd_err)
  | "graphm"  | "gm"  ->
    (try parse_graph_model st (List.tl lst_cmd) with _ -> Unknown gm_err)
  | "plotres"         ->
    if List.nth lst_cmd 1 = "dataset" then PlotRes st.data
    else (try parse_plotres st (List.tl lst_cmd) with _ -> Unknown plr_err)
  | "poisson"         ->
    (try parse_poisson (List.tl lst_cmd) false with _ -> Unknown p_err)
  | "poissonc"        ->
    (try parse_poisson (List.tl lst_cmd) true with _ -> Unknown pc_err)
  | "binomial"        ->
    (try parse_binom (List.tl lst_cmd) false with _ -> Unknown b_err)
  | "binomialc"       ->
    (try parse_binom (List.tl lst_cmd) true with _ -> Unknown bc_err)
  | "rsquared" | "rsq"  | "r2"  ->
    if List.nth lst_cmd 1 = "dataset" then RSqr st.data
    else (try parse_rsqr (List.tl lst_cmd) with _ -> Unknown rs_err)
  | "standarderr" | "err"     ->
    (try parse_stderr st (List.tl lst_cmd) with _ -> Unknown se_err)
  | "variance"    | "var"     ->
    (try parse_var st (List.tl lst_cmd) false with _ -> Unknown v_err)
  | "stddev"                  ->
    (try parse_var st (List.tl lst_cmd) true with _ -> Unknown sd_err)
  | "chisquared"  | "chisq"   ->
    (try parse_chisq st (List.tl lst_cmd) with _ -> Unknown c_err)
  | _ -> Unknown "Command not recognized, please try again."

(* PRE: st has type state. cmd has type string.
 * POST: Evaluates user input string command. Returns an updated state.
 * Updates funct and data fields when inputs contain a function or dataset.
 * Updates funct and data fields when a new function or dataset is computed
 * by the command.
 * Updates message to be output to user based on command type, or error
 * message if command is invalid. *)
let eval st cmd =
  match parse_command st cmd with
  | LinReg data ->
    (try let f = lin_reg data in
    let m = "y = "^(List.hd f |> string_of_float)^" x + "^
      (List.nth f 1 |> string_of_float) in
    {message=m; funct=f; data=data}
  with _ -> {st with message=lr_err})
  | PolyReg(n,data) ->
    (try
    if n=1 then
    let f = lin_reg data in
    let m = "y = "^(List.hd f |> string_of_float)^" x + "^
      (List.nth f 1 |> string_of_float) in
    {message=m; funct=f; data=data}
    else if n=2 then
    let f = second_poly_regression data in
    let m = "y = "^(List.hd f |> string_of_float)^" x^2 + "^
      (List.nth f 1 |> string_of_float)^" x + "^
      (List.nth f 2 |> string_of_float) in
    {message=m; funct=f; data=data}
    else if n=3 then
    let f = third_poly_regression data in
    let m = "y = "^(List.hd f |> string_of_float)^" x^3 + "^
      (List.nth f 1 |> string_of_float)^" x^2 + "^
      (List.nth f 2 |> string_of_float)^" x + "^
      (List.nth f 3 |> string_of_float) in
    {message=m; funct=f; data=data}
    else {st with message=pr_err}
    with _ -> {st with message=pr_err})
  | Poisson(mean,var) ->
    (try let p = poisson mean var in
    {st with message="Probability = "^(string_of_float p)}
    with _ -> {st with message=p_err})
  | PoissonC(mean,var) ->
    (try let p = poissonc mean var in
    {st with message="Probability = "^(string_of_float p)}
    with _ -> {st with message=pc_err})
  | Binomial(n,p,x) ->
    (try let p = binomial n p x in
    {st with message="Probability = "^(string_of_float p)}
    with _ -> {st with message=b_err})
  | BinomialC(n,p,x) ->
    (try let p = binomialc n p x in
    {st with message="Probability = "^(string_of_float p)}
    with _ -> {st with message=bc_err})
  | RSqr data ->
    (try let r = rsq data in
    {st with message="r^2 = "^(string_of_float r); data=data}
    with _ -> {st with message=rs_err})
  | StdErr data ->
    (try let r = stderror data in
    {st with message="Standard Error = "^(string_of_float r); data=data}
    with _ -> {st with message=se_err})
  | StdErrf(funct,data) ->
    (try let r = stderror ~func:funct data in
    {message="Standard Error = "^(string_of_float r); data=data; funct=funct}
    with _ -> {st with message=se_err})
  | Variance data ->
    (try let v = variance data in
    {st with message="Variance = "^(string_of_float v); data=data}
    with _ -> {st with message=v_err})
  | Variancef(funct,data) ->
    (try let v = variance ~func:funct data in
    {message="Variance = "^(string_of_float v); data=data; funct=funct}
    with _ -> {st with message=v_err})
  | Stddev data ->
    (try let d = stddev data in
    {st with message="Standard Deviation = "^(string_of_float d); data=data}
    with _ -> {st with message=sd_err})
  | Stddevf(funct,data) ->
    (try let d = stddev ~func:funct data in
    {message="Standard Deviation = "^(string_of_float d); data=data;
    funct=funct}
    with _ -> {st with message=sd_err})
  | Chisq data ->
    (try let c = chisq data in
    {st with message="Chi-squared = "^(string_of_float c); data=data}
    with _ -> {st with message=c_err})
  | Chisqf(f,data) ->
    (try let c = chisq ~func:f data in
    {message="Chi-squared = "^(string_of_float c); data=data; funct=f}
    with _ -> {st with message=c_err})
  | GraphFunct(f,(x1,x2),(y1,y2)) ->
    (try let fname = plot_funct f (x1,x2) (y1,y2) in
    {st with message="Graph has been saved as "^fname; funct=f}
    with _ -> {st with message=gf_err})
  | GraphData(d,(x1,x2),(y1,y2)) ->
    (try let fname = plot_data d (x1,x2) (y1,y2) in
    {st with message="Graph has been saved as "^fname; data=d}
    with _ -> {st with message=gd_err})
  | GraphModel(f,d,(x1,x2),(y1,y2)) ->
    (try let fname = plot_model f d (x1,x2) (y1,y2) in
    {message="Graph has been saved as "^fname; data=d; funct=f}
    with _ -> {st with message=gm_err})
  | PlotRes data ->
    (try 
      let residuals = residuals_plot data in 
      let fname = plot_res residuals in
    {st with message="Residual plot has been saved as "^fname; data=data}
    with _ -> {st with message=plr_err})
  | PlotResf (f,data) ->
    (try 
      let residuals = residuals_plot ~func:f data in
      let fname = plot_res residuals in
    {st with message="Residual plot has been saved as "^fname;funct=f;data=data}
    with _ -> {st with message=plr_err})
  | Model(f,n,s,(xmin,xmax)) ->
    (try let result = create_model f n s xmin xmax in
      let fname = fst result in
      let data = snd result in
    {st with message="Modeled dataset has been saved as "^fname; funct=f; data=data}
    with _ -> {st with message=m_err})
  | Unknown str -> {st with message=str}

