
type funct = float list
type dataset = (float * float) list

exception Illegal

(* POST: [sum lst] is a float that is sum of the elements [lst].
 * PRE: [lst] is a float list of a finite length.
 *)
let sum lst = List.fold_left (+.) 0. lst

(* POST: [shuffle lst] is the list [lst] with its elements randomly
 * randomly permutated.
 * PRE: [lst] is a list of a finite length.
 *)
 let shuffle lst =
	let tags = List.map (fun x -> Random.float 10.) lst in
	let paired = List.combine tags lst in
	let sorted_pairs = List.sort Pervasives.compare paired in
	snd (List.split sorted_pairs)

(* POST: [lin_reg_stats dset] is a float list of length 3 representing the
 * result of a linear regression on [dset]. The first element is the slope of
 * the linear model; the second element is the y-intercept of the linear model;
 * and the third element is the R squared value of the linear model.
 * PRE: [dset] is a (float * float) list of elements in the form
 * (x coordinate, y coordinate) and of a finite length.
 *)
let lin_reg_stats dset =
	if List.length dset < 2 then raise Illegal else
	let n = float_of_int (List.length dset) in
	let xylst = List.split dset in
	let xlst = fst xylst in
	let ylst = snd xylst in
	let xsum = sum xlst in
	let x2sum = sum (List.map (fun x -> x*.x) xlst) in
	let ysum = sum ylst in
	let y2sum = sum (List.map (fun x -> x*.x) ylst) in
	let xysum = sum (List.map (fun (x,y) -> x*.y) dset) in

	let alpha = (((n *. xysum) -. (xsum *. ysum)) /.
	 ((n *. x2sum) -. (xsum *. xsum))) in

	let beta = (((x2sum *. ysum) -. (xsum *. xysum)) /.
	 ((n *. x2sum) -. (xsum *. xsum))) in

	let r = ((n *. xysum) -. (xsum *. ysum)) /. (sqrt (((n *. x2sum)
	  -. (xsum ** 2.)) *. ((n*. y2sum) -. (ysum ** 2.)))) in

	let rsq = r**2. in

	[alpha; beta; rsq]

(* POST: [lin_reg dset] is a float list of length 2 representing the result of
 * linear regression on [dset]. The first element is the slope of the linear
 * model; the second element is the y-intercept of the linear model.
 * PRE: [dset] is a (float * float) list of elements in the form
 * (x coordinate, y coordinate).
 *)
let lin_reg dset =
	let abr = lin_reg_stats dset in
	if List.length abr < 3 then raise Illegal
	else (List.nth abr 0)::(List.nth abr 1)::[]

(*POST: [nth matr i j] is the jth element in the ith list
	using one based indexing (instead of zero based),*)
(*PRE: matr is a list of lists and i and j are ints*)
let nth matr i j =
	List.nth (List.nth matr (i-1)) (j-1)


(*POST: [calc_det matr] is determinant of matrix as a float*)
(*PRE: input is a list of lists with 3 lists with 
	3 elements in each list*)
let calc_det matr =
	(nth matr 1 1)*.(nth matr 2 2)*.(nth matr 3 3)+.
	(nth matr 1 2)*.(nth matr 2 3)*.(nth matr 3 1)+.
	(nth matr 1 3)*.(nth matr 2 1)*.(nth matr 3 2)-.
	((nth matr 3 1)*.(nth matr 2 2)*.(nth matr 1 3)+.
	(nth matr 3 2)*.(nth matr 2 3)*.(nth matr 1 1)+.
	(nth matr 3 3)*.(nth matr 2 1)*.(nth matr 1 2))


(*POST: [calc_det_four matr] is determinant of matrix as a float*)
(*PRE: input is a list of lists with 4 lists with 
	4 elements in each list*)
let calc_det_four matr =
	(nth matr 1 1)*.(nth matr 2 2)*.(nth matr 3 3)*.(nth matr 4 4)+.
	(nth matr 1 1)*.(nth matr 2 3)*.(nth matr 3 4)*.(nth matr 4 2)+.
	(nth matr 1 1)*.(nth matr 2 4)*.(nth matr 3 2)*.(nth matr 4 3)+.
	(nth matr 1 2)*.(nth matr 2 1)*.(nth matr 3 4)*.(nth matr 4 3)+.
	(nth matr 1 2)*.(nth matr 2 3)*.(nth matr 3 1)*.(nth matr 4 4)+.
	(nth matr 1 2)*.(nth matr 2 4)*.(nth matr 3 3)*.(nth matr 4 1)+.
	(nth matr 1 3)*.(nth matr 2 1)*.(nth matr 3 2)*.(nth matr 4 4)+.
	(nth matr 1 3)*.(nth matr 2 2)*.(nth matr 3 4)*.(nth matr 4 1)+.
	(nth matr 1 3)*.(nth matr 2 4)*.(nth matr 3 1)*.(nth matr 4 2)+.
	(nth matr 1 4)*.(nth matr 2 1)*.(nth matr 3 3)*.(nth matr 4 2)+.
	(nth matr 1 4)*.(nth matr 2 2)*.(nth matr 3 1)*.(nth matr 4 3)+.
	(nth matr 1 4)*.(nth matr 2 3)*.(nth matr 3 2)*.(nth matr 4 1)-.
	((nth matr 1 1)*.(nth matr 2 2)*.(nth matr 3 4)*.(nth matr 4 3)+.
	(nth matr 1 1)*.(nth matr 2 3)*.(nth matr 3 2)*.(nth matr 4 4)+.
	(nth matr 1 1)*.(nth matr 2 4)*.(nth matr 3 3)*.(nth matr 4 2)+.
	(nth matr 1 2)*.(nth matr 2 1)*.(nth matr 3 3)*.(nth matr 4 4)+.
	(nth matr 1 2)*.(nth matr 2 3)*.(nth matr 3 4)*.(nth matr 4 1)+.
	(nth matr 1 2)*.(nth matr 2 4)*.(nth matr 3 1)*.(nth matr 4 3)+.
	(nth matr 1 3)*.(nth matr 2 1)*.(nth matr 3 4)*.(nth matr 4 2)+.
	(nth matr 1 3)*.(nth matr 2 2)*.(nth matr 3 1)*.(nth matr 4 4)+.
	(nth matr 1 3)*.(nth matr 2 4)*.(nth matr 3 2)*.(nth matr 4 1)+.
	(nth matr 1 4)*.(nth matr 2 1)*.(nth matr 3 2)*.(nth matr 4 3)+.
	(nth matr 1 4)*.(nth matr 2 2)*.(nth matr 3 3)*.(nth matr 4 1)+.
	(nth matr 1 4)*.(nth matr 2 3)*.(nth matr 3 1)*.(nth matr 4 2))


(*POST: [calc_transpose matr] is a float list list 
	transpose of matr as a 3 lists with 3 elements in each list*)
(*PRE: input is a list of lists with 3 lists with 
	3 elements in each list*)
let calc_transpose matr =
	[[nth matr 1 1; nth matr 2 1; nth matr 3 1];
	[nth matr 1 2; nth matr 2 2; nth matr 3 2];
	[nth matr 1 3; nth matr 2 3; nth matr 3 3]]


(*POST: [calc_transpose_four matr] is a float list list 
	transpose of matr as a 4 lists with 4 elements in each list*)
(*PRE: input is a list of lists with 4 lists with 
	4 elements in each list*)
let calc_transpose_four matr =
	[[nth matr 1 1; nth matr 2 1; nth matr 3 1; nth matr 4 1];
	[nth matr 1 2; nth matr 2 2; nth matr 3 2; nth matr 4 2];
	[nth matr 1 3; nth matr 2 3; nth matr 3 3; nth matr 4 3];
	[nth matr 1 4; nth matr 2 4; nth matr 3 4; nth matr 4 4]]

(*POST: [calc_invert4 matr] is a float list list of inversion of matrix with
 	4 lists with 4 elements in each list*)
(*PRE: input is a list of lists with 4 lists with 
	4 elements in each list*)
let calc_invert4 matr = 
	let adj_matr =
	[[calc_det [[(nth matr 2 2);(nth matr 2 3);(nth matr 2 4)];
	[(nth matr 3 2);(nth matr 3 3);(nth matr 3 4)];
	[(nth matr 4 2);(nth matr 4 3);(nth matr 4 4)]];
	calc_det [[-.(nth matr 2 1);-.(nth matr 2 3);-.(nth matr 2 4)];
	[-.(nth matr 3 1);-.(nth matr 3 3);-.(nth matr 3 4)];
	[-.(nth matr 4 1);-.(nth matr 4 3);-.(nth matr 4 4)]];
	calc_det [[(nth matr 2 1);(nth matr 2 2);(nth matr 2 4)];
	[(nth matr 3 1);(nth matr 3 2);(nth matr 3 4)];
	[(nth matr 4 1);(nth matr 4 2);(nth matr 4 4)]];
	calc_det [[-.(nth matr 2 1);-.(nth matr 2 2);-.(nth matr 2 3)];
	[-.(nth matr 3 1);-.(nth matr 3 2);-.(nth matr 3 3)];
	[-.(nth matr 4 1);-.(nth matr 4 2);-.(nth matr 4 3)]];];

	[calc_det [[-.(nth matr 1 2);-.(nth matr 1 3);-.(nth matr 1 4)];
	[-.(nth matr 3 2);-.(nth matr 3 3);-.(nth matr 3 4)];
	[-.(nth matr 4 2);-.(nth matr 4 3);-.(nth matr 4 4)]];
	calc_det [[(nth matr 1 1);(nth matr 1 3);(nth matr 1 4)];
	[(nth matr 3 1);(nth matr 3 3);(nth matr 3 4)];
	[(nth matr 4 1);(nth matr 4 3);(nth matr 4 4)]];
	calc_det [[-.(nth matr 1 1);-.(nth matr 1 2);-.(nth matr 1 4)];
	[-.(nth matr 3 1);-.(nth matr 3 2);-.(nth matr 3 4)];
	[-.(nth matr 4 1);-.(nth matr 4 2);-.(nth matr 4 4)]];
	calc_det [[(nth matr 1 1);(nth matr 1 2);(nth matr 1 3)];
	[(nth matr 3 1);(nth matr 3 2);(nth matr 3 3)];
	[(nth matr 4 1);(nth matr 4 2);(nth matr 4 3)]];];

	[calc_det [[(nth matr 1 2);(nth matr 1 3);(nth matr 1 4)];
	[(nth matr 2 2);(nth matr 2 3);(nth matr 2 4)];
	[(nth matr 4 2);(nth matr 4 3);(nth matr 4 4)]];
	calc_det [[-.(nth matr 1 1);-.(nth matr 1 3);-.(nth matr 1 4)];
	[-.(nth matr 2 1);-.(nth matr 2 3);-.(nth matr 2 4)];
	[-.(nth matr 4 1);-.(nth matr 4 3);-.(nth matr 4 4)]];
	calc_det [[(nth matr 1 1);(nth matr 1 2);(nth matr 1 4)];
	[(nth matr 2 1);(nth matr 2 2);(nth matr 2 4)];
	[(nth matr 4 1);(nth matr 4 2);(nth matr 4 4)]];
	calc_det [[-.(nth matr 1 1);-.(nth matr 1 2);-.(nth matr 1 3)];
	[-.(nth matr 2 1);-.(nth matr 2 2);-.(nth matr 2 3)];
	[-.(nth matr 4 1);-.(nth matr 4 2);-.(nth matr 4 3)]];];

	[calc_det [[-.(nth matr 1 2);-.(nth matr 1 3);-.(nth matr 1 4)];
	[-.(nth matr 2 2);-.(nth matr 2 3);-.(nth matr 2 4)];
	[-.(nth matr 3 2);-.(nth matr 3 3);-.(nth matr 3 4)]];
	calc_det [[(nth matr 1 1);(nth matr 1 3);(nth matr 1 4)];
	[(nth matr 2 1);(nth matr 2 3);(nth matr 2 4)];
	[(nth matr 3 1);(nth matr 3 3);(nth matr 3 4)]];
	calc_det [[-.(nth matr 1 1);-.(nth matr 1 2);-.(nth matr 1 4)];
	[-.(nth matr 2 1);-.(nth matr 2 2);-.(nth matr 2 4)];
	[-.(nth matr 3 1);-.(nth matr 3 2);-.(nth matr 3 4)]];
	calc_det [[(nth matr 1 1);(nth matr 1 2);(nth matr 1 3)];
	[(nth matr 2 1);(nth matr 2 2);(nth matr 2 3)];
	[(nth matr 3 1);(nth matr 3 2);(nth matr 3 3)]]]] in
	let det = calc_det_four matr in
	if det = 0. then raise Illegal else
	[List.map (fun x -> (1./.det)*.x) (List.nth adj_matr 0);
	List.map (fun x -> (1./.det)*.x) (List.nth adj_matr 1);
	List.map (fun x -> (1./.det)*.x) (List.nth adj_matr 2);
	List.map (fun x -> (1./.det)*.x) (List.nth adj_matr 3)]


(*POST: [calc_invert matr] is a float list list of inversion of matrix with
 	3 lists with 3 elements in each list*)
(*PRE: input is a list of lists with 3 lists with 
	3 elements in each list*)
let calc_invert matr =
	let adj_matr =
	[[(nth matr 2 2)*.(nth matr 3 3)-.((nth matr 3 2)*.(nth matr 2 3));
	-.((nth matr 2 1)*.(nth matr 3 3)-.((nth matr 3 1)*.(nth matr 2 3)));
	(nth matr 2 1)*.(nth matr 3 2)-.((nth matr 3 1)*.(nth matr 2 2))];
	[-.((nth matr 1 2)*.(nth matr 3 3)-.((nth matr 3 2)*.(nth matr 1 3)));
	(nth matr 1 1)*.(nth matr 3 3)-.((nth matr 3 1)*.(nth matr 1 3));
	-.((nth matr 1 1)*.(nth matr 3 2)-.((nth matr 3 1)*.(nth matr 1 2)))];
	[(nth matr 1 2)*.(nth matr 2 3)-.((nth matr 2 2)*.(nth matr 1 3));
	-.((nth matr 1 1)*.(nth matr 2 3)-.((nth matr 2 1)*.(nth matr 1 3)));
	(nth matr 1 1)*.(nth matr 2 2)-.((nth matr 2 1)*.(nth matr 1 2))]] in
	let trans_matr = calc_transpose adj_matr in
	let det = calc_det matr in
	if det = 0. then raise Illegal else
	[List.map (fun x -> (1./.det)*.x) (List.nth trans_matr 0);
	List.map (fun x -> (1./.det)*.x) (List.nth trans_matr 1);
	List.map (fun x -> (1./.det)*.x) (List.nth trans_matr 2)]

(*POST: [mult_matr_four a b] is a float list list matrix multiplication of 
	two lists, a and b*)
(*PRE: a is a list of lists with 4 elements in each list and 4 lists
 and b is a list of lists with 1 element in each list and 4 lists*)
let mult_matr_four a b =
	[((nth a 1 1)*.(nth b 1 1)+.(nth a 1 2)*.(nth b 2 1)+.
		(nth a 1 3)*.(nth b 3 1)+.(nth a 1 4)*.(nth b 4 1));
		((nth a 2 1)*.(nth b 1 1)+.(nth a 2 2)*.(nth b 2 1)+.
		(nth a 2 3)*.(nth b 3 1)+.(nth a 2 4)*.(nth b 4 1));
		((nth a 3 1)*.(nth b 1 1)+.(nth a 3 2)*.(nth b 2 1)+.
		(nth a 3 3)*.(nth b 3 1)+.(nth a 3 4)*.(nth b 4 1));
		((nth a 4 1)*.(nth b 1 1)+.(nth a 4 2)*.(nth b 2 1)+.
		(nth a 4 3)*.(nth b 3 1)+.(nth a 4 4)*.(nth b 4 1));]

(*POST: [mult_matr a b] is a float list list matrix multiplication of 
	two lists, a and b*)
(*PRE: a is a list of lists with 3 elements in each list and 3 lists
 and b is a list of lists with 1 element in each list and 3 lists*)
let mult_matr a b =
	[((nth a 1 1)*.(nth b 1 1)+.(nth a 1 2)*.(nth b 2 1)+.
		(nth a 1 3)*.(nth b 3 1));((nth a 2 1)*.(nth b 1 1)+.
		(nth a 2 2)*.(nth b 2 1)+.(nth a 2 3)*.(nth b 3 1));
		((nth a 3 1)*.(nth b 1 1)+.(nth a 3 2)*.(nth b 2 1)+.
		(nth a 3 3)*.(nth b 3 1));]

(* POST: [second_poly_regression dset] is a float list of length 3 
 * representing the result of second order polynomial regression on [dset].
 * The resulting list represents the coefficients of the curve in descending
 * order.  
 * PRE: [dataset] is a (float * float) list of elements in the form
 * (x coordinate, y coordinate).
 *)
let second_poly_regression dataset =
	if List.length dataset < 3 then raise Illegal else
	let n = float_of_int (List.length dataset) in
	let xylst = List.split dataset in
	let xlst = fst xylst in
	let ylst = snd xylst in
	let xsum = sum xlst in
	let x2sum = sum (List.map (fun x -> x*.x) xlst) in
	let x3sum = sum (List.map (fun x -> x*.x*.x) xlst) in
	let x4sum = sum (List.map (fun x -> x*.x*.x*.x) xlst) in
	let ysum = sum ylst in
	let xysum = sum (List.map (fun (x,y) -> x*.y) dataset) in
	let x2ysum = sum (List.map (fun (x,y) -> x*.x*.y) dataset) in
	let matrix = [[n;xsum;x2sum];[xsum;x2sum;x3sum];[x2sum;x3sum;x4sum]] in
	let inv_matr = calc_invert matrix in
	let matr2 = [[ysum];[xysum];[x2ysum]] in
	List.rev (mult_matr inv_matr matr2)


(* POST: [third_poly_regression dset] is a float list of length 4 
 * representing the result of third order polynomial regression on [dset]. 
 * The resulting list represents the coefficients of the curve in descending
 * order.   
 * PRE: [dataset] is a (float * float) list of elements in the form
 * (x coordinate, y coordinate).
 *)
let third_poly_regression dataset =
	if List.length dataset < 4 then raise Illegal else
	let n = float_of_int (List.length dataset) in
	let xylst = List.split dataset in
	let xlst = fst xylst in
	let ylst = snd xylst in
	let xsum = sum xlst in
	let x2sum = sum (List.map (fun x -> x*.x) xlst) in
	let x3sum = sum (List.map (fun x -> x*.x*.x) xlst) in
	let x4sum = sum (List.map (fun x -> x*.x*.x*.x) xlst) in
	let x5sum = sum (List.map (fun x -> x*.x*.x*.x*.x) xlst) in
	let x6sum = sum (List.map (fun x -> x*.x*.x*.x*.x*.x) xlst) in
	let ysum = sum ylst in
	let xysum = sum (List.map (fun (x,y) -> x*.y) dataset) in
	let x2ysum = sum (List.map (fun (x,y) -> x*.x*.y) dataset) in
	let x3ysum = sum (List.map (fun (x,y) -> x*.x*.x*.y) dataset) in
	let matrix = [[n;xsum;x2sum;x3sum];[xsum;x2sum;x3sum;x4sum];
	[x2sum;x3sum;x4sum;x5sum];[x3sum;x4sum;x5sum;x6sum]] in
	let inv_matr = calc_invert4 matrix in
	let matr2 = [[ysum];[xysum];[x2ysum];[x3ysum]] in
	List.rev (mult_matr_four inv_matr matr2)

(* POST: [sub_adjacent_lst lst] is a float list of length (List.length lst)-1 
 * that subtracts adjacent elements, i.e. sub_adjacent_lst [e1;e2;...;en] returns
 * [e2-.e1;e3-e2;...;en-e(n-1)].
 * PRE: [lst] is a float list. 
 *)
let rec sub_adjacent_lst lst =
	match lst with
	|[] -> []
	|h1::h2::[] -> (h2-.h1)::sub_adjacent_lst []
	|h1::h2::t -> (h2-.h1)::sub_adjacent_lst (h2::t)
	|_::[] -> []


(* POST: [generate_lst_with_sum n sum] is a float list of length [n]
 * where the elements of the list sum to [sum].
 * PRE: [n] is an int. [sum] is a positive float.
 *)
let generate_lst_with_sum n sum =
	let rand_arr = Array.init (n-1) (fun x -> Random.float sum) in
	let rand_arr2 = Array.append rand_arr [|0.;sum|] in
	let () = Array.sort compare rand_arr2 in
	let sorted_rand_lst = Array.to_list rand_arr2 in
	sub_adjacent_lst sorted_rand_lst

(* POST: [last_digit n] is an int that is the last digit in a float [n]. 
 * PRE: [n] is a float with number of digits >= 1. 
 *)
let last_digit n =
	let str = string_of_float n in
	if String.length str < 1 then raise Illegal
	else let last = (String.length str) - 1 in
	int_of_string (Char.escaped (String.get str last))

(* POST: [generate_res_lst n sumressq] is a float list of length [n]
 * that represents a list of residuals where [sumressq] is the sum of the square
 * of all elements in the list.
 * PRE: [n] is an int. [sumressq] is a positive float.
 *)
let generate_res_lst n sumressq =
	let resq_lst = generate_lst_with_sum n sumressq in
	let res_pos_lst = List.map (fun x -> sqrt(x)) resq_lst in
	let neg_balance_lst = List.map (fun x -> if ((last_digit x) mod 2 = 0) then
		x else ~-.x) res_pos_lst in
	shuffle neg_balance_lst 


(* POST: [generate_y_lst func xlst] is a float list of length (List.length xlst)
 * that represents a list of the expected y-values for a list of x values [xlst]
 * generated with the function [func].
 * PRE: [func] is a float list of length 2, 3, or 4. [xlst] is a float list of
 * finite length.
 *)
let generate_y_lst func xlst =
	match (List.length func) with
		|2 -> let slope = List.nth func 0 in
			  let intercept = List.nth func 1 in
			  List.map (fun x -> (slope *. x) +. intercept) xlst
		|3 -> let co1 = List.nth func 0 in
		      let co2 = List.nth func 1 in
		      let intercept = List.nth func 2 in
		      List.map (fun x -> ((co1 *. (x**2.)) +. 
		      	(co2 *. x) +. intercept)) xlst
		|4 -> let co1 = List.nth func 0 in
		      let co2 = List.nth func 1 in
		      let co3 = List.nth func 2 in
		      let intercept = List.nth func 3 in
		      List.map (fun x -> ((co1 *. (x**3.)) +. (co2 *. (x**2.)) +. 
		      	(co3 *. x) +. intercept)) xlst
		|_ -> raise Illegal
		

(* POST: [model func n std xmin xmax erange] is a (float * float) list of length
 * [n]. It is a list of elements in the form (x coordinate, y coordinate) that fit
 * the function [func] with a standard deviation of [std]. All elements in the
 * list have x-coordinates in the range [xmin]..[xmax] and have y-coordinates
 * within [erange] units of the expected value given by the function [func].
 * PRE: [func] is a float list of length 2, 3, or 4. [n] is a positive int.
 * [std] is a positive float. [xmin] is a float. [xmax] is a float. [erange] is
 * a positive float.
 *)
let model func (n:int) (stderror:float) (xmin:float) (xmax:float) =
	let sum_ressq = ((stderror**2.) *. ((float_of_int n) -. 2.)) in
	let res_lst = generate_res_lst n sum_ressq in
	let diff = xmax -. xmin in
	let x_empty = Array.create_float n in
	let x_array = Array.map (fun x -> (Random.float diff) +. xmin) x_empty in
	let x_lst = List.sort compare (Array.to_list x_array) in
	let exp_y_lst = generate_y_lst func x_lst in
	let expy_and_res = List.combine exp_y_lst res_lst in
	let y_lst = List.map (fun (y,r) -> y +. r) expy_and_res in
	List.combine x_lst y_lst

(* inc_fname returns the next filename to be used when saving csv files.
 * Csv file names are integers. *)
let next_fnum = ref 0
let inc_fname () = next_fnum := 1 + !next_fnum;
  (string_of_int !next_fnum) ^ ".csv"

(* Helper function for dataset_to_file.
 * Takes a float tuple as input.
 * Returns a string list of the contents of the tuple. *)
let tuple_to_list tup =
  match tup with
  | x,y -> (string_of_float x)::(string_of_float y)::[]

(* Takes dataset as input.
 * Converst the dataset into a string list list as needed by the csv library.
 * Returns unit, saves a file to the current directory with filename fname. *)
let dataset_to_file fname data = List.map tuple_to_list data |> Csv.save fname

let create_model funct n stderr xmin xmax =
	let data = model funct n stderr xmin xmax in
	let fname = inc_fname () in
	let () = data |> dataset_to_file fname in
	(fname,data)
