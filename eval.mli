type funct = float list
type dataset = (float * float) list

val lin_reg_stats          : dataset -> float list
val sum                    : float list -> float
val generate_y_lst         : funct -> float list -> float list
val lin_reg                : dataset -> funct
val second_poly_regression : dataset -> funct
val third_poly_regression  : dataset -> funct
val create_model           : funct -> int -> float -> float -> float -> string * dataset

exception Illegal