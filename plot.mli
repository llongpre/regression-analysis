type funct = float list
type dataset = (float * float) list

val plot_funct : funct -> float * float -> float * float -> string
val plot_data : dataset -> float * float -> float * float -> string
val plot_model : funct -> dataset -> float * float -> float * float -> string
val plot_res : dataset -> string

exception Illegal