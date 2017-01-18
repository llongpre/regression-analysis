type funct = float list
type dataset = (float * float) list

val rsq       : dataset -> float
val residuals : ?func:funct -> dataset -> float list
val residuals_plot : ?func:funct -> dataset -> (float * float) list
val stderror  : ?func:funct -> dataset -> float
val variance  : ?func:funct -> dataset -> float
val stddev    : ?func:funct -> dataset -> float
val chisq     : ?func:funct -> dataset -> float
val poisson   : float -> int -> float
val poissonc  : float -> int -> float
val binomial  : int -> float -> int -> float
val binomialc : int -> float -> int -> float