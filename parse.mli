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

val eval : state -> string -> state
val parse_command : state -> string -> command