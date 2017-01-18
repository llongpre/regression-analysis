type funct = float list
type dataset = (float * float) list
type state = {message: string; funct: funct; data: dataset}

(* The following variables contain string messages to be printed to the user
 * after different commands. The 80-word limit is intentionally not followed
 * because it changes the formatting of printing in the terminal.*)
let please = "Please enter a command."
let help = "
  Enter \"list\" for a list of valid commands.
  Enter \"<command> help\" for details of a specific command.
  Enter \"quit\" to exit.\n\n> "
let instructions = please^help
let invalid_command = "Command not recognized, please try again."
let list_commands = "
  linreg <dataset file>
  polyreg <order(0-3)> <dataset file>
  model <function> <num datapoints> <std error> <xmin> <xmax>
  graphf <function> <xmin> <xmax> <ymin> <ymax>
  graphd <dataset file> <xmin> <xmax> <ymin> <ymax>
  graphm <function> <dataset file> <xmin> <xmax> <ymin> <ymax>
  plotres <optional function> <dataset file>
  rsquared <dataset file>
  stderr <optional function> <dataset file>
  stddev <optional function> <dataset file>
  variance <optional function> <dataset file>
  chisqr <optional function> <dataset file>
  poisson <mean> <random variable>
  poissonc <mean> <random variable>
  binomial <num trials> <prob of success> <num successes>
  binomialc <num trials> <prob of success> <num successes>\n\n"
let linreg_help = "
linreg command instructions:

  Option 1:
  Enter \"linreg <dataset file>\"
  Example: \"linreg csv.csv\"
  This will perform linear regression analysis on the dataset in your dataset file.
  <dataset file> should be the name of the .csv file in your current directory.
  Format of dataset file: your .csv file must contain only two columns of integers or floats. The first column contains x values and the second column contains y values.
  An example .csv file is in this directory under filename csv.csv.

  Option 2:
  Enter \"linreg dataset\"
  This will perform linear regression analysis on the most recent dataset entered or computed.\n\n"
let polyreg_help = "
polyreg command instructions:

  Option 1:
  Enter \"polyreg <order(0-3)> <dataset file>\"
  Example: \"polyreg 2 csv.csv\"
  This will perform polynomial regression analysis to the <order> degree on the dataset in your dataset file.
  <order> should be an integer between 0 and 3.
  <dataset file> should be the name of the .csv file in your current directory.
  Format of dataset file: your .csv file must contain only two columns of integers or floats. The first column contains x values and the second column contains y values.
  An example .csv file is in this directory under filename csv.csv.

  Option 2:
  Enter \"polyreg <order(0-3) dataset\"
  Example: \"polyreg 3 dataset\"
  This will perform polynomial regression analysis to the <order> degree on the most recent dataset entered or computed.\n\n"
let model_help = "
model command instructions:

  Option 1:
  Enter \"model <function> <num datapoints> <std error> <xmin> <xmax>\"
  Example: \"model y=x^2+8 50 5 0 100\"
  This will create a dataset from your input function with the specified number of datapoints and standard error within specified x value range.
  Your dataset will be saved as a .csv file in your current directory.
  Format of dataset file: the .csv file will contain only two columns of floats. The first column contains x values and the second column contains y values.
  <function> must be in the format \"y=c1x^3+c2x^2+c3x+c4\". Function must contain at least 1 order (not all are required). Orders must be in descending order. Coefficients ci are optional. For orders n=0,1 please do not include \"^n\". For order n=1, write \"cx\". For order n=0, write \"c\". Function must not contain any spaces.
    Examples:
    y=5x^3+2x^2+8x+9
    y=x^3+x
    y=x^2

  Option 2:
  Enter \"model function <num datapoints> <std error> <xmin> <xmax>\"
  Example: \"model function 50 5 0 100\"
  This will create a dataset from the most recent function entered or computed. with the specified number of datapoints and standard error within specified x value range.
  Your dataset will be saved as a .csv file in your current directory.
  Format of dataset file: the .csv file will contain only two columns of floats. The first column contains x values and the second column contains y values.\n\n"
let graphf_help = "
graphf command instructions:

  Option 1:
  Enter \"graphf <function> <xmin> <xmax> <ymin> <ymax>\"
  Example: \"graphf y=x^2+8 -10 10 -20 20\"
  This will create a graph from your input function with specified x value and y value ranges.
  Your graph will be saved as a .png file in your current directory.
  <function> must be in the format \"y=c1x^3+c2x^2+c3x+c4\". Function must contain at least 1 order (not all are required). Orders must be in descending order. Coefficients ci are optional. For orders n=0,1 please do not include \"^n\". For order n=1, write \"cx\". For order n=0, write \"c\". Function must not contain any spaces.
    Examples:
    y=5x^3+2x^2+8x+9
    y=x^3+x
    y=x^2
  Bounds <xmin> <xmax> <ymin> and <ymax> must be integers or floats.

  Option 2:
  Enter \"graphf function <xmin> <xmax> <ymin> <ymax>\"
  Example: \"graphf function 0 100 0 100\"
  This will create a graph from the most recent function entered or computed with specified x value and y value ranges.
  Your graph will be saved as a .png file in your current directory.
  Bounds <xmin> <xmax> <ymin> and <ymax> must be integers or floats.\n\n"
let graphd_help = "
graphd command instructions:

  Option 1:
  Enter \"graphd <dataset file> <xmin> <xmax> <ymin> <ymax>\"
  Example: \"graphd csv.csv -10 10 -20 20\"
  This will create a graph from your input dataset with specified x value and y value ranges.
  Your graph will be saved as a .png file in your current directory.
  <dataset file> should be the name of the .csv file in your current directory.
  Format of dataset file: your .csv file must contain only two columns of integers or floats. The first column contains x values and the second column contains y values.
  An example .csv file is in this directory under filename csv.csv.
  Bounds <xmin> <xmax> <ymin> and <ymax> must be integers or floats.

  Option 2:
  Enter \"graphd dataset <xmin> <xmax> <ymin> <ymax>\"
  Example: \"graphf csv.csv 0 100 0 100\"
  This will create a graph from the most recent dataset entered or computed with specified x value and y value ranges.
  Your graph will be saved as a .png file in your current directory.
  Bounds <xmin> <xmax> <ymin> and <ymax> must be integers or floats.\n\n"
let graphm_help = "
graphm command instructions:

  Option 1:
  Enter \"graphm <function> <dataset file> <xmin> <xmax> <ymin> <ymax>\"
  Example: \"graphm y=x^2+8 csv.csv -10 10 -20 20\"
  This will create a graph from your input function and dataset with specified x value and y value ranges.
  Your graph will be saved as a .png file in your current directory.
  <function> must be in the format \"y=c1x^3+c2x^2+c3x+c4\". Function must contain at least 1 order (not all are required). Coefficients ci are optional. For orders n=0,1 please do not include \"^n\". For order n=1, write \"cx\". For order n=0, write \"c\". Function must not contain any spaces.
    Examples:
    y=5x^3+2x^2+8x+9
    y=x^3+x
    y=x^2
  <dataset file> should be the name of the .csv file in your current directory.
  Format of dataset file: your .csv file must contain only two columns of integers or floats. The first column contains x values and the second column contains y values.
  An example .csv file is in this directory under filename csv.csv.
  Bounds <xmin> <xmax> <ymin> and <ymax> must be integers or floats.

  Option 2:
  Replace <function> from Option1 with keyword \"function\"
  Example: \"graphm function csv.csv -10 10 -10 10\"
  This will create a graph from your input dataset and most recent function entered or computed with specified x value and y value ranges.

  Option 3:
  Replace <dataset> from Option1 with keyword \"dataset\"
  Example: \"graphm y=x^2+8 csv.csv -10 10 -10 10\"
  This will create a graph from your input function and most recent dataset entered or computed with specified x value and y value ranges.

  Option 4:
  Replace both <function> and <dataset file> from Option1 with keywords \"function\" and \"dataset\"
  Example: \"graphm function dataset -10 10 -10 10\"
  This will create a graph from most recent function and dataset entered or computed with specified x value and y value ranges.\n\n"
let stderr_help = "
stderr command instructions:

  Option 1:
  Enter \"stderr <function> <dataset file>\"
  Example: \"stderr y=x^2+8 csv.csv\"
  This will return the stderr of your input dataset from your input function.
  <function> must be in the format \"y=c1x^3+c2x^2+c3x+c4\". Function must contain at least 1 order (not all are required). Orders must be in descending order. Coefficients ci are optional. For orders n=0,1 please do not include \"^n\". For order n=1, write \"cx\". For order n=0, write \"c\". Function must not contain any spaces.
    Examples:
    y=5x^3+2x^2+8x+9
    y=x^3+x
    y=x^2
  <dataset file> should be the name of the .csv file in your current directory.
  Format of dataset file: your .csv file must contain only two columns of integers or floats. The first column contains x values and the second column contains y values.
  An example .csv file is in this directory under filename csv.csv.

  Option 2:
  Replace <function> from Option1 with keyword \"function\"
  Example: \"stderr function csv.csv\"
  This will return the stderr of your input dataset from the most recent function entered or computed.

  Option 3:
  Replace <dataset> from Option1 with keyword \"dataset\"
  Example: \"variace y=x^2+8 dataset\"
  This will return the stderrof the most recent dataset entered or computed from your input function.

  Option 4:
  Replace both <function> and <dataset file> from Option1 with keywords \"function\" and \"dataset\"
  Example: \"stderr function dataset\"
  This will return the stderr of the Pearson's chi-squared test of the most recent dataset entered or computed from the most recent function entered or computed.

  Option 5:
  Enter \"stderr <dataset file>\"
  Example: \"stderr csv.csv\"
  This will return the stderr of your input dataset from the linear regression function generated by the dataset.
  <dataset file> should be the name of the .csv file in your current directory.
  Format of dataset file: your .csv file must contain only two columns of integers or floats. The first column contains x values and the second column contains y values.
  An example .csv file is in this directory under filename csv.csv.

  Option 6:
  Replace <dataset file> from Option5 with keyword \"dataset\"
  Example: \"stderr dataset\"
  This will return the stderr of your the most recent dataset entered or computed from the linear regression function generated by the dataset.
  \n\n"
let stddev_help = "
stddev command instructions:

  Option 1:
  Enter \"stddev <function> <dataset file>\"
  Example: \"stddev y=x^2+8 csv.csv\"
  This will return the stddev of your input dataset from your input function.
  <function> must be in the format \"y=c1x^3+c2x^2+c3x+c4\". Function must contain at least 1 order (not all are required). Orders must be in descending order. Coefficients ci are optional. For orders n=0,1 please do not include \"^n\". For order n=1, write \"cx\". For order n=0, write \"c\". Function must not contain any spaces.
    Examples:
    y=5x^3+2x^2+8x+9
    y=x^3+x
    y=x^2
  <dataset file> should be the name of the .csv file in your current directory.
  Format of dataset file: your .csv file must contain only two columns of integers or floats. The first column contains x values and the second column contains y values.
  An example .csv file is in this directory under filename csv.csv.

  Option 2:
  Replace <function> from Option1 with keyword \"function\"
  Example: \"stddev function csv.csv\"
  This will return the stddev of your input dataset from the most recent function entered or computed.

  Option 3:
  Replace <dataset> from Option1 with keyword \"dataset\"
  Example: \"variace y=x^2+8 dataset\"
  This will return the stddevof the most recent dataset entered or computed from your input function.

  Option 4:
  Replace both <function> and <dataset file> from Option1 with keywords \"function\" and \"dataset\"
  Example: \"stddev function dataset\"
  This will return the stddev of the Pearson's chi-squared test of the most recent dataset entered or computed from the most recent function entered or computed.

  Option 5:
  Enter \"stddev <dataset file>\"
  Example: \"stddev csv.csv\"
  This will return the stddev of your input dataset from the linear regression function generated by the dataset.
  <dataset file> should be the name of the .csv file in your current directory.
  Format of dataset file: your .csv file must contain only two columns of integers or floats. The first column contains x values and the second column contains y values.
  An example .csv file is in this directory under filename csv.csv.

  Option 6:
  Replace <dataset file> from Option5 with keyword \"dataset\"
  Example: \"stddev dataset\"
  This will return the stddev of your the most recent dataset entered or computed from the linear regression function generated by the dataset.
  \n\n"
let variance_help = "
variance command instructions:

  Option 1:
  Enter \"variance <function> <dataset file>\"
  Example: \"variance y=x^2+8 csv.csv\"
  This will return the variance of your input dataset from your input function.
  <function> must be in the format \"y=c1x^3+c2x^2+c3x+c4\". Function must contain at least 1 order (not all are required). Orders must be in descending order. Coefficients ci are optional. For orders n=0,1 please do not include \"^n\". For order n=1, write \"cx\". For order n=0, write \"c\". Function must not contain any spaces.
    Examples:
    y=5x^3+2x^2+8x+9
    y=x^3+x
    y=x^2
  <dataset file> should be the name of the .csv file in your current directory.
  Format of dataset file: your .csv file must contain only two columns of integers or floats. The first column contains x values and the second column contains y values.
  An example .csv file is in this directory under filename csv.csv.

  Option 2:
  Replace <function> from Option1 with keyword \"function\"
  Example: \"variance function csv.csv\"
  This will return the variance of your input dataset from the most recent function entered or computed.

  Option 3:
  Replace <dataset> from Option1 with keyword \"dataset\"
  Example: \"variace y=x^2+8 dataset\"
  This will return the varianceof the most recent dataset entered or computed from your input function.

  Option 4:
  Replace both <function> and <dataset file> from Option1 with keywords \"function\" and \"dataset\"
  Example: \"variance function dataset\"
  This will return the variance of the Pearson's chi-squared test of the most recent dataset entered or computed from the most recent function entered or computed.

  Option 5:
  Enter \"variance <dataset file>\"
  Example: \"variance csv.csv\"
  This will return the variance of your input dataset from the linear regression function generated by the dataset.
  <dataset file> should be the name of the .csv file in your current directory.
  Format of dataset file: your .csv file must contain only two columns of integers or floats. The first column contains x values and the second column contains y values.
  An example .csv file is in this directory under filename csv.csv.

  Option 6:
  Replace <dataset file> from Option5 with keyword \"dataset\"
  Example: \"variance dataset\"
  This will return the variance of your the most recent dataset entered or computed from the linear regression function generated by the dataset.
  \n\n"
let chisqr_help = "
chisqr command instructions:

  Option 1:
  Enter \"chisqr <function> <dataset file>\"
  Example: \"chisqr y=x^2+8 csv.csv\"
  This will return the result of the Pearson's chi-squared test of your input dataset on your function.
  <function> must be in the format \"y=c1x^3+c2x^2+c3x+c4\". Function must contain at least 1 order (not all are required). Orders must be in descending order. Coefficients ci are optional. For orders n=0,1 please do not include \"^n\". For order n=1, write \"cx\". For order n=0, write \"c\". Function must not contain any spaces.
    Examples:
    y=5x^3+2x^2+8x+9
    y=x^3+x
    y=x^2
  <dataset file> should be the name of the .csv file in your current directory.
  Format of dataset file: your .csv file must contain only two columns of integers or floats. The first column contains x values and the second column contains y values.
  An example .csv file is in this directory under filename csv.csv.

  Option 2:
  Replace <function> from Option1 with keyword \"function\"
  Example: \"chisqr function csv.csv\"
  This will return the result of the Pearson's chi-squared test of your input dataset on the most recent function entered or computed.

  Option 3:
  Replace <dataset> from Option1 with keyword \"dataset\"
  Example: \"chisqr y=x^2+8 dataset\"
  This will return the result of the Pearson's chi-squared test of the most recent dataset entered or computed on your function.

  Option 4:
  Replace both <function> and <dataset file> from Option1 with keywords \"function\" and \"dataset\"
  Example: \"chisqr function dataset\"
  This will return the result of the Pearson's chi-squared test of the most recent dataset entered or computed on the most recent function entered or computed.

  Option 5:
  Enter \"chisqr <dataset file>\"
  Example: \"chisqr csv.csv\"
  This will return the result of the Pearson's chi-squared test of your input dataset on the linear regression function generated by the dataset.
  <dataset file> should be the name of the .csv file in your current directory.
  Format of dataset file: your .csv file must contain only two columns of integers or floats. The first column contains x values and the second column contains y values.
  An example .csv file is in this directory under filename csv.csv.

  Option 6:
  Replace <dataset file> from Option5 with keyword \"dataset\"
  Example: \"chisqr dataset\"
  This will return the result of the Pearson's chi-squared test of the most recent dataset entered or computed on the linear regression function generated by the dataset.
  \n\n"
let plotres_help = "
plotres command instructions:

  Option 1:
  Enter \"plotres <function> <dataset file>\"
  Example: \"plotres y=x^2+8 csv.csv\"
  This will create a graph of the residuals of the dataset applied to the function.
  Your graph will be saved as a .png file in your current directory.
  <function> must be in the format \"y=c1x^3+c2x^2+c3x+c4\". Function must contain at least 1 order (not all are required). Orders must be in descending order. Coefficients ci are optional. For orders n=0,1 please do not include \"^n\". For order n=1, write \"cx\". For order n=0, write \"c\". Function must not contain any spaces.
    Examples:
    y=5x^3+2x^2+8x+9
    y=x^3+x
    y=x^2
  <dataset file> should be the name of the .csv file in your current directory.
  Format of dataset file: your .csv file must contain only two columns of integers or floats. The first column contains x values and the second column contains y values.
  An example .csv file is in this directory under filename csv.csv.

  Option 2:
  Replace <function> from Option1 with keyword \"function\"
  Example: \"plotres function csv.csv\"
  This will create a graph of the residuals of the input dataset applied to the most recently function most recently entered or computed.
  Your graph will be saved as a .png file in your current directory.

  Option 3:
  Replace <dataset> from Option1 with keyword \"dataset\"
  Example: \"variace y=x^2+8 dataset\"
  This will create a graph of the residuals of the most recent dataset entered or computed applied to the function.
  Your graph will be saved as a .png file in your current directory.

  Option 4:
  Replace both <function> and <dataset file> from Option1 with keywords \"function\" and \"dataset\"
  Example: \"plotres function dataset\"
  This will create a graph of the residuals of the most recent dataset entered or computed applied to the most recent function entered or computed.
  Your graph will be saved as a .png file in your current directory.

  Option 5:
  Enter \"plotres <dataset file>\"
  Example: \"plotres csv.csv\"
  This will create a graph of the residuals of the dataset applied to the linear regression function generated by the dataset.
  Your graph will be saved as a .png file in your current directory.
  <dataset file> should be the name of the .csv file in your current directory.
  Format of dataset file: your .csv file must contain only two columns of integers or floats. The first column contains x values and the second column contains y values.
  An example .csv file is in this directory under filename csv.csv.

  Option 6:
  Replace <dataset file> from Option5 with keyword \"dataset\"
  Example: \"plotres dataset\"
  This will create a graph of the residuals of the dataset applied to the linear regression function generated by the most recent dataset entered or computed.
  Your graph will be saved as a .png file in your current directory."
let rsqr_help = "
rsquared command instructions:

  Option 1:
  Enter \"rsquared <dataset file>\"
  Example: \"rsquared csv.csv\"
  This will return the rsquared value of your input dataset on the linear regression function generated by the dataset.
  <dataset file> should be the name of the .csv file in your current directory.
  Format of dataset file: your .csv file must contain only two columns of integers or floats. The first column contains x values and the second column contains y values.
  An example .csv file is in this directory under filename csv.csv.

  Option 2:
  Replace <dataset file> from Option5 with keyword \"dataset\"
  Example: \"rsquared dataset\"
  This will return the rsquared value of the most recent dataset entered or computed on the linear regression function generated by the dataset.
  \n\n"
let poisson_help = "
poisson command instructions:

  Enter \"poisson <mean> <random variable>\"
  Example: \"poisson 5 10\"
  This will return the probability that the random variable is equal to the number of successes given a poisson distribution.
  <mean> must be a float or integer greater than 0.
  <random variable> must be an integer greater than or equal to 0.\n\n"
let poissonc_help = "
poissonc command instructions:

  Enter \"poissonc <mean> <random variable>\"
  Example: \"poisson 5 10\"
  This will return the probability that the random variable is less than or equal to the number of successes given a poisson distribution.
  <mean> must be a float or integer greater than 0.
  <random variable> must be an integer greater than or equal to 0.\n\n"
let binomial_help = "
binomial command instructions:

  Enter \"binomial <num trials> <prob of success> <num successes>\"
  Example: \"binomial 10 0.5 5\"
  This will return the probability that in a given number of trials, there will be the specified number of sucesses, given the probability of success and a binomial distribution.
  <num trials> must be an integer greater than 0.
  <probability of success> must be a float between 0 and 1, inclusive.
  <num successes> must be an integer greater than or equal to 0 and less than or equal to <num trials>.\n\n"
let binomialc_help = "
binomialc command instructions:

  Enter \"binomialc <num trials> <prob of success> <num successes>\"
  Example: \"binomialc 10 0.5 5\"
  This will return the probability that in a given number of trials, there will be at most the specified number of sucesses, given the probability of success and a binomial distribution.
  <num trials> must be an integer greater than 0.
  <probability of success> must be a float between 0 and 1, inclusive.
  <num successes> must be an integer greater than or equal to 0 and less than or equal to <num trials>.\n\n"

(* REPL that interacts with the user. Handles help commands locally. Calls
 * eval in Parse to actually evaluate mathematical commands. *)
let rec repl st c =
  let lst_c = Str.split (Str.regexp "[\t\r ]+") c in
  match lst_c with
  | "quit"::_ -> print_endline "Quitting...";
  | "list"::_ ->
    print_string (list_commands^instructions);
    let next_command = read_line () in
    String.lowercase_ascii next_command |> repl st
  | cmd::"help"::_ -> let cmd_help =
    (match cmd with
     | "linreg"    -> linreg_help^instructions
     | "polyreg"   -> polyreg_help^instructions
     | "model"     -> model_help^instructions
     | "graphf"    -> graphf_help^instructions
     | "graphd"    -> graphd_help^instructions
     | "graphm"    -> graphm_help^instructions
     | "rsquared"  -> rsqr_help^instructions
     | "stderr"    -> stderr_help^instructions
     | "stddev"    -> stddev_help^instructions
     | "variance"  -> variance_help^instructions
     | "chisqr"    -> chisqr_help^instructions
     | "plotres"   -> plotres_help^instructions
     | "poisson"   -> poisson_help^instructions
     | "poissonc"  -> poissonc_help^instructions
     | "binomial"  -> binomial_help^instructions
     | "binomialc" -> binomialc_help^instructions
     | _ -> invalid_command^help ) in
    print_string cmd_help;
    let next_command = read_line () in
    String.lowercase_ascii next_command |> repl st
  | _ -> let st' = Parse.eval st c in
    print_endline ("\n"^st'.message^"\n");
    print_string instructions;
    let next_command = read_line () in
    String.lowercase_ascii next_command |> repl st'

let () =
  ANSITerminal.(print_string []
    "\n\nWelcome to the 3110 Regression Analysis Tool.\n\n");
  print_string instructions;
  let command = read_line () in
  repl {message=""; funct=[0.]; data=[]} command