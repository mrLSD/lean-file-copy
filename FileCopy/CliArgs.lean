namespace CliArgs

open IO (println)

/--
  Structure to hold parsed command-line arguments.
-/
structure CliArgs where
  inputFile  : Option String := none
  outputFile : Option String := none
  showHelp   : Bool := false
  showVersion: Bool := false
  deriving Repr

inductive CliError
  | unknownArg (arg : String)

instance : ToString CliError where
  toString
    | CliError.unknownArg arg => s!"Unknown argument: {arg}"

/--
  Parses command-line arguments into a `CliArgs` structure.
  Returns `Except` to handle potential errors.
-/
def parseArgs (args : List String) : Except CliError CliArgs :=
  let rec parse (currentArgs : List String) (acc : CliArgs) : Except CliError CliArgs :=
    match currentArgs with
    | [] => .ok acc
    | "-h" :: tail | "--help"    :: tail => parse tail { acc with showHelp := true }
    | "-v" :: tail | "--version" :: tail => parse tail { acc with showVersion := true }
    | "-i" :: file :: tail | "--input"  :: file :: tail => parse tail { acc with inputFile := some file }
    | "-o" :: file :: tail | "--output" :: file :: tail => parse tail { acc with outputFile := some file }
    | unknown :: _ => .error (.unknownArg unknown)
  parse args {}

/--
  Outputs the help message.
-/
def printHelp : IO Unit := do
  println "To use: cli_file_copy -i <input_file> -o <output_file>"
  println "\nOptions:"
  println "  -i, --input <FILE>    Specifies the input file to read."
  println "  -o, --output <FILE>   Specifies the output file to write."
  println "  -v, --version         Show the program version."
  println "  -h, --help            Show this help message."

end CliArgs
