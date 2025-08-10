import FileCopy

open IO (println eprintln)

def VERSION := "0.1.0"
def BUFFER_SIZE : USize := 1024

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

/--
  Recursively copies data from the input handle to the output handle in chunks.
-/
partial def copyLoop (inHandle outHandle : IO.FS.Handle) : IO Unit := do
  let buffer ← inHandle.read BUFFER_SIZE
  if buffer.isEmpty then
    return ()
  else
    outHandle.write buffer
    copyLoop inHandle outHandle

def main (args : List String) : IO UInt32 := do
  match parseArgs args with
  | .error msg =>
    eprintln s!"{msg}"
    return 1
  | .ok parsedArgs =>
    if parsedArgs.showHelp then
      printHelp
      return 0
    else if parsedArgs.showVersion then
      println s!"Version: {VERSION}"
      return 0
    else
      match parsedArgs.inputFile, parsedArgs.outputFile with
      | some inputFile, some outputFile =>
        try
          IO.FS.withFile inputFile .read λ inHandle =>
            IO.FS.withFile outputFile .write λ outHandle =>
              copyLoop inHandle outHandle
          println s!"File '{inputFile}' successfully copied to '{outputFile}'"
          return 0
        catch e =>
          eprintln s!"An error occurred while working with files: {e}"
          return 1
      | _, _ =>
        eprintln "Error: Input and output files must be specified"
        printHelp
        return 1
