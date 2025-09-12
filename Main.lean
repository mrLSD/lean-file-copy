import FileCopy.CliArgs
import FileCopy.Basic

open FileCopyBase (copyLoop processCopy)
open CliArgs (parseArgs printHelp)

open IO (println eprintln)

def VERSION := "0.1.0"

/--
  Prints the version information.
-/
def printVersion := println s!"Version: {VERSION}"


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
      printVersion
      return 0
    else
      match parsedArgs.inputFile, parsedArgs.outputFile with
      | some inputFile, some outputFile =>
        try
          processCopy inputFile outputFile
          println s!"File '{inputFile}' successfully copied to '{outputFile}'"
          return 0
        catch e =>
          eprintln s!"An error occurred while working with files: {e}"
          return 1
      | _, _ =>
        eprintln "Error: Input and output files must be specified"
        printHelp
        return 1
