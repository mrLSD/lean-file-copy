namespace FileCopyBase

def BUFFER_SIZE : USize := 1024

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

/--
  Processes the file copy operation from `inputFile` to `outputFile`.
-/
def processCopy (inputFile outputFile : String) : IO Unit := do
  IO.FS.withFile inputFile .read λ inHandle =>
    IO.FS.withFile outputFile .write λ outHandle =>
      copyLoop inHandle outHandle

end FileCopyBase
