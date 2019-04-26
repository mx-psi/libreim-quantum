module Print(captureTo, printToFile) where
import Quipper
import Quipper.Printing
import System.IO
import GHC.IO.Handle
import Control.Exception

-- | Capture output to a file
-- This function uses GHC's internal libraries
captureTo :: FilePath -> IO a -> IO a
captureTo file action = withFile
  file
  WriteMode
  (\handle -> bracket (capture handle) restore (const action))
 where
  -- Capture stdout to a given handle and return a duplicate of stdout
  capture handle = do
    buffering  <- hGetBuffering stdout
    stdout_dup <- hDuplicate stdout
    hDuplicateTo handle stdout
    pure (stdout_dup, buffering)

  -- Restore stdout
  restore (stdout_dup, buffering) = do
    hDuplicateTo  stdout_dup stdout
    hSetBuffering stdout     buffering
    hClose stdout_dup

-- | Print a circuit to a file
printToFile
  :: (QCData qa, QCData qb)
  => FilePath
  -> Format
  -> (qa -> Circ qb)
  -> qa
  -> IO ()
printToFile _ Preview _ = error "[printToFile] Can't print preview to file"
printToFile file format circ = captureTo file . print_generic format circ
