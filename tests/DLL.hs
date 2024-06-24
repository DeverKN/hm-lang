module DLL where
import Data.IORef (IORef)

-- data DLLNode a = DLLNode (IORef (DLLNode a)) (IORef a) (IORef (DLLNode a)) | DLLStart 