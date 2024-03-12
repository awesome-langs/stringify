module Example exposing (..)
import Dict exposing (Dict)
import Posix.IO as IO exposing (..)
import Posix.IO.Process as Process
import Posix.IO.File as File
import Round

type alias ToS a =
    {
        toS : a -> String
    }

int : ToS Int
int =
    {
        toS = .toString
    }

string : ToS String
string =
    {
        toS = .identity
    }


program : Process -> IO ()
program process =
    let iToS = int
        sToS = string
    in
    Process.print (iToS.toS 1)