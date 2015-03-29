module Dang.Syntax.Ann where

import Dang.Utils.Pretty

import qualified System.Console.ANSI as ANSI
import           System.IO (Handle)


-- | The type of annotations for parsed syntax.
data SynAnn = Decl
            | Expr
            | Symbol
            | Literal
              deriving (Show)

renderSymAnnot :: Handle -> PPEnv -> PPDoc SynAnn -> IO ()
renderSymAnnot h env doc = renderIO h env startSynAnn endSynAnn done doc
  where
  done = putStrLn ""

startSynAnn :: Handle -> SynAnn -> IO ()
startSynAnn h ann = ANSI.hSetSGR h $ case ann of
  Decl    -> [ fg ANSI.Blue,  underline, bold ]
  Expr    -> [ fg ANSI.Green, underline, bold ]
  Symbol  -> [ fg ANSI.Yellow                 ]
  Literal -> [ fg ANSI.Magenta,          bold ]
  where
  fg        = ANSI.SetColor ANSI.Foreground ANSI.Vivid
  underline = ANSI.SetUnderlining ANSI.SingleUnderline
  bold      = ANSI.SetConsoleIntensity ANSI.BoldIntensity

endSynAnn :: Handle -> SynAnn -> IO ()
endSynAnn h _ = ANSI.hSetSGR h []
