{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Dang.ModuleSystem.Rename
import           Dang.Monad
import           Dang.Syntax.Format (formatMessage)
import           Dang.Syntax.Lexer
import           Dang.Syntax.Location
    (SourceRange(..),SourcePos(..),HasRange(..),interactive)
import           Dang.Syntax.Parser
import           Dang.Syntax.Signatures
import qualified Dang.TypeCheck.KindCheck as KC
import           Dang.Utils.PP

import qualified Data.Foldable as F
import           Data.List (sortBy)
import           Data.Ord (comparing)
import qualified Data.Text as S
import qualified Data.Text.IO as S
import           System.Exit (exitFailure)
import           System.Environment (getArgs)
import           Text.Show.Pretty (pPrint)


main :: IO ()
main  = runDang $
  do args <- io getArgs
     file <- case args of
               [file] -> return file
               _      -> io $ do putStrLn "Usage: dang file.dg"
                                 exitFailure

     txt <- io (S.readFile file)
     io (mapM_ (print . lexemeToken) (lexWithLayout (S.pack file) Nothing txt))

     let dumpMessages ms =
           io $ printDoc defaultConfig
              $ vcat
              $ map (formatMessage (S.pack file) txt)
              $ sortBy (comparing (sourceIndex . sourceFrom . range))
              $ F.toList ms

     (mbMod,ms) <- collectMessages $ try $
       do pMod  <- parseModule (S.pack file) txt
          sMod  <- resolveSignatures pMod
          rnMod <- renameModule sMod
          -- KC.checkModule rnMod
          return rnMod

     io (pPrint mbMod)

     if null ms
        then io (putStrLn "No messages")
        else dumpMessages ms
