{-# LANGUAGE NoImplicitPrelude #-}
module MyLib
  ( parseFile
  , intoSCA2
  , Entry(..)
  , Case(..)
  , Line(..)
  , Token(..)
  , tokenString
  , IpaType(..)
  ) where

import MyLib.Entry (Entry(..), Line(..), Case(..))
import MyLib.EntryParser (parseFile, Token(..), tokenString, IpaType(..))
import MyLib.Printing (intoSCA2)
