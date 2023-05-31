module Model
  where

import ActionSpaces
import Components
import SupportFunctions
import Types

import OpenGames.Engine.Engine
import OpenGames.Preprocessor

{-
Contains the basic state change logic for an individual user.
By instantiating a initial state, the incentives underlying each individual for any scenario can be checked.
-}
