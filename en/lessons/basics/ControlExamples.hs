{-# LANGUAGE OverloadedStrings #-}

import Data.Text ( Text )

-- The classic prisoners' dilemma
-- Input is a tuple, where the value is true if a prisoner tells his captors
-- that the other prisoner did the crime, and false if he stays silent
-- Output is number of years in jail for both of them
prisonersDilemma :: (Bool, Bool) -> (Int, Int)
prisonersDilemma (prisoner1Talks, prisoner2Talks)
    | prisoner1Talks && prisoner2Talks         = (2, 2)
    | prisoner1Talks && not prisoner2Talks     = (0, 3)
    | not prisoner1Talks && prisoner2Talks     = (3, 0)
    | not prisoner1Talks && not prisoner2Talks = (1, 1)

-- The classic prisoners' dilemma
-- The input is a tuple of bools, where a value is True if a prisoner tells 
-- his captors that the other prisoner did the crime, and False if he stays 
-- silent
-- Note how the sentences are defined by a guard in the `where` clause 
-- Both sentences are set at once by putting them together in a tuple
prisonersDilemma2 :: (Bool, Bool) -> Text
prisonersDilemma2 (prisoner1Talks, prisoner2Talks) =
    "Prisoner 1 sentenced to " <> prisoner1Sentence 
    <> " and Prisoner 2 sentenced to " <> prisoner2Sentence
  where
    (prisoner1Sentence, prisoner2Sentence)
        | prisoner1Talks     && prisoner2Talks     = (medium   , medium)
        | prisoner1Talks     && not prisoner2Talks = (scotFree, heavy)
        | not prisoner1Talks && prisoner2Talks     = (heavy    , scotFree)
        | not prisoner1Talks && not prisoner2Talks = (light    , light)
    scotFree  = "0 years"
    light     = "2 years"
    medium    = "4 years"
    heavy     = "6 years"


--         | prisoner1Talks && prisoner2Talks         = ("4", "4")
--         | prisoner1Talks && not prisoner2Talks     = ("0", "6")
--         | not prisoner1Talks && prisoner2Talks     = ("6", "0")
--         | not prisoner1Talks && not prisoner2Talks = ("2", "2")
