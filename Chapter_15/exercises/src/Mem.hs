-- !!! QuickCHeck?

module Mem 
     ( Mem (Mem, runMem)
     ) where

import Test.QuickCheck

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Semigroup a => Semigroup (Mem s a) where
    (Mem f) <> (Mem g) =
        Mem $ \s -> let 
            (af, sf) = f s
            (ag, sg) = g s
            (agf, sgf) = f sg
            in (af <> ag, sgf)
            --Mem $ \s -> ( (fst (fResult s)) <> (fst (gResult s)) , (snd (fResult (snd (gResult s)))) )
            --fResult = runMem f
            --gResult = runMem g

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)
    