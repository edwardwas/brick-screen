{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}

module Brick.Screen.Single where

import           Brick
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Writer

data SingleScreen n e s f a = SingleScreen
  { drawSingleScreen   :: s -> Widget n
  , updateSingleScreen :: e -> s -> f (a, s)
  } deriving (Functor)

hoistSingleScreen ::
     (forall x. f x -> g x) -> SingleScreen n e s f a -> SingleScreen n e s g a
hoistSingleScreen func (SingleScreen draw update) =
  SingleScreen draw (\e s -> func $ update e s)

zoomSingleScreen ::
     Functor f => Lens' s t -> SingleScreen n e t f a -> SingleScreen n e s f a
zoomSingleScreen l (SingleScreen draw update) =
  SingleScreen
    { drawSingleScreen = \t -> draw (t ^. l)
    , updateSingleScreen = \e t -> (\(a, s) -> (a, set l s t)) <$> update e (t ^. l)
    }

zoomMultiSingleScreen ::
     (Monad f, Monoid a)
  => (Widget n -> Widget n -> Widget n)
  -> Traversal' s t
  -> SingleScreen n e t f a
  -> SingleScreen n e s f a
zoomMultiSingleScreen combineFunc l (SingleScreen draw update) =
  SingleScreen
    { drawSingleScreen = foldlOf (l . to draw) combineFunc emptyWidget
    , updateSingleScreen =
        \e t ->
          let helper s = do
                (a, s') <- lift $ update e s
                s' <$ tell a
              flipTuple (a, b) = (b, a)
           in flipTuple <$> runWriterT (traverseOf l helper t)
    }
