{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module Brick.Screen.Single where

import           Brick
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Profunctor
import           Data.Profunctor.Choice
import           Data.Profunctor.Strong

data SingleScreen n s f e a = SingleScreen
  { drawSingleScreen   :: s -> Widget n
  , updateSingleScreen :: e -> s -> f (a, s)
  } deriving (Functor)

instance Functor f => Profunctor (SingleScreen n s f) where
    lmap f (SingleScreen draw update) =
        SingleScreen draw (\e s -> update (f e) s)
    rmap = fmap


instance Functor f => Strong (SingleScreen n s f) where
    first' (SingleScreen draw update) =
        SingleScreen draw $ \(e, c) s -> over _1 (, c) <$> update e s

instance Applicative f => Choice (SingleScreen n s f) where
    left' (SingleScreen draw update) =
        let helper (Left e) s  = over _1 Left <$> update e s
            helper (Right c) s = pure (Right c,s)
         in SingleScreen draw helper

hoistSingleScreen ::
     (forall x. f x -> g x) -> SingleScreen n s f e a -> SingleScreen n s g e a
hoistSingleScreen func (SingleScreen draw update) =
  SingleScreen draw (\e s -> func $ update e s)

zoomSingleScreen ::
     Functor f => Lens' s t -> SingleScreen n t f e a -> SingleScreen n s f e a
zoomSingleScreen l (SingleScreen draw update) =
  SingleScreen
    { drawSingleScreen = \t -> draw (t ^. l)
    , updateSingleScreen = \e t -> (\(a, s) -> (a, set l s t)) <$> update e (t ^. l)
    }

zoomMultiSingleScreen ::
     (Monad f, Monoid a)
  => (Widget n -> Widget n -> Widget n)
  -> Traversal' s t
  -> SingleScreen n t f e a
  -> SingleScreen n s f e a
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
