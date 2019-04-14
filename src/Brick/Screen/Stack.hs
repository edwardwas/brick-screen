{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RankNTypes                 #-}

module Brick.Screen.Stack where

import           Brick.Screen.Single
import           Control.Monad.TrackedIO

import           Brick
import           Control.Lens
import           Control.Monad.Free
import           Control.Monad.State
import           Data.Monoid             (Ap (..))
import qualified Graphics.Vty            as V

newtype ScreenStack n e s f a =
  ScreenStack (Free (SingleScreen n e s f) a)
    deriving (Functor, Applicative, Monad)

screen ::
     Functor f => (s -> Widget n) -> (e -> s -> f (a, s)) -> ScreenStack n e s f a
screen draw update = ScreenStack $ liftF $ SingleScreen draw update

screenState ::
     Functor f => (s -> Widget n) -> (e -> StateT s f a) -> ScreenStack n e s f a
screenState draw update = screen draw (\e s -> runStateT (update e) s)

hoistScreenStack ::
     Functor g
  => (forall x. f x -> g x)
  -> ScreenStack n e s f a
  -> ScreenStack n e s g a
hoistScreenStack func (ScreenStack inner) =
  ScreenStack $ hoistFree (hoistSingleScreen func) inner

zoomScreenStack ::
     Functor f => Lens' s t -> ScreenStack n e t f a -> ScreenStack n e s f a
zoomScreenStack l (ScreenStack inner) =
  ScreenStack $ hoistFree (zoomSingleScreen l) inner

zoomMultiScreenStack ::
     (Monoid a, Monad f)
  => (Widget n -> Widget n -> Widget n)
  -> Traversal' s t
  -> ScreenStack n e t f a
  -> ScreenStack n e s f a
zoomMultiScreenStack combineFunc l (ScreenStack inner) =
  let helper (Free screen) =
        Free
          (helper . getAp <$>
           zoomMultiSingleScreen combineFunc l (Ap <$> screen))
      helper (Pure a) = Pure a
   in ScreenStack $ helper inner

screenStackApp :: App (s, ScreenStack n (BrickEvent n e) s TrackedIO a) e n
screenStackApp =
  let eventHelper (s, ScreenStack (Free (SingleScreen _ update))) e =
        let helper (Free f, s') = continue (s', ScreenStack $ Free f)
            helper (Pure x, s') = halt (s', ScreenStack $ Pure x)
         in case update e s of
              NoIOUsed x    -> helper x
              TrackedIO act -> liftIO act >>= helper
   in App
        { appDraw =
            \case
              (s, ScreenStack (Free (SingleScreen draw _))) -> [draw s]
              (_, ScreenStack (Pure _)) -> error "Can't draw pure"
        , appChooseCursor = \_ _ -> Nothing
        , appHandleEvent = eventHelper
        , appStartEvent = pure
        , appAttrMap = \_ -> attrMap V.defAttr []
        }

screenStackMain ::
     Ord n => ScreenStack n (BrickEvent n e) s TrackedIO () -> s -> IO s
screenStackMain stack s = fst <$> defaultMain screenStackApp (s, stack)
