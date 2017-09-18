module Gorebab where

import Control.Monad.Eff.Uncurried (EffFn3)
import Data.Array (fromFoldable)
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Data.List (List, (:))
import Data.Monoid (mempty)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import JollyPong (ActionVariant)
import RxJS.Observable (ObservableImpl)
import RxJS.Scheduler (Scheduler)
import Type.Row (class RowToList, Cons, Nil, RLProxy(RLProxy), kind RowList)
import Unsafe.Coerce (unsafeCoerce)

-- | The type of an Epic
newtype Epic e action store options = Epic (
  EffFn3
    e
    (ActionsObservable action)
    store
    options
    (ObservableImpl action)
)

-- | An ActionsObservable
foreign import data ActionsObservable :: Type -> Type

toObservable :: forall a. ActionsObservable a -> ObservableImpl a
toObservable = unsafeCoerce

-- | Make an ActionsObservable from an array of values
of_ :: forall a. Array a -> ActionsObservable a
of_ = runFn1 _of

-- | Make an ActionsObservable from an array of values and Scheduler
from :: forall a. Array a -> Scheduler -> ActionsObservable a
from = runFn2 _from

-- | Combine a homogenous Epics array. I don't see how this could handle not being homogenous.
combineEpics :: forall e action store options
   . Array (Epic e action store options)
  -> Epic e action store options
combineEpics = runFn1 _combineEpics

-- | Filter ActionsObservable by the provided labels of the action variants.
-- | You can provide anything that carries the actions row, like a RProxy or Record
ofType :: forall actions selected proxy kl trash
   . RowToList selected kl
  => GetKeys selected kl
  => Union selected trash actions
  => proxy selected
  -> ActionsObservable (ActionVariant actions)
  -> ActionsObservable (ActionVariant selected)
ofType _ observable = runFn2 _ofType keys observable
  where
    keys = fromFoldable (getKeys (RLProxy :: RLProxy kl))

class GetKeys (row :: # Type) (rl :: RowList)
  | rl -> row where
  getKeys :: RLProxy rl -> List String

instance keysCons ::
  ( IsSymbol name
  , GetKeys row tail
  ) => GetKeys row (Cons name ty tail) where
  getKeys _ = first : rest
    where
      first = reflectSymbol (SProxy :: SProxy name)
      rest = getKeys (RLProxy :: RLProxy tail)

instance keysNil :: GetKeys trash Nil where
  getKeys _ = mempty

foreign import _of :: forall a.
  Fn1
    (Array a)
    (ActionsObservable a)

foreign import _from :: forall a.
  Fn2
    (Array a)
    Scheduler
    (ActionsObservable a)

foreign import _ofType :: forall keys a b.
  Fn2
    (Array keys)
    (ActionsObservable a)
    (ActionsObservable b)

foreign import _combineEpics :: forall action store options e.
  Fn1
    (Array (Epic e action store options))
    (Epic e action store options)
