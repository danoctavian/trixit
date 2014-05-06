module Utils where

import Prelude as P 
import Control.Monad.Trans.Either
import Control.Monad

if' c a b = if c then a else b


-- breakable loops (with quit)
iterateForever :: Monad m => (a -> m a) -> a -> m b
iterateForever f v = f v >>= iterateForever f

loop :: (Monad m) => EitherT e m a -> m e
loop = liftM (either id id) . runEitherT . forever

iterateLoop :: (Monad m) => (a -> EitherT e m a) -> a -> m e
iterateLoop c start = liftM (either id id) . runEitherT $ iterateForever c start

quit :: (Monad m) => e -> EitherT e m r
quit = left


fromEither :: Either a a -> a
fromEither (Left x) = x
fromEither (Right x) = x

fromRight (Right x) = x