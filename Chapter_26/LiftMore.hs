import Control.Monad.Trans.Class

import Control.Monad.Trans.Except

instance MonadTrans (EitherT e) where
    lift = EitherT . (liftM Right)