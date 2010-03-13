module DeepSeqInstances where

import Control.DeepSeq
import qualified Data.ByteString as B

instance NFData B.ByteString where
  rnf a = a `seq` ()
