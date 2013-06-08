module PostProcessing
( 
) where

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Database.PostgreSQL.Simple

import DB
import KripkeTypes
import WebSpider

-- |Fold the subsites data in lambda into the respective main worlds data and
-- then delete the subsite world from lambda.
foldLambda :: Connection -> LambdaType -> IO ()
foldLambda c lamType = do
    ws <- liftM (filter isSubsiteUrl) (worldsInLambda c lamType)
    mapM_ (foldLambdaSubWorld c lamType) ws

-- |Fold a single subworld in lambda into the respective main worlds data and
-- delete the subsite world from lambda.
foldLambdaSubWorld :: Connection -> LambdaType -> T.Text -> IO ()
foldLambdaSubWorld c lamType subW = do
    let mainW  = getDomainAsText subW
    mainFmlCnt <- worldFmlsAndCounts c lamType mainW
    subFmlCnt  <- worldFmlsAndCounts c lamType subW
    mapM_ (foldLambdaSubworldEntry c lamType mainW mainFmlCnt) subFmlCnt
    deleteLambdaWorld c lamType subW

-- |Fold a single subworld entry in lambda into the main worlds data.
foldLambdaSubworldEntry :: Connection -> LambdaType -> T.Text ->
                           [(T.Text, Int)] -> (T.Text, Int) -> IO ()
foldLambdaSubworldEntry c lamType mainW mainFmlCnt (f, cnt) = do
    let oldCnt = fromMaybe 0 (lookup f mainFmlCnt)
    if oldCnt == 0
      then insertLambdaRelation c lamType
             (OneToNtuples mainW (S.singleton (f, cnt)))
      else updateFmlCount c lamType (LambdaEntry mainW f (oldCnt + cnt))

-- |True, if the given url is unequal to the domain only part of it.
isSubsiteUrl :: T.Text -> Bool
isSubsiteUrl url = url /= getDomainAsText url
