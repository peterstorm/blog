{-# LANGUAGE CPP               #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import qualified Common
import Data.Proxy ( Proxy(..) )
import Control.Lens ( (^.), (+=), (-=), (.=), makeLenses )
import qualified Servant.API as Servant
import Servant.API ( (:<|>)(..) )
#if MIN_VERSION_servant(0,10,0)
import qualified Servant.Links as Servant
#endif
import qualified Miso
import Miso ( View, App(..) )
import qualified Miso.String as Miso
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign.Callback

main :: IO ()
main =
  Miso.miso $ \currentURI -> App
    { initialAction = Common.NoOp
    , model         = Common.Model currentURI
    , update        = Miso.fromTransition . updateModel
    , view          = viewModel
    , events        = Miso.defaultEvents
    , subs          = [ Miso.uriSub Common.HandleURIChange ]
    , mountPoint    = Nothing
    }
      where 
        viewModel m = case Miso.runRoute (Proxy :: Proxy Common.ViewRoutes ) Common.handlers Common._uri m of
                       Left _  -> Common.page404View
                       Right v -> v

updateModel
    :: Common.Action
    -> Miso.Transition Common.Action Common.Model ()
updateModel action =
    case action of
      Common.NoOp          -> pure ()
      Common.ChangeURI uri ->
        Miso.scheduleIO $ do
          Miso.pushURI uri
          pure Common.NoOp
      Common.HandleURIChange uri -> Common.uri .= uri
      Common.InitMasonry -> Miso.scheduleIO $ do 
        initMasonry
        pure Common.NoOp

foreign import javascript unsafe "initMasonry" initMasonry :: IO ()
