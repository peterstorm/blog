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

import Model
import Action
import Routes

main :: IO ()
main =
  Miso.miso $ \currentURI -> App
    { initialAction = NoOp
    , model         = Model currentURI
    , update        = Miso.fromTransition . updateModel
    , view          = viewModel
    , events        = Miso.defaultEvents
    , subs          = [ Miso.uriSub HandleURIChange ]
    , mountPoint    = Nothing
    }
      where 
        viewModel m = case Miso.runRoute (Proxy :: Proxy ViewRoutes ) Common.handlers _uri m of
                       Left _  -> Common.page404View
                       Right v -> v

updateModel
    :: Action
    -> Miso.Transition Action Model ()
updateModel action =
    case action of
      NoOp          -> pure ()
      ChangeURI uri ->
        Miso.scheduleIO $ do
          Miso.pushURI uri
          pure NoOp
      HandleURIChange uri -> Model.uri .= uri
      InitMasonry -> Miso.scheduleIO $ do 
        initMasonry
        pure NoOp

foreign import javascript unsafe "initMasonry" initMasonry :: IO ()
