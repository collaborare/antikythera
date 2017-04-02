module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import Database.Persist.Sql -- (ConnectionPool, runSqlPool)
import Settings.StaticFiles
import Settings
import Model
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Yesod.Core.Types
-- snip
import qualified Data.Text as T
import Data.Text.Encoding
import Network.Wai as Wai

data App = App
  { appSettings    :: AppSettings
  , appStatic      :: Static -- ^ Settings for static file serving.
  , appConnPool    :: ConnectionPool -- ^ Database connection pool.
  , appHttpManager :: Manager
  , appLogger      :: Logger
  }

instance HasHttpManager App where
  getHttpManager = appHttpManager

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

renderLayout :: Widget -> Handler Html
renderLayout widget = do
  master <- getYesod
  mmsg <- getMessage
  wc <- widgetToPageContent widget

  pc <- widgetToPageContent $ do
      mapM_ addScript $ map StaticR
          [ 
          ]
      mapM_ addStylesheet $ map StaticR
          [ 
          ]
      $(widgetFile "default-layout")

  withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

approotRequest :: App -> Request -> T.Text
approotRequest master req =
  case requestHeaderHost req of
    Just a  -> prefix `T.append` decodeUtf8 a
    Nothing -> appRoot $ appSettings master
  where
    prefix =
      if 
        "https://" `T.isPrefixOf` appRoot (appSettings master)
        then
          "https://"
        else
          "http://"

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  --approot = ApprootMaster $ appRoot . appSettings
  approot = ApprootRequest approotRequest

  -- change maximum content length
  maximumContentLength _ _ = Just $ 1024 ^ (5 :: Int)

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is 120 minutes
  makeSessionBackend _ = Just <$> defaultClientSessionBackend
    120    -- timeout in minutes
    "config/client_session_key.aes"

  -- defaultLayout widget =
  --   renderLayout $(widgetFile "default-widget")
  defaultLayout = renderLayout

  -- This is done to provide an optimization for serving static files from
  -- a separate domain. Please see the staticRoot setting in Settings.hs
  -- urlRenderOverride y (StaticR s) =
  --     Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
  -- urlRenderOverride _ _ = Nothing

  -- The page to be redirected to when authentication is required.
  -- authRoute _ = Just $ AuthR LoginR

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
    where
      -- Generate a unique filename based on the content itself
      genFileName lbs = "autogen-" ++ base64md5 lbs

  -- Place Javascript at head so scripts become loaded before content
  jsLoader _ = BottomOfHeadBlocking

  -- What messages should be logged. The following includes all messages when
  -- in development, and warnings and errors in production.
  shouldLog app _source level =
    (const False) (appSettings app)
      || level == LevelWarn
      || level == LevelError

  makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner appConnPool

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage
