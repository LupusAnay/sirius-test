module Main where

import Control.Exception (IOException, catch)
import Control.Lens ((^.))
import Control.Monad.Logger (LogLevel (LevelDebug))
import Data.Aeson (eitherDecodeFileStrict)
import Data.Generics.Labels ()
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Hasql.Pool (acquire)
import Hasql.Pool (Settings)
import Lib (Config (..), Env (..), app)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Servant.Errors
import Servant (JSON)

main :: IO ()
main = do
  appConfig <- readConfig
  dbPool <- acquire $ dbSettingsFromConfig appConfig
  let port = appConfig ^. #serverPort
  run port $ errorMw @JSON @["error", "status"] $ app (Env dbPool appConfig)

readConfig :: IO Config
readConfig = do
  local <- safeDecodeFile "settings.json.local"
  tracked <- safeDecodeFile "settings.json"
  pure $ chooseConfig local tracked

chooseConfig :: Either String Config -> Either String Config -> Config
chooseConfig (Right local) _ = local
chooseConfig (Left _) (Right tracked) = tracked
chooseConfig _ _ = defaultConfig

safeDecodeFile :: String -> IO (Either String Config)
safeDecodeFile path = catch (eitherDecodeFileStrict path) errorHandler
  where
    errorHandler e = pure (Left $ show (e :: IOException))

dbSettingsFromConfig :: Config -> Settings
dbSettingsFromConfig conf = (conf ^. #dbPoolSize, conf ^. #dbPoolTimeout, dbUrl)
  where
    dbUrl =
      encodeUtf8 $
        T.concat
          [ "postgresql://",
            conf ^. #dbUser,
            ":",
            conf ^. #dbPassword,
            "@",
            conf ^. #dbHost,
            ":",
            T.pack . show $ conf ^. #dbPort,
            "/",
            conf ^. #dbName
          ]

defaultConfig :: Config
defaultConfig =
  Config
    { dbHost = "localhost",
      dbPort = 5432,
      dbUser = "postgres",
      dbName = "postgres",
      dbPassword = "postgres",
      dbPoolSize = 1,
      dbPoolTimeout = 1,
      serverPort = 8001,
      logLevel = LevelDebug
    }
