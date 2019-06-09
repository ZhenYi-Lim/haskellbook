{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Database where

import Control.Exception
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket hiding (close, recv)
import Text.RawString.QQ
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

data DuplicateData = DuplicateData deriving (Eq, Show, Typeable)

instance Exception DuplicateData

data User =
    User {
    userId :: Integer
    , username :: Text
    , shell :: Text
    , homeDirectory :: Text
    , realName :: Text
    , phone :: Text
    } deriving (Eq, Show)
    
instance FromRow User where
    fromRow = User <$> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field

instance ToRow User where
    toRow (User id_ username shell homeDir realName phone) =
        toRow (id_, username, shell, homeDir, realName, phone)

createUsers :: Query
createUsers = [r|
    CREATE TABLE IF NOT EXISTS users
        (id INTEGER PRIMARY KEY AUTOINCREMENT,
        username TEXT UNIQUE,
        shell TEXT, homeDirectory TEXT,
        realName TEXT, phone TEXT)
    |]

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers = "SELECT * from users"

getUserQuery :: Query
getUserQuery = "SELECT * from users where username = ?"

type UserRow = (Null, Text, Text, Text, Text, Text)

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    [] -> return $ Nothing
    [user] -> return $ Just user
    _ -> throwIO DuplicateData

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute conn insertUser meRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
    where meRow :: UserRow
          meRow = (Null, "callen", "/bin/zsh", "/home/callen", "Chris Allen", "555-123-4567")

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) = BS.concat
  ["Login: ", e username, "\t\t\t\t",
  "Name: ", e realName, "\n",
  "Directory: ", e homeDir, "\t\t\t",
  "Shell: ", e shell, "\n"]
    where e = encodeUtf8

modQueryUsername :: String -> Query
modQueryUsername value = Query $ T.pack ("UPDATE users SET username = \"" ++ value ++ "\" where username = ?")

modQueryShell :: String -> Query
modQueryShell value = Query $ T.pack ("UPDATE users SET shell = \"" ++ value ++ "\" where username = ?")

modQueryHome :: String -> Query
modQueryHome value = Query $ T.pack ("UPDATE users SET homeDirectory = \"" ++ value ++ "\" where username = ?")

modQueryName :: String -> Query
modQueryName value = Query $ T.pack ("UPDATE users SET realName = \"" ++ value ++ "\" where username = ?")

modQueryPhone :: String -> Query
modQueryPhone value = Query $ T.pack ("UPDATE users SET phone = \"" ++ value ++ "\" where username = ?")
