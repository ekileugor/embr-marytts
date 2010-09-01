module Paths_Embr where

getDataFileName :: FilePath -> IO FilePath
getDataFileName f = return $ "./data/" ++ f