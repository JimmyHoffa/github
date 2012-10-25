{-# LANGUAGE OverloadedStrings #-}
module Main (
    main
) where

import Plane
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status404)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.Monoid
import Data.Text (Text, unpack)

import Control.Applicative ((<*>),
                            (*>),
                            (<$>),
                            (<|>),
                            pure)
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as AC
import Data.Attoparsec.Text (Parser)
import Control.Monad.Trans.Resource
import Network.HTTP.Types

main :: IO ()
main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Request -> ResourceT IO Response
app req = do
    return $ moveArtifactResponse path
    where path = pathInfo req



moveArtifactResponse :: [Text] -> Response
moveArtifactResponse splitPath@(oldX:oldY:newX:newY:_) =
    case (maybeArtifact, maybeX, maybeY) of
        (Just artifact, Just x, Just y) -> createJsonResponse $ show $ location $ moveArtifact examplePlane (artifact) (x, y)
        (_, _, _) -> notFoundResponse splitPath
    where maybeArtifact = textToArtifact oldX oldY
          maybeX = textToFloat newX
          maybeY = textToFloat newY

moveArtifactResponse splitPath = notFoundResponse splitPath

notFoundResponse :: [Text] -> Response
notFoundResponse path =
    createErrorResponse status200 $ "404 NOT FOUND LOCATION READ AS: " ++ (show $ fmap textToFloatString path) ++ "<br/>" ++ (concat $ fmap show examplePlane)

createResponse :: BU.ByteString -> Status -> (String -> Response)
createResponse contentType status response = do
    ResponseBuilder status [("Content-Type", contentType)] . mconcat . fmap copyByteString $ [BU.fromString response]

createErrorResponse = createResponse "text/html"
createJsonResponse = createResponse "text/javascript" status200
createHtmlResponse = createResponse "text/html" status200

textToFloat :: Text -> Maybe Float
textToFloat x
    | (length $ textReads x) /= 1 = Nothing
    | (snd $ head $ textReads x) /= [] = Nothing
    | otherwise = Just $ fst $ head $ textReads x
    where textReads = reads . unpack :: Text -> [(Float, String)]

textToArtifact :: Text -> Text -> Maybe Artifact
textToArtifact textX textY =
    case (maybeX, maybeY) of
    (Just x, Just y) -> Just $ Artifact Rectangle (x, y) (1,1)
    (_, _) -> Nothing
    where maybeX = textToFloat textX
          maybeY = textToFloat textY

textToFloatString :: Text -> Maybe String
textToFloatString x
    | textReads x == [] = Nothing
    | (snd $ head $ textReads x) /= [] = Nothing
    | otherwise = Just $ unpack x
    where textReads = reads . unpack :: Text -> [(Float, String)]

examplePlane :: Plane
examplePlane = [
    Artifact Rectangle (3, 3) (2,2),
    Artifact Rectangle (3, 8) (2,2),
    Artifact Rectangle (8, 3) (2,2),
    Artifact Rectangle (8, 8) (2,2)]
