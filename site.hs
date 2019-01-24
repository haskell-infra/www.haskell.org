--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import Data.Time.Clock
import Data.Time.Calendar
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = do
  (year, _, _) <- toGregorian . utctDay <$> getCurrentTime
  hakyll $ do
    let ctx = constField "year" (show year)
           <> defaultContext

    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "*.html" $ do
        route idRoute
        compile $ do

            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
