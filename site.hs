--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import Data.Time.Clock
import Data.Time.Calendar
import System.FilePath.Posix
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = do
  ctx <- mkContext
  hakyll $ do
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "index.html" $ do
        route idRoute
        compile $ defCompiler ctx

    match "*.html" $ do
        route cleanRoute
        compile $ defCompiler ctx


    match "templates/*" $ compile templateCompiler


defCompiler :: Context String -> Compiler (Item String)
defCompiler ctx = getResourceBody
  >>= applyAsTemplate ctx
  >>= loadAndApplyTemplate "templates/default.html" ctx
  >>= relativizeUrls


mkContext :: IO (Context String)
mkContext = do
  (year, _, _) <- toGregorian . utctDay <$> getCurrentTime
  return $ constField "year" (show year)
        <> dropIndexHtml
        <> defaultContext


cleanRoute :: Routes
cleanRoute = customRoute $
  (\(p, _) -> p </> "index" <.> "html") . splitExtension . toFilePath


dropIndexHtml :: Context a
dropIndexHtml = mapContext transform (urlField "url") where
    transform url = case splitFileName url of
                        (p, "index.html") -> takeDirectory p
                        _                 -> url
