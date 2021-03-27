--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid           ((<>))
import           Data.Time.Calendar
import           Data.Time.Clock
import           Hakyll
import           System.FilePath.Posix


--------------------------------------------------------------------------------
main :: IO ()
main = mkContext >>= \ctx -> hakyll $ do
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

  match ("**/**/*.markdown" .||. "**/*.markdown" .||. "*.markdown") $ do
    route cleanRoute
    compile $ mdCompiler ctx

  match "*.pdf" $ do
    route   idRoute
    compile copyFileCompiler

  match "templates/*" $
    compile templateCompiler


mdCompiler :: Context String -> Compiler (Item String)
mdCompiler ctx =
  pandocCompiler
  >>= applyAsTemplate ctx
  >>= loadAndApplyTemplate "templates/default.html" ctx
  >>= relativizeUrls


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
