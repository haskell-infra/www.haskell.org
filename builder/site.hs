--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid           ((<>))
import           Data.Time.Calendar
import           Data.Time.Clock
import           Hakyll
import           Hakyll.Core.Routes
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Provider
import           System.FilePath.Posix
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Testimonial
import Control.Monad


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

  match ("**/*.markdown" .||. "*.markdown") $ do
    route cleanRoute
    compile $ mdCompiler ctx

  match "*.pdf" $ do
    route   idRoute
    compile copyFileCompiler

  match "templates/*" $
    compile templateCompiler

  match "testimonials/logos/*" $ do
    route idRoute
    compile copyFileCompiler

  match "testimonials/*.yaml" $ do
    compile parseTestimonialCompiler

  create ["testimonials.json"] $ do
    route idRoute
    compile $ do
      unsafeCompiler $ putStrLn "inside of create testimonials compiler"
      testimonials <- loadAll "testimonials/*.yaml" :: Compiler [Item Testimonial]
      unsafeCompiler $ putStrLn $ "testimonials: " <> show testimonials
      item <- makeItem $ (BL.unpack . encode . map itemBody) testimonials
      saveSnapshot "_final" item
      pure item

parseTestimonialCompiler :: Compiler (Item Testimonial)
parseTestimonialCompiler = do
  identifier  <- getUnderlying
  provider    <- compilerProvider <$> compilerAsk
  body        <- unsafeCompiler $ BL.readFile (resourceFilePath provider identifier)
  testimonial <- parseTestimonialM body
  makeItem testimonial

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
