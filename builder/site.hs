--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
import           Data.Aeson
import qualified Data.ByteString.Lazy          as BL
import           Data.Monoid                   ((<>))
import           Data.Time.Calendar
import           Data.Time.Clock
import qualified GHC.IO.Encoding               as Encoding
import           Hakyll
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Provider
import           System.FilePath.Posix
import           Testimonial

--------------------------------------------------------------------------------
main :: IO ()
main = do
  Encoding.setLocaleEncoding Encoding.utf8
  ctx <- mkContext
  hakyllWith configuration $ do
    match "testimonials/logos/*" $ do
      route idRoute
      compile copyFileCompiler

    match "testimonials/*.yaml" $ do
      compile parseTestimonialCompiler

    create ["testimonials.json"] $ do
      route idRoute
      compile $ do
        testimonials <- loadAll @Testimonial "testimonials/*.yaml"
        item <- (makeItem . BL.unpack . encode . map itemBody) testimonials
        saveSnapshot "_final" item
        pure item

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
      compile $ do
        testimonials <- loadAll @Testimonial "testimonials/*.yaml"
        let
          indexCtx = listField "testimonials" testimonialContext (pure testimonials) `mappend`
                     ctx
        defCompiler indexCtx

    let redirects = [ ("humor/index.html", "https://wiki.haskell.org/Humor")
                    , ("hat/index.html", "https://wiki.haskell.org/Hat")
                    , ("development/views.html", "http://web.archive.org/web/20040107202217/http://haskell.org/development/views.html")
                    ]
    version "redirects" $ createRedirects redirects

    match ("**/*.markdown" .||. "*.markdown") $ do
      route cleanRoute
      compile $ mdCompiler ctx

    match "*.pdf" $ do
      route   idRoute
      compile copyFileCompiler

    match "templates/*" $
      compile templateCompiler


configuration :: Configuration
configuration = defaultConfiguration{ providerDirectory = "site" }

parseTestimonialCompiler :: Compiler (Item Testimonial)
parseTestimonialCompiler = do
  identifier  <- getUnderlying
  provider    <- compilerProvider <$> compilerAsk
  body        <- unsafeCompiler $ BL.readFile (resourceFilePath provider identifier)
  testimonial <- parseTestimonialM (BL.toStrict body)
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

testimonialContext :: Context Testimonial
testimonialContext =
  mconcat [ field "companyName" (pure . companyName . itemBody)
          , field "logoURL" (pure . logoURL . itemBody)
          , field "shortTestimonial" (pure . shortTestimonial . itemBody)
          , field "companyURL" (pure . companyURL . itemBody)
          ]
