{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Testimonial where
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import Hakyll
import Hakyll.Core.Util.File
import Hakyll.Core.Compiler
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.Writable
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

data Testimonial = Testimonial
  { companyName      :: String
  , logoURL          :: String
  , shortTestimonial :: String
  } deriving (Eq, Show, Generic)

instance Binary Testimonial where
  put Testimonial{..} = do
    put companyName
    put logoURL
    put shortTestimonial

  get = Testimonial <$> get <*> get <*> get

instance Writable Testimonial where
  write path Item{..} =
    BL.writeFile path $ Aeson.encode itemBody

instance Aeson.FromJSON Testimonial
instance Aeson.ToJSON Testimonial

parseTestimonialM :: MonadFail m => BS.ByteString -> m Testimonial
parseTestimonialM body =
  case Yaml.decodeEither' body of
    Left err -> fail $ "failed to parse testimonial file: " <> show err
    Right parsed -> pure parsed
