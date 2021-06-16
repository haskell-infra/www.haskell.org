{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Testimonial where
import GHC.Generics
import qualified Data.Aeson as Aeson
import Hakyll
import Hakyll.Core.Util.File
import Hakyll.Core.Compiler
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.Writable
import Data.Binary
import qualified Data.ByteString.Lazy as BL

data Testimonial = Testimonial
  { company          :: String
  , logo             :: String
  , shortTestimonial :: String
  } deriving (Eq, Show, Generic)

instance Binary Testimonial where
  put Testimonial{..} = do
    put company
    put logo
    put shortTestimonial

  get = Testimonial <$> get <*> get <*> get

instance Writable Testimonial where
  write path Item{..} =
    BL.writeFile path $ Aeson.encode itemBody

instance Aeson.FromJSON Testimonial
instance Aeson.ToJSON Testimonial

parseTestimonialM :: MonadFail m => BL.ByteString -> m Testimonial
parseTestimonialM body = pure $ Testimonial "company" "logo" "shortTestimonial"
  -- case Aeson.eitherDecode body of
  --   Left err -> fail err
  --   Right t -> pure t
