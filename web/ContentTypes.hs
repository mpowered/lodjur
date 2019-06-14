{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}

module ContentTypes where

import           Data.String.Conversions        ( cs )
import           Data.Text                      ( Text )
import           Network.HTTP.Media             ( (//), (/:) )
import           Servant

data Javascript

instance Accept Javascript where
   contentType _ = "application" // "javascript" /: ("charset", "utf-8")

instance MimeRender Javascript Text where
  mimeRender _ = cs