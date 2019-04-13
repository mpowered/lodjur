module Lodjur.Internal.JSON where

import           Data.Aeson

options :: Options
options = defaultOptions { tagSingleConstructors = True }
