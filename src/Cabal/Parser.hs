module Cabal.Parser
    ( compatParseGenericPackageDescription
    , ParseResult(..)
    , GenericPackageDescription
    ) where

import           Control.Monad

import           Data.ByteString                       (ByteString)
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import qualified Data.Text.Encoding.Error              as T

import           Distribution.InstalledPackageInfo     (PError (..))
import           Distribution.PackageDescription       (GenericPackageDescription (..),
                                                        specVersion)
import           Distribution.PackageDescription.Parse (parseGenericPackageDescription)
import           Distribution.ParseUtils               (ParseResult (..))
import           Distribution.Version                  (mkVersion)

import qualified Cabal.Parser.V122                     as V122
import qualified Cabal.Parser.V124                     as V124
import qualified Cabal.Parser.V200                     as V200

unpackUTF8 :: ByteString -> String
unpackUTF8 raw = case T.unpack t of
                '\65279':cs -> cs
                cs          -> cs
  where
    t = T.decodeUtf8With T.lenientDecode raw

-- | Augmented version of 'parseGenericPackageDescription' which additional compatibility checks
compatParseGenericPackageDescription :: ByteString -> ParseResult GenericPackageDescription
compatParseGenericPackageDescription bs = case parseGenericPackageDescription (unpackUTF8 bs) of
                                            pe@(ParseFailed {})  -> pe
                                            pok@(ParseOk _pwarns gpd) ->
                                              case go (specVersion (packageDescription gpd)) of
                                                Nothing   -> pok
                                                Just perr -> ParseFailed perr
 where
   go v = msum $ [ goV200 | v < mkVersion [2,1]  ]
              ++ [ goV124 | v < mkVersion [1,25] ]
              -- NB: cabal spec versions prior to cabal-version:2.0 need to be parseable by older parsers as well
              ++ [ goV122 | v < mkVersion [1,25] ]

   goV200 :: Maybe PError
   goV200 = case V200.parseGenericPackageDescription (unpackUTF8 bs) of
              V200.ParseFailed perr -> Just $! convertPerr perr
              V200.ParseOk _ _      -> Nothing
     where
       convertPerr :: V200.PError -> PError
       convertPerr (V200.AmbiguousParse s lno) = AmbiguousParse ("[v2.0] " ++ s) lno
       convertPerr (V200.FromString s mlno)    = FromString     ("[v2.0] " ++ s) mlno
       convertPerr (V200.NoParse s lno)        = NoParse        ("[v2.0] " ++ s) lno
       convertPerr (V200.TabsError lno)        = TabsError                       lno

   goV124 :: Maybe PError
   goV124 = case V124.parsePackageDescription (unpackUTF8 bs) of
              V124.ParseFailed perr -> Just $! convertPerr perr
              V124.ParseOk _ _      -> Nothing
     where
       convertPerr :: V124.PError -> PError
       convertPerr (V124.AmbiguousParse s lno) = AmbiguousParse ("[v1.24] " ++ s) lno
       convertPerr (V124.FromString s mlno)    = FromString     ("[v1.24] " ++ s) mlno
       convertPerr (V124.NoParse s lno)        = NoParse        ("[v1.24] " ++ s) lno
       convertPerr (V124.TabsError lno)        = TabsError                        lno

   goV122 :: Maybe PError
   goV122 = case V122.parsePackageDescription (unpackUTF8 bs) of
              V122.ParseFailed perr -> Just $! convertPerr perr
              V122.ParseOk _ _      -> Nothing
     where
       convertPerr :: V122.PError -> PError
       convertPerr (V122.AmbiguousParse s lno) = AmbiguousParse ("[v1.22] " ++ s) lno
       convertPerr (V122.FromString s mlno)    = FromString     ("[v1.22] " ++ s) mlno
       convertPerr (V122.NoParse s lno)        = NoParse        ("[v1.22] " ++ s) lno
       convertPerr (V122.TabsError lno)        = TabsError                        lno
