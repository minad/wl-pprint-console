{-# LANGUAGE Rank2Types #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Console.WL
-- Copyright   :  Daniel Mendler (c) 2016,
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  mail@daniel-mendler.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This is a pretty printer with support for annotations.
-- The annotations can be mapped to ANSI escape sequences
-- to allow for colorful output on consoles. For this purpose
-- the console-style library is used.
-----------------------------------------------------------

module Text.PrettyPrint.Console.WL (
  module Text.PrettyPrint.Annotated.WL

  -- * Display documents annotated with pairs of strings
  , displayWrapped, displayWrappedT, displayWrappedB, displayWrappedS

  -- * Display as HTML
  , displayHTML, displayHTMLT, displayHTMLB, displayHTMLS

  -- * Display to a file handle with ANSI escape sequences
  , displayStyle, hPutDocStyle, putDocStyle

  -- * Display without annotations (Missing from wl-pprint-annotated)
  , displayB
) where

import Text.PrettyPrint.Annotated.WL
import System.Console.Style.Flat
import System.IO (Handle, hPutStr, stdout)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BL

-- | @(display simpleDoc)@ takes the output @simpleDoc@ from a
-- rendering function and outputs a 'ByteString'. Along the way, all annotations are
-- discarded.
displayB :: SimpleDoc a -> BL.ByteString
displayB = BL.toLazyByteString . displayDecorated cm cm BL.stringUtf8
 where cm = const mempty

-- | Escape a HTML string by replacing special characters with HTML entities.
escapeHTML :: String -> String
escapeHTML = concatMap $ \c ->
  case c of
    '"' -> "&quot;"
    '&' -> "&amp;"
    '<' -> "&lt;"
    '>' -> "&gt;"
    _   -> [c]

-- | Display a rendered document which is annotated with pairs of strings @(String,String)@ and
-- output a 'Monoid'.
--
-- The first element of the pair is prepended to the annotated region,
-- the second after the annotated region.
displayWrapped :: Monoid o => (String -> o) -> SimpleDoc (String, String) -> o
displayWrapped f = displayDecorated (f . fst) (f . snd) f

-- | Display a rendered document which is annotated with pairs of strings @(String,String)@ and
-- output 'Text'.
--
-- The first element of the pair is prepended to the annotated region,
-- the second after the annotated region.
displayWrappedT :: SimpleDoc (String, String) -> TL.Text
displayWrappedT = TL.toLazyText . displayWrapped TL.fromString

-- | Display a rendered document which is annotated with pairs of strings @(String,String)@ and
-- output 'ByteString'.
--
-- The first element of the pair is prepended to the annotated region,
-- the second after the annotated region.
displayWrappedB :: SimpleDoc (String, String) -> BL.ByteString
displayWrappedB = BL.toLazyByteString . displayWrapped BL.stringUtf8

-- | Display a rendered document which is annotated with pairs of strings @(String,String)@ and
-- output a 'ShowS' function.
--
-- The first element of the pair is prepended to the annotated region,
-- the second after the annotated region.
displayWrappedS :: SimpleDoc (String, String) -> ShowS
displayWrappedS = displayDecoratedA ((++) . fst) ((++) . snd) (++)

-- | Display a rendered document as HTML and output a 'Monoid'.
--
-- The annotated region is wrapped by @<span class="f a">..</span>@ with the @class@ attribute
-- given by the annotation function.
displayHTML :: Monoid o => (String -> o) -> (a -> String) -> SimpleDoc a -> o
displayHTML f g = displayDecorated push pop str
  where push t = f "<span class=\"" `mappend` f (g t) `mappend` f "\">"
        pop    = const $ f "</span>"
        str    = f . escapeHTML

-- | Display a rendered document as HTML and output 'Text'.
--
-- The annotated region is wrapped by @<span class="f a">..</span>@ with the @class@ attribute
-- given by the annotation function.
displayHTMLT :: (a -> String) -> SimpleDoc a -> TL.Text
displayHTMLT f = TL.toLazyText . displayHTML TL.fromString f

-- | Display a rendered document as HTML and output 'ByteString'.
--
-- The annotated region is wrapped by @<span class="f a">..</span>@ with the @class@ attribute
-- given by the annotation function.
displayHTMLB :: (a -> String) -> SimpleDoc a -> BL.ByteString
displayHTMLB f = BL.toLazyByteString . displayHTML BL.stringUtf8 f

-- | Display a rendered document as HTML and output a 'ShowS' function.
--
-- The annotated region is wrapped by @<span class="f a">..</span>@ with the @class@ attribute
-- given by the annotation function.
displayHTMLS :: (a -> String) -> SimpleDoc a -> ShowS
displayHTMLS f = (++) . displayHTML id f

-- | Display a rendered document with ANSI escape sequences and output a 'Styled' array.
--
-- The annotations are mapped to a 'Styled' array.
displayStyle :: (String -> o) -> (forall x. a -> [Styled x]) -> SimpleDoc a -> [Styled o]
displayStyle f g d = displayDecoratedA push pop str d []
  where push = (++) . (Push:) . g
        pop = const (Pop:)
        str = (++) . (:[]) . Value . f

-- | The action @(putDocStyle f doc)@ pretty prints @doc@ to 'stdout'
-- using the annotations.
--
-- The annotations are mapped by @f@ to 'Styled' arrays.
putDocStyle :: Term -> (forall x. a -> [Styled x]) -> Doc a -> IO ()
putDocStyle = hPutDocStyle stdout

-- | The action @(hPutDocStyle handle f doc)@ pretty prints @doc@ to file handle @handle@
-- using the annotations.
--
-- The annotations are mapped by @f@ to 'Styled' arrays.
hPutDocStyle :: Handle -> Term -> (forall x. a -> [Styled x]) -> Doc a -> IO ()
hPutDocStyle handle term f = hPrintStyled hPutStr handle term . displayStyle id f . renderPrettyDefault
