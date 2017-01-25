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

  -- * Display with ANSI escape sequences
  , displayColoredT, displayColoredB, displayColoredS

  -- * Display to a file handle with ANSI escape sequences
  , displayColored, hPutDocColored, putDocColored

  -- * Display without annotations (Missing from wl-pprint-annotated)
  , displayB
) where

import Text.PrettyPrint.Annotated.WL
import Data.Monoid.Colorful.Flat
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

-- | Display a rendered document with ANSI escape sequences and output a 'ShowS' function.
--
-- The annotations are mapped to a '[Colored a]' array.
displayColoredS :: (a -> [Colored String]) -> Term -> SimpleDoc a -> ShowS
displayColoredS f term = showColoredS term . displayColored id f

-- | Display a rendered document with ANSI escape sequences and output 'Text'.
--
-- The annotations are mapped to a '[Colored TL.Builder]' array.
displayColoredT :: (a -> [Colored TL.Builder]) -> Term -> SimpleDoc a -> TL.Text
displayColoredT f term = TL.toLazyText . showColored id TL.fromString term . displayColored TL.fromString f

-- | Display a rendered document with ANSI escape sequences and output 'ByteString'.
--
-- The annotations are mapped to a '[Colored BL.Builder]' array.
displayColoredB :: (a -> [Colored BL.Builder]) -> Term -> SimpleDoc a -> BL.ByteString
displayColoredB f term = BL.toLazyByteString . showColored id BL.stringUtf8 term . displayColored BL.stringUtf8 f

-- | Display a rendered document with ANSI escape sequences and output a 'Colored' array.
--
-- The annotations are mapped to a 'Colored' array.
displayColored :: (String -> o) -> (a -> [Colored o]) -> SimpleDoc a -> [Colored o]
displayColored f g d = displayDecoratedA push pop str d []
  where push = (++) . (Push:) . g
        pop = const (Pop:)
        str = (++) . (:[]) . Value . f

-- | The action @(putDocColored f doc)@ pretty prints @doc@ to 'stdout'
-- using the annotations.
--
-- The annotations are mapped by @f@ to 'Colored' arrays.
putDocColored :: Term -> (a -> [Colored String]) -> Doc a -> IO ()
putDocColored = hPutDocColored stdout

-- | The action @(hPutDocColored handle f doc)@ pretty prints @doc@ to file handle @handle@
-- using the annotations.
--
-- The annotations are mapped by @f@ to 'Colored' arrays.
hPutDocColored :: Handle -> Term -> (a -> [Colored String]) -> Doc a -> IO ()
hPutDocColored handle term f = hPrintColored hPutStr handle term . displayColored id f . renderPrettyDefault
