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

  -- * Display documents annotated with pair of strings
  , displayWrapped, displayWrappedT, displayWrappedS

  -- * Display as HTML
  , displayHTML, displayHTMLT, displayHTMLS

  -- * Display with ANSI escape sequences
  , displayStyleCode, displayStyleCodeT, displayStyleCodeS

  -- * Display to a file handle with ANSI escape sequences
  , hDisplayStyle, displayStyle, hPutDocStyle, putDocStyle
) where

import Text.PrettyPrint.Annotated.WL
import System.Console.Style
import Control.Monad.Trans
import System.IO (Handle, hPutStr, stdout)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL

-- | Escape a HTML string by replacing special characters with HTML entities.
escapeHTML :: String -> String
escapeHTML = concatMap $ \c ->
  case c of
    '"' -> "&quot;"
    '&' -> "&amp;"
    '<' -> "&lt;"
    '>' -> "&gt;"
    _   -> [c]

-- | Display a rendered document which is annotated with pairs of strings and
-- output a 'Monoid'.
--
-- The first element of the pair is prepended to the annotated region,
-- the second after the annotated region.
displayWrapped :: Monoid o => (String -> o) -> SimpleDoc (String, String) -> o
displayWrapped f = displayDecorated (f . fst) (f . snd) f

-- | Display a rendered document which is annotated with pairs of strings and
-- output 'Text'.
--
-- The first element of the pair is prepended to the annotated region,
-- the second after the annotated region.
displayWrappedT :: SimpleDoc (String, String) -> TL.Text
displayWrappedT = TL.toLazyText . displayWrapped TL.fromString

-- | Display a rendered document which is annotated with pairs of strings and
-- output a 'ShowS' function.
--
-- The first element of the pair is prepended to the annotated region,
-- the second after the annotated region.
displayWrappedS :: SimpleDoc (String, String) -> ShowS
displayWrappedS = displayDecoratedA ((++) . fst) ((++) . snd) (++)

-- | Display a rendered document as HTML and output a 'Monoid'.
--
-- The annotated region is wrapped by 'span' with the 'class' attribute
-- given by the annotation function.
displayHTML :: Monoid o => (String -> o) -> (a -> String) -> SimpleDoc a -> o
displayHTML f g = displayDecorated push pop str
  where push t = f "<span class=\"" `mappend` f (g t) `mappend` f "\">"
        pop    = const $ f "</span>"
        str    = f . escapeHTML

-- | Display a rendered document as HTML and output 'Text'.
--
-- The annotated region is wrapped by 'span' with the 'class' attribute
-- given by the annotation function.
displayHTMLT :: (a -> String) -> SimpleDoc a -> TL.Text
displayHTMLT f = TL.toLazyText . displayHTML TL.fromString f

-- | Display a rendered document as HTML and output a 'ShowS' function.
--
-- The annotated region is wrapped by 'span' with the 'class' attribute
-- given by the annotation function.
displayHTMLS :: (a -> String) -> SimpleDoc a -> ShowS
displayHTMLS f = (++) . displayHTML id f

-- | Display a rendered document with ANSI escape sequences and output a 'Monoid'.
--
-- The annotations are mapped to a '[SetStyle]' array.
displayStyleCode :: Monoid o => (String -> o) -> (a -> [SetStyle]) -> Term -> SimpleDoc a -> o
displayStyleCode f g term = runStyle term . displayDecoratedA push pop (pure . f)
 where push  x = f <$> styleCode (Save:g x)
       pop   _ = f <$> styleCode (Restore:[])

-- | Display a rendered document with ANSI escape sequences and output a 'ShowS' function.
--
-- The annotations are mapped to a '[SetStyle]' array.
displayStyleCodeS :: (a -> [SetStyle]) -> Term -> SimpleDoc a -> ShowS
displayStyleCodeS f term = (++) . displayStyleCode id f term

-- | Display a rendered document with ANSI escape sequences and output 'Text'.
--
-- The annotations are mapped to a '[SetStyle]' array.
displayStyleCodeT :: (a -> [SetStyle]) -> Term -> SimpleDoc a -> TL.Text
displayStyleCodeT f term = TL.toLazyText . displayStyleCode TL.fromString f term

-- | Display a rendered document with ANSI escape sequences to a given 'Handle'.
--
-- The annotations are mapped to a '[SetStyle]' array.
hDisplayStyle :: MonadIO m => Handle -> (a -> [SetStyle]) -> SimpleDoc a -> m ()
hDisplayStyle h f = hRunWithStyle h [] . displayDecoratedA push pop (liftIO . hPutStr h)
 where push  x = setStyle (Save:f x)
       pop   _ = setStyle (Restore:[])

-- | Display a rendered document with ANSI escape sequences to 'stdout'.
--
-- The annotations are mapped to a '[SetStyle]' array.
displayStyle :: MonadIO m => (a -> [SetStyle]) -> SimpleDoc a -> m ()
displayStyle = hDisplayStyle stdout

-- | The action @(putDocStyle f doc)@ pretty prints document @doc@ to standard output
-- using the annotations.
--
-- The annotations are mapped by @f@ to @[SetStyle]@ arrays.
putDocStyle :: (a -> [SetStyle]) -> Doc a -> IO ()
putDocStyle = hPutDocStyle stdout

-- | The action @(hPutDocStyle handle f doc)@ pretty prints document @doc@ to file handle @handle@
-- using the annotations.
--
-- The annotations are mapped by @f@ to @[SetStyle]@ arrays.
hPutDocStyle :: Handle -> (a -> [SetStyle]) -> Doc a -> IO ()
hPutDocStyle handle f = hDisplayStyle handle f . renderPrettyDefault
