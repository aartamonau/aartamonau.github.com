{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

import Hakyll

import Control.Monad ( forM_ )
import Control.Arrow ( (>>>), (&&&), arr, returnA )

import Data.List ( isPrefixOf )

import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Maybe ( fromJust )

import Data.String ( fromString )

dumpMeta :: Compiler (Page a) (Page a)
dumpMeta = unsafeCompiler (\p -> (print $ pageMetadata p) >> return p)

pages = [ "index.markdown"
        , "projects.markdown"
        , "about.markdown"
        ]

withMenu :: Compiler (Page a) (Page a)
withMenu =
  proc page -> do
    let current = getField "path" page

    content <- mapCompiler renderItem >>> arr unlines -< zip (repeat current) pages
    menu    <- renderTemplate "templates/menu.html" -< [("items", content)]

    returnA -< setField "menu" menu page

  where renderItem :: Compiler (String, String) String
        renderItem =
          proc (current, pageName) -> do
            (title, url, _) <- pageInfo -< pageName

            let mapping  = [("url", url), ("title", title)]
            let render   = renderTemplate "template"

            if current == pageName || (isBlogPost current && isBlog pageName)
               then renderTemplate "templates/menu/current.html" -< mapping
               else renderTemplate "templates/menu/item.html"    -< mapping

          where isBlogPost = isPrefixOf "posts/"
                isBlog     = isPrefixOf "index"

        resource :: String -> Resource
        resource = Resource . fromString

        renderTemplate :: Identifier -> Compiler [(String, String)] String
        renderTemplate template =
          arr (fromMap . Map.fromList)   >>>
          applyTemplateCompiler template >>>
          arr pageBody

-- title, url and date
type PageInfo = (String, String, String)

pageInfo :: Compiler FilePath PageInfo
pageInfo =
  proc path -> do
    page  <- readPageCompiler >>> postProcess -< resource path
    route <- fmap fromJust getRouteFor        -< parseIdentifier path

    let title = getField "title" page
    let url   = toUrl route
    let date  = getField "date" page

    returnA -< (title, url, date)

  where resource    = Resource . fromString
        dateFormat  = "%B %e, %Y"
        postProcess = addDefaultFields
                  >>> arr (renderDateField "date" dateFormat "")

main :: IO ()
main = hakyll $ do
    route   "css/*" idRoute
    compile "css/*" compressCssCompiler

    forM_ ["images/*"] $ \f -> do
        route   f idRoute
        compile f copyFileCompiler

    -- Pages
    forM_ (map fromString pages) $ \p -> do
        route   p $ setExtension "html"
        compile p $ pageCompiler
            >>> withMenu
            >>> applyTemplateCompiler "templates/default.html"

    route   "posts/*" $ setExtension "html"
    compile "posts/*" $ pageCompiler
        >>> applyTemplateCompiler "templates/post.html"
        >>> withMenu
        >>> applyTemplateCompiler "templates/default.html"

    -- Templates
    compile "templates/*" templateCompiler
    compile "templates/menu/*" templateCompiler
