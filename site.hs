--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

--------------------------------------------------------------------------------
import Bibtex
import System.IO
import Text.Megaparsec (runParser)
import Text.PrettyPrint (render)

import Control.Monad (forM_)

getAllBibs :: IO [Bib]
getAllBibs = do
  leobib <- openFile "bib/leo.bib" ReadMode
  inp <- hGetContents leobib
  case runParser bibs "" inp of
    Right bibs -> return $ bibs
    Left err -> error $ show err

createBibs :: [Bib] -> IO ()
createBibs bibs =
  forM_ bibs $ \bib -> do
    bibfile <- openFile ("bib/" ++ (_name bib) ++ ".bib") WriteMode
    hPutStrLn bibfile (render (pp bib))

insertBibs :: [Bib] -> IO ()
insertBibs bibs = do
  rtemp <- openFile "research-template.html" ReadMode
  routp <- openFile "research.html" WriteMode
  inp <- hGetContents rtemp
  let (front, _:back) = break (=="STUB") $ lines inp
      bibadd = map (render . toHtml) bibs
      outp = unlines $ front ++ bibadd ++ back
  hPutStrLn routp outp

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

main :: IO ()
main = do
  bibs <- getAllBibs
  createBibs bibs  
  insertBibs bibs
  hakyllWith config $ do  
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "bib/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "pdf/*" $ do
        route   idRoute
        compile copyFileCompiler

--    match (fromList ["about.rst", "contact.markdown"]) $ do
--        route   $ setExtension "html"
--        compile $ pandocCompiler
--            >>= loadAndApplyTemplate "templates/default.html" defaultContext
--            >>= relativizeUrls

    match "news/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/news.html"    newsCtx
--            >>= loadAndApplyTemplate "templates/default.html" newsCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            news <- recentFirst =<< loadAll "news/*"
            let archiveCtx =
                    listField "news" newsCtx (return news) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match (fromList ["research.html", "CoqWSL.html", "misc.html", "students.html"]) $ do
        route idRoute
        compile $ do
            getResourceBody
                >>= applyAsTemplate defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            news <- recentFirst =<< loadAll "news/*"
            let indexCtx =
                    listField "news" newsCtx (return news) `mappend`
                    constField "title" "Home"              `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
pubsCtx :: Context String
pubsCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

newsCtx :: Context String
newsCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

