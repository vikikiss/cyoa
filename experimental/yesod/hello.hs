{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}

import Yesod
import Data.Text
  
data HelloWorld = HelloWorld Int

mkYesod "HelloWorld" [$parseRoutes|
  / HomeR GET
  /person/#Int PersonR GET
  |]

instance Yesod HelloWorld where
  approot _=""
  defaultLayout contents = do
    PageContent title headTags bodyTags <- widgetToPageContent $ do
      addCassius [$cassius|
#body
    font-family: sans-serif
#wrapper
    width: 760px
    margin: 0 auto
      |]        
      addWidget contents
    hamletToRepHtml [$hamlet|
!!!

<html>
    <head>
        <title>#{title}
        ^{headTags}
    <body>
        <div id="wrapper">
            ^{bodyTags}
          |]                            

getHomeR = do
  (HelloWorld n) <- getYesod
  defaultLayout $ do
    setTitle "Hello World Title!"
    addHamlet [$hamlet|
<h1>Hello World! #{show n}
<p>Here are some of my favorite links: #{show $ (+) 1 1}
<ul>
    <li>
        <a href=http://www.yesodweb.com/>Yesod Web Framework Docs
    <li>
        <a href=http://www.haskell.org/>Haskell Homepage
<p>Thanks for visiting!                         
    |]

getPersonR n = defaultLayout [$hamlet|<h1>Hello #{show n}!|]
    
main = warpDebug 3000 (HelloWorld 42)
