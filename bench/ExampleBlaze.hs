{-# LANGUAGE OverloadedStrings #-}

module ExampleBlaze (hackageUpload) where

import Data.String
import Text.Blaze.Html5 ((!))

import qualified Text.Blaze.Html4.Strict     as H4
import qualified Text.Blaze.Html5            as B
import qualified Text.Blaze.Html5.Attributes as BA

-- [2020-11-07] http://hackage.haskell.org/upload
hackageUpload :: B.Html -> B.Html
hackageUpload title =
  B.docTypeHtml $ do
    B.head $ do
      B.meta ! BA.name "viewport" ! BA.content "width=device-width, initial-scale=1"
      B.link ! BA.href "https://fonts.googleapis.com/css?family=PT+Sans:400,400i,700" ! BA.rel "stylesheet"
      B.link ! BA.rel "stylesheet" ! BA.href "/static/hackage.css" ! BA.type_ "text/css"
      B.link ! BA.rel "icon" ! BA.type_ "image/png" ! BA.href "/static/favicon.png"
      B.link ! BA.rel "search" ! BA.type_ "application/opensearchdescription+xml" ! BA.title "Hackage" ! BA.href "/packages/opensearch.xml"
      "<!-- Global site tag (gtag.js) - Google Analytics -->"
      B.script ! BA.async mempty ! BA.src "https://www.googletagmanager.com/gtag/js?id=UA-83290513-3" $ mempty
      B.script $ do
        fromString $ unlines
          [ "window.dataLayer = window.dataLayer || [];"
          , "function gtag(){dataLayer.push(arguments);}"
          , "gtag('js', new Date());"
          , "gtag('config', 'UA-83290513-3');"
          ]
      B.title $ title -- "Uploading packages and package candidates | Hackage"
    B.body $ do
      B.div ! BA.id "page-header" $ do
        B.a ! BA.class_ "caption" ! BA.href "/" $ "Hackage :: [Package]"
        B.ul ! BA.class_ "links" ! BA.id "page-menu" $ do
          B.li $ do
            B.form ! BA.action "/packages/search" ! BA.method "get" ! BA.class_ "search" $ mempty
            B.button ! BA.type_ "submit" $ "Search&nbsp;"
            B.input ! BA.type_ "text" ! BA.name "terms"
          B.li $ do
            B.a ! BA.href "/packages/browse" $ "Browse"
          B.li $ do
            B.a ! BA.href "/packages/recent" $ "What's new"
          B.li $ do
            B.a ! BA.href "/upload" $ "Upload"
          B.li $ do
            B.a ! BA.href "/accounts" $ "User accounts"
      B.div ! BA.id "content" $ do
        B.h2 $ "Uploading packages"
        B.div ! BA.style "font-size: large; text-align: center;" $ do
          "Upload and publish a package "
          B.strong $ "permanently"
          ": "
          B.a ! BA.href "/packages/upload" $ "Upload"
        B.p $ do
          "Uploading a package puts it in the "
          B.a ! BA.href "/packages/browse" $ "package index"
          "so that anyone can download it and view information about it. "
          B.strong $ "You can only upload a package version once and this cannot be undone"
          ", so try to get it right the first time! To reduce the risk of mistakes it's recommended to use the "
          B.a ! BA.href "!candidates" $ "package candidates feature"
          " described below."
        B.p $ do
          "Because each package added to the main package index has a cost of operation and maintenance associated to it, "
          B.strong $ "your package should strive to provide value for the community by being intended to be useful to others"
          ", which entails giving your package a meaningful synopsis/description as well as ensuring your package is installable by helping with providing accurate meta-data."
        B.p $ do
          "Packages must be in the form produced by Cabal's "
          B.a ! BA.href "http://www.haskell.org/cabal/users-guide/installing-packages.html!setup-sdist" $ "sdist"
          " command: a gzipped tar file "
          B.em $ "package"
          "-"
          B.em $ "version"
          H4.tt $ ".tar.gz"
          " comprising a directory "
          B.em $ "package"
          "-"
          B.em $ "version"
          " containing a package of that name and version, including "
          B.em $ "package"
          H4.tt $ ".cabal"
          ". See the notes at the bottom of the page."
        B.h3 ! BA.id "versioning_and_curation" $ "Package versioning and curation"
        B.p $ do
          "By default, uploaded packages are "
          B.i $ "curated"
          " which means that both maintainers and hackage trustees may revise their metadata (particularly involving version bounds) to guide build tools in producing install-plans. (For more information on revisions, see the "
          B.a ! BA.href "https://github.com/haskell-infra/hackage-trustees/blob/master/revisions-information.md" $ "FAQ"
          ")."
        B.p $ do
          "In order to ensure the integrity and well-functioning of the Hackage/Cabal ecosystem, all curated packages "
          B.abbr ! BA.title "[RFC2119] The word 'should' is intended to denote that there may exist valid reasons in particular circumstances to ignore a particular item, but the full implications MUST be understood and carefully weighed before choosing a different course" $ "should"
          " follow Haskell's "
          B.a ! BA.href "https://pvp.haskell.org/"$"Package Versioning Policy (PVP)"
          "."
        B.p $ do
          "In particular, be aware that although the "
          B.a ! BA.href "https://pvp.haskell.org/" $ "PVP"
          " and "
          B.a ! BA.href "http://semver.org/" $ "SemVer"
          " are based on the same concepts they differ significantly in structure and consequently are "
          B.em $ "not compatible"
          " with each other. Please consult the "
          B.a ! BA.href "https://pvp.haskell.org/faq/!semver" $ "PVP/SemVer FAQ section"
          " for more details about the differences and related issues."
        B.p $ do
          "Further, an important property of the PVP contract is that it can only be effective and provide strong enough guarantees if it is followed not only by an individual package, but also by that package's transitive dependencies. Consequently, packages which are curated should aim to depend only on other curated packages."
        B.p $ do
          "In the course of the curation process, the "
          B.a ! BA.href "/packages/trustees" $ "Hackage Trustees"
          " need to be able to contact package maintainers, to inform them about and help to resolve issues with their packages (including its meta-data) which affect the Hackage ecosystem."
        B.p $ do
          "Package uploaders may choose to exclude individual package uploads from curation, by setting the "
          H4.tt $ "x-curation:"
          " field of the package's cabal file to "
          H4.tt $ "uncurated"
          ". Packages which are uncurated have no expectations on them regarding versioning policy.  Trustees or maintainers may "
          B.i $ "adopt"
          " uncurated packages into the curated layer through metadata revisions. Metadata revisions must not set the value of the"
          H4.tt $ "x-curation"
          " field to any variant of "
          H4.tt $ "uncurated"
          "."
        B.p $ do
          "Two variants of the "
          H4.tt $ "uncurated"
          " property are supported. First, "
          H4.tt $ "uncurated-no-trustee-contact"
          ", which indicates that maintainers do not wish to be contacted by trustees regarding any metadata issues with the package. (Contact may still occur over issues that are not related to curation, such as licensing, etc.). Second, "
          H4.tt $ "uncurated-seeking-adoption"
          ", which indicates that maintainers would like their package to be adopted in the curated layer, but currently some issue prevents this, which they would like assistance with."
        B.p $ "In the future, metadata regarding curation will be made available in the UI of Hackage, and different derived indexes will be provided for the uncurated and curated layers of packages."
        B.h3 $ "Open source licenses"
        B.p $ do
          "The code and other material you upload and distribute via this site must be under an "
          B.em $ "open source license"
          ". This is a service operated for the benefit of the community and that is our policy. It is also so that we can operate the service in compliance with copyright laws."
        B.p $ do
          "The Hackage operators do not want to be in the business of making judgements on what is and is not a valid open source license, but we retain the right to remove packages that are not under licenses that are open source in spirit, or that conflict with our ability to operate this service. (If you want advice, see the ones "
          B.a ! BA.href "/package/Cabal/docs/Distribution-License.html" $ "Cabal recommends"
          ".)"
        B.p $ do
          "The Hackage operators do "
          B.em $ "not"
          " need and are "
          B.em $ "not"
          " asking for any rights beyond those granted by the open source license you choose to use. All normal open source licenses grant enough rights to be able to operate this service."
        B.p $ do
          "In particular, we expect as a consequence of the license that:"
        B.ol $ do
          B.li $ do
            "we have the right to distribute what you have uploaded to other people"
          B.li $ do
            "we have the right to distribute certain derivatives and format conversions, including but not limited to:"
            B.ul $ do
              B.li $ do
                "documentation derived from the package"
              B.li $ do
                "alternative presentations and formats of code (e.g. html markup)"
              B.li $ do
                "excerpts and presentation of package metadataw"
              B.li $ do
                "modified versions of package metadata"
        B.p $ "Please make sure that you comply with the license of all code and other material that you upload. For example, check that your tarball includes the license files of any 3rd party code that you include."
        B.h3 $ "Privileges"
        B.p $ do
          "To upload a package, you'll need a Hackage "
          B.a ! BA.href "/accounts" $ "username"
          " and password."
        B.p $ "If you upload a package or package candidate and no other versions exist in the package database, you become part of the maintainer group for that package, and you can add other maintainers if you wish. If a maintainer group exists for a package, only its members can upload new versions of that package."
        B.p $ do
          "If there is no maintainer, the uploader can remove themselves from the group, and a "
          B.a ! BA.href "/packages/trustees" $ "package trustee"
          " can add anyone who wishes to assume the responsibility. The "
          B.code $ "Maintainer"
          " field of the Cabal file should be "
          B.code $ "None"
          " in this case. If a package is being maintained, any release not approved and supported by the maintainer should use a different package name. Then use the "
          B.code $ "Maintainer"
          " field as above either to commit to supporting the fork yourself or to mark it as unsupported."
        B.h3 ! BA.id "candidates" $ "Package Candidates"
        B.p $ do
          B.a ! BA.href "/packages/candidates" $ do
            "Package "
            B.em $ "candidates"
          " are a way to preview the package page, view any warnings or possible errors you might encounter, and let others install it before publishing it to the main index. (Note: you can view these warnings with 'cabal check'.) You can have multiple candidates for each package at the same time so long as they each have different versions. Finally, you can publish a candidate to the main index if it's not there already."
        B.p $ do
          "Package candidates have not yet been fully implemented and are still being improved; see "
          B.a ! BA.href "https://github.com/haskell/hackage-server/projects/1" $ "Package Candidates Project Dashboard"
          " for an overview of what still needs to be done."
        B.div ! BA.style "font-size: large; text-align: center;" $ do
          B.a ! BA.href "/packages/candidates/upload" $ "Upload a package candidate"
        B.h3 $ "Notes"
        B.ul $ do
          B.li $ do
            "You should check that your source bundle builds, including the haddock documentation if it's a library."
          B.li $ do
            "Categories are determined by whatever you put in the "
            B.code $ "Category"
            " field. You should try to pick existing categories when possible. You can have more than one category, separated by commas. If no other versions of the package exist, the categories automatically become the package's tags."
          B.li $ do
            "Occasional changes to the GHC base package can mean that some work needs to be done to make packages compatible across a range of versions. See "
            B.a ! BA.href "https://github.com/haskell-infra/hackage-trustees/blob/master/cookbook.md" $ "these notes"
            " for some tips in how to do so. There are some notes for upgrading"
            B.a ! BA.href "http://www.haskell.org/haskellwiki/Upgrading_packages" $ "much older"
            " packages as well."
          B.li $ do
            "The hackage-server attempts to build documentation for library packages, but this can fail. Maintainers can generate their own documentation and upload it by using something along the lines of the shell script below (note that the last two commands are the key ones):"
            B.pre $ do
              fromString $ unlines
                [ "#!/bin/sh"
                , "set -e"
                , ""
                , "dir=$(mktemp -d dist-docs.XXXXXX)"
                , "trap 'rm -r \"$dir\"' EXIT"
                , "# assumes cabal 2.4 or later"
                , "cabal v2-haddock --builddir=\"$dir\" --haddock-for-hackage --enable-doc"
                , "cabal upload -d --publish $dir/*-docs.tar.gz"
                ]
