{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
{-# LANGUAGE RebindableSyntax #-}

module ExampleTypeOfHtml (hackageUpload) where

import Prelude
import Html
import Html.Obsolete (Element(Tt))

-- [2020-11-07] http://hackage.haskell.org/upload
hackageUpload title = let (>>) = (#) in do
  DOCTYPE
  Html :> do
    Head :> do
      Meta :@ (NameA:="viewport" # ContentA:="width=device-width, initial-scale=1")
      Link :@ (HrefA:="https://fonts.googleapis.com/css?family=PT+Sans:400,400i,700" # RelA:="stylesheet")
      Link :@ (RelA:="stylesheet" # HrefA:="/static/hackage.css" # TypeA:="text/css")
      Link :@ (RelA:="icon" # TypeA:="image/png" # HrefA:="/static/favicon.png")
      Link :@ (RelA:="search" # TypeA:="application/opensearchdescription+xml" # TitleA:="Hackage" # HrefA:="/packages/opensearch.xml")
      Raw "<!-- Global site tag (gtag.js) - Google Analytics -->"
      Script :@ (AsyncA # SrcA:="https://www.googletagmanager.com/gtag/js?id=UA-83290513-3")
      Script :> do
        Raw $ unlines
          [ "window.dataLayer = window.dataLayer || [];"
          , "function gtag(){dataLayer.push(arguments);}"
          , "gtag('js', new Date());"
          , "gtag('config', 'UA-83290513-3');"
          ]
      Title :> title -- "Uploading packages and package candidates | Hackage"
    Body :> do
      Div :@ IdA:="page-header" :> do
        A :@ (ClassA:="caption" # HrefA:="/") :> "Hackage :: [Package]"
        Ul :@ (ClassA:="links" # IdA:="page-menu") :> do
          Li :> do
            Form :@ (ActionA:="/packages/search" # MethodA:="get" # ClassA:="search")
            Button :@ TypeA:="submit" :> "Search&nbsp;"
            Input :@ (TypeA:="text" # NameA:="terms")
          Li :> do
            A :@ HrefA:="/packages/browse" :> "Browse"
          Li :> do
            A :@ HrefA:="/packages/recent" :> "What's new"
          Li :> do
            A :@ HrefA:="/upload" :> "Upload"
          Li :> do
            A :@ HrefA:="/accounts" :> "User accounts"
      Div :@ IdA:="content" :> do
        H2 :> "Uploading packages"
        Div :@ StyleA:="font-size: large; text-align: center;" :> do
          "Upload and publish a package "
          Strong :> "permanently"
          ": "
          A :@ HrefA:="/packages/upload" :> "Upload"
        P :> do
          "Uploading a package puts it in the "
          A :@ HrefA:="/packages/browse" :> "package index"
          "so that anyone can download it and view information about it. "
          Strong :> "You can only upload a package version once and this cannot be undone"
          ", so try to get it right the first time! To reduce the risk of mistakes it's recommended to use the "
          A :@ HrefA:="#candidates" :> "package candidates feature"
          " described below."
        P :> do
          "Because each package added to the main package index has a cost of operation and maintenance associated to it, "
          Strong :> "your package should strive to provide value for the community by being intended to be useful to others"
          ", which entails giving your package a meaningful synopsis/description as well as ensuring your package is installable by helping with providing accurate meta-data."
        P :> do
          "Packages must be in the form produced by Cabal's "
          A :@ HrefA:="http://www.haskell.org/cabal/users-guide/installing-packages.html#setup-sdist" :> "sdist"
          " command: a gzipped tar file "
          Em :> "package"
          "-"
          Em :> "version"
          Tt :> ".tar.gz"
          " comprising a directory "
          Em :> "package"
          "-"
          Em :> "version"
          " containing a package of that name and version, including "
          Em :> "package"
          Tt :> ".cabal"
          ". See the notes at the bottom of the page."
        H3 :@ IdA:="versioning_and_curation" :> "Package versioning and curation"
        P :> do
          "By default, uploaded packages are "
          I :> "curated"
          " which means that both maintainers and hackage trustees may revise their metadata (particularly involving version bounds) to guide build tools in producing install-plans. (For more information on revisions, see the "
          A :@ HrefA:="https://github.com/haskell-infra/hackage-trustees/blob/master/revisions-information.md" :> "FAQ"
          ")."
        P :> do
          "In order to ensure the integrity and well-functioning of the Hackage/Cabal ecosystem, all curated packages "
          Abbr :@ TitleA:="[RFC2119] The word 'should' is intended to denote that there may exist valid reasons in particular circumstances to ignore a particular item, but the full implications MUST be understood and carefully weighed before choosing a different course" :> "should"
          " follow Haskell's "
          A :@ HrefA:="https://pvp.haskell.org/":>"Package Versioning Policy (PVP)"
          "."
        P :> do
          "In particular, be aware that although the "
          A :@ HrefA:="https://pvp.haskell.org/" :> "PVP"
          " and "
          A :@ HrefA:="http://semver.org/" :> "SemVer"
          " are based on the same concepts they differ significantly in structure and consequently are "
          Em :> "not compatible"
          " with each other. Please consult the "
          A :@ HrefA:="https://pvp.haskell.org/faq/#semver" :> "PVP/SemVer FAQ section"
          " for more details about the differences and related issues."
        P :> do
          "Further, an important property of the PVP contract is that it can only be effective and provide strong enough guarantees if it is followed not only by an individual package, but also by that package's transitive dependencies. Consequently, packages which are curated should aim to depend only on other curated packages."
        P :> do
          "In the course of the curation process, the "
          A :@ HrefA:="/packages/trustees" :> "Hackage Trustees"
          " need to be able to contact package maintainers, to inform them about and help to resolve issues with their packages (including its meta-data) which affect the Hackage ecosystem."
        P :> do
          "Package uploaders may choose to exclude individual package uploads from curation, by setting the "
          Tt :> "x-curation:"
          " field of the package's cabal file to "
          Tt :> "uncurated"
          ". Packages which are uncurated have no expectations on them regarding versioning policy.  Trustees or maintainers may "
          I :> "adopt"
          " uncurated packages into the curated layer through metadata revisions. Metadata revisions must not set the value of the"
          Tt :> "x-curation"
          " field to any variant of "
          Tt :> "uncurated"
          "."
        P :> do
          "Two variants of the "
          Tt :> "uncurated"
          " property are supported. First, "
          Tt :> "uncurated-no-trustee-contact"
          ", which indicates that maintainers do not wish to be contacted by trustees regarding any metadata issues with the package. (Contact may still occur over issues that are not related to curation, such as licensing, etc.). Second, "
          Tt :> "uncurated-seeking-adoption"
          ", which indicates that maintainers would like their package to be adopted in the curated layer, but currently some issue prevents this, which they would like assistance with."
        P :> "In the future, metadata regarding curation will be made available in the UI of Hackage, and different derived indexes will be provided for the uncurated and curated layers of packages."
        H3 :> "Open source licenses"
        P :> do
          "The code and other material you upload and distribute via this site must be under an "
          Em :> "open source license"
          ". This is a service operated for the benefit of the community and that is our policy. It is also so that we can operate the service in compliance with copyright laws."
        P :> do
          "The Hackage operators do not want to be in the business of making judgements on what is and is not a valid open source license, but we retain the right to remove packages that are not under licenses that are open source in spirit, or that conflict with our ability to operate this service. (If you want advice, see the ones "
          A :@ HrefA:="/package/Cabal/docs/Distribution-License.html" :> "Cabal recommends"
          ".)"
        P :> do
          "The Hackage operators do "
          Em :> "not"
          " need and are "
          Em :> "not"
          " asking for any rights beyond those granted by the open source license you choose to use. All normal open source licenses grant enough rights to be able to operate this service."
        P :> do
          "In particular, we expect as a consequence of the license that:"
        Ol :> do
          Li :> do
            "we have the right to distribute what you have uploaded to other people"
          Li :> do
            "we have the right to distribute certain derivatives and format conversions, including but not limited to:"
            Ul :> do
              Li :> do
                "documentation derived from the package"
              Li :> do
                "alternative presentations and formats of code (e.g. html markup)"
              Li :> do
                "excerpts and presentation of package metadataw"
              Li :> do
                "modified versions of package metadata"
        P :> "Please make sure that you comply with the license of all code and other material that you upload. For example, check that your tarball includes the license files of any 3rd party code that you include."
        H3 :> "Privileges"
        P :> do
          "To upload a package, you'll need a Hackage "
          A :@ HrefA:="/accounts" :> "username"
          " and password."
        P :> "If you upload a package or package candidate and no other versions exist in the package database, you become part of the maintainer group for that package, and you can add other maintainers if you wish. If a maintainer group exists for a package, only its members can upload new versions of that package."
        P :> do
          "If there is no maintainer, the uploader can remove themselves from the group, and a "
          A :@ HrefA:="/packages/trustees" :> "package trustee"
          " can add anyone who wishes to assume the responsibility. The "
          Code :> "Maintainer"
          " field of the Cabal file should be "
          Code :> "None"
          " in this case. If a package is being maintained, any release not approved and supported by the maintainer should use a different package name. Then use the "
          Code :> "Maintainer"
          " field as above either to commit to supporting the fork yourself or to mark it as unsupported."
        H3 :@ IdA:="candidates" :> "Package Candidates"
        P :> do
          A :@ HrefA:="/packages/candidates" :> do
            "Package "
            Em :> "candidates"
          " are a way to preview the package page, view any warnings or possible errors you might encounter, and let others install it before publishing it to the main index. (Note: you can view these warnings with 'cabal check'.) You can have multiple candidates for each package at the same time so long as they each have different versions. Finally, you can publish a candidate to the main index if it's not there already."
        P :> do
          "Package candidates have not yet been fully implemented and are still being improved; see "
          A :@ HrefA:="https://github.com/haskell/hackage-server/projects/1" :> "Package Candidates Project Dashboard"
          " for an overview of what still needs to be done."
        Div :@ StyleA:="font-size: large; text-align: center;" :> do
          A :@ HrefA:="/packages/candidates/upload" :> "Upload a package candidate"
        H3 :> "Notes"
        Ul :> do
          Li :> do
            "You should check that your source bundle builds, including the haddock documentation if it's a library."
          Li :> do
            "Categories are determined by whatever you put in the "
            Code :> "Category"
            " field. You should try to pick existing categories when possible. You can have more than one category, separated by commas. If no other versions of the package exist, the categories automatically become the package's tags."
          Li :> do
            "Occasional changes to the GHC base package can mean that some work needs to be done to make packages compatible across a range of versions. See "
            A :@ HrefA:="https://github.com/haskell-infra/hackage-trustees/blob/master/cookbook.md" :> "these notes"
            " for some tips in how to do so. There are some notes for upgrading"
            A :@ HrefA:="http://www.haskell.org/haskellwiki/Upgrading_packages" :> "much older"
            " packages as well."
          Li :> do
            "The hackage-server attempts to build documentation for library packages, but this can fail. Maintainers can generate their own documentation and upload it by using something along the lines of the shell script below (note that the last two commands are the key ones):"
            Pre :> do
              Raw $ unlines
                [ "#!/bin/sh"
                , "set -e"
                , ""
                , "dir=$(mktemp -d dist-docs.XXXXXX)"
                , "trap 'rm -r \"$dir\"' EXIT"
                , "# assumes cabal 2.4 or later"
                , "cabal v2-haddock --builddir=\"$dir\" --haddock-for-hackage --enable-doc"
                , "cabal upload -d --publish $dir/*-docs.tar.gz"
                ]
