{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Demo where

import           Data.Tree

import           PrettyPrint
import           Project
import           Reporting

someProject :: Project
someProject = ProjectGroup "Sweden" [stockholm, göteborg, malmö]
  where
    stockholm = SingleProject 1 "Stockholm"
    göteborg = SingleProject 2 "Gothenburg"
    malmö = ProjectGroup "Malmö" [city, limhamn]
    city = SingleProject 3 "Malmö City"
    limhamn = SingleProject 4 "Limhamn"
