{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Demo where

import           Data.Tree

import           PrettyPrint
import           Project
import           Reporting

someProject :: Project () ProjectId
someProject = ProjectGroup "Sweden" () [stockholm, göteborg, malmö]
  where
    stockholm = SingleProject "Stockholm" 1
    göteborg = SingleProject "Gothenburg" 2
    malmö = ProjectGroup "Malmö" () [city, limhamn]
    city = SingleProject "Malmö City" 3
    limhamn = SingleProject "Limhamn" 4
