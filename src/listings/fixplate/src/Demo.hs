{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Demo where

import           Data.Generics.Fixplate.Draw

import           PrettyPrint
import           Project
import           Reporting

someProject :: Project
someProject = projectGroup "Sweden" [stockholm, göteborg, malmö]
  where
    stockholm = singleProject 1 "Stockholm"
    göteborg = singleProject 2 "Gothenburg"
    malmö = projectGroup "Malmö" [city, limhamn]
    city = singleProject 3 "Malmö City"
    limhamn = singleProject 4 "Limhamn"
