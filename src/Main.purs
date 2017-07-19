module Main where

--import Formlets (formComponent, myForm)
import Formlets2 (formComponent, initMyForm, myForm, validateMyForm, myView)

import Prelude (Unit, bind, unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

--main :: Eff (HA.HalogenEffects ( console :: CONSOLE )) Unit
main = HA.runHalogenAff do
--  result
  body <- HA.awaitBody
  runUI (
    formComponent
      initMyForm
      myForm
      validateMyForm
      myView) unit body