module Formlets where

import Data.Validation.Semigroup (V, invalid, isValid)
import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types (Event)
import Data.Array (foldl)
import Data.Map (Map, empty, insert, keys, lookup, union, values)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.String (length)
import Data.Tuple (Tuple(..))
import Halogen (liftEff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


newtype InputKey = InputKey String
newtype InputLabel = InputLabel String
derive instance eqInputKey :: Eq InputKey
derive instance ordInputKey :: Ord InputKey
instance showInputKey :: Show InputKey where
  show (InputKey key) = key

type Env = Map InputKey String

data FormletValue view a = FormletValue view (Env -> a)
data Formlet view a = Formlet (FormletValue view a)

instance applicativeFormlet :: Monoid view => Applicative (Formlet view) where
  pure v =
    Formlet (FormletValue mempty (\_ -> v))

instance functorFormlet :: Functor (Formlet view) where
  map f (Formlet a) =
    Formlet $ h
    where
      h = FormletValue x1 (\env -> f (g env))
        where
          FormletValue x1 g = a

instance applyFormlet :: Monoid view => Apply (Formlet view) where
  apply (Formlet f) (Formlet a) =
    Formlet $ h
    where
      h =
        FormletValue (x1 <> x2) (\env -> g env (q env))
          where
            FormletValue x1 g = f
            FormletValue x2 q = a

run :: forall view a. Formlet view a -> Env -> a
run (Formlet f) =
  e
  where
    FormletValue _ e = f

render :: forall view a. Formlet view a -> view
render (Formlet f) =
  view
  where
    FormletValue view _ = f



-- general notion of form input from the viewpoint of Formlet

input :: forall view. InputKey -> (InputKey -> view) -> Formlet (Map InputKey view) String
input key renderHtml =
  Formlet $ f
  where
    f = FormletValue (insert key view empty) collector
      where
        collector env = fromMaybe "" $ lookup key env
        view = renderHtml key

inputGroup :: forall view a. InputKey -> (Map InputKey view -> view) -> Formlet (Map InputKey view) a -> Formlet (Map InputKey view) a
inputGroup key renderHtml (Formlet(FormletValue fragments collector)) =
  Formlet $ f
  where
    f = FormletValue (insert key view empty) collector
    view = renderHtml fragments


-- halogen input renderer

data MyHTML t = MyHTML (State -> Array (HH.HTML t (Query Unit)))

instance semigroupMyHTML :: Semigroup (MyHTML t) where
  append (MyHTML html1) (MyHTML html2) = MyHTML $ \state ->
    html1 state <> html2 state


renderHalogenInput :: forall t. InputLabel -> InputKey -> MyHTML t 
renderHalogenInput (InputLabel label) = \key -> MyHTML $ \state ->
  [ HH.p_ [
    HH.label_ [ HH.text label ]
    , HH.br_
    , HH.input
      [ HP.value (fromMaybe "" $ lookup key state.fields)
      , HE.onValueInput (HE.input (Input key))
      , HE.onBlur (HE.input_ (Blur key))]
    , HH.div_ [ HH.text ( fromMaybe "" $ lookup key state.errors ) ]
  ]]

renderHalogenInputGroup :: forall t. Map InputKey (MyHTML t) -> MyHTML t
renderHalogenInputGroup = \fragments -> 
  foldl (<>) (MyHTML $ \state -> []) $ values fragments
--  foldl (<>) [] $ map (\(MyHTML fragment) -> fragment state) (values fragments)

type State =
  { initial :: Env
  , fields :: Env
  , errors :: Map InputKey String }

data Query a
  = Input InputKey String a
  | Blur InputKey a
  | Submit Event a

formComponent :: forall form m. Show form => Formlet (Map InputKey (MyHTML Void)) form -> H.Component HH.HTML Query Unit Void (Aff ( dom :: DOM, console :: CONSOLE | m ))
formComponent my_form =
  H.component
  { initialState: const initialState
  , render: renderForm
  , eval: evalForm
  , receiver: const Nothing
  }
  where
  initialState =
    { initial: empty
    , fields: empty
    , errors: empty
    }

  renderForm :: State -> H.ComponentHTML Query
  renderForm state =
    HH.form [ HE.onSubmit (HE.input Submit) ] ([ HH.text "Hello world" ]
            <> form state
            <> [ HH.div_ [ HH.text (show state.fields) ]
            , HH.div_ [ HH.text (show ( run myForm $ state.fields )) ]
            , HH.button_ [ HH.text "Submit" ]
            ])
    where
    MyHTML form = fromMaybe (MyHTML $ \_ -> []) ( lookup (InputKey "form") $ render my_form )
    
  evalForm :: Query ~> H.ComponentDSL State Query Void (Aff ( dom :: DOM, console :: CONSOLE | m ))
  evalForm (Input key value next) = do
    H.modify (\state ->
      state { fields = insert key value state.fields } )
    pure next

  evalForm (Blur key next) = do
    liftEff (log $ "Blur " <> show key)
    pure next
    
  evalForm (Submit event next) = do
    liftEff $ log "Submit"
    liftEff $ preventDefault event
    pure next


-- example form
newtype RegisterForm = RegisterForm { username :: String, password :: V String String }
instance showMyForm :: Show RegisterForm where
  show (RegisterForm f) = "RegisterForm {" <> f.username <> "/" <> show f.password <> "}"


validatePasswordsMatch :: Tuple String String -> V String String
validatePasswordsMatch (Tuple password1 password2) =
  if password1 == password2
    then
      pure password1
    else
      invalid "Passwords don't match!"

validateLength :: String -> V String String
validateLength password =
  if length password >= 7
    then
      pure password
    else
      invalid "Password needs to be at least 7 characters long!"

validate :: Tuple String String -> V String String
validate (Tuple password1 password2) =
  if isValid l
    then
      validatePasswordsMatch (Tuple password1 password2)
    else
      l
  where
    l = validateLength password1

passwordFormlet :: forall t. Formlet (Map InputKey (MyHTML t)) (V String String)
passwordFormlet =
  inputGroup (InputKey "password") renderHalogenInputGroup $ map validate (Tuple
               <$> input (InputKey "password1") (renderHalogenInput (InputLabel "Password: "))
               <*> input (InputKey "password2") (renderHalogenInput (InputLabel "Password again: ")))

myForm :: forall t. Formlet (Map InputKey (MyHTML t)) RegisterForm
myForm = inputGroup (InputKey "form") renderHalogenInputGroup $ map RegisterForm $
         { username : _, password: _ }
           <$> input (InputKey "username") (renderHalogenInput (InputLabel "Username: "))
           <*> passwordFormlet

