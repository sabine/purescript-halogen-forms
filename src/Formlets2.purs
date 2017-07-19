module Formlets2 where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
--import Control.Monad.Except.Trans (ExceptT, runExceptT)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types (Event)
import Data.Array (foldl)
import Data.Map (Map, empty, insert, keys, lookup, union, unionWith, values)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.String (length)
import Data.Tuple (Tuple(..), fst)
import Data.Validation.Semigroup (V, invalid, isValid, unV)
import Halogen (liftEff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP



type Value names = Map names String


-- errors

data FieldError names = FieldError (Map names (Array String))

instance showFieldError :: Show names => Show (FieldError names) where
  show (FieldError err) = show err

instance semigroupFieldError :: Ord names => Semigroup (FieldError names) where
  append (FieldError x) (FieldError y) = FieldError (unionWith (\a b -> a <> b) x y)

mkFieldError :: forall names. Ord names => names -> String -> FieldError names
mkFieldError name x = FieldError (insert name [x] empty)

-- form result

data State names a
  = InvalidState (FieldError names)
  | ValidState a

instance functorState :: Functor (State names) where
  map f state =
    case state of
      InvalidState errors ->
        InvalidState errors
      ValidState a ->
        ValidState (f a)

instance applyState :: Ord names => Apply (State names) where
  apply (InvalidState e0) (InvalidState e1) = InvalidState (e0 <> e1)
  apply (InvalidState e) (ValidState _) = InvalidState e
  apply (ValidState _) (InvalidState e) = InvalidState e
  apply (ValidState f) (ValidState x) = ValidState (f x)

instance applicativeState :: Ord names => Applicative (State names) where
  pure = ValidState

-- formlet

newtype Formlet names m a = Formlet (Value names -> m (State names a))

instance functorFormlet :: Functor m => Functor (Formlet names m) where
  map :: forall a b. (a -> b) -> Formlet names m a -> Formlet names m b
  map f (Formlet x) = Formlet (\v -> map (map f) (x v))

instance applyFormlet :: (Apply m, Applicative m, Ord names) => Apply (Formlet names m) where
  apply (Formlet f) (Formlet x) = Formlet (\v -> pure (<*>) <*> f v <*> x v)

instance applicativeFormlet :: (Applicative m, Ord names) => Applicative (Formlet names m) where
  pure x = (Formlet <<< const <<< pure) (ValidState x)

-- second-stage form result

data FormResult names a =
  FormResultError (FieldError names)
  | FormResultSuccess a

derive instance eqFormResult :: (Eq (FieldError names), Eq a) => Eq (FormResult names a)
instance showFormResult :: (Show (FieldError names), Show a) => Show (FormResult names a) where
  show (FormResultError e) = "FormResultError: " <> show e
  show (FormResultSuccess x) = "FormResultSuccess: " <> show x




-- creating forms

field :: forall names m a. Ord names => Applicative m => names -> (String -> V String a) -> Formlet names m a
field name check = Formlet $ \v -> do
  let
    r = lookup name v
  case r of
    Nothing -> pure $ InvalidState (mkFieldError name "couldn't find value")
    
    Just r' ->
      pure $ unV (\verr -> InvalidState $ mkFieldError name verr) (\x -> ValidState x) (check r')

field' :: forall names m. Ord names => Applicative m => names -> Formlet names m String
field' name = field name (\x -> pure x)


withCheck :: forall names m a b. Monad m => Ord names => names -> (a -> V String b) -> Formlet names m a -> Formlet names m b
withCheck name check (Formlet f) =
  Formlet $ \v -> do
    r <- f v
    case r of
      ValidState s ->
        pure $ unV (\verr -> InvalidState $ mkFieldError name verr) (\x -> ValidState x) (check s)
      InvalidState err ->
        pure $ InvalidState err
        

runForm :: forall names m a b. Monad m => Show names => Formlet names m a -> Value names -> (a -> m (FormResult names b)) -> m (FormResult names b)
runForm (Formlet p) v f = do
  r <- p v
  case r of
    InvalidState validationError ->
      pure $ FormResultError validationError
    ValidState x ->
      f x
            


-- halogen input renderer

type MyHTML t names = Array (HH.HTML t (Query names Unit))

renderHalogenInput :: forall names t. Ord names => names -> String -> Value names -> Maybe (FieldError names) -> MyHTML t names
renderHalogenInput name label initial errors = 
  [ HH.p_ [
    HH.label_ [ HH.text label ]
    , HH.br_
    , HH.input
      [ HP.value value
      , HE.onValueInput (HE.input (Input name))
      , HE.onBlur (HE.input_ (Blur name))]
      , HH.div_ [ HH.ul_ errorList ]
  ]]
  where
    FieldError field_errors = fromMaybe (FieldError empty) errors
    errorList = lookup name field_errors
              # fromMaybe []
              # map (\v -> [HH.text v])
              # map HH.li_
    value = fromMaybe "" $ lookup name initial

type ComponentState names form =
  { fields :: Value names
  , result :: Maybe (FormResult names form) }

data Query names a
  = Input names String a
  | Blur names a
  | Submit Event a


type AffMonad m = Aff ( dom :: DOM, console :: CONSOLE | m )
type EffMonad m = Eff ( dom :: DOM, console :: CONSOLE | m )


formComponent :: forall form names m.
                 Show names => Ord names =>
                 Value names ->
                 Formlet names (EffMonad m) form ->
                 (form -> EffMonad m (FormResult names form)) ->
                 (Value names -> Maybe (FormResult names form) -> MyHTML Void names) ->
                 H.Component HH.HTML (Query names) Unit Void (AffMonad m)
formComponent init my_form validate view =
  H.component
  { initialState: const initialState
  , render: renderForm
  , eval: evalForm
  , receiver: const Nothing
  }
  where
  initialState =
    { fields: init
    , result: Nothing
    }

  renderForm :: ComponentState names form -> H.ComponentHTML (Query names)
  renderForm state =
    HH.form [ HE.onSubmit (HE.input Submit) ] ([ HH.text "Hello world" ]
            <> view init state.result
            <> [ HH.div_ [ HH.text (show state.fields) ]
            , HH.div_ [ HH.text (case state.result of
              Just (FormResultSuccess _) ->
                "Success!"
              Just (FormResultError err) ->
                show err
              Nothing ->
                "Form didn't run yet!"
                             ) ]
            , HH.button_ [ HH.text "Submit" ]
            ])

    
  evalForm :: Query names ~> H.ComponentDSL (ComponentState names form) (Query names) Void (AffMonad m)
  evalForm (Input key value next) = do
    H.modify (\state ->
      state { fields = insert key value state.fields } )

    s <- H.get
    r <- liftEff $ (runForm my_form s.fields $ validate)

    H.modify (\state ->
      state { result = Just r })

    pure next

  evalForm (Blur key next) = do
    liftEff (log $ "Blur " <> show key)      
    pure next
    
  evalForm (Submit event next) = do
    liftEff $ log "Submit"
    liftEff $ preventDefault event
    pure next


-- example

data FieldNames
  = LoginUsername
  | LoginPassword
  | LoginPassword1
  | LoginPassword2

instance showFieldNames :: Show FieldNames where
  show LoginUsername = "username"
  show LoginPassword = "password"
  show LoginPassword1 = "password1"
  show LoginPassword2 = "password2"

derive instance eqFieldNames :: Eq FieldNames
derive instance ordFieldNames :: Ord FieldNames

data LoginForm = LoginForm
 { username :: String
 , password :: String
 , confirmedNewPassword :: String }

initMyForm :: Value FieldNames
initMyForm =
  empty
  # insert LoginUsername "Bob"
  # insert LoginPassword ""
  # insert LoginPassword1 ""
  # insert LoginPassword2 ""

notEmpty :: String -> V String String
notEmpty txt =
  if txt == "" then invalid "Please enter something!" else pure txt

matches :: String -> String -> String -> V String String
matches value error txt =
  if txt == "" then invalid "Please enter something!" else 
    if txt == value then pure txt else invalid error

passwordsMatch :: Tuple String String -> V String String
passwordsMatch (Tuple p1 p2) =
  if p1 == p2 then pure p1 else invalid "Passwords don't match!"

newPasswordForm :: forall m. Monad m => Apply m => Applicative m => Formlet FieldNames m String
newPasswordForm =
  withCheck LoginPassword2 passwordsMatch
  (Tuple <$> field LoginPassword1 notEmpty <*> field LoginPassword2 notEmpty)

myForm :: forall m. Monad m => Apply m => Applicative m => Formlet FieldNames m LoginForm
myForm =
  withCheck LoginUsername validateUsername
  ((\u p np -> LoginForm $ { username : u, password : p, confirmedNewPassword: np})
  <$> field LoginUsername notEmpty
  <*> field LoginPassword (matches "bobishere" "The correct password is 'bobishere'!")
  <*> newPasswordForm)
  where
  validateUsername (LoginForm f) =
    case f.username of
      "Bob" ->
        pure $ LoginForm f
      _ ->
        invalid "Wrong username! Try 'Bob'..."

myView :: forall t form. Value FieldNames -> Maybe (FormResult FieldNames form) -> MyHTML t FieldNames
myView initial res =
  renderHalogenInput LoginUsername "Username: " initial errors
  <> renderHalogenInput LoginPassword "Password: " initial errors
  <> renderHalogenInput LoginPassword1 "Password (new): " initial errors
  <> renderHalogenInput LoginPassword2 "Password (confirm): " initial errors
  where
    errors = case res of
      Just (FormResultError err) -> Just err
      _ -> Nothing

validateMyForm :: forall m. LoginForm -> Eff ( console :: CONSOLE | m ) (FormResult FieldNames LoginForm)
validateMyForm (LoginForm f) = do
  log f.username
  log f.password
  pure $ FormResultSuccess (LoginForm f)  



--result :: Eff ( console :: CONSOLE ) Unit
--result = do
--  r <- runForm myForm myInput $ validateMyForm

--  case r of
--    FormResultSuccess _ ->
--      log "Success!"
--    FormResultError err ->
--      logShow err
--  pure unit
    
