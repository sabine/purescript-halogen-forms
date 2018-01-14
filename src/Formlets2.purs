module Formlets2 where

import Data.Either
import Network.HTTP.Affjax
import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except.Trans (ExceptT, lift, runExceptT, throwError)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types (Event)
import Data.Map (Map, empty, insert, lookup, unionWith)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Halogen (liftAff, liftEff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.StatusCode (StatusCode(..))


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

data FormResult names a
  = Invalid (FieldError names)
  | Valid a

instance functorState :: Functor (FormResult names) where
  map f state =
    case state of
      Invalid errors ->
        Invalid errors
      Valid a ->
        Valid (f a)

instance applyState :: Ord names => Apply (FormResult names) where
  apply (Invalid (FieldError e0)) (Invalid (FieldError e1)) = Invalid (FieldError (unionWith (\a b -> a <> b) e0 e1))
  apply (Invalid e) (Valid _) = Invalid e
  apply (Valid _) (Invalid e) = Invalid e
  apply (Valid f) (Valid x) = Valid (f x)

instance applicativeState :: Ord names => Applicative (FormResult names) where
  pure = Valid

-- formlet

newtype Formlet names m a = Formlet (Value names -> m (FormResult names a))

instance functorFormlet :: Functor m => Functor (Formlet names m) where
  map :: forall a b. (a -> b) -> Formlet names m a -> Formlet names m b
  map f (Formlet x) = Formlet (\v -> map (map f) (x v))

instance applyFormlet :: (Apply m, Applicative m, Ord names) => Apply (Formlet names m) where
  apply (Formlet f) (Formlet x) = Formlet (\v -> pure (<*>) <*> f v <*> x v)

instance applicativeFormlet :: (Applicative m, Ord names) => Applicative (Formlet names m) where
  pure x = (Formlet <<< const <<< pure) (Valid x)




-- creating forms

field :: forall names m a. Ord names => Monad m => names -> (String -> ExceptT String m a) -> Formlet names m a
field name check = Formlet $ \v -> do
  let
    r = lookup name v
  case r of
    Nothing -> pure $ Invalid (mkFieldError name "couldn't find value")
    
    Just r' -> do
      e <- runExceptT (check r')
      pure $ case e of
        Left verr ->
          Invalid $ mkFieldError name verr
        Right x ->
          Valid x

field' :: forall names m. Ord names => Monad m => names -> Formlet names m String
field' name = field name (\x -> pure x)


withCheck :: forall names m a b. Monad m => Ord names => names -> (a -> ExceptT String m b) -> Formlet names m a -> Formlet names m b
withCheck name check (Formlet f) =
  Formlet $ \v -> do
    r <- f v
    case r of
      Valid s -> do
        e <- runExceptT (check s)
        pure $ case e of
          Left verr ->
            Invalid $ mkFieldError name verr
          Right x ->
            Valid x
      Invalid err ->
        pure $ Invalid err

runForm :: forall names m a. Monad m => Show names => Formlet names m a -> Value names -> m (FormResult names a)
runForm (Formlet p) v = do
  p v
            


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
                 Formlet names (AffMonad m) form ->
                 (Value names -> Maybe (FormResult names form) -> MyHTML Void names) ->
                 H.Component HH.HTML (Query names) Unit Void (AffMonad m)
formComponent init my_form view =
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
              Just (Valid _) ->
                "Success!"
              Just (Invalid err) ->
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
    r <- liftAff $ (runForm my_form s.fields)

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

notEmpty :: forall m. Monad m => String -> ExceptT String m String
notEmpty txt =
  if txt == "" then throwError "Please enter something!" else pure txt

matches :: forall m. Monad m => String -> String -> String -> ExceptT String m String
matches value error txt =
  if txt == "" then throwError "Please enter something!" else 
    if txt == value then pure txt else throwError error

passwordsMatch :: forall m. Monad m => Tuple String String -> ExceptT String m String
passwordsMatch (Tuple p1 p2) =
  if p1 == p2 then pure p1 else throwError "Passwords don't match!"

checkUsernameAvailable :: forall m. String -> Aff (ajax :: AJAX | m) String
checkUsernameAvailable name = do
  res <- get $ "/test-available-username?name=" <> name
  pure $ res.response
      
usernameAvailable :: forall m. String -> ExceptT String (Aff (ajax::AJAX | m)) String
usernameAvailable name = do
  x <- lift $ do
    checkUsernameAvailable name
  case x of
    "yes" ->
      pure name
    _ ->
      throwError "Username is already taken!"

newPasswordForm :: forall m. Monad m => Formlet FieldNames m String
newPasswordForm =
  withCheck LoginPassword2 passwordsMatch
  (Tuple <$> field LoginPassword1 notEmpty <*> field LoginPassword2 notEmpty)

myForm :: forall m. Formlet FieldNames (Aff (ajax :: AJAX | m)) LoginForm
myForm =
  withCheck LoginUsername validateUsername
  ((\u p np -> LoginForm $ { username : u, password : p, confirmedNewPassword: np})
  <$> field LoginUsername notEmpty
  <*> field LoginPassword (matches "bobishere" "The correct password is 'bobishere'!")
  <*> newPasswordForm)
  where
  validateUsername :: LoginForm -> ExceptT String (Aff (ajax::AJAX|m)) LoginForm
  validateUsername (LoginForm f) = do
    x <- lift $ do
      checkUsernameAvailable f.username
    case x of
      "yes" ->
        pure $ LoginForm f
      _ ->
        throwError "Username is already taken!"


newPasswordView field1 field2 initial errors =
  renderHalogenInput field1 "Password (new):" initial errors
  <> renderHalogenInput field2 "Password (confirm):" initial errors

myView :: forall t form. Value FieldNames -> Maybe (FormResult FieldNames form) -> MyHTML t FieldNames
myView initial res =
  renderHalogenInput LoginUsername "Username: " initial errors
  <> renderHalogenInput LoginPassword "Password: " initial errors
  <> newPasswordView LoginPassword1 LoginPassword2 initial errors
  where
    errors = case res of
      Just (Invalid err) -> Just err
      _ -> Nothing
