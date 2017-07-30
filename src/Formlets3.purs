module Formlets3 where

import Data.Either (Either(..))
import Network.HTTP.Affjax (AJAX, get)
import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except.Trans (ExceptT, lift, runExceptT, throwError)
import Control.Parallel (parTraverse, parTraverse_)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types (Event)
import Data.Foldable (elem)
import Data.List (List(..), singleton, delete)
import Data.Map (Map, empty, insert, lookup, unionWith)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid, mempty)
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
-- names = field names
data FormResult names async a
  = Invalid (FieldError names) (List (Tuple names async))
  | Valid a (List (Tuple names async))

instance functorState :: Functor (FormResult names async) where
  map f state =
    case state of
      Invalid errors asyncV ->
        Invalid errors asyncV
      Valid a asyncV ->
        Valid (f a) asyncV

instance applyFormResult :: (Ord names) => Apply (FormResult names async) where
  apply (Invalid (FieldError e0) asyncV0) (Invalid (FieldError e1) asyncV1) = Invalid (FieldError (unionWith (\a b -> a <> b) e0 e1)) (asyncV0 <> asyncV1)
  apply (Invalid e asyncV0) (Valid _ asyncV1) = Invalid e (asyncV0 <> asyncV1)
  apply (Valid _ asyncV0) (Invalid e asyncV1) = Invalid e (asyncV0 <> asyncV1)
  apply (Valid f asyncV0) (Valid x asyncV1) = Valid (f x) (asyncV0 <> asyncV1)

instance applicativeFormResult :: Ord names => Applicative (FormResult names async) where
  pure x = Valid x mempty


newtype Formlet names m async a = Formlet (Value names -> m (FormResult names async a))

instance functorFormlet :: (Functor m) => Functor (Formlet names m async) where
  map :: forall a b. (a -> b) -> Formlet names m async a -> Formlet names m async b
  map f (Formlet x) = Formlet (\v -> map (map f) (x v))

instance applyFormlet :: (Apply m, Applicative m, Ord names) => Apply (Formlet names m async) where
  apply (Formlet f) (Formlet x) = Formlet (\v -> pure (<*>) <*> f v <*> x v)

instance applicativeFormlet :: (Applicative m, Ord names) => Applicative (Formlet names m async) where
  pure x = (Formlet <<< const <<< pure) (Valid x mempty)




-- creating forms

field :: forall names m async a.
         Ord names => Monad m =>
         names ->
         (String -> ExceptT String m a) ->
         Maybe (String -> async) ->
         Formlet names m async a
field name check async = Formlet $ \v -> do
  let
    r = lookup name v
  case r of
    Nothing -> pure $ Invalid (mkFieldError name "couldn't find value") mempty
    
    Just r' -> do
      e <- runExceptT (check r')
      pure $ case e of
        Left verr ->
          Invalid (mkFieldError name verr) mempty
        Right x ->
          case async of
            Just a ->
              Valid x (pure (Tuple name (a r')))
            Nothing ->
              Valid x mempty

field' :: forall names m async.
          Ord names => Monad m =>
          names ->
          Formlet names m async String
field' name = field name (\x -> pure x) Nothing


withCheck :: forall names m async a b.
             Monad m => Ord names =>
             names ->
             (a -> ExceptT String m b) ->
             Formlet names m async a ->
             Formlet names m async b
withCheck name check (Formlet f) =
  Formlet $ \v -> do
    r <- f v
    case r of
      Valid s async -> do
        e <- runExceptT (check s)
        pure $ case e of
          Left verr ->
            Invalid (mkFieldError name verr) async
          Right x ->
            Valid x async
      Invalid err async ->
        pure $ Invalid err async

withAsync :: forall names m async a.
             Monad m => Ord names =>
             names ->
             (a -> async) ->
             Formlet names m async a ->
             Formlet names m async a
withAsync name asyncCheck (Formlet f) =
  Formlet $ \v -> do
    r <- f v
    pure $ case r of
      Valid s async' ->
        Valid s (async' <> (singleton $ Tuple name (asyncCheck s)))
        -- in case the synchronous validation succeeds, we want to do the async one too
      Invalid e async' ->
        Invalid e async'

runForm :: forall names m async a. Monad m => Show names => Formlet names m async a -> Value names -> m (FormResult names async a)
runForm (Formlet p) v = do
  p v
            


-- halogen input renderer

type MyHTML t names = Array (HH.HTML t (Query names Unit))

renderHalogenInput :: forall names t. Ord names => names -> String -> Value names -> Maybe (FieldError names) -> (names -> Boolean) -> MyHTML t names
renderHalogenInput name label initial errors asyncValidationInProgress = 
  [ HH.p_ [
    HH.label_ [ HH.text label ]
    , HH.br_
    , HH.input
      [ HP.value value
      , HE.onValueInput (HE.input (Input name))
      , HE.onBlur (HE.input_ (Blur name))]
      , HH.div_ [ HH.ul_ errorList ]
      , HH.text $ if asyncValidationInProgress name then "Validating (async)!" else ""
  ]]
  where
    FieldError field_errors = fromMaybe (FieldError empty) errors
    errorList = lookup name field_errors
              # fromMaybe []
              # map (\v -> [HH.text v])
              # map HH.li_
    value = fromMaybe "" $ lookup name initial

data Result names form =
  ValidR form
  | InvalidR (FieldError names)

type ComponentState names form =
  { fields :: Value names
  , result :: Maybe (Result names form)
  , asyncValidationInProgress :: List names }

data Query names a
  = Input names String a
  | Blur names a
  | Submit Event a
  | AsyncError names String a
  | AsyncSuccess names a


type AffMonad m = Aff ( dom :: DOM, console :: CONSOLE, ajax :: AJAX | m )
type EffMonad m = Eff ( dom :: DOM, console :: CONSOLE | m )
 
type Async m = ExceptT String (Aff ( dom :: DOM, console :: CONSOLE, ajax :: AJAX | m )) Unit


addAsyncError :: forall names form. Ord names => names -> String -> Maybe (Result names form) -> Maybe (Result names form)
addAsyncError name error result =
  case result of
    Nothing -> Nothing
    Just r ->
      case r of
        ValidR x ->
          Just $ InvalidR (mkFieldError name error)
        InvalidR err ->
          Just $ InvalidR (err <> mkFieldError name error)
          

formComponent :: forall form names m.
                 Show names => Ord names =>
                 Value names ->
                 Formlet names (AffMonad m) (Async m) form ->
                 (Value names ->
                     Maybe (Result names form) ->
                     (names -> Boolean) ->
                     MyHTML Void names) ->
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
    , asyncValidationInProgress: Nil
    }

  renderForm :: ComponentState names form -> H.ComponentHTML (Query names)
  renderForm state =
    HH.form [ HE.onSubmit (HE.input Submit) ] ([ HH.text "Hello world" ]
            <> view init state.result (\name -> elem name state.asyncValidationInProgress)
            <> [ HH.div_ [ HH.text (show state.fields) ]
            , HH.div_ [ HH.text (case state.result of
              Just (ValidR _)  ->
                "Success!"
              Just (InvalidR err) ->
                show err
              Nothing ->
                "Form didn't run yet!"
                             ) ]
            , HH.div_ [ ]
            , HH.button_ [ HH.text "Submit" ]
            ])

    
  evalForm :: Query names ~> H.ComponentDSL (ComponentState names form) (Query names) Void (AffMonad m)
  evalForm (Input key value next) = do
    H.modify (\state ->
      state { fields = insert key value state.fields } )
    pure next

  evalForm (AsyncSuccess name next) = do
    H.modify (\state ->
      state { asyncValidationInProgress = delete name state.asyncValidationInProgress })
    pure next

  evalForm (AsyncError name err next) = do
    H.modify (\state ->
      state { result = addAsyncError name err state.result
            , asyncValidationInProgress = delete name state.asyncValidationInProgress })
    liftEff $ log $ ("ERROR added: " <> show name <> " " <> err)
    pure next

  evalForm (Blur key next) = do
    liftEff (log $ "Blur " <> show key)
    s <- H.get
    r <- liftAff $ (runForm my_form s.fields)

    case r of
      Valid f async -> do
        H.modify (\state ->
          state { result = Just (ValidR f) })

        validateAsync async

      Invalid err async -> do
        H.modify (\state ->
          state { result = Just (InvalidR err) })

        validateAsync async

    pure next

    where
      validateAsync async = do
        H.modify (\state -> state { asyncValidationInProgress = map (\(Tuple name _) -> name) async })
        parTraverse_ runAsyncValidation async
        pure unit
        
      runAsyncValidation (Tuple name aff) = do
        r <- liftAff $ runExceptT aff
        case r of
            Right _ -> do
              liftEff $ log "Success!"
              evalForm (H.action (AsyncSuccess name))
            Left err -> do
              liftEff $ log $ ("ERROR: " <> show name <> " " <> err)
              evalForm (H.action (AsyncError name err))
    
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

newPasswordForm :: forall m async. Monad m => Formlet FieldNames m async String
newPasswordForm =
  withCheck LoginPassword2 passwordsMatch
  (Tuple <$> field LoginPassword1 notEmpty Nothing <*> field LoginPassword2 notEmpty Nothing)

myForm :: forall m async. Monad m => Formlet FieldNames m (Async async) LoginForm
myForm =
  --withAsync LoginUsername validateUsername
  ((\u p np -> LoginForm $ { username : u, password : p, confirmedNewPassword: np})
  <$> field LoginUsername (notEmpty >=> bobortom) (Just validateUsername)
  <*> field LoginPassword (matches "bobishere" "The correct password is 'bobishere'!") Nothing
  <*> newPasswordForm)
  where
  bobortom name =
    if name == "Bob" then pure name
      else
        if name == "Tom" then pure name
        else throwError "Try 'Bob' or 'Tom'..."
             
  --validateUsername :: String -> ExceptT String (Async async) Unit
  validateUsername username = do
    x <- lift $ do
      checkUsernameAvailable username
    case x of
      "yes" ->
        pure $ unit
      _ ->
        throwError "Username is already taken!"


newPasswordView field1 field2 initial errors asyncV =
  renderHalogenInput field1 "Password (new):" initial errors asyncV
  <> renderHalogenInput field2 "Password (confirm):" initial errors asyncV

myView :: forall t form. Value FieldNames -> Maybe (Result FieldNames form) -> (FieldNames -> Boolean) -> MyHTML t FieldNames
myView initial res asyncV =
  renderHalogenInput LoginUsername "Username: " initial errors asyncV
  <> renderHalogenInput LoginPassword "Password: " initial errors asyncV
  <> newPasswordView LoginPassword1 LoginPassword2 initial errors asyncV
  where
    errors = case res of
      Just (InvalidR err) -> Just err
      _ -> Nothing
