{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QualifiedDo           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Plutarch.Extra.Record (
  mkRecord,
  mkRecordConstr,
  (.=),
  (.&),
  RecordMorphism,
  FieldName,
) where

import Control.Category (Category (id, (.)))
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Type.Equality (type (~))
import GHC.OverloadedLabels (IsLabel (fromLabel))
import GHC.TypeLits (Symbol)
import Plutarch (PlutusType, S, Term, pcon, (#))
import Plutarch.Builtin (PAsData)
import Plutarch.DataRepr (PDataRecord, PLabeledType ((:=)), pdcons, pdnil)
import Prelude (($))

{- | Like 'Data.Proxy.Proxy' but local to this module.

 @since 1.3.0
-}
data FieldName (sym :: Symbol) = FieldName

{- | The use of two different 'Symbol's here allows unification to happen,
     ensuring 'FieldName' has a fully inferred 'Symbol'.

     For example, @'mkRecord' (#foo .= 'pconstantData' (42 :: 'Integer'))@ gets
     the correct type. Namely, @'Term' s ('PDataRecord' '["foo" ':= 'PInteger'])@.

     @since 1.3.0
-}
instance
  forall (sym :: Symbol) (sym' :: Symbol).
  (sym ~ sym') =>
  IsLabel sym (FieldName sym)
  where
  fromLabel = FieldName

{- | Turn a constant 'RecordMorphism' into a fully built 'PDataRecord'.

 @since 1.3.0
-}
mkRecord :: forall (r :: [PLabeledType]) (s :: S). RecordMorphism s '[] r -> Term s (PDataRecord r)
mkRecord f = runRecordMorphism f pdnil

{- | 'mkRecord' but for known data-types.

This allows you to dynamically construct a record type constructor.

=== Example:
@
'mkRecordConstr'
  'PScriptContext'
  ( #txInfo '.=' '(Your PTxInfo)'
      '.&' #purpose '.=' '(Your PScriptPurpose)'
  )
@
Is the same as

@
'pconstant' ('ScriptContext' '(Your TxInfo)' '(Your ScriptPurpose)')
@

@since 1.3.0
-}
mkRecordConstr ::
  forall (r :: [PLabeledType]) (s :: S) (pt :: S -> Type).
  (PlutusType pt) =>
  -- | The constructor. This is just the Haskell-level constructor for the type.
  --   For 'Plutarch.Api.V2.Maybe.PMaybeData', this would
  --   be 'Plutarch.Api.V2.Maybe.PDJust', or 'Plutarch.Api.V2.Maybe.PNothing'.
  (forall s'. Term s' (PDataRecord r) -> pt s') ->
  -- | The morphism that builds the record.
  RecordMorphism s '[] r ->
  Term s pt
mkRecordConstr ctr = pcon . ctr . mkRecord

{- | A morphism from one 'PDataRecord' to another, representing some sort of consing of data.

 @since 1.3.0
-}
newtype RecordMorphism (s :: S) (as :: [PLabeledType]) (bs :: [PLabeledType])
  = RecordMorphism (Term s (PDataRecord as) -> Term s (PDataRecord bs))

-- | @since 3.8.0
runRecordMorphism ::
  forall (s :: S) (as :: [PLabeledType]) (bs :: [PLabeledType]).
  RecordMorphism s as bs ->
  Term s (PDataRecord as) ->
  Term s (PDataRecord bs)
runRecordMorphism (RecordMorphism f) = f

-- | @since 1.3.0
instance Category (RecordMorphism s) where
  id = RecordMorphism id
  f . g = coerce $ runRecordMorphism f . runRecordMorphism g

infix 7 .=

{- | Cons a labeled type as a 'RecordMorphism'.

 @since 3.1.0
-}
(.=) ::
  forall (sym :: Symbol) (a :: S -> Type) (as :: [PLabeledType]) (s :: S).
  -- | The field name. You can use @-XOverloadedLabels@ to enable the syntax:
  --   @#hello ~ 'FieldName' "hello"@
  FieldName sym ->
  -- | The value at that field. This must be 'PAsData', because the underlying
  --   type is @'PlutusCore.Data.Constr' 'Integer' ['PlutusCore.Data.Data']@.
  Term s (PAsData a) ->
  RecordMorphism s as ((sym ':= a) ': as)
_ .= x = RecordMorphism $ \rest -> pdcons # x # rest

infixr 6 .&

{- | Compose two 'RecordMorphism's.

 @since 1.3.0
-}
(.&) ::
  forall
    (s :: S)
    (a :: [PLabeledType])
    (b :: [PLabeledType])
    (c :: [PLabeledType]).
  RecordMorphism s b c ->
  RecordMorphism s a b ->
  RecordMorphism s a c
(.&) = (.)
