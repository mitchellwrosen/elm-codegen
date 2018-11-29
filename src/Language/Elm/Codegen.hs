{-# LANGUAGE DeriveLift, LambdaCase, TupleSections #-}

-- | @Haskell AST -> Maybe (Elm AST)@

module Language.Elm.Codegen
  ( fromName
  , fromInfo
  , fromDec
  , fromType
  , ElmDec(..)
  , ElmDataType(..)
  , ElmTypeAlias(..)
  , ElmConstructor(..)
  , ElmType(..)
  ) where

import Data.Foldable (traverse_)
import Data.Traversable (for)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data ElmDec
  = ElmDecDataType ElmDataType
  | ElmDecTypeAlias ElmTypeAlias
  deriving (Eq, Lift, Show)

data ElmDataType
  = ElmDataType String [String] [ElmConstructor]
  deriving (Eq, Lift, Show)

data ElmTypeAlias
  = ElmTypeAlias String [String] ElmType
  deriving (Eq, Lift, Show)

data ElmConstructor
  = ElmConstructor String [ElmType]
  deriving (Eq, Lift, Show)

data ElmType
  = ElmTypeCon String [ElmType]
  | ElmTypeLambda ElmType ElmType
  | ElmTypeRecord [(String, ElmType)] (Maybe String)
  | ElmTypeTuple ElmType ElmType (Maybe ElmType)
  | ElmTypeUnit
  | ElmTypeVar String
  deriving (Eq, Lift, Show)

-- | Construct an Elm declaration from a Haskell name.
--
-- @
-- $(fromName ''Foo) :: 'ElmDec'
-- @
fromName :: Name -> Q Exp
fromName name = do
  info <- reify name
  either fail lift (fromInfo info)

-- | Construct an Elm declaration from info about a Haskell name.
fromInfo :: Info -> Either String ElmDec
fromInfo = \case
  ClassI dec _  -> fromDec dec
  TyConI dec    -> fromDec dec
  FamilyI dec _ -> fromDec dec

  ClassOpI{}   -> die "type class method"
  DataConI{}   -> die "data constructor"
  PatSynI{}    -> die "pattern synonym"
  PrimTyConI{} -> die "primitive type constructor"
  TyVarI{}     -> die "type variable"
  VarI{}       -> die "variable"

  where
    die s = Left ("Cannot generate an Elm declaration from a " ++ s)

-- | Construct an Elm declaration from a Haskell declaration.
fromDec :: Dec -> Either String ElmDec
fromDec = \case
  DataD ctx name tvs mkind cons _deriv ->
    fromDataDec ctx name tvs mkind cons

  NewtypeD ctx name tvs mkind cons _deriv ->
    fromDataDec ctx name tvs mkind [cons]

  TySynD name tvs ty ->
    fromTypeDec name tvs ty

  ClassD{}            -> die "type class"
  ClosedTypeFamilyD{} -> die "type family"
  DataFamilyD{}       -> die "data family"
  DataInstD{}         -> die "data family instance"
  DefaultSigD{}       -> die "default signature"
  ForeignD{}          -> die "foreign declaration"
  FunD{}              -> die "function"
  InfixD{}            -> die "fixity declaration"
  InstanceD{}         -> die "type class instance"
  NewtypeInstD{}      -> die "data family instance"
  OpenTypeFamilyD{}   -> die "type family"
  PatSynD{}           -> die "pattern synonym"
  PatSynSigD{}        -> die "pattern synonym type signature"
  PragmaD{}           -> die "pragma"
  RoleAnnotD{}        -> die "role annotation"
  SigD{}              -> die "type signature"
  StandaloneDerivD{}  -> die "standalone deriving clause"
  TySynInstD{}        -> die "type family instance"
  ValD{}              -> die "value"

  where
    die s = Left ("Cannot generate an Elm declaration from a " ++ s)

fromDataDec ::
     Cxt
  -> Name
  -> [TyVarBndr]
  -> Maybe Type
  -> [Con]
  -> Either String ElmDec
fromDataDec ctx name tvs mkind cons = do
  -- Three checks on the Haskell data declaration:
  --
  -- ∙ It has no data type context
  -- ∙ Its type variables are kind Type
  -- ∙ It's kind Type
  assertNoContext ctx
  assertTyvarsKindType tvs
  assertKindType mkind

  ElmDecDataType <$>
    (ElmDataType
      <$> pure (nameBase name)
      <*> pure (tyvarsToStrings tvs)
      <*> traverse fromCon cons)

fromTypeDec :: Name -> [TyVarBndr] -> Type -> Either String ElmDec
fromTypeDec name tvs ty = do
  assertTyvarsKindType tvs
  ElmDecTypeAlias <$>
    (ElmTypeAlias
      <$> pure (nameBase name)
      <*> pure (tyvarsToStrings tvs)
      <*> fromType ty)

assertNoContext :: Cxt -> Either String ()
assertNoContext = \case
  [] -> Right ()
  _  -> Left "Data type context not supported"

assertTyvarsKindType :: [TyVarBndr] -> Either String ()
assertTyvarsKindType =
  traverse_ $ \case
    PlainTV _ -> Right ()
    KindedTV _ StarT -> Right ()
    KindedTV _ _ -> Left "Type variables may only have kind Type"

assertKindType :: Maybe Kind -> Either String ()
assertKindType =
  traverse_ $ \case
    StarT -> Right ()
    _ -> Left "Type may only have kind Type"


--------------------------------------------------------------------------------
-- Constructor
--------------------------------------------------------------------------------

fromCon :: Con -> Either String ElmConstructor
fromCon con0 =
  case con0 of
    ForallC _ _ _ ->
      Left "Existentially quantified types not supported"

    NormalC name fields ->
      ElmConstructor (nameBase name) <$>
        traverse (fromType . snd) fields

    RecC name fields -> do
      ty <- fromRecord fields
      Right (ElmConstructor (nameBase name) [ty])

    _ ->
      error (show con0)


--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | Construct an Elm type from a Haskell type.
fromType :: Type -> Either String ElmType
fromType ty0 =
  case ty0 of
    AppT t1 t2 ->
      case t1 of
        AppT ArrowT t1' -> do
          fromArrow t1' t2

        _ ->
          case reverse (t2 : uncurryApp t1) of
            AppT _ _ : _ ->
              error (show ty0)

            ArrowT : _ ->
              Left "Partially applied (->) not supported"

            ConT name : ts ->
              fromTypeCon name ts

            ConstraintT : _ ->
              error (show ty0)

            EqualityT : _ ->
              Left "(~) not supported"

            ForallT _ _ _ : _ ->
              error (show ty0)

            InfixT _ _ _ : _ ->
              Left "Type operators not supported"

            ListT : [ty] ->
              fromTypeCon (mkName "List") [ty]

            LitT _ : _ ->
              error (show ty0)

            ParensT _ : _ ->
              error (show ty0)

            PromotedT _ : _ ->
              Left "Promoted types not supported"

            PromotedConsT : _ ->
              Left "Promoted lists not supported"

            PromotedNilT : _ ->
              error (show ty0)

            PromotedTupleT _ : _ ->
              Left "Promoted tuples not supported"

            ListT : _ ->
              Left "Partially applied list not supported"

            SigT _ _ : _ ->
              error (show ty0)

            StarT : _ ->
              error (show ty0)

            TupleT n : ts
              | n == length ts ->
                  fromTuple ts
              | otherwise ->
                  Left "Partially applied tuple not supported"

            UInfixT _ _ _ : _ ->
              Left "Type operators not supported"

            UnboxedSumT _ : _ ->
              Left "Unboxed sum not supported"

            UnboxedTupleT _ : _ ->
              Left "Unboxed tuple not supported"

            VarT _ : _ ->
              error (show ty0)

            WildCardT : _ ->
              error (show ty0)

            [] ->
              undefined

    ArrowT ->
      Left "(->) not supported"

    ConT name ->
      fromTypeCon name []

    ConstraintT ->
      Left "Constriant kind not supported"

    EqualityT ->
      Left "(~) not supported"

    InfixT _ _ _ ->
      Left "Type operators not supported"

    ForallT _ _ _ ->
      Left "Higher-rank types not supported"

    ListT ->
      Left "[] not supported"

    LitT _ ->
      Left "Type literal not supported"

    ParensT ty ->
      fromType ty

    PromotedT _ ->
      Left "Promoted types not supported"

    PromotedConsT ->
      Left "Promoted lists not supported"

    PromotedNilT ->
      Left "Promoted lists not supported"

    PromotedTupleT _ ->
      Left "Promoted tuples not supported"

    SigT ty _ ->
      fromType ty

    StarT ->
      Left "Type kind not supported"

    TupleT 0 ->
      pure ElmTypeUnit

    TupleT _ ->
      Left ("Tuple not supported: " ++ show ty0)

    UInfixT _ _ _ ->
      Left "Type operators not supported"

    UnboxedSumT _ ->
      Left "Unboxed sum not supported"

    UnboxedTupleT _ ->
      Left "Unboxed tuple not supported"

    VarT name ->
      pure (ElmTypeVar (nameBase name))

    WildCardT ->
      Left "Wildcard not supported"

  where
    uncurryApp :: Type -> [Type]
    uncurryApp = \case
      AppT t1 t2 ->
        strip t2 : uncurryApp t1

      ty ->
        [strip ty]

    strip :: Type -> Type
    strip = \case
      ParensT ty -> ty
      SigT ty _ -> ty
      ty -> ty

fromArrow :: Type -> Type -> Either String ElmType
fromArrow t1 t2 = do
  t1' <- fromType t1
  t2' <- fromType t2
  pure (ElmTypeLambda t1' t2')

fromRecord :: [VarBangType] -> Either String ElmType
fromRecord fields = do
  fields' <-
    for fields $ \(fname, _bang, fty) ->
      (nameBase fname ,) <$> fromType fty

  pure (ElmTypeRecord fields' Nothing)

fromTuple :: [Type] -> Either String ElmType
fromTuple = \case
  [t1, t2] -> do
    t1' <- fromType t1
    t2' <- fromType t2
    pure (ElmTypeTuple t1' t2' Nothing)

  [t1, t2, t3] -> do
    t1' <- fromType t1
    t2' <- fromType t2
    t3' <- fromType t3
    pure (ElmTypeTuple t1' t2' (Just t3'))

  _ ->
    Left "Only 2- and 3-element tuples are supported"

fromTypeCon :: Name -> [Type] -> Either String ElmType
fromTypeCon name args =
  ElmTypeCon (nameBase name) <$> traverse fromType args

tyvarsToStrings :: [TyVarBndr] -> [String]
tyvarsToStrings =
  map (nameBase . tyVarBndrName)
  where
    tyVarBndrName :: TyVarBndr -> Name
    tyVarBndrName = \case
      PlainTV  name   -> name
      KindedTV name _ -> name
