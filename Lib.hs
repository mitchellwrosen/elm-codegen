{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DefaultSignatures, DeriveAnyClass,
             DeriveGeneric, DerivingStrategies, FlexibleContexts,
             FlexibleInstances, InstanceSigs, KindSignatures, LambdaCase,
             OverloadedStrings, PolyKinds, ScopedTypeVariables, TupleSections,
             TypeApplications, ViewPatterns, UnicodeSyntax #-}

module Lib where

import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Data.Foldable
import Data.Function
import Data.Semigroup
import Data.Traversable

import qualified AST.Source as Elm
import qualified Elm.Name as Elm (Name)
import qualified Elm.Name
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Reporting.Annotation as Elm.Annotation
import qualified Reporting.Region as Elm.Region
import qualified Reporting.Render.Type as Elm.Render
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty

data ElmDec
  = ElmDecType ElmType
  | ElmDecTypeAlias ElmTypeAlias
  deriving (Show)

data ElmType
  = ElmType Elm.Name [Elm.Name] [ElmConstructor]
  deriving (Show)

data ElmTypeAlias
  = ElmTypeAlias Elm.Name [Elm.Name] Elm.Type
  deriving (Show)

data ElmConstructor
  = ElmConstructor Elm.Name [Elm.Type]
  deriving (Show)

prettyElmDec :: ElmDec -> Pretty.Doc
prettyElmDec = \case
  ElmDecType ty -> prettyElmType ty
  ElmDecTypeAlias ty -> prettyElmTypeAlias ty

prettyElmType :: ElmType -> Pretty.Doc
prettyElmType (ElmType name tvs cons) =
  Pretty.hsep ("type" : map prettyName (name : tvs)) <>
    if null cons
      then
        mempty
      else
        mconcat
          [ Pretty.space
          , "="
          , Pretty.line
          , cons
              & map prettyElmConstructor
              & mapTail (\p -> "|" <> Pretty.space <> p)
              & Pretty.vsep
              & Pretty.indent 2
          ]
  where
    mapTail _ []     = []
    mapTail f (x:xs) = x : map f xs

prettyElmTypeAlias :: ElmTypeAlias -> Pretty.Doc
prettyElmTypeAlias (ElmTypeAlias name tvs ty) =
  mconcat
    [ "type"
    , Pretty.space
    , "alias"
    , Pretty.space
    , Pretty.hsep (map prettyName (name : tvs))
    , Pretty.space
    , "="
    , Pretty.line
    , Pretty.indent 2 (Elm.Render.srcToDoc Elm.Render.None ty)
    ]

prettyElmConstructor :: ElmConstructor -> Pretty.Doc
prettyElmConstructor (ElmConstructor name fields) =
  Pretty.hsep
    (prettyName name :
      map (Elm.Render.srcToDoc Elm.Render.App) fields)

prettyName :: Elm.Name -> Pretty.Doc
prettyName =
  Pretty.text . Elm.Name.toString

declareElmType :: TH.Name -> TH.Q TH.Exp
declareElmType =
  declareElmType_ False

declareElmTypeAlias :: TH.Name -> TH.Q TH.Exp
declareElmTypeAlias =
  declareElmType_ True

declareElmType_ :: Bool -> TH.Name -> TH.Q TH.Exp
declareElmType_ alias =
  declareElmType__ alias >=>
    TH.lift
      . ($ "")
      . Pretty.displayS
      . Pretty.renderPretty 0.4 80
      . prettyElmDec

declareElmType__ :: Bool -> TH.Name -> TH.Q ElmDec
declareElmType__ alias name =
  runExceptT (nameToElmDec alias name)
    >>= either fail pure

nameToElmDec ::
     Bool
  -> TH.Name
  -> ExceptT String TH.Q ElmDec
nameToElmDec alias =
  lift . TH.reify >=> infoToElmDec alias

infoToElmDec ::
     Bool
  -> TH.Info
  -> ExceptT String TH.Q ElmDec
infoToElmDec alias = \case
    TH.TyConI dec ->
      case dec of
        TH.ClassD _ _ _ _ _ ->
          throwE "Cannot generate an Elm type from a type class"

        TH.ClosedTypeFamilyD _ _ ->
          throwE "Cannot generate an Elm type from a type family"

        TH.DataD ctx name tvs mkind cons _ ->
          dataDecToElmDec alias ctx name tvs mkind cons

        TH.DataFamilyD _ _ _ ->
          throwE "Cannot generate an Elm type from a data family"

        TH.DataInstD _ _ _ _ _ _ ->
          throwE "Cannot generate an Elm type from a data family instance"

        TH.DefaultSigD _ _ ->
          throwE "Cannot generate an Elm type from a default signature"

        TH.ForeignD _ ->
          throwE "Cannot generate an Elm type from a foreign declaration"

        TH.FunD _ _ ->
          throwE "Cannot generate an Elm type from a function"

        TH.InfixD _ _ ->
          throwE "Cannot generate an Elm type from a fixity declaration"

        TH.InstanceD _ _ _ _ ->
          throwE "Cannot generate an Elm type from a type class instance"

        TH.NewtypeD ctx name tvs mkind cons _ ->
          dataDecToElmDec alias ctx name tvs mkind [cons]

        TH.NewtypeInstD _ _ _ _ _ _ ->
          throwE "Cannot generate an Elm type from a data family instance"

        TH.OpenTypeFamilyD _ ->
          throwE "Cannot generate an Elm type from a type family"

        TH.PatSynD _ _ _ _ ->
          throwE "Cannot generate an Elm type from a pattern synonym"

        TH.PatSynSigD _ _ ->
          throwE "Cannot generate an Elm type from a pattern synonym type signature"

        TH.PragmaD _ ->
          throwE "Cannot generate an Elm type from a pragma"

        TH.RoleAnnotD _ _ ->
          throwE "Cannot generate an Elm type from a role annotation"

        TH.SigD _ _ ->
          throwE "Cannot generate an Elm type from a type signature"

        TH.StandaloneDerivD _ _ _ ->
          throwE "Cannot generate an Elm type from a standalone deriving clause"

        TH.TySynD name tvs ty ->
          tysynToElmDec alias name tvs ty

        TH.TySynInstD _ _ ->
          throwE "Cannot generate an Elm type from a type family instance"

        TH.ValD _ _ _ ->
          throwE "Cannot generate an Elm type from a value"

    _ ->
      throwE "Not a type constructor"

dataDecToElmDec ::
     Bool
  -> TH.Cxt
  -> TH.Name
  -> [TH.TyVarBndr]
  -> Maybe TH.Type
  -> [TH.Con]
  -> ExceptT String TH.Q ElmDec
dataDecToElmDec alias ctx name tvs mkind cons = do
  -- Three checks on the Haskell data declaration:
  --
  -- * It has no data type context
  -- * Its type variables are kind Type
  -- * It's kind Type
  assertNoContext ctx
  assertTyvarsKindType tvs
  assertKindType mkind

  if alias
    then ElmDecTypeAlias <$> dataDecToElmTypeAlias name tvs cons
    else ElmDecType <$> dataDecToElmType name tvs cons

assertNoContext :: TH.Cxt -> ExceptT String TH.Q ()
assertNoContext = \case
  [] -> pure ()
  _  -> throwE "Data type context not supported"

assertTyvarsKindType :: [TH.TyVarBndr] -> ExceptT String TH.Q ()
assertTyvarsKindType =
  traverse_ $ \case
    TH.PlainTV _ -> pure ()
    TH.KindedTV _ TH.StarT -> pure ()
    TH.KindedTV _ _ -> throwE "Type variables may only have kind Type"

assertKindType :: Maybe TH.Kind -> ExceptT String TH.Q ()
assertKindType =
  traverse_ $ \case
    TH.StarT -> pure ()
    _ -> throwE "Type may only have kind Type"

dataDecToElmType ::
     TH.Name
  -> [TH.TyVarBndr]
  -> [TH.Con]
  -> ExceptT String TH.Q ElmType
dataDecToElmType name tvs cons =
  ElmType
    <$> pure (nameToElmName name)
    <*> pure (tyvarsToElmNames tvs)
    <*> traverse conToElmCon cons

tyvarsToElmNames :: [TH.TyVarBndr] -> [Elm.Name]
tyvarsToElmNames =
  map (nameToElmName . tyVarBndrName)
  where
    tyVarBndrName :: TH.TyVarBndr -> TH.Name
    tyVarBndrName = \case
      TH.PlainTV  name   -> name
      TH.KindedTV name _ -> name

dataDecToElmTypeAlias ::
     TH.Name
  -> [TH.TyVarBndr]
  -> [TH.Con]
  -> ExceptT String TH.Q ElmTypeAlias
dataDecToElmTypeAlias name tvs = \case
  [TH.RecC _ fields] -> do
    ty <- recordToElmType fields
    pure (ElmTypeAlias (nameToElmName name) (tyvarsToElmNames tvs) ty)

  _ ->
    throwE "Cannot make a type alias from a non-record type"

tysynToElmDec ::
     Bool
  -> TH.Name
  -> [TH.TyVarBndr]
  -> TH.Type
  -> ExceptT String TH.Q ElmDec
tysynToElmDec alias name tvs ty = do
  assertTyvarsKindType tvs

  if alias
    then ElmDecTypeAlias <$> tysynToElmTypeAlias name tvs ty
    else ElmDecType <$> tysynToElmType name tvs ty

tysynToElmTypeAlias ::
     TH.Name
  -> [TH.TyVarBndr]
  -> TH.Type
  -> ExceptT String TH.Q ElmTypeAlias
tysynToElmTypeAlias name tvs ty =
  ElmTypeAlias
    <$> pure (nameToElmName name)
    <*> pure (tyvarsToElmNames tvs)
    <*> typeToElmType ty

tysynToElmType ::
     TH.Name
  -> [TH.TyVarBndr]
  -> TH.Type
  -> ExceptT String TH.Q ElmType
tysynToElmType (nameToElmName -> name) tvs ty = do
  ty' <- typeToElmType ty
  pure
    (ElmType
      name
      (tyvarsToElmNames tvs)
      [ElmConstructor name [ty']])

conToElmCon :: TH.Con -> ExceptT String TH.Q ElmConstructor
conToElmCon con0 =
  case con0 of
    TH.ForallC _ _ _ ->
      throwE "Existentially quantified types not supported"

    TH.NormalC name fields ->
      ElmConstructor (nameToElmName name) <$>
        traverse (typeToElmType . snd) fields

    TH.RecC name fields -> do
      ty <- recordToElmType fields
      pure (ElmConstructor (nameToElmName name) [ty])

    _ ->
      error (show con0)

typeToElmType :: TH.Type -> ExceptT String TH.Q Elm.Type
typeToElmType ty0 =
  case ty0 of
    TH.AppT t1 t2 ->
      case t1 of
        TH.AppT TH.ArrowT t1' -> do
          arrowToElmType t1' t2

        _ ->
          case reverse (t2 : uncurryApp t1) of
            TH.AppT _ _ : _ ->
              error (show ty0)

            TH.ArrowT : _ ->
              throwE "Partially applied (->) not supported"

            TH.ConT name : ts ->
              typeConstructorToElmType name ts

            TH.ConstraintT : _ ->
              error (show ty0)

            TH.EqualityT : _ ->
              throwE "(~) not supported"

            TH.ForallT _ _ _ : _ ->
              error (show ty0)

            TH.InfixT _ _ _ : _ ->
              throwE "Type operators not supported"

            TH.ListT : [ty] ->
              typeConstructorToElmType (TH.mkName "List") [ty]

            TH.LitT _ : _ ->
              error (show ty0)

            TH.ParensT _ : _ ->
              error (show ty0)

            TH.PromotedT _ : _ ->
              throwE "Promoted types not supported"

            TH.PromotedConsT : _ ->
              throwE "Promoted lists not supported"

            TH.PromotedNilT : _ ->
              error (show ty0)

            TH.PromotedTupleT _ : _ ->
              throwE "Promoted tuples not supported"

            TH.ListT : _ ->
              throwE "Partially applied list not supported"

            TH.SigT _ _ : _ ->
              error (show ty0)

            TH.StarT : _ ->
              error (show ty0)

            TH.TupleT n : ts
              | n == length ts ->
                  tupleToElmType ts
              | otherwise ->
                  throwE "Partially applied tuple not supported"

            TH.UInfixT _ _ _ : _ ->
              throwE "Type operators not supported"

            TH.UnboxedSumT _ : _ ->
              throwE "Unboxed sum not supported"

            TH.UnboxedTupleT _ : _ ->
              throwE "Unboxed tuple not supported"

            TH.VarT _ : _ ->
              error (show ty0)

            TH.WildCardT : _ ->
              error (show ty0)

            [] ->
              undefined

    TH.ArrowT ->
      throwE "(->) not supported"

    TH.ConT name ->
      typeConstructorToElmType name []

    TH.ConstraintT ->
      throwE "Constriant kind not supported"

    TH.EqualityT ->
      throwE "(~) not supported"

    TH.InfixT _ _ _ ->
      throwE "Type operators not supported"

    TH.ForallT _ _ _ ->
      throwE "Higher-rank types not supported"

    TH.ListT ->
      throwE "[] not supported"

    TH.LitT _ ->
      throwE "Type literal not supported"

    TH.ParensT ty ->
      typeToElmType ty

    TH.PromotedT _ ->
      throwE "Promoted types not supported"

    TH.PromotedConsT ->
      throwE "Promoted lists not supported"

    TH.PromotedNilT ->
      throwE "Promoted lists not supported"

    TH.PromotedTupleT _ ->
      throwE "Promoted tuples not supported"

    TH.SigT ty _ ->
      typeToElmType ty

    TH.StarT ->
      throwE "Type kind not supported"

    TH.TupleT 0 ->
      pure (noloc Elm.TUnit)

    TH.TupleT _ ->
      throwE ("Tuple not supported: " ++ show ty0)

    TH.UInfixT _ _ _ ->
      throwE "Type operators not supported"

    TH.UnboxedSumT _ ->
      throwE "Unboxed sum not supported"

    TH.UnboxedTupleT _ ->
      throwE "Unboxed tuple not supported"

    TH.VarT name ->
      pure (noloc (Elm.TVar (nameToElmName name)))

    TH.WildCardT ->
      throwE "Wildcard not supported"

  where
    uncurryApp :: TH.Type -> [TH.Type]
    uncurryApp = \case
      TH.AppT t1 t2 ->
        strip t2 : uncurryApp t1

      ty ->
        [strip ty]

    strip :: TH.Type -> TH.Type
    strip = \case
      TH.ParensT ty -> ty
      TH.SigT ty _ -> ty
      ty -> ty

nameToElmName :: TH.Name -> Elm.Name
nameToElmName =
  Elm.Name.fromString . TH.nameBase

arrowToElmType :: TH.Type -> TH.Type -> ExceptT String TH.Q Elm.Type
arrowToElmType t1 t2 = do
  t1' <- typeToElmType t1
  t2' <- typeToElmType t2
  pure (noloc (Elm.TLambda t1' t2'))

recordToElmType :: [TH.VarBangType] -> ExceptT String TH.Q Elm.Type
recordToElmType fields = do
  fields' <-
    for fields $ \(fname, _bang, fty) ->
      (noloc (nameToElmName fname) ,) <$> typeToElmType fty

  pure (noloc (Elm.TRecord fields' Nothing))

tupleToElmType :: [TH.Type] -> ExceptT String TH.Q Elm.Type
tupleToElmType = \case
  [t1, t2] -> do
    t1' <- typeToElmType t1
    t2' <- typeToElmType t2
    pure (noloc (Elm.TTuple t1' t2' []))

  [t1, t2, t3] -> do
    t1' <- typeToElmType t1
    t2' <- typeToElmType t2
    t3' <- typeToElmType t3
    pure (noloc (Elm.TTuple t1' t2' [t3']))

  _ ->
    throwE "Only 2- and 3-element tuples are supported"

typeConstructorToElmType :: TH.Name -> [TH.Type] -> ExceptT String TH.Q Elm.Type
typeConstructorToElmType name args = do
  noloc . Elm.TType Elm.Region.zero (nameToElmName name) <$>
    traverse typeToElmType args

noloc :: a -> Elm.Annotation.Located a
noloc =
  Elm.Annotation.At Elm.Region.zero
