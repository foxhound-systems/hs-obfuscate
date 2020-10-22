{-# LANGUAGE TemplateHaskell #-}

module Web.Obfuscate.TH where

import           Control.Monad
import           Language.Haskell.TH
import           Web.Obfuscate

data ObfuscationOptions = ObfuscationOptions
  { obfuscatedFieldPrefix       :: String
  , obfuscatedConstructorPrefix :: String
  }

defaultObfuscationOptions :: ObfuscationOptions
defaultObfuscationOptions = ObfuscationOptions
  { obfuscatedFieldPrefix = "ob"
  , obfuscatedConstructorPrefix = "Obfuscated"
  }

deriveObfuscate :: ObfuscationOptions -> Name -> Q [Dec]
deriveObfuscate options tyName = do
  TyConI ty <- reify tyName
  (obfuscatedName, obfuscatedTy) <- obfuscatedD ty
  let obfuscatedInstance = TySynInstD $
                            TySynEqn
                                Nothing
                                (ConT ''Obfuscated `AppT` ConT tyName)
                                (ConT obfuscatedName)
  obfuscateFunD <- mkObfuscateFunD ty
  deobfuscateFunD <- mkDeobfuscateFunD ty
  let canObfuscateInstance = InstanceD Nothing [] (ConT ''CanObfuscate `AppT` ConT tyName) [obfuscateFunD]
  let canDeobfuscateInstance = InstanceD Nothing [] (ConT ''CanDeobfuscate `AppT` ConT tyName) [deobfuscateFunD]
  pure [obfuscatedTy, obfuscatedInstance, canObfuscateInstance, canDeobfuscateInstance]
    where
      fieldPrefix = obfuscatedFieldPrefix options
      constructorPrefix = obfuscatedConstructorPrefix options

      obfuscatedFieldName n = mkName (fieldPrefix ++ nameBase n)
      obfuscatedConstructorName n = mkName (constructorPrefix ++ nameBase n)

      mkObfuscateFunD (DataD _ _ _ _ cs _) = do
        clauses <- traverse obfuscateClause cs
        pure $ FunD (mkName "obfuscate") clauses
      mkObfuscateFunD (NewtypeD _ _ _ _ c _) = do
        clauses <- traverse obfuscateClause [c]
        pure $ FunD (mkName "obfuscate") clauses

      mkDeobfuscateFunD (DataD _ _ _ _ cs _) = do
        clauses <- traverse deobfuscateClause cs
        pure $ FunD (mkName "deobfuscate") clauses
      mkDeobfuscateFunD (NewtypeD _ _ _ _ c _) = do
        clauses <- traverse deobfuscateClause [c]
        pure $ FunD (mkName "deobfuscate") clauses

      deobfuscateClause (NormalC n tys) = do
        let ctx = mkName "ctx"
            x = mkName "x"
        body <- [| undefined |]
        pure $ Clause [VarP ctx, VarP x] (NormalB body) []

      deobfuscateClause (RecC n tys) = do
        let ctx = mkName "ctx"
            x = mkName "x"
            pureFn = VarE $ mkName "pure"
        (deobfuscatedBinds, deobfuscatedFields) <- fmap mconcat $ traverse assignDeobfuscatedField tys

        let body = DoE $ join
                    [ fmap (\(p, e) -> BindS p e) deobfuscatedBinds
                    , [NoBindS (pureFn `AppE` (RecConE n deobfuscatedFields))]
                    ]
        pure $ Clause [VarP ctx, VarP x] (NormalB body) []

      assignDeobfuscatedField (n, _, ty) = do
        isObfuscateable <- isFieldObfuscateable ty
        if isObfuscateable then do
          let deobfuscatedFieldName = mkName $ "d" ++ nameBase n
          deobfuscatedE <- [| deobfuscate ctx $ $(varE $ obfuscatedFieldName n) x |]
          pure $ ([(VarP deobfuscatedFieldName, deobfuscatedE)], [(n, VarE deobfuscatedFieldName)])
        else do
          fieldValue <- [|$(varE $ obfuscatedFieldName n) x|]
          pure $ ([], [(n, fieldValue)])

      obfuscateNormalField (x, (_, ty)) = do
        isObfuscateable <- isFieldObfuscateable ty
        if isObfuscateable then
          [| obfuscate ctx $(varE x) |]
        else
          varE x

      obfuscateClause c@(NormalC n tys) = do
        let ctx = mkName "ctx"
        xs <- traverse (const $ newName "x") tys
        let ps = fmap VarP xs
        fields <- traverse obfuscateNormalField (zip xs tys)
        let body = foldl AppE (ConE (obfuscatedConstructorName n)) fields
        pure $ Clause [VarP ctx, ConP n ps] (NormalB body) []

      obfuscateClause (RecC n tys) = do
        let ctx = mkName "ctx"
            x = mkName "x"
        obfuscatedFields <- traverse assignObfuscatedField tys
        let body = RecConE (obfuscatedConstructorName n) obfuscatedFields
        pure $ Clause [VarP ctx, VarP x] (NormalB body) []

      assignObfuscatedField (fieldName, _, ty) = do
        isObfuscateable <- isFieldObfuscateable ty
        (,) <$> pure (obfuscatedFieldName fieldName) <*>
          if isObfuscateable then
            [|obfuscate ctx ($(varE fieldName) x)|]
          else
            [|$(varE fieldName) x|]

      obfuscatedD (DataD cxt n tvbs mKind cs dcs) = do
        obfuscatedCs <- traverse obfuscatedC cs
        let obfuscatedName = obfuscatedConstructorName n
        pure $ (obfuscatedName, DataD cxt obfuscatedName tvbs mKind obfuscatedCs dcs)
      obfuscatedD (NewtypeD cxt n tvbs mKind c dcs) = do
        obfuscatedC <- obfuscatedC c
        let obfuscatedName = obfuscatedConstructorName n
        pure $ (obfuscatedName, NewtypeD cxt obfuscatedName tvbs mKind obfuscatedC dcs)

      obfuscatedC (NormalC n tys) = do
        obfuscatedTys <- traverse (\(b, t) -> (,) <$> pure b <*> obfuscatedT t) tys
        let obfuscatedName = obfuscatedConstructorName n
        pure $ NormalC obfuscatedName obfuscatedTys
      obfuscatedC (RecC n tys) = do
        obfuscatedTys <- traverse (\(n, b, t) -> (,,) <$> pure (obfuscatedFieldName n) <*> pure b <*> obfuscatedT t) tys
        let obfuscatedName = obfuscatedConstructorName n
        pure $ RecC obfuscatedName obfuscatedTys
      obfuscatedC (GadtC ns tys ty) = do
        obfuscatedTys <- traverse (\(b, t) -> (,) <$> pure b <*> obfuscatedT t) tys
        let obfuscatedNames = fmap obfuscatedConstructorName ns
        pure $ GadtC obfuscatedNames obfuscatedTys ty
      obfuscatedC (RecGadtC ns tys ty) = do
        obfuscatedTys <- traverse (\(n, b, t) -> (,,) <$> pure (obfuscatedFieldName n) <*> pure b <*> obfuscatedT t) tys
        let obfuscatedNames = fmap obfuscatedConstructorName ns
        pure $ RecGadtC obfuscatedNames obfuscatedTys ty
      obfuscatedC (ForallC tvbs cxt c) = do
        ForallC tvbs cxt <$> obfuscatedC c
      obfuscatedC c = pure c

      isFieldObfuscateable ty = do
          canObfuscateInstances <- reifyInstances ''CanObfuscate [ty]
          filteredObfuscateInstances <- filterM (validateInstance ty) canObfuscateInstances
          canDeobfuscateInstances <- reifyInstances ''CanDeobfuscate [ty]
          filteredDeobfuscateInstances <- filterM (validateInstance ty) canDeobfuscateInstances
          let canObfuscate = length filteredObfuscateInstances > 0
              canDeobfuscate = length filteredDeobfuscateInstances > 0
          pure $ canObfuscate && canDeobfuscate

      -- This is very limited but should suffice for now.
      -- Its not 100% accurate but meets the needs of the base instances provided
      validateInstance typ (InstanceD _ cxt _ _) =
        and <$> traverse (validateCxt typ) cxt
      validateCxt :: Type -> Pred -> Q Bool
      validateCxt (AppT ListT ty) (AppT (ConT klass) (VarT _)) = do
        classInstances <- reifyInstances klass [ty]
        pure $ length classInstances > 0
      validateCxt (AppT (ConT _) ty) (AppT (ConT klass) (VarT _)) = do
        classInstances <- reifyInstances klass [ty]
        pure $ length classInstances > 0
      validateCxt (ConT ty) _ = do
        pure $ ty /= ''String

      obfuscatedT ty = do
        isObfuscateable <- isFieldObfuscateable ty
        if isObfuscateable then
          pure $ ConT ''Obfuscated `AppT` ty
        else
          pure ty
