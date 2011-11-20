{-# LANGUAGE ExistentialQuantification #-}
-----------------------------------------------------------------------------
--
-- Module      :  Code.New.Symbol
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Code.New.Symbol (
    Exportable(..), Exported,

    SymbolLoc(..),
    LocSym(..),

    DSymbol(..), DSym(..), FDSym(..),
    eqd,

    Func(..),

    TName(..),
    TSymbol(..), TSym(..),
    eqt,

    DataType(..), Constructor(..),

) where

import Data.Maybe(fromJust)
import Language.Haskell.Exts.Syntax

-----------------------------------------------------------------------------

--type Exported = Bool
--
---- | Symbols that can be exported
--class Exportable e where
--    isExported  :: e -> Exported
--    setExported :: e -> Exported -> e

-----------------------------------------------------------------------------

-- Location info
--data SymbolLoc
--    = Imported ModuleName (Maybe ModuleName)
--    | LocalDec [Decl]
--    deriving (Eq, Ord, Show)


-- | Symbols that have location info
--class LocSym s where
--    getLocation :: s -> SymbolLoc
--    isImported  :: s -> Bool
--    isImported  = isImport . getLocation
--    impSrc      :: s -> Maybe (ModuleName, Maybe ModuleName)
--    impSrc      = getImportSrc . getLocation
--    impSrc'     :: s -> (ModuleName, Maybe ModuleName)
--    impSrc'     = fromJust . impSrc
--
--isImport :: SymbolLoc -> Bool
--isImport (Imported _ _) = True
--isImport _              = False
--
--getImportSrc :: SymbolLoc -> Maybe (ModuleName, Maybe ModuleName)
--getImportSrc (Imported mn qn) = Just (mn, qn)
--getImportSrc (LocalDec _)     = Nothing

-----------------------------------------------------------------------------

-- Symbols in the data scope e.g. functions, constants, constructors, etc.
class DSymbol s where
    dsymName    :: s -> Name

-- | Check if two DSymbol names are the same
eqd :: (DSymbol d1, DSymbol d2) => d1 -> d2 -> Bool
eqd d1 d2 = dsymName d1 == dsymName d2

-- Wrapper for DSymbol-s
data DSym = forall s. DSymbol s => DSym s

--instance Exportable DSym where
--    isExported  (DSym d)    = isExported d
--    setExported (DSym d) e  = DSym $ setExported d e
instance DSymbol DSym where
    dsymName (DSym d) = dsymName d

-- Wrapper for free DSymbol-s (functions, constants but not Constructors,
-- typeclass functions)
data FDSym = forall s. (DSymbol s) => FDSym s

--instance Exportable FDSym where
--    isExported  (FDSym d)   = isExported d
--    setExported (FDSym d) e = FDSym $ setExported d e
instance DSymbol FDSym where
    dsymName (FDSym d) = dsymName d
--instance LocSym FDSym where
--    getLocation (FDSym d) = getLocation d

-----------------------------------------------------------------------------

-- | Function symbol
data Func
    = Func Name -- SymbolLoc Exported
    deriving (Eq, Ord)

--instance Exportable Func where
--    isExported  (Func _ _ e)    = e
--    setExported (Func n s _) e  = Func n s e

instance DSymbol Func where
    dsymName (Func n _ _) = n

--instance LocSym Func where
--    getLocation (Func _ l _) = l

-----------------------------------------------------------------------------

-- | name used by a type symbol, to prevent mixing with Dsymbols
newtype TName = TName { getTNname :: Name }
    deriving (Eq, Ord, Show)

-- | Symbols in the type scope
class TSymbol t where
    tsymName   :: t -> TName
    assocDSyms :: t -> [DSym]

-- | check if two TSymbol are the same
eqt :: (TSymbol t1, TSymbol t2) => t1 -> t2 -> Bool
eqt t1 t2 = tsymName t1 == tsymName t2

-- | Wrapper for TSymbol-s
data TSym = forall t. TSymbol t => TSym t

--instance Exportable TSym where
--    isExported  (TSym t)    = isExported t
--    setExported (TSym t) e  = TSym $ setExported t e
--instance LocSym TSym where
--    getLocation (TSym t) = getLocation t
instance TSymbol TSym where
    tsymName   (TSym t) = tsymName t
    assocDSyms (TSym t) = assocDSyms t

-----------------------------------------------------------------------------

-- | A DataType symbol
data DataType
    = DType TName {- SymbolLoc Exported -} [Constructor]
    deriving (Eq, Ord)

--instance Exportable DataType where
--    isExported  (DType _ _ e _ )    = e
--    setExported (DType n l _ cs) e  = DType n l e cs
--
--instance LocSym DataType where
--    getLocation (DType _ l _ _) = l

instance TSymbol DataType where
    tsymName   (DType n _ _ _ ) = n
    assocDSyms (DType _ _ _ cs) = map DSym cs

-- TODO records
-- | A Constructor belonging to a DataType
data Constructor
    = Con' Name --Exported
    deriving (Eq, Ord, Show)

--instance Exportable Constructor where
--    isExported  (Con' _ e)   = e
--    setExported (Con' n _) e = Con' n e

instance DSymbol Constructor where
    dsymName    (Con' n _) = n

-----------------------------------------------------------------------------
