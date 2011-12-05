-----------------------------------------------------------------------------
--
-- Module      :  Code.Expressions
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

module Code.Generating.Expressions (
    replaceSubExp,
    etaReduce,
    specialeEtaReduce,
    etaVar,
    removeEtaVar,

) where

import Code.Generating.Utils
import Language.Haskell.Exts.Syntax

replaceSubExp :: Exp -> Exp -> Exp -> Exp
replaceSubExp subExp repl ex
    | ex == subExp = repl
    | otherwise = case ex of
        InfixApp ex1 qop ex2 -> InfixApp            (rep ex1) qop (rep ex2)
        App ex1 ex2          -> App                 (rep ex1)     (rep ex2)
        NegApp ex1           -> NegApp              (rep ex1)
        Lambda src pat ex1   -> Lambda      src pat (rep ex1) --TODO check pat
        Let bnd ex1          -> Let             bnd (rep ex1) --TODO replace in bnd
        If ex1 ex2 ex3       -> If                  (rep ex1)     (rep ex2) (rep ex3)
        Case ex1 als         -> Case                (rep ex1) als --TODO fix als
        Tuple exs            -> Tuple $ map rep exs
        Do  stm              -> Do  $ map repInStmt stm
        MDo stm              -> MDo $ map repInStmt stm
        TupleSection exs     -> TupleSection $ map (fmap rep) exs
        List exs             -> List $ map rep exs
        Paren ex1            -> Paren               (rep ex1)
        LeftSection ex1 qop  -> LeftSection         (rep ex1) qop
        RightSection qop ex1 -> RightSection    qop (rep ex1)
        --RecConstr qn fu
        RecUpdate ex1 fus    -> RecUpdate           (rep ex1) fus -- TODO fus
        EnumFrom ex1         -> EnumFrom            (rep ex1)
        EnumFromTo ex1 ex2   -> EnumFromTo          (rep ex1)     (rep ex2)
        EnumFromThen ex1 ex2 -> EnumFromThen        (rep ex1)     (rep ex2)
        EnumFromThenTo ex1 ex2 ex3 -> EnumFromThenTo (rep ex1)    (rep ex2) (rep ex3)
        ListComp ex1 qss     -> ListComp            (rep ex1) qss --TODO qss
        -- ParComp
        ExpTypeSig src ex1 ty-> ExpTypeSig      src (rep ex1) ty
        __________           -> ex -- others after ExpTypeSig
    where rep = replaceSubExp subExp repl
          repInStmt st = case st of
                Generator src pat ex1 -> Generator src pat (rep ex1) -- TODO check pat
                Qualifier ex1         -> Qualifier         (rep ex1)
                LetStmt bns           -> LetStmt bns -- TODO bns
                RecStmt stm           -> RecStmt $ map repInStmt stm


etaVar :: Exp -> Maybe QName
etaVar e = case e of
    Var n -> Just n
    InfixApp e1 _ e2 -> etaVar e2 >>= \eta -> if isBound eta e1 then Nothing else Just eta
    App e1 e2        -> etaVar e2 >>= \eta -> if isBound eta e1 then Nothing else Just eta
    __  -> Nothing

removeEtaVar :: Exp -> Exp
removeEtaVar (App e1 (Var _)) = e1
removeEtaVar (App e1 e2)      = App e1 $ removeEtaVar e2
removeEtaVar (InfixApp e1 op (Var _)) | op == (QVarOp $ unQualS "$") = e1
                                      | otherwise = qopToExp op @@ e1
removeEtaVar (InfixApp e1 op e2) | op == (QVarOp $ unQualS "$") = e1 ... removeEtaVar e2
                                 | otherwise = qopToExp op @@ e1 ... removeEtaVar e2
removeEtaVar _ = undefined

etaReduce :: [Pat] -> Exp -> ([Pat], Exp)
etaReduce = specialeEtaReduce (\_ _ -> Nothing)

specialeEtaReduce :: (Name -> Pat -> Maybe (Exp -> Exp)) -> [Pat] -> Exp -> ([Pat], Exp)
specialeEtaReduce _ [] ex = ([], ex)
specialeEtaReduce modifier pats ex =
    let (p:ps) = reverse pats -- splitAt (length pats -1) pats
        pats' = reverse ps
    in case etaVar ex of
        Nothing  -> (pats, ex)
        Just (UnQual name) -> maybe (pats, ex)
                                    (\modFunc -> (pats', modFunc $ removeEtaVar ex)) $ matchesPat name p
        Just (Qual _ _) -> (pats, ex)
        Just (Special _) -> (pats, ex)
    where matchesPat ename (PParen p)   = matchesPat ename p
          matchesPat ename (PVar pname) = if ename == pname then Just id else Nothing
          matchesPat ename  pat         = modifier ename pat

qopToExp :: QOp -> Exp
qopToExp (QConOp qname) = Con qname
qopToExp (QVarOp qname) = Var qname


isBound :: QName -> Exp -> Bool
isBound name e = case e of
    Var n -> n == name
    InfixApp e1 _ e2 -> any (isBound name) [e1, e2] -- TODO add checks for the op
    App e1 e2        -> any (isBound name) [e1, e2]
    If e1 e2 e3      -> any (isBound name) [e1, e2, e3]
    Con n            -> n == name
    NegApp e1        -> isBound name e1
    Tuple exs        -> any (isBound name) exs
    _                -> True -- TODO further expand


