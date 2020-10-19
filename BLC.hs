module BLC (
    LambdaVar,
    LambdaBodyElement,
    LambdaBody,
    LambdaFunc,
) where

import qualified Data.Set as Set
import qualified Data.List as List

data LambdaVar = LambdaVar String
data LambdaBodyElement = LambdaBodyVar LambdaVar | LambdaBodyFunc LambdaFunc | LambdaBodyNested LambdaBody
data LambdaBody = LambdaBody [LambdaBodyElement]
data LambdaFunc = LambdaFunc LambdaVar LambdaBody

-- expression for variable substitution

subse :: LambdaBodyElement -> LambdaVar -> LambdaBodyElement -> LambdaBodyElement
subse e v1 (LambdaBodyVar v2) = if (v1 == v2) then e else (LambdaBodyVar v2)
subse e v1 (LambdaBodyFunc (LambdaFunc v2 (LambdaBody b2))) = LambdaBodyFunc (LambdaFunc v2 (LambdaBody (map (subse e v1) b2)))
subse e v1 (LambdaBodyNested (LambdaBody es2)) =  LambdaBodyNested (LambdaBody (map (subse e v1) es2))


-- variable for variable substitution

subsVarv :: LambdaVar -> LambdaVar -> LambdaVar -> LambdaVar
subsVarv vOld vNew v = if (v == vOld) then vNew else v

subsVare :: LambdaVar -> LambdaVar -> LambdaBodyElement -> LambdaBodyElement
subsVare vOld vNew (LambdaBodyVar v) = LambdaBodyVar (subsVarv vOld vNew v)
subsVare vOld vNew (LambdaBodyFunc f) = LambdaBodyFunc (subsVarf vOld vNew f)
subsVare vOld vNew (LambdaBodyNested b) = LambdaBodyNested (subsVarb vOld vNew b)

subsVarb :: LambdaVar -> LambdaVar -> LambdaBody -> LambdaBody
subsVarb vOld vNew (LambdaBody es) = LambdaBody (map (subsVare vOld vNew) es)

subsVarf :: LambdaVar -> LambdaVar -> LambdaFunc -> LambdaFunc
subsVarf vOld vNew (LambdaFunc v b) = LambdaFunc (subsVarv vOld vNew v) (subsVarb vOld vNew b)


-- set of variables used in an expression

varsv :: LambdaVar -> Set.Set LambdaVar
varsv v = Set.singleton v

varse :: LambdaBodyElement -> Set.Set LambdaVar
varse (LambdaBodyVar v) = varsv v
varse (LambdaBodyFunc f) = varsf f
varse (LambdaBodyNested b) = varsb b

varsb :: LambdaBody -> Set.Set LambdaVar
varsb (LambdaBody es)  = foldl Set.union Set.empty ((map varse) es)

varsf :: LambdaFunc -> Set.Set LambdaVar
varsf (LambdaFunc v b) = Set.union (Set.singleton v) (varsb b)


-- set of bound variables used in an expression

boundVarsv :: LambdaVar -> Set.Set LambdaVar
boundVarsv v = Set.empty

boundVarse :: LambdaBodyElement -> Set.Set LambdaVar
boundVarse (LambdaBodyVar v) = boundVarsv v
boundVarse (LambdaBodyFunc f) = boundVarsf f
boundVarse (LambdaBodyNested b) = boundVarsb b

boundVarsb :: LambdaBody -> Set.Set LambdaVar
boundVarsb (LambdaBody es)  = foldl Set.union Set.empty ((map boundVarse) es)

boundVarsf :: LambdaFunc -> Set.Set LambdaVar
boundVarsf (LambdaFunc v b) = Set.union (Set.singleton v) (boundVarsb b)


-- infinite supply of replacement of variables

replacementVars :: [LambdaVar]
replacementVars = [LambdaVar ("x" ++ (show i)) | i <- [1..]]


-- find new varibles which are not allready in use

findReplacementVars :: Set.Set LambdaVar -> Int -> [LambdaVar]
findReplacementVars varsInUse n = f replacementVars []
    where
        f (rv:rvs) vsNew = if ((length vsNew) == n)
                           then vsNew
                           else (
                               if (Set.member rv varsInUse)
                               then (f rvs vsNew)
                               else (f rvs (rv:vsNew))
                           )


-- replace variables in a lambda body element to avoid overlap with variables allready in use

replaceVarse :: Set.Set LambdaVar -> LambdaBodyElement -> LambdaBodyElement
replaceVarse varsInUse e = (applyAll ops) e
    where
        boundVarsOld :: Set.Set LambdaVar
        boundVarsOld = Set.filter (\v -> Set.member v varsInUse) (boundVarse e)
        boundVarsNew :: [LambdaVar]
        boundVarsNew = findReplacementVars varsInUse (length boundVarsOld)
        h (x,y) = subsVare x y
        ops :: [LambdaBodyElement -> LambdaBodyElement]
        ops = map h (zip (Set.toList boundVarsOld) boundVarsNew)
        applyAll :: [LambdaBodyElement -> LambdaBodyElement] -> (LambdaBodyElement -> LambdaBodyElement)
        applyAll fs = foldl (.) id fs


-- apply function to lambda body element

applye :: LambdaFunc -> LambdaBodyElement -> LambdaBody
applye (LambdaFunc v1 (LambdaBody b1)) e = LambdaBody ((map (subse eNew v1)) b1)
    where
        eNew = replaceVarse (varsf (LambdaFunc v1 (LambdaBody b1))) e


-- evaluation

evalb :: LambdaBody -> LambdaBody
evalb (LambdaBody []) = LambdaBody []
evalb (LambdaBody (LambdaBodyFunc f1:e2:es)) = LambdaBody (LambdaBodyNested (applye f1 e2) : es)
evalb (LambdaBody (LambdaBodyNested b:es))
    | ((len bNew) == 0) = LambdaBody es                                -- remove empty body
    | ((len bNew) == 1) = LambdaBody ((fst bNew) : es)                 -- resolve nesting
    | (True)            = LambdaBody ((LambdaBodyNested bNew) : es)    -- keep nesting
        where
            bNew = evalb b
            len (LambdaBody es) = length es
            fst (LambdaBody (e:es)) = e
evalb (LambdaBody es) = LambdaBody es

-- instances

instance Eq LambdaVar where
    LambdaVar v1 == LambdaVar v2 = v1 == v2

instance Ord LambdaVar where
    compare (LambdaVar v1) (LambdaVar v2) = compare v1 v2

instance Show LambdaVar where
    show (LambdaVar x) = x

instance Show LambdaBodyElement where
    show (LambdaBodyVar v) = show v
    show (LambdaBodyFunc f) = "(" ++ (show f) ++ ")"
    show (LambdaBodyNested b) = "(" ++(show b) ++ ")"

instance Show LambdaBody where
    show (LambdaBody []) = ""
    show (LambdaBody es) = List.intercalate " " (map show es)

instance Show LambdaFunc where
    show (LambdaFunc var body) = "\\" ++ (show var) ++ ". " ++ (show body)


-- examples

a = LambdaVar "a"
b = LambdaVar "b"
c = LambdaVar "c"
d = LambdaVar "d"
x = LambdaVar "x"
y = LambdaVar "y"
z = LambdaVar "z"
fid = LambdaFunc x (LambdaBody [LambdaBodyVar x])
f1 = LambdaFunc x (LambdaBody [LambdaBodyVar x])
f2 = LambdaFunc x (LambdaBody [LambdaBodyNested (LambdaBody [LambdaBodyVar x])])
f3 = LambdaFunc x (LambdaBody [LambdaBodyVar x, LambdaBodyVar y])
f4 = LambdaFunc x (LambdaBody [LambdaBodyVar x, LambdaBodyVar x])
f5 = LambdaFunc x (LambdaBody [LambdaBodyVar x, LambdaBodyVar x, LambdaBodyVar x])
b1 = (LambdaBody [LambdaBodyFunc f1, LambdaBodyFunc f1, LambdaBodyFunc f1])
b2 = (LambdaBody [LambdaBodyFunc f4, LambdaBodyFunc f4, LambdaBodyFunc f4])
b3 = (LambdaBody [LambdaBodyFunc f5, LambdaBodyFunc f5, LambdaBodyFunc f5])

fTrue = LambdaFunc x (LambdaBody [LambdaBodyFunc (LambdaFunc y (LambdaBody [LambdaBodyVar x, LambdaBodyVar y]))])
fFalse = LambdaFunc x (LambdaBody [LambdaBodyFunc (LambdaFunc y (LambdaBody [LambdaBodyVar y, LambdaBodyVar x]))])
fNot = fFalse
fIf = LambdaFunc x (LambdaBody [LambdaBodyFunc (
      LambdaFunc y (LambdaBody [LambdaBodyFunc (
      LambdaFunc z (LambdaBody [LambdaBodyVar x, LambdaBodyVar y, LambdaBodyVar z]))]))])

bIfTrue = LambdaBody [LambdaBodyFunc fIf, LambdaBodyFunc fTrue]
bIfTrueTrueFalse = LambdaBody [LambdaBodyFunc fIf, LambdaBodyFunc fTrue, LambdaBodyFunc fTrue, LambdaBodyFunc fFalse]

