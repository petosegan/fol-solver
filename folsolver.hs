
type Function = String
type Constant = String
type Variable = String

data Term = FuncTerm Function Term | ConstTerm Constant | VaryTerm Variable deriving (Eq)

instance Show Term where
	show (FuncTerm ff tt) = ff ++ "(" ++ show tt ++ ")"
	show (ConstTerm cc) = cc
	show (VaryTerm vv) = vv

type Predicate = String

data Atom = Predication Predicate Term | Equality Term Term |AtomBool Bool deriving (Eq)

instance Show Atom where
	show (Predication pp tt) = pp ++ "(" ++ show tt ++ ")"
	show (Equality t1 t2) = show t1 ++ " = " ++ show t2
	show (AtomBool bb) = show bb

data Quantifier = ForAll | ThereExists deriving (Eq)

instance Show Quantifier where
	show ForAll = "FORALL "
	show ThereExists = "EXISTS "

data Sentence = AtomicSentence Atom | NotSentence Sentence | AndSentence Sentence Sentence | OrSentence Sentence Sentence | ImplySentence Sentence Sentence | IffSentence Sentence Sentence | QuantSentence Quantifier Variable Sentence deriving (Eq)

instance Show Sentence where
	show = renderSentence

renderSentence :: Sentence -> String
renderSentence (AtomicSentence sym) = show sym
renderSentence (NotSentence s1) = concat ["NOT ", renderSentence s1]
renderSentence (AndSentence s1 s2) = concat ["(", renderSentence s1, " AND ", renderSentence s2, ")"]
renderSentence (OrSentence s1 s2) = concat ["(",renderSentence s1, " OR ", renderSentence s2, ")"]
renderSentence (ImplySentence s1 s2) = concat ["(",renderSentence s1, " => ", renderSentence s2, ")"]
renderSentence (IffSentence s1 s2) = concat ["(",renderSentence s1, " <=> ", renderSentence s2, ")"]
renderSentence (QuantSentence qq vv ss) = "(" ++ show qq ++ vv ++ " {" ++ renderSentence ss ++ "})"

defSentenceTfm :: (Sentence -> Sentence) -> Sentence -> Sentence
defSentenceTfm tfm (NotSentence ss) = NotSentence (tfm ss)
defSentenceTfm tfm (AndSentence s1 s2) = AndSentence (tfm s1) (tfm s2)
defSentenceTfm tfm (OrSentence s1 s2) = OrSentence (tfm s1) (tfm s2)
defSentenceTfm tfm (ImplySentence s1 s2) = ImplySentence (tfm s1) (tfm s2)
defSentenceTfm tfm (IffSentence s1 s2) = IffSentence (tfm s1) (tfm s2)
defSentenceTfm tfm (QuantSentence qq vv ss) = QuantSentence qq vv (tfm ss)

type Substitution = (Variable, Term)

type BindingList = [Substitution]

substitute :: BindingList -> Sentence -> Sentence
substitute bl (AtomicSentence aa) = AtomicSentence (substAtom bl aa)
substitute bl (QuantSentence qq vv ss) = undefined
substitute bl ss= defSentenceTfm (substitute bl) ss

substAtom :: BindingList -> Atom -> Atom
substAtom bl (Predication pp tt) = Predication pp (substTerm bl tt)
substAtom bl (Equality t1 t2) = Equality (substTerm bl t1) (substTerm bl t2)
substAtom bl (AtomBool bb) = AtomBool bb

substTerm :: BindingList -> Term -> Term
substTerm bl (FuncTerm ff tt) = FuncTerm ff (substTerm bl tt)
substTerm bl (ConstTerm cc) = ConstTerm cc
substTerm bl (VaryTerm vv) = substVary bl vv

substVary :: BindingList -> Variable -> Term
substVary = undefined
-- test cases

test_sentence = QuantSentence ForAll v1 s1

v1 = "m"
v1T = VaryTerm v1
s1 = (ImplySentence isMonkeyS likesBananasS)
isMonkeyS = AtomicSentence isMonkeyA
isMonkeyA = Predication "IsMonkey" v1T

likesBananasS = AtomicSentence likesBananasA
likesBananasA = Predication "LikesBananas" v1T

test_sentence2 = QuantSentence ThereExists v1 isMonkeyS
