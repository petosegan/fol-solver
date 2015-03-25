
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

