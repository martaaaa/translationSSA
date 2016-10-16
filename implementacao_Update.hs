exemplo :: PROG
exemplo = [("bullet", (SEQ "i" (CONST 1) (SEQ "j" (CONST 1) (SEQ "k" (CONST 0) (GOTO "b"))))), ("b", BRANCH (LE (VAR "k") (CONST 100)) ("c") ("d")), ("c", (SEQ "j" (VAR "i") (SEQ "k" (SOMA (VAR "k") (CONST 1) ) (GOTO "e")))), ("d", (SEQ "j" (VAR "k") (SEQ "k" (SOMA (VAR "k") (CONST 2) ) (GOTO "e")))), ("e", RET (VAR "k"))]

fact :: PROG
fact = [("bullet", (SEQ "x" (CONST 5) (SEQ "f" (CONST 1) (SEQ "c" (CONST 1) (GOTO "l"))))), ("l", BRANCH (LE (VAR "c") (VAR "x")) ("l'") ("l''")), ("l'", (SEQ "f" (MULT (VAR "f") (VAR "c")) (SEQ "c" (SOMA (VAR "c") (CONST 1) ) (GOTO "l")))), ("l''", RET (VAR "f"))]

op2 :: OP
op2 = [("bullet",[]),("l",["c","f"]),("l'",[]),("l''",[])]

suma :: PROG
suma = [("bullet", (SEQ "n" (CONST 10) (SEQ "i" (CONST 0) (SEQ "s" (CONST 0) (GOTO "l"))))), ("l", BRANCH (LE (VAR "i") (VAR "n")) ("l'") ("l''")), ("l'", (SEQ "s" (SOMA (VAR "s") (VAR "i")) (SEQ "i" (SOMA (VAR "i") (CONST 1) ) (GOTO "l")))), ("l''", RET (VAR "s"))]

op :: OP
op = [("bullet",[]),("l",["i","s"]),("l'",[]),("l''",[])]
{-
"a":   
    i:=1; 
    j:=1; 
    k:=0; 
    goto b;
"b":    
    branch k<=100 c d
"c":
    j:=i; 
    k:=k+1; 
    goto e;
"d" :
    j:=k; 
    k:=k+2; 
    goto e;
"e":     // k = kc, kd 
    return k;

-}
type V = String {- V = variaveis -} 
type L = String {- L = Labels -}    
type N = Int {-N = número natural -}    
type C = Int {-C = constante -} 

type VS = [(V,N)] -- versões!
--type ES = [(L,[VS])]
type LS = [(L,(N,[VS],[Vsa]))]
type Ctx = (VS,LS)

type Vsa = (V,N) -- variaveis em SA 
type Lsa = (L,N)

data EXP = VAR V | CONST C | SOMA EXP EXP | MULT EXP EXP | EQU EXP EXP | GE EXP EXP | LE EXP EXP 
instance Show EXP where  
    show (VAR v) = v 
    show (CONST c) = show c 
    show (SOMA exp exp1) = (show exp) ++ "+" ++ (show exp1) 
    show (MULT exp exp1) = (show exp) ++ "*" ++ (show exp1) 
    show (EQU exp exp1) = (show exp) ++ "==" ++ (show exp1) 
    show (GE exp exp1) = (show exp) ++ "=>" ++ (show exp1) 
    show (LE exp exp1) = (show exp) ++ "=<" ++ (show exp1) 
    

data EXPsa = VARsa Vsa | CONSTsa C | SOMAsa EXPsa EXPsa | MULTsa EXPsa EXPsa | EQUsa EXPsa EXPsa | GEsa EXPsa EXPsa | LEsa EXPsa EXPsa 
instance Show EXPsa where  
    show (VARsa v) = "(" ++ fst v ++ "," ++ (show (snd v)) ++ ")"  
    show (CONSTsa c) = show c 
    show (SOMAsa exp exp1) = (show exp) ++ "+" ++ (show exp1)
    show (MULTsa exp exp1) = (show exp) ++ "*" ++ (show exp1) 
    show (EQUsa exp exp1) = (show exp) ++ "==" ++ (show exp1) 
    show (GEsa exp exp1) = (show exp) ++ "=>" ++ (show exp1) 
    show (LEsa exp exp1) = (show exp) ++ "=<" ++ (show exp1)  

data B = SEQ V EXP B | RET EXP | GOTO L | BRANCH EXP L L 
instance Show B where  
    show (SEQ v exp b) = "     " ++ v ++ ":=" ++ (show exp) ++ "; \n" ++ (show b) 
    show (RET exp) = "     return " ++ (show exp) ++ "\n" 
    show (GOTO l) = "     goto " ++ l  ++ "\n"
    show (BRANCH exp l1 l2) = "     branch " ++ (show exp) ++ "  " ++ l1 ++ "  " ++ l2 ++ "\n"
    

data Bsa = SEQsa Vsa EXPsa Bsa | RETsa EXPsa | GOTOsa Lsa | BRANCHsa EXPsa Lsa Lsa
instance Show Bsa where 
    show (SEQsa v exp b) = "     " ++ (show v) ++ ":=" ++ (show exp) ++ "; \n" ++ (show b)
    show (RETsa exp) = "     return " ++ (show exp)  ++ "\n"
    show (GOTOsa l) = "     goto " ++ (show l) ++ "\n"
    show (BRANCHsa exp l1 l2) = "     branch " ++ (show exp) ++ (show l1) ++ (show l2) ++ "\n"

-- ERa C --
type PROG = [(L,B)]
type PROGI = [(L,Bsa)]
type PROGsa = [(L,PHI,Bsa)]
type PHI = [(Vsa,[EXPsa])]
type OP = [(L,[V])] 

-- Função union : junta duas listas sem repetição de elementos --   
union :: (Ord a) => [a] -> [a] -> [a]
union [] [] =[]
union [] (x:xs) =(x:xs)
union (x:xs) [] =(x:xs)
union (x:xs)(y:ys)
                | x<y = x:union xs (y:ys)
                | x==y = x: union xs ys
                | otherwise = y: union (x:xs) ys

vars :: PROG -> [V]
vars [] = []
vars ((l,b):t) = union (varsB b) (vars t)

varsB :: B -> [V]
varsB (SEQ v exp b1)= union (union [v] (varsB b1)) (varsE exp)
varsB (RET exp) = varsE(exp)
varsB (GOTO l) = []
varsB (BRANCH exp l1 l2) = varsE(exp)


varsE :: EXP -> [V]
varsE (CONST c) = []
varsE (VAR v) = [v]
varsE (SOMA exp exp1) = union (varsE exp) (varsE exp1)
varsE (MULT exp exp1) = union (varsE exp) (varsE exp1)
varsE (EQU exp exp1) = union (varsE exp) (varsE exp1)
varsE (GE exp exp1) = union (varsE exp) (varsE exp1)
varsE (LE exp exp1) = union (varsE exp) (varsE exp1)

labels :: PROG -> [L]
labels  ((l,b):t) = union [l] (labels t)
labels [] = []

vsInit :: PROG -> VS
vsInit p = [(x,0) | x<-(vars p)]

lsInit :: PROG -> LS
lsInit p = [(l,(0,[],vs0)) | l<-(labels p)]
    where vs0 = vsInit p 

initC :: PROG -> Ctx 
initC p = (vs0,ls0)
    where
        vs0 = vsInit p
        ls0 = lsInit p 

inc :: VS -> VS
inc [] = []
inc vs = [(x,n+1) | (x,n)<-vs]

-- renomeia as expressões -- 
hatE :: VS -> EXP -> EXPsa
hatE vs (CONST c) = CONSTsa c
hatE vs (VAR x) = VARsa (hatV vs x)
hatE vs (SOMA exp exp1) = let 
                        y = hatE vs exp
                        m = hatE vs exp1
                        in SOMAsa y m 
hatE vs (MULT exp exp1) = let 
                        y = hatE vs exp
                        m = hatE vs exp1
                        in MULTsa y m 
hatE vs (EQU exp exp1) = let
                        y = hatE vs exp
                        m = hatE vs exp1
                        in EQUsa y m 
hatE vs (GE exp exp1) = let
                        y = hatE vs exp
                        m = hatE vs exp1
                        in GEsa y m 
hatE vs (LE exp exp1) = let
                        y = hatE vs exp
                        m = hatE vs exp1
                        in LEsa y m 

-- renomeia as variaveis -- 
hatV :: VS -> V -> Vsa
hatV [] x = (x,-1)
hatV ((y,n):t) x = if x==y 
                    then (x,n)
                    else hatV t x

consultaV :: VS -> V -> Int
consultaV ((h,x):t) v = if h==v
                            then x
                            else consultaV t v

consultaL :: LS -> L -> (Int,[VS],VS)
consultaL ((h,x):t) v = if h==v
                            then x
                            else consultaL t v
consultaL [] _ = (0,[],[]) 

nextV :: VS -> V -> VS 
nextV ((y,n):t) x = if x==y
                    then (y,n+1):t
                    else (y,n):nextV t x

updateL :: LS -> L -> (Int,[VS],VS) -> LS
updateL ((l,(n1,d,w)):t) l1 (n,lvs,vs) = if l1==l 
                                        then (l,(n,lvs,vs)):t
                                        else (l,(n1,d,w)):updateL t l1 (n,lvs,vs)
updateL [] _ _ = []

rename :: PROG -> OP -> (PROGI, Ctx)
rename p o = tl p o (initC p) 

sobre :: VS -> VS -> VS
sobre ((x,y):t) ((x1,y1):t1) = if (x==x1) 
                                then (x,y1):(sobre t t1)
                                else (x,y):(sobre t t1)
sobre vs [] = vs
sobre [] vs = []

consultaOP :: OP -> L -> [V]
consultaOP ((l,v):t) l1 = if (l==l1)
                        then v
                        else consultaOP t l1 

incVersion :: VS -> [V] -> VS
incVersion vs@((x,n):t) (x1:t1) = if (x==x1)
                            then (x,n+1):incVersion vs t1
                            else incVersion vs t1
incVersion vs [] = []
incVersion [] v = [] 

tl :: PROG -> OP -> Ctx -> (PROGI, Ctx)
tl ((l,b):t) o (vs,ls) = (((l,b'):t'), c'') 
                        where 
                            (n,d,_) = consultaL ls l
                            vs1 = sobre vs (incVersion vs (consultaOP o l))
                            lv = updateL ls l (n,d,(map (hatV (inc vs)) (consultaOP o l)))
                            (b',c') = tb (b,l) (vs1,lv)
                            (t',c'') = tl t o c'
tl [] o c = ([], c)

tb :: (B,L) -> Ctx -> (Bsa,Ctx)
tb ((SEQ v exp b1), l) (vs,ls) = (SEQsa (v,(consultaV vs v)+1) (hatE vs exp) b', c') 
                            where
                                c = (nextV vs v, ls)
                                (b',c') = tb (b1,l) c 
tb ((GOTO a), l) (vs,ls) = (GOTOsa (a, n+1), c')
                            where
                                (n,d,w) = consultaL ls a 
                                ls' = updateL ls a (n+1, d++[vs],w) 
                                c' = (vs, ls')
tb ((RET exp), l) c@(vs,ls) = (RETsa (hatE vs exp), c)
tb ((BRANCH exp a b), l) (vs,ls) = (BRANCHsa (hatE vs exp) (a, n+1) (b, n'+1), c)
                            where
                                (n,d,w) = consultaL ls a 
                                l' = updateL ls a (n+1, d++[vs], w)
                                (n',d',w') = consultaL l' b
                                l'' = updateL l' b (n'+1, d++[vs], w')
                                c = (vs,l'')

dom :: VS -> [V]
dom ((h,n):t) = h : dom t 
dom [] = []

sync :: L -> Ctx -> PHI
sync l (vs,ls) = [ ((x,consultaV w x) , [VARsa (x, consultaV vs0 x) | vs0 <- d] ) | x <- dom w ]
                where 
                    (_,d,w) = consultaL ls l 

sl :: (PROGI,Ctx) -> [(L,PHI,Bsa)]
sl ([],c)  = [] 
sl (("bullet",b):t, c) = ("bullet",[],b): sl(t,c)
sl (((l,b):t),c) = (l, sync l c, b): sl(t,c)

final :: (PROG,OP) -> PROGsa
final (p,o) = sl (rename p o)

intremediop :: PROG -> OP -> (PROGI,Ctx)
intremediop p o = rename p o 

auximprimePROG :: PROG -> String
auximprimePROG ((l,b):t) = (show l) ++ ":" ++ "     \n" ++ (show b) ++ "\n" ++ auximprimePROG t
auximprimePROG [] = ""

-- Para imprimir direitinho o PROG -- 
imprimePROG :: PROG -> IO ()
imprimePROG p = putStr (auximprimePROG p) 

-- Para imprimir direitinho o PROGSA -- 
imprimeListaEXPsa :: [EXPsa] -> String
imprimeListaEXPsa (h:[]) = show h
imprimeListaEXPsa (h:t) = show h ++ "," ++ imprimeListaEXPsa t  
imprimeListaEXPsa [] = ""


imprimeIntermedio :: (PROGI, Ctx) -> String
imprimeIntermedio ([],_) = ""
imprimeIntermedio (((l,b):[]),c) = (show l) ++ ":" ++ "     \n" ++ (show b) ++ "\nFinal Context:" ++ show c ++ "\n"
imprimeIntermedio (((l,b):t),c) = (show l) ++ ":" ++ "     \n" ++ (show b) ++ "\n" ++ imprimeIntermedio (t, c)


intermedio_total :: PROG -> OP -> IO()
intermedio_total p o = putStr (imprimeIntermedio(rename p o ))

imprimePHI :: PHI -> String
imprimePHI ((v,e):t) = "     "++(show v) ++ ":= phi(" ++ imprimeListaEXPsa e ++ ")\n"++imprimePHI t
imprimePHI [] = ""

auximprimePROGSA :: PROGsa -> String
auximprimePROGSA ((l,s,b):t) = (show l) ++ ":" ++ "     \n" ++ (imprimePHI s) ++ (show b) ++ auximprimePROGSA t
auximprimePROGSA [] = ""

-- Para imprimir direitinho o PROGsa -- 
imprimePROGSA :: PROGsa -> IO ()
imprimePROGSA p = putStr (auximprimePROGSA p) 

-- Dado um PROG imprime direitinho o PROGsa-- 
traduction :: PROG -> OP -> IO ()
traduction p o = imprimePROGSA (final (p,o))

