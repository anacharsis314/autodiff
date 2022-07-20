-- tipo que representa um numero dual
-- D x x' onde x é o valor da função em x e x' é sua derivada
data D a = D a a deriving (Eq)

-- Instancia de Show para D, serve para imprimir o valor de um jeito ok
instance (Show a,Num a) => Show (D a) where
	show (D x y) = "x = " ++ show x ++ "; dx = " ++ show b

-- constante, possui um valor mas derivada é zero
constD :: Num a => a -> D a
constD x = D x 0

-- identidade, possui valor e derivada unitaria
idD    :: Num a => a -> D a
idD x    = D x 1

-- é preciso definir instancias de Num e Fractional para poder fazer uma instancia de Floating
-- ao definir Floating, vai sair quase toda tabela de derivadas "de graça"
-- Instancia Num de D x x', define operações basicas + * negate signum fromInteger
instance Num a => Num(D a)  where
        (D x dx) + (D b b')  = D (x + b) (dx + b')
	(D x dx) * (D b b')  = D (x * b) (dx * b + b' * x)
        negate (D x dx)      = D (negate x) (negate dx)
	signum (D x _ )      = D (signum x) 0
	abs (D x dx)         = D (abs x) (dx * signum x) 
	fromInteger x        = constD (fromInteger x)

-- Instancia Fractional D x x', define fromRational e /
instance Fractional a  => Fractional(D a) where
	fromRational x =  constD (fromRational x)
	-- regra reciproca, (1/f(x))' = f(x)'/f(x)^2
	recip (D x dx) =  D (recip x) ((-dx)/(x * x))

-- Instancia Floating D x x', define varias funcoes
instance Floating a => Floating (D a) where
	pi             = constD (pi)
	exp   (D x dx) = D (exp x)   ( dx * exp x )
	log   (D x dx) = D (log x)   ( dx / x)
	sin   (D x dx) = D (sin x)   ( dx * (cos x))
	cos   (D x dx) = D (cos x)   ( dx * (-1) * sin x)
	asin  (D x dx) = D (asin x)  ( dx / sqrt(1 - x*x)) 
	acos  (D x dx) = D (acos x)  ( dx / (-sqrt(1 - x*x)))
	atan  (D x dx) = D (atan x)  ( dx / (1 + x * x)) 
	sinh  (D x dx) = D (sinh x)  ( dx * cosh x)
	cosh  (D x dx) = D (cosh x)  ( dx * sinh x)
	asinh (D x dx) = D (asinh x) ( dx / (sqrt (dx * dx + 1))) 
	acosh (D x dx) = D (acosh x) (-dx / (sqrt (dx * dx - 1)))
	atanh (D x dx) = D (atanh x) ( dx / (1 - dx * dx)) 
