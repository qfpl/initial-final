The expression problem with Backpack

# The problem

# The final encoding with finally tagless

# The final encoding with Backpack

# The initial encoding with classy `Prism`s

# The initial encoding with Backpack

# Looking at the core

## Vanilla

lit1 :: Int
lit1 = I# 2#

lit2 :: Term
lit2 = Lit lit1

evalAddSmall :: Term -> Term
evalAddSmall = \ (tm :: Term) -> evalTerm (Add tm tm)

evalAddBig4 :: Int
evalAddBig4 = I# 3#

evalAddBig3 :: Term
evalAddBig3 = Lit evalAddBig4

evalAddBig2 :: Int
evalAddBig2 = I# 5#

evalAddBig1 :: Term
evalAddBig1 = Lit evalAddBig2

evalAddMulSmall :: Term -> Term
evalAddMulSmall
  = \ (tm :: Term) -> evalTerm (Add (Mul tm evalAddBig3) evalAddBig1)

evalAddBig :: Term -> Term
evalAddBig
  = \ (tm :: Term) ->
      evalTerm (Add (Add tm evalAddBig3) (Add tm evalAddBig1))

evalAddMulBig4 :: Int
evalAddMulBig4 = I# 7#

evalAddMulBig3 :: Term
evalAddMulBig3 = Lit evalAddMulBig4

evalAddMulBig2 :: Int
evalAddMulBig2 = I# 11#

evalAddMulBig1 :: Term
evalAddMulBig1 = Lit evalAddMulBig2

evalAddMulBig :: Term -> Term
evalAddMulBig
  = \ (tm :: Term) ->
      evalTerm
        (Add
           (Mul (Add tm evalAddBig3) (Add tm evalAddBig1))
           (Mul (Add tm evalAddMulBig3) (Add tm evalAddMulBig1)))
## Final

lit1 :: Int
lit1 = I# 2#

lit2 :: Eval
lit2 = lit1 `cast` <Co:2>

evalAddSmall1 :: Eval -> Int
evalAddSmall1
  = \ (tm :: Eval) ->
      $fNumInt_$c+ (tm `cast` <Co:12>) (tm `cast` <Co:13>)

evalAddSmall :: Eval -> Eval
evalAddSmall = evalAddSmall1 `cast` <Co:18>

evalAddMulSmall1 :: Eval -> Int
evalAddMulSmall1
  = \ (tm :: Eval) ->
      case tm `cast` <Co:12> of { I# x -> I# (+# (*# x 3#) 5#) }

evalAddMulSmall :: Eval -> Eval
evalAddMulSmall = evalAddMulSmall1 `cast` <Co:18>

evalAddBig1 :: Eval -> Int
evalAddBig1
  = \ (tm :: Eval) ->
      case tm `cast` <Co:12> of { I# x -> I# (+# (+# x 3#) (+# x 5#)) }

evalAddBig :: Eval -> Eval
evalAddBig = evalAddBig1 `cast` <Co:18>

evalAddMulBig1 :: Eval -> Int
evalAddMulBig1
  = \ (tm :: Eval) ->
      case tm `cast` <Co:12> of { I# x ->
      I# (+# (*# (+# x 3#) (+# x 5#)) (*# (+# x 7#) (+# x 11#)))
      }

evalAddMulBig :: Eval -> Eval
evalAddMulBig = evalAddMulBig1 `cast` <Co:18>

## Final with Backpack

lit2 :: Repr
lit2 = I# 2#

evalAddSmall :: Repr -> Repr
evalAddSmall = \ (tm :: Repr) -> $fNumInt_$c+ tm tm

evalAddMulSmall :: Repr -> Repr
evalAddMulSmall
  = \ (tm :: Repr) -> case tm of { I# x -> I# (+# (*# x 3#) 5#) }

evalAddBig :: Repr -> Repr
evalAddBig
  = \ (tm :: Repr) ->
      case tm of { I# x -> I# (+# (+# x 3#) (+# x 5#)) }

evalAddMulBig :: Repr -> Repr
evalAddMulBig
  = \ (tm :: Repr) ->
      case tm of { I# x ->
      I# (+# (*# (+# x 3#) (+# x 5#)) (*# (+# x 7#) (+# x 11#)))
      }

## Initial

## Initial with Backpack
