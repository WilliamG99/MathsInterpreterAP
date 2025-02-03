// interpreter.fs
namespace MathsInterpreterBackend

module Interpreter =

    let piValue = System.Math.PI

    type Rational = {Numerator: int; Denominator: int}
    type Complex = {Real: int; Imaginary: int}
    type ComplexRational = {RealRational: Rational; ImaginaryRational: Rational}
    
    type Range = {LowerBound : int; UpperBound: int}

    type Number =
    | Int of int
    | Float of float
    | Rational of Rational
    | Complex of Complex
    | ComplexRational of ComplexRational
    | Range of Range


    let rec gcf a b =
        if b = 0 then a else gcf b (a % b)

    let simplifyRational rational =
        let commonFactor = gcf rational.Numerator rational.Denominator
        { Numerator = rational.Numerator / commonFactor; Denominator = rational.Denominator / commonFactor }

    let addRationals r1 r2 =
        let numerator = (r1.Numerator * r2.Denominator) + (r2.Numerator * r1.Denominator)
        let denominator = (r1.Denominator * r2.Denominator)
        simplifyRational{ Numerator = numerator; Denominator = denominator }

    let subtractRationals r1 r2 =
        let numerator = (r1.Numerator * r2.Denominator) - (r2.Numerator * r1.Denominator)
        let denominator = (r1.Denominator * r2.Denominator)
        simplifyRational{ Numerator = numerator; Denominator = denominator }

    let multiplyRationals r1 r2 =
        let numerator = (r1.Numerator * r2.Numerator)
        let denominator = (r1.Denominator * r2.Denominator)
        simplifyRational{ Numerator = numerator; Denominator = denominator }

    let divideRationals r1 r2 =
        let numerator = (r1.Numerator * r2.Denominator)
        let denominator = (r1.Denominator * r2.Numerator)
        simplifyRational{ Numerator = numerator; Denominator = denominator }


    let addComplex c1 c2 =
        let real = (c1.Real + c2.Real)
        let imaginary = (c1.Imaginary + c2.Imaginary)
        { Real = real; Imaginary = imaginary }

    let subtractComplex c1 c2 =
        let real = (c1.Real - c2.Real)
        let imaginary = (c1.Imaginary - c2.Imaginary)
        { Real = real; Imaginary = imaginary }

    let multiplyComplex c1 c2 =
        let real = (c1.Real * c2.Real) - (c1.Imaginary * c2.Imaginary)
        let imaginary = (c1.Real * c2.Imaginary) + (c1.Imaginary * c2.Real)
        { Real = real; Imaginary = imaginary }

    let divideComplex c1 c2 = 
        let c2Conjugate:Complex = { Real = c2.Imaginary; Imaginary = -(c2.Imaginary) }

        let numerator = multiplyComplex c1 c2Conjugate
        let denominator = multiplyComplex c2 c2Conjugate

        { RealRational = { Numerator = numerator.Real; Denominator = denominator.Real} ; ImaginaryRational = { Numerator = numerator.Imaginary; Denominator = denominator.Real} }


    let addNumbers a b =
        match (a, b) with
        | (Int x, Int y) -> Int (x + y)
        | (Float x, Float y) -> Float (x + y)
        | (Int x, Float y) -> Float ((float x) + y)
        | (Float x, Int y) -> Float (x + (float y))

        | (Rational x, Rational y) -> Rational (addRationals x y)
        | (Rational x, Int y) -> Rational (addRationals x {Numerator = y; Denominator = 1})
        | (Int x, Rational y) -> Rational (addRationals {Numerator = x; Denominator = 1} y)

        | (Complex x, Complex y) -> Complex (addComplex x y)
        | (Complex x, Int y) -> Complex (addComplex x {Real = y; Imaginary = 0})
        | (Int x, Complex y) -> Complex (addComplex {Real = x; Imaginary = 0} y)

        // Handle invalid case: Float and Rational mixed
        | (Float _, Rational _) | (Rational _, Float _) -> 
            raise (System.Exception("Cannot use Float with a Rational number"))

        // Handle invalid case: Float and Compelex mixed
        | (Float _, Complex _) | (Complex _, Float _) -> 
            raise (System.Exception("Cannot use Float with a Complex number"))

    let subtractNumbers a b =
        match (a, b) with
        | (Int x, Int y) -> Int (x - y)
        | (Float x, Float y) -> Float (x - y)
        | (Int x, Float y) -> Float ((float x) - y)
        | (Float x, Int y) -> Float (x - (float y))

        | (Rational x, Rational y) -> Rational (subtractRationals x y)
        | (Rational x, Int y) -> Rational (subtractRationals x {Numerator = y; Denominator = 1})
        | (Int x, Rational y) -> Rational (subtractRationals {Numerator = x; Denominator = 1} y)

        | (Complex x, Complex y) -> Complex (subtractComplex x y)
        | (Complex x, Int y) -> Complex (subtractComplex x {Real = y; Imaginary = 0})
        | (Int x, Complex y) -> Complex (subtractComplex {Real = x; Imaginary = 0} y)
        //| (Complex x, Float y) -> Complex (addComplex x {Real = y; Imaginary = 0})
        //| (Float x, Complex y) -> Complex (addComplex {Real = x; Imaginary = 0} y)

        // Handle invalid case: Float and Rational mixed
        | (Float _, Rational _) | (Rational _, Float _) -> 
            raise (System.Exception("Cannot use Float with a Rational number"))

        // Handle invalid case: Float and Compelex mixed
        | (Float _, Complex _) | (Complex _, Float _) -> 
            raise (System.Exception("Cannot use Float with a Complex number"))

    let multiplyNumbers a b =
        match (a, b) with
        | (Int x, Int y) -> Int (x * y)
        | (Float x, Float y) -> Float (x * y)
        | (Int x, Float y) -> Float ((float x) * y)
        | (Float x, Int y) -> Float (x * (float y))

        | (Rational x, Rational y) -> Rational (multiplyRationals x y)
        | (Rational x, Int y) -> Rational (multiplyRationals x {Numerator = y; Denominator = 1})
        | (Int x, Rational y) -> Rational (multiplyRationals {Numerator = x; Denominator = 1} y)

        | (Complex x, Complex y) -> Complex (multiplyComplex x y)
        | (Complex x, Int y) -> Complex (multiplyComplex x {Real = y; Imaginary = 0})
        | (Int x, Complex y) -> Complex (multiplyComplex {Real = x; Imaginary = 0} y)

        // Handle invalid case: Float and Rational mixed
        | (Float _, Rational _) | (Rational _, Float _) -> 
            raise (System.Exception("Cannot use Float with a Rational number"))

    let divideNumbers a b =
        match (a, b) with
        | (Int x, Int y) -> Int (x / y)
        | (Float x, Float y) -> Float (x / y)
        | (Int x, Float y) -> Float ((float x) / y)
        | (Float x, Int y) -> Float (x / (float y))

        | (Rational x, Rational y) -> Rational (divideRationals x y)
        | (Rational x, Int y) -> Rational (divideRationals x {Numerator = y; Denominator = 1})
        | (Int x, Rational y) -> Rational (divideRationals {Numerator = x; Denominator = 1} y)

        | (Complex x, Complex y) -> ComplexRational (divideComplex x y)
        | (Complex x, Int y) -> ComplexRational (divideComplex x {Real = y; Imaginary = 0})
        | (Int x, Complex y) -> ComplexRational (divideComplex {Real = x; Imaginary = 0} y)

        // Handle invalid case: Float and Rational mixed
        | (Float _, Rational _) | (Rational _, Float _) -> 
            raise (System.Exception("Cannot use Float with a Rational number"))

    let moduloNumbers a b =
        match (a, b) with
        | (Int x, Int y) -> Int (x % y)
        | (Float x, Float y) -> Float (x % y)
        | (Int x, Float y) -> Float ((float x) % y)
        | (Float x, Int y) -> Float (x % (float y))

        | _ -> raise (System.Exception("Cannot use Modulo with Rational numbers"))

    let powerNumbers a b =
        match (a, b) with
        | (Int x, Int y) -> Int (System.Convert.ToInt32(System.Math.Pow(float x, float y)))
        | (Float x, Float y) -> Float (System.Math.Pow(x, y))
        | (Int x, Float y) -> Float (System.Math.Pow((float x), y))
        | (Float x, Int y) -> Float (System.Math.Pow(x, (float y)))

        | _ -> raise (System.Exception("Cannot use Powers with Rational numbers"))

    let negateNumber a =
        match a with
        | Int x -> Int (-x)
        | Float x -> Float (-x)

        | _ -> raise (System.Exception("Negation Error"))


