type Vector2D = double * double

let vectorScalarOps (op : double -> double -> double) (n : double) (s : Vector2D) : Vector2D =
    match s with
    | (x, y) -> (op n x, op n y)

let vectorOps (op : double -> double -> double) (f : Vector2D) (s : Vector2D) : Vector2D = 
    match (f, s) with
    | ((x, y), (x', y')) -> (op x x', op y y')

let (|+|) = vectorOps (+)

let (-*-) = vectorScalarOps (*)

let addVecs ns = 
    ns |> Seq.fold (|+|) (0.0, 0.0)

let translate translation vecs =
    vecs |> Seq.map (fun v -> v |+| translation)

let vectorsToScatterLists vecs =
    vecs |> Seq.fold (fun (xs, ys) (x, y) -> (x::xs, y::ys)) ([], [])

let (|-|) = vectorOps (-)

let xys : seq<Vector2D> = 
    seq { 
        for x in -5.0 .. 5.0 do
        for y in -5.0 .. 5.0 ->
        (x, y) }

let thing : Vector2D -> seq<Vector2D> -> seq<Vector2D> = ((-*-) 10.0) >> translate 

let other : seq<seq<Vector2D> -> seq<Vector2D>> = Seq.map thing xys

let findSide otherSide hypotenuse = 
    Math.Sqrt (Math.Pow (hypotenuse, 2.0) - Math.Pow (otherSide, 2.0))

// ---------------------- XPlot Helpers ----------------------
// let vecArrow (originx: double, originy: double) color (x: double, y: double) =
//     Annotation(showarrow=true, arrowcolor = color, x = x, y = y, axref="x", ayref="y", ax=originx, ay=originy)

// let vecArrowFromOrigin : string -> double * double -> Annotation = vecArrow (0.0, 0.0)

// let scatterListsToPlot (xs, ys) =
//     Scatter(x = xs, y = ys, mode = "markers+lines")