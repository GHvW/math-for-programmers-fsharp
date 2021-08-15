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


let addRevised (vectors: seq<Vector2D>) =
    (vectors |> Seq.map (fun (x, _) -> x) |> Seq.sum, vectors |> Seq.map (fun (_, y) -> y) |> Seq.sum)


let translate translation vecs =
    vecs |> Seq.map (fun v -> v |+| translation)


let vectorsToScatterLists vecs =
    vecs |> Seq.fold (fun (xs, ys) (x, y) -> (x::xs, y::ys)) ([], [])


let distance (v : Vector2D) : double =
    let (x, y) = v
    Math.Sqrt ((Math.Pow (x, 2.0)) + (Math.Pow (y, 2.0)))


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


let distanceBetween first second = distance (first |-| second)


let perimeter vecs = 
    vecs
    |> Seq.windowed 2
    |> Seq.map (fun window -> 
        match window with
        | [| x; y |] -> distanceBetween x y
        | _ -> 0.0)
    |> Seq.sum


let toDegrees r = r * 180.0 / Math.PI


let toRadians d = d * Math.PI / 180.0


type PolarCoordinate = double * double

let toCartesian polarCoordinate : Vector2D =
    match polarCoordinate with
    | (r, theta) -> (r * Math.Cos theta, r * Math.Sin theta)


let toPolar vec : PolarCoordinate =
    match vec with
    | (x, y) -> (distance vec, Math.Atan2 (y, x))


// ---------------------- XPlot Helpers ----------------------
// let vecArrow (originx: double, originy: double) color (x: double, y: double) =
//     Annotation(showarrow=true, arrowcolor = color, x = x, y = y, axref="x", ayref="y", ax=originx, ay=originy)

// let vecArrowFromOrigin : string -> double * double -> Annotation = vecArrow (0.0, 0.0)

// let scatterListsToPlot (xs, ys) =
//     Scatter(x = xs, y = ys, mode = "markers+lines")