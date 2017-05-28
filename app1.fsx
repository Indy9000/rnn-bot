#r "node_modules/fable-core/Fable.Core.dll"

open Fable.Core
open Fable.Import.Browser

[<Emit("Math.random()")>]
let random (): float = failwith "JS only"
[<Emit("Math.PI")>]
let PI:float = failwith "JS only"
[<Emit("Math.sign($0)")>]
let sign(x) = failwith "JS only"
[<Emit("Math.log($0)")>]
let log(x) = failwith "JS only"
[<Emit("Math.exp($0)")>]
let exp(x) = failwith "JS only"
[<Emit("Math.round($0)")>]
let round(x) = failwith "JS only"
[<Emit("Math.tanh($0)")>]
let tanh(x) = failwith "JS only"

let RandNoise (mu:float)(sigma:float) = random()*sigma-(sigma/2.0)+mu
let getRandomFloat min max = random() * (max-min) + min
let getRandomInt (min:int) (max:int) = (getRandomFloat (float min) (float max))
                                        |> floor |> int
let fromPolar (v:float,theta:float) = ((v*cos theta),(v*sin theta))
let clamp (k:float) (min:float) (max:float) =
      if k < min then min elif k > max then max else k

let ShuffleIndices (len:int) =
  let retval =[]
  let k = [|for i in 0 .. len-1 -> -1|]

  let rec insert (indices:int[]) (r:int list) =
    if (List.length r) = len then
      r
    else
      let i = getRandomInt 0 len
      let r_ =  if indices.[i] = -1 then
                  indices.[i] <- 1
                  r @ [i]
                else
                  r

      insert indices r_

  insert k retval

let ToRadians(degrees: float) = (PI * degrees) / 180.0
let ToDegrees(rads:float) = rads * 180.0/ PI

//exponential scaling to a range
//For example scale v to exp range (0.1,10), which means scale
// v to a linear scale of log(0.1)=-1,log(10)=1 and then taking exponent of the resulting value
let ExpScale (min:float) (max:float) (v:float) =
    let min_e = log(min);
    let max_e = log(max);
    exp(v*(max_e-min_e) + min_e)

//scale a value of range[0,1] linearly to the range (min,max)
let LinScale (min:float) (max:float) (v:float) =  v * (max-min) + min;

//------------------------------------------------------------------------------
type Vector(x:float,y:float) =
  member this.x = x
  member this.y = y
  member this.norm = sqrt(x*x+y*y)
  member this.normsq = x*x+y*y
  member this.unit = Vector(this.x/this.norm,this.y/this.norm)
  member this.dot (v:Vector) = this.x *v.x + this.y *v.y
  member this.angle(v:Vector) = acos (this.dot(v) / (this.norm * v.norm))

  static member (~-)(v:Vector)= Vector(-1.0*v.x, -1.0*v.y)
  static member (*)(v:Vector,a) = Vector(a*v.x, a*v.y)
  static member (+)(v1:Vector,v2:Vector) = Vector(v1.x+v2.x,v1.y+v2.y)
  static member (-)(v1:Vector,v2:Vector) = Vector(v1.x-v2.x,v1.y-v2.y)

  static member fromPolar (v:float)(theta:float) =
                          Vector(v*cos theta,v*sin theta)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
type ChartType = |Linear|Log
type Chart ={
    margin:float
    v_range:float
    h_range:float
    y_r:float
    x_r:float
    min_v:float
    max_v:float
    values:float[]
    pos:Vector
    size:Vector
    chartType:ChartType
    i:int
}

let ChartInit (position_:Vector) (size_:Vector) (ct:ChartType) (capacity:int)=
    {
        margin = 5.0;
        v_range = 1.0;
        h_range = 1.0;
        y_r = 1.0;
        x_r = 1.0;
        min_v = 1.0e36;
        max_v = -1.0e36;
        values = [|for i in 0 .. capacity-1 -> 0.|];
        pos = position_;
        size = size_;
        chartType = ct;
        i = 0;
    }

let ChartUpdate (chart:Chart) (value:float) =
    if chart.i >= chart.values.Length then
        console.log("Can't update chart. Value buffer full",chart.i);
        chart
    else
        let new_value =
            if chart.chartType = ChartType.Log then
                if value = 0.0 then 1.0e-10 else value
            else
                value

        let min_v_ = min new_value chart.min_v
        let max_v_ = max new_value chart.max_v
        let v_range_ =  if chart.chartType = ChartType.Log then
                            log(max_v_) - log(min_v_)
                        else
                            max_v_ - min_v_
        let h_range_ = float (chart.values.Length-1)
        let y_r_ = (chart.size.y - chart.margin * 2.0) / v_range_
        let x_r_ = (chart.size.x - chart.margin * 2.0) / h_range_
        chart.values.[chart.i] <- new_value

        {chart with
            min_v = min_v_;
            max_v = max_v_;
            v_range = v_range_;
            h_range = h_range_;
            y_r = y_r_;
            x_r = x_r_;
            i = chart.i + 1;
        }

let draw_horiz_text (ctx:CanvasRenderingContext2D) chart s xx =
    let x = chart.pos.x + xx;
    let y = chart.pos.y + chart.size.y + chart.margin * 2.0
    ctx.font <- "9px arial"
    ctx.fillStyle <-U3.Case1 "#555"
    ctx.fillText(s,x,y);
    ctx.fillRect(x,y - (chart.margin * 2.0),1.0,2.0)

let draw_vert_text (ctx:CanvasRenderingContext2D) chart s yy =
    let x = chart.pos.x - chart.margin * 5.0;
    let y = chart.pos.y + yy
    ctx.font <- "9px arial"
    ctx.fillStyle <-U3.Case1 "#555"
    ctx.fillText(s,x,y);
    ctx.fillRect(chart.pos.x-chart.margin*0.5,y,2.0,1.0)

let ChartDraw (ctx:CanvasRenderingContext2D) (chart:Chart) =
    ctx.save();
    ctx.setTransform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0);
    ctx.fillStyle <- U3.Case1 "rgba(128,128,128,0.5)"; //"#ccc"
    ctx.fillRect(chart.pos.x,chart.pos.y,chart.size.x,chart.size.y);
    ctx.lineWidth <- 0.3;
    ctx.strokeStyle <- U3.Case1 "#000"
    ctx.strokeRect(chart.pos.x,chart.pos.y,chart.size.x,chart.size.y);

    if (chart.i = 0) || (chart.min_v = chart.max_v) then
        chart
    else
        let v_offset = chart.pos.y + chart.size.y
        let x = chart.pos.x + chart.margin;

        let y = if chart.chartType = ChartType.Log then
                    v_offset - ((log(chart.values.[0])-log(chart.min_v)) * chart.y_r + chart.margin)
                else
                    v_offset - ((chart.values.[0]-chart.min_v) * chart.y_r + chart.margin);

        ctx.beginPath();
        ctx.moveTo(x,y);
        ctx.lineWidth <- 0.5;
        ctx.strokeStyle <- U3.Case1 "#F00";

        if chart.chartType = ChartType.Log then
            for xx in 0..chart.i-1 do
              let v = chart.values.[xx]
              let x = chart.pos.x + (float xx) * chart.x_r + chart.margin
              let y = v_offset - ((log(v) - log(chart.min_v)) * chart.y_r + chart.margin)
              ctx.lineTo(x,y)
              ctx.stroke()
        else
            for xx in 0..chart.i-1 do
              let v = chart.values.[xx]
              let x = chart.pos.x + (float xx) * chart.x_r + chart.margin
              let y = v_offset - ((v - chart.min_v) * chart.y_r + chart.margin)
              ctx.lineTo(x,y)
              ctx.stroke()

        ctx.closePath();
        //draw axis markers

        //start marker
        draw_horiz_text ctx chart "0" (chart.margin)

        //end marker
        draw_horiz_text ctx chart (sprintf "%d" (chart.values.Length-1)) (chart.size.x - chart.margin)
        //mid 2 marker
        let ss = sprintf "%.0f" (( float chart.values.Length)* 1.0 / 3.0 )
        draw_horiz_text ctx chart ss (chart.size.x*1.0/3.0 - chart.margin)
        let ss = sprintf "%.0f" (( float chart.values.Length)* 2.0 / 3.0 )
        draw_horiz_text ctx chart ss (chart.size.x*2.0/3.0 - chart.margin)

        ctx.restore();

        //max val markert
        let ss1= sprintf "%.2f" (chart.max_v)
        draw_vert_text ctx chart ss1 (chart.margin)

        let ss2= sprintf "%.2f" (chart.min_v)
        draw_vert_text ctx chart ss2 (chart.size.y - chart.margin)

        chart


//------------------------------------------------------------------------------
let POPULATION_COUNT = 50
let GENERATIONS_PER_EPOCH = 600
let canvas =  document.getElementsByTagName_canvas().[0]
let W = 1080.
let H = 480.
canvas.width <- W
canvas.height <- H
let x_mid = (W /2.0)
let y_mid = (H/2.0)
let paddle_width = 10.
let paddle_height= 50.

let ClearScreen (ctx:CanvasRenderingContext2D) =
    ctx.fillStyle <- U3.Case1 "#bbb"
    ctx.fillRect(0.,0.,W,H)

let DrawText x y t =
    let ctx = canvas.getContext_2d()
    ctx.fillStyle <- U3.Case1 "#FFF"
    ctx.font <- "12px monospace"
    ctx.fillText(t,x,y)
//------------------------------------------------------------------------------
//CTRNN
//------------------------------------------------------------------------------
type Network =
  {
    NodeCount:int;
    Z:float[];
    Y:float[];
    I:float[];
    Tau:float[];
    W:float[][];
  }

let NetworkInit (weights:float[]) (tau:float[])=
    let nodeCount = int (sqrt (float weights.Length))
    if nodeCount = 0 then 
        {
            NodeCount = nodeCount;
            Z = [||];I = [||];Y = [||];Tau = [||];W=[|[||]|]
        }
    else
        {
            NodeCount = nodeCount
            Z = [|for i in 0 .. nodeCount-1 -> 0.|];
            Y = [|for i in 0 .. nodeCount-1 -> 0.|];
            I = [|for i in 0 .. nodeCount-1 -> 0.|];
            Tau = tau;
            W = [| for i in 0 .. nodeCount-1 ->
                    [|for j in 0 .. nodeCount-1 -> weights.[i*nodeCount+j] |]
                |]
        }

let ComputeActivations network=
  {network with
    Z = [|for j in 0 .. network.NodeCount-1 ->
            // 1.0/(1.0 + exp(-network.Y.[j]-network.B.[j]))|]
            tanh network.Y.[j] 
        |]
  }
let ComputeYdot network i s =  -(network.Y.[i])/(network.Tau.[i]) + (s+network.I.[i])
let EulerIntegrate network i h dydt = network.Y.[i] + h * dydt

let Evaluate network=
    {network with
        Y = [|
                for i in 0 .. network.NodeCount-1 ->
                [|for j in 0 .. network.NodeCount-1 ->
                    network.W.[i].[j] * network.Z.[j]
                |]
                |> Array.sum
                |> ComputeYdot network i
                |> EulerIntegrate network i 0.1  //= the integration constant
            |]
    }
    |> ComputeActivations

//------------------------------------------------------------------------------
//Feed Forward NN
//------------------------------------------------------------------------------
type Node={
    B:float
    W:float[]
    Z:float
}

let NodeInit (weights:float[])(bias:float)=
    {
        B = bias
        W  = weights
        Z = 0.0
    }

let inline Sigmoid (s:float) = 1.0/(1.0+exp(-s))
let inline ComputeNode (n:Node) (I:float[]) = 
    Array.map2(fun i w-> i*w) I n.W
    |>Array.sum
    |>fun k->Sigmoid (k+n.B)

type FNN = {
    Layers:Node[][]
}

let GenerateOneShotArray (size:int)(i:int)(value:float) =
    let arr = [|for i in 0..size-1->0.0|];
    arr.[i] <- value;
    arr

let FNNInit (weights:float[])(biases:float[]) =
    //Layer 1 has 10 nodes
    let layer1 = [| for i in 0..9 ->
                        let ww = GenerateOneShotArray 10 i weights.[i] 
                        NodeInit ww (biases.[i]) 
                 |]
    let n1 = 10
    //layer 2 has 8 nodes
    let layer2 = [| for i in 0..7 -> NodeInit (weights.[(n1+i*10) .. (n1+i*10+7)]) (biases.[n1+i]) |]
    let n2 = 18
    let wn = 10+10*8
    //layer 3 has 6 nodes
    let layer3 = [| for i in 0..5 -> NodeInit (weights.[(wn+i*5) .. (wn+i*5+5)]) (biases.[n2+i]) |]
    {
        Layers = [|layer1;layer2;layer3;|]
    }

let ComputeLayer (layer:Node[]) (inputs:float[])=
    [|for n in 0.. layer.Length-1 -> ComputeNode (layer.[n]) inputs|] //|> logger "ComputeLayer"

let rec ComputeFNN i (fnn:FNN) (I:float[])=
    if i=fnn.Layers.Length then
        I
    else
        I
        |>ComputeLayer fnn.Layers.[i]
        |>ComputeFNN (i+1) fnn  

//------------------------------------------------------------------------------
//GA
//------------------------------------------------------------------------------
type GeneticAlgo = {
    Population:float[][];
    CurrentGeneration:int;
    MaxGenerations:int;
    pMutation:float;
    pCrossover:float;
    PopulationCount:int;
    ChromosomeLength:int;
}

let InitGA (populationCount:int) (pMutation_:float) (pCrossover_:float)
           (chromosomeLength:int) (maxGenCount:int)=
  {
    Population = [| for i in 0 .. populationCount-1 ->
                    [| for j in 0 .. chromosomeLength-1 ->
                        getRandomFloat 0. 1.
                    |]
                 |];
    CurrentGeneration = 0;
    MaxGenerations = maxGenCount;
    pMutation = pMutation_;
    pCrossover = pCrossover_;
    PopulationCount = populationCount;
    ChromosomeLength = chromosomeLength;
  }

let Mutate (ga:GeneticAlgo) (winner:int) (loser:int) (len:int)=
    let mf = match (float ga.CurrentGeneration)/(float ga.MaxGenerations) with
             | r    when r>0.00 && r<0.25   -> 0.2
             | r    when r>0.25 && r<0.50   -> 0.1
             | r    when r>0.50 && r<0.75   -> 0.05
             | r    when r>0.75 && r<1.00   -> 0.01
             | _    -> 0.0001

    let p_mutate = [|for i in 0 .. len-1 -> getRandomFloat 0. 1. |]
    let mutations = [| for i in 0 .. len-1 -> getRandomFloat -mf mf|]
    let p_crossover = [|for i in 0 .. len-1 -> getRandomFloat 0. 1.|]

    [| for i in 0 .. len-1 ->
        (if p_crossover.[i] < ga.pCrossover then
            ga.Population.[winner].[i]
        else
            ga.Population.[loser].[i]
        )
        |>(fun allele->
            if p_mutate.[i] < ga.pMutation then
                clamp (allele + mutations.[i]) 0. 1.
            else
                allele
          )
    |]

let Evolve (ga:GeneticAlgo) (fitness:float[]) =
    let shuffled_indices = ShuffleIndices ga.PopulationCount
  
    for k in 0 .. (ga.PopulationCount/2)-1 do
        let I1 = shuffled_indices.[2*k]
        let I2 = shuffled_indices.[2*k+1]
        let winner,loser =  if fitness.[I1] <= fitness.[I2] then
                                I2,I1
                            else
                                I1,I2
        ga.Population.[loser] <-  Mutate ga winner loser ga.ChromosomeLength

    {ga with
        //Fitness = fitness;
        Population = ga.Population;
        CurrentGeneration = ga.CurrentGeneration + 1;
    }

//------------------------------------------------------------------------------
type Polar = {value:float;theta:float}
let Polar2Cartesian (p:Polar) = ((p.value*cos p.theta),(p.value*sin p.theta))
let Cartesian2Polar (x:float)(y:float)={value=(sqrt(x*x+y*y));theta=(atan2 y x)}
//------------------------------------------------------------------------------
//Game Constants
//------------------------------------------------------------------------------
let ESCount = 0 //getRandomInt 5 8 //number of energy sources are between 5 & 8
let RockCount = getRandomInt 30 45
let ThrusterGain = 0.10 //associated with keyboard arrow key presses
let CrystalFactor = 10.0 //converts the energy to visual size (radius)
let CrystalProcurementDelta = 5.0

//------------------------------------------------------------------------------
//Keyboard interaction module
//https://github.com/fsprojects/Fable/blob/master/samples/browser/ozmo/ozmo.fsx
//------------------------------------------------------------------------------
module Keyboard =
  let mutable keysPressed = Set.empty
  let code x = if keysPressed.Contains(x) then 1 else 0
  let arrows () = (code 39 - code 37, code 40 - code 38)
  let zx () = (code 90 - code 88)
  let update (e : KeyboardEvent, pressed) =
    let keyCode = int e.keyCode
    let op =  if pressed then Set.add else Set.remove
    keysPressed <- op keyCode keysPressed
    null
  let init () =
    window.addEventListener_keydown(fun e -> update(e, true))
    window.addEventListener_keyup(fun e -> update(e, false))
//------------------------------------------------------------------------------
type Object = { x:float;y:float;r:float;l:float}

let CrystalInit ()= 
    let radius = getRandomFloat 10.0 20.0
    {
        x = getRandomFloat 100.0 (W-100.0);
        y = getRandomFloat 10.0 (H-10.0);
        r = radius;
        l = 0.0 * radius;
    }

let CrystalDraw (ctx:CanvasRenderingContext2D) (es:Object) =
  //let r = es.r * CrystalFactor
  ctx.beginPath()
  ctx.arc(es.x, es.y, es.r, 0.0, 2.0 * PI, false);
  ctx.fillStyle <- U3.Case1 "#1ce"
  ctx.fill()
  ctx.lineWidth <- 1.0
  ctx.strokeStyle <- U3.Case1 "0aa"
  ctx.stroke()
//------------------------------------------------------------------------------
//type Rock = { x:float;y:float;r:float }

let RockInit ()= 
    let radius = getRandomFloat 10.0 30.0

    {
        x = getRandomFloat 100.0 (W-100.0);
        y = getRandomFloat 10.0 (H-10.0);
        r = radius;
        l = 100.0 * radius;
    }

let RockDraw (ctx:CanvasRenderingContext2D) (rock:Object) =
  ctx.beginPath()
  ctx.arc(rock.x, rock.y, rock.r, 0.0, 2.0 * PI, false);
  ctx.fillStyle <- U3.Case1 "#8B4513"
  ctx.fill()
  ctx.lineWidth <- 1.0
  ctx.strokeStyle <- U3.Case1 "0aa"
  ctx.stroke()

//------------------------------------------------------------------------------
type SpaceShip = {
    x:float;  y:float;
    vx:float; vy:float;
    r:float;// radius of size
    energy:float;
    fitness1:float;

    sensorAlpha:float;//midline sensor direction - variable
    alphaGain:float;
    sensorBeta:float;//angle between 2 sensors - 
    betaGain:float; 

    sensorCount:int;
    sensorVectors:Vector[];
    rayVectors:Vector[];
    sensorInputs:float[]

    network:Network;
    motorGain1:float;
    motorGain2:float;
    sensorGain:float;
    impactSensor:float;
    impactSensorGain:float;
  }

let ComputeSensorVectors ship =
    [| for i in 0..ship.sensorCount-1 ->
        let b = (ship.sensorAlpha-ship.sensorBeta/2.0) + (float i) * (ship.sensorBeta/(float ship.sensorCount))
        let v1 = Vector.fromPolar ship.r b
        let v2 = Vector(ship.x, ship.y)
        v1 + v2
    |]

let SpaceShipInit sensorCount_= {
    x = 10.0;
    y = (H/2.0);
    vx = 1.0; vy = 0.0;
    r = 10.0; // size radius
    energy = 200.0;
    fitness1 = 0.0;

    sensorAlpha = ToRadians 0.0;//midline sensor direction - variable
    alphaGain = 0.0;
    sensorBeta = ToRadians 30.0;//angle between midline and sensors - fixed
    betaGain = 0.0;
    sensorCount = sensorCount_
    sensorVectors = [|for ii in 0..sensorCount_-1-> Vector(0.0,0.0)|]
    rayVectors = [|for ii in 0..sensorCount_-1-> Vector(0.0,0.0)|]
    sensorInputs = Array.zeroCreate sensorCount_        

    network = NetworkInit [||] [||];
    motorGain1 = 0.0;
    motorGain2 = 0.0;
    sensorGain = 0.0;
    impactSensor = 0.0;
    impactSensorGain = 0.0;
  }

let CTRNN_Play (ship:SpaceShip) = 
    //set the sensor inputs to the network
    let mu = 0.0 //0.00125
    let sigma = 0.0125
    ship.sensorInputs 
    |>Array.iteri(fun i k-> 
        ship.network.I.[0+i] <- (ship.sensorInputs.[i]+(RandNoise mu sigma))*ship.sensorGain
        )
    let ii = ship.sensorCount
    
    //horizontal position
    ship.network.I.[ii] <- LinScale -1.0 1.0 ((ship.x-ship.r) / (W - ship.r*2.0)) // normalized vert pos rescaled to -1 +1

    //vertical position; 
    ship.network.I.[ii+1] <- LinScale -1.0 1.0 ((ship.y-ship.r) / (H - ship.r*2.0)) // normalized vert pos rescaled to -1 +1

    //compute the network k times
    let k = 1 //5 times
    let rec reps n k = 
        if k <= 0 then 
            n
        else
            reps (Evaluate ship.network) (k-1)

    let network_ = reps ship.network k

    //read out the motor speeds
    let mu = 0.0
    let sigma = 0.01
    let q1 = (network_.Z.[0] + 1.0)/2.0 //network_.Z.[0] + 2.0
    let q2 = (network_.Z.[10] + 1.0)/2.0 //network_.Z.[1] + 2.0 
    let dvx = ((q1-q2)  + (RandNoise mu sigma)) * ship.motorGain1

    let q3 = (network_.Z.[20] + 1.0)/2.0 //network_.Z.[0] + 2.0
    let q4 = (network_.Z.[30] + 1.0)/2.0 //network_.Z.[1] + 2.0 
    let dvy = ((q3-q4)  + (RandNoise mu sigma)) * ship.motorGain2

    //read out the sensor midline angle alpha
    let sigma = 0.01
    // let energy_ = ship.energy - 10.0*(sqrt (dvx * dvx + dvy * dvy))
    let energy_ = ship.energy - 1.0*(sqrt (dvx * dvx + dvy * dvy))

    //angle of the sensor midline direction
    let a = (network_.Z.[40] + (RandNoise mu sigma) + 1.0)/2.0
    let alpha = LinScale (ToRadians 1.0) (ToRadians 360.0) a
    // let alpha = (network_.Z.[0] * PI) * ship.alphaGain;
    // let beta = ToRadians 60.0 //let beta = (network_.Z.[3] + 1.0)/2.0 * ship.betaGain;
    // let beta = (network_.Z.[1] + 1.0)/2.0 * ship.betaGain;
    
    //angle between two sensors
    let b = (network_.Z.[50] + (RandNoise mu sigma) + 1.0)/2.0
    let beta = LinScale (ToRadians 10.0) (ToRadians 120.0) b //beta between 10 and 120 degrees
    // let beta = LinScale (PI*6.0/18.0) (PI*12.0/18.0) b //beta between 60 and 120 degrees
    
    {ship with
        network = network_
        vx = ship.vx + dvx
        vy = ship.vy + dvy
        // vx = dvx
        // vy = dvy
        sensorAlpha = alpha
        sensorBeta = beta
        energy = if energy_ <= 0.0 then 0.0 else energy_
    }

let SpaceShipMove ship =
  let dvx,dvy = (Keyboard.arrows()) |> fun (a,b) -> (float a),(float b)
  let vx_ = ship.vx + dvx * ThrusterGain;
  let vy_ = ship.vy + dvy * ThrusterGain;
  let energy_ = ship.energy - (sqrt (dvx * dvx + dvy * dvy))
  let dir = (Keyboard.zx())
  let alpha_ = ship.sensorAlpha + (float dir * 0.1)

  {ship with
    x = ship.x + vx_;
    y = ship.y + vy_;
    vx = vx_; vy = vy_;
    energy = energy_;
    sensorAlpha = alpha_
  }

//v1 = ship centre v2=sensor on circumference
//Returns the intersection point (vector) at boundary walls
let ComputeLineIntersectionsWithBorder (v1:Vector)(v2:Vector) =
  //y=mx+c
  let m = (v1.y-v2.y) / (v1.x-v2.x)
  let c = v1.y - m*v1.x
  let getX (y:float) = (y - c)/m
  let getY (x:float) = m*x+c

  let topY = 0.0
  let topX = getX topY

  let bottomY = H
  let bottomX = getX bottomY

  let leftX = 0.0
  let leftY = getY leftX

  let rightX = W
  let rightY = getY rightX

  //NOTE: when boundary is intersected, we must select the correct boundary
  //      that is infront of the sensors (not behind)
  let dx1 = sign(v2.x - v1.x)
  let dx2 = sign(topX-v2.x)
  let dx3 = sign(bottomX-v2.x)
  let dy1 = sign(v2.y - v1.y)
  let dy2 = sign(leftY-v2.y)
  let dy3 = sign(rightY-v2.y)
  if topX >= 0.0 && topX <=W && (dx1=dx2) then
    Vector(topX,topY)
  elif bottomX >= 0.0 && bottomX <= W && (dx1=dx3) then
    Vector(bottomX,bottomY)
  elif leftY>=0.0 && leftY <=H && (dy1=dy2) then
    Vector(leftX,leftY)
  elif rightY>=0.0 && rightY <=H && (dy1=dy3) then
    Vector(rightX,rightY)
  else
    v2

//v1 = ship centre
//v2 = boundary intersection point
//returns objects that intersect the ray
let ComputeRayCollision (v1:Vector)(v2:Vector) (rocks:Object[]) =
  //get region of interest
  let x1 = min v1.x v2.x
  let x2 = max v1.x v2.x
  let y1 = min v1.y v2.y
  let y2 = max v1.y v2.y
  let within c1 c2 mi1 ma1 mi2 ma2=(c1>=mi1 && c1<=ma1)&&(c2>=mi2 && c2<=ma2)
  rocks
  |>Array.choose(fun k->
    let xx1 = x1 - k.r
    let xx2 = x2 + k.r
    let yy1 = y1 - k.r
    let yy2 = y2 + k.r
    if (within k.x k.y xx1 xx2 yy1 yy2) then
      Some k
    else
      None
    )//objects are within region of interest
  |>Array.sortBy(fun k->
      let vdist = Vector(k.x,k.y) - v1
      vdist.norm
   )//sort objects by distance from ship
  |>Array.tryFind(fun k->
      //We use projection based technique to find out if the line
      //intersects with a circle
      //reference image: http://stackoverflow.com/a/1079478/85150
      let vc = Vector(k.x,k.y)
      let ac = vc - v1 //vector between sensor and centre of object
      let ab = v2 - v1 //vector of the ray
      //project vc on to ac
      let ad = (ac.dot ab)/ab.norm //distance along ray to the proj point
      let cd_sq = (ac.normsq) - (ad*ad)
      (k.r*k.r) >= cd_sq
      )//first object that intersect the line
  |>function s->match s with
                |Some s-> let vc = Vector(s.x,s.y)
                          let ac = vc-v1
                          let ab = v2-v1
                          let ad = (ac.dot ab)/ab.norm
                          let cd_sq = ac.normsq - (ad*ad)
                          let pd = sqrt (s.r*s.r - cd_sq)
                          let d = ad - pd
                          let vv = ab.unit * d  //ray incident point on rock
                          //console.log("intersect",ab,ab.norm,ab.unit)
                          Some ((vv+v1),s)
                          //Some vc
                |None  -> //console.log("no intersect")
                          None
                //s

let SpaceShipUpdate rocks crystals ship=
    let vc = Vector(ship.x,ship.y)
    let objects = Array.append rocks crystals
    let sensorVectors_ = ComputeSensorVectors ship
    sensorVectors_
    |>Array.map(ComputeLineIntersectionsWithBorder vc)
    |>Array.map(fun v-> (ComputeRayCollision vc v objects),v)
    |>Array.mapi(fun i (v1,vr)->
            match v1 with
            |Some (v,obj) -> (v, obj.l/ (v-vc).norm,1.0)
            |None   -> (vr,  1.0 / (1.0+(vr-vc).norm),0.0)
        )
    |>fun k ->
            let rayVectors_ = k|>Array.map(fun(v,_,_)->v)
            let inputs      = k|>Array.map(fun(_,ii,_)->ii)
            let collisions  = k|>Array.map(fun(_,_,ii)->ii)
  
            let h_dist_percent =clamp ( (ship.x-ship.r)/(W-(ship.r * 2.0)) ) 0.0 1.0
            // printfn"h_dist_percent = %f"h_dist_percent
            {ship with
                sensorVectors = sensorVectors_
                rayVectors = rayVectors_;
                sensorInputs = inputs;
                fitness1 =  h_dist_percent
            }
  
let DrawLine (ctx:CanvasRenderingContext2D) x1 y1 x2 y2 =
  ctx.beginPath()
  ctx.moveTo(x1,y1)
  ctx.lineTo(x2,y2)
  ctx.lineWidth <- 0.5
  ctx.strokeStyle <- U3.Case1 "555"
  ctx.stroke()

let SensorLinesDraw(ctx:CanvasRenderingContext2D) ship =
    let x = ship.x
    let y = ship.y
    ship.sensorVectors
    |>Array.iter(fun v->
            DrawLine ctx x y v.x v.y //bottom    
        )
    
    Array.iter2(fun (v1:Vector) (v2:Vector)-> DrawLine ctx v1.x v1.y v2.x v2.y) 
                    ship.rayVectors ship.sensorVectors 


let SpaceShipDraw (ctx:CanvasRenderingContext2D) ship =
  ctx.beginPath()
  ctx.arc(ship.x, ship.y, ship.r, 0.0, 2.0 * PI, false);
  ctx.fillStyle <- U3.Case1 "#fde"
  ctx.fill()
  ctx.lineWidth <- 1.0
  ctx.strokeStyle <- U3.Case1 "fde"
  ctx.stroke()

  SensorLinesDraw ctx ship

  ship

//------------------------------------------------------------------------------
type Game = {
    id:int
    ship:SpaceShip 
    crystals:Object[] 
    rocks:Object[]
    avgScore:float;
    }

let GameInit (id_:int) (sensorCount_:int) 
                (betaGain_:float)(alphaGain_:float)
                (motorGain1_:float) 
                (motorGain2_:float)(sensorGain_:float)(impactSensorGain_:float) 
                (weights:float[])(tau:float[])= 

    let ship_ ={(SpaceShipInit sensorCount_) with
                    alphaGain = alphaGain_
                    betaGain = betaGain_
                    motorGain1 = motorGain1_
                    motorGain2 = motorGain2_
                    sensorGain = sensorGain_
                    impactSensorGain = impactSensorGain_
                    network = NetworkInit weights tau
                }

    let es =[|for i in 0..ESCount-1 -> CrystalInit ()|];
    let rocks_ = [|for i in 0..RockCount-1 -> RockInit ()|];

    {
        id = id_
        ship = ship_
        crystals = es
        rocks = rocks_
        avgScore = 0.0
    }
//------------------------------------------------------------------------------
let DetectOOB game =
  let ship = game.ship
  let dx1 = ship.x + ship.r
  let dx2 = ship.x - ship.r
  let dy1 = ship.y + ship.r
  let dy2 = ship.y - ship.r
  let isOOB = (dx1 > W || dx2 < 0.0 || dy1 > H || dy2 < 0.0)
  isOOB,game

let isCollided x1 x2 y1 y2 r1 r2 =
  let d1 = (x1-x2)
  let d2 = (y1-y2)
  (sqrt (d1*d1 + d2*d2)) < (r1 + r2) //collide

// detects collision with a crystal and extracts energy from it while in contact
let DetectCrystalCollision game =
  let ship = game.ship
  let collisionMask =
    game.crystals
    |> Array.map(fun k->
                  if ( isCollided k.x ship.x k.y ship.y k.r ship.r) then
                    1.0
                  else
                    0.0
                )

  let crystals_ =
    Array.zip (game.crystals) collisionMask
      |> Array.map (fun (c,m)->
          let energy = c.r * CrystalFactor
          let energy_left = clamp (energy-CrystalProcurementDelta*m) 0.0 1000.0
          let r_ = energy_left / CrystalFactor
          {c with
              r = r_
          })

  let collectedEnergy = CrystalProcurementDelta * (Array.sum collisionMask)

  {game with
    crystals = crystals_;
    ship = {ship with energy = ship.energy + collectedEnergy}
  }

let DetectRockCollision (oob,game) =
  let ship = game.ship
  game.rocks
  |> Array.map(fun k->isCollided k.x ship.x k.y ship.y ship.r k.r)
  |> Array.reduce(fun a i-> a || i)
  |> function rockCollided -> (rockCollided || oob), game

let DetectCollisions game =
  game
  |> DetectCrystalCollision
  |> DetectOOB
  |> DetectRockCollision
//------------------------------------------------------------------------------
let GameUpdate game =
  {game with
    ship = game.ship
           |>CTRNN_Play     //evil ai bot
           |>SpaceShipMove  //manual
           |>SpaceShipUpdate game.rocks game.crystals
  }
  |> DetectCollisions
  |> function (collided,game) ->
                // printfn"COLLIDED %A %A"collided game
                // if collided then printfn "COLLIDED" else ()
                ((game.ship.energy <= 0.0) || collided),game

let GameDraw (ctx:CanvasRenderingContext2D) game =
  game.crystals |> Array.iter(fun k-> CrystalDraw ctx k)
  game.rocks |> Array.iter(fun k-> RockDraw ctx k)
  game.ship |> SpaceShipDraw ctx |> ignore
  game
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
type EvolutionaryState = |Eval|NextGen|Test|Done
type Evolution ={
    games:Game[];
    measure1:float[];
    ga:GeneticAlgo;
    state:EvolutionaryState;
    i:int; //current game index in the population
    generation:int; //current generation
    charts:Chart[]
}

let rec Update ctx (rc:int) (fc:int) (evolution:Evolution) game = async{
    ClearScreen ctx
    //update and draw
    let (shouldStop,game_) = game |> GameUpdate
    game_|> GameDraw ctx |> ignore

    DrawText 10. 20. (sprintf"Robot id= %d rc= %02d score= %0.2f"game_.id rc game_.ship.fitness1)
    DrawText 10. 40. (sprintf"generation= %d " evolution.generation)
    
    let s1 = game_.ship.sensorInputs |> Array.map(sprintf"%0.3f")|>Array.reduce(fun a l-> a+" "+l)
    DrawText 10. 60. (sprintf"Sensor Input %s"s1)
    DrawText 10. 70. (sprintf"Sensor Gain %.3f"game_.ship.sensorGain) 
    DrawText 10. 80. (sprintf"Motor Gain %.3f %.3f"game_.ship.motorGain1 game_.ship.motorGain2)

    DrawText 10. 90. (sprintf"Sensor alpha %.2f" (ToDegrees game_.ship.sensorAlpha))
    DrawText 10. 100. (sprintf"Sensor beta %.2f" (ToDegrees game_.ship.sensorBeta))
    DrawText 10. 110. (sprintf"Vx %.2f  Vy%.2f" game_.ship.vx game_.ship.vy)
    DrawText 10. 120. (sprintf"Energy %.2f" game_.ship.energy)

    DrawText 300. 10. (sprintf"fitness1")
    
    evolution.charts |> Array.map (ChartDraw ctx) |>ignore
     
    //wait and repeat
    let maxRepeatCount = 5;
    if shouldStop then
        printfn"FITNESS %d %f" rc game_.ship.fitness1
        if rc = maxRepeatCount then
            return (game_.ship.fitness1 + game_.avgScore)/(float rc)
        else
            //re init ship location and rocks and crystals
            let game_fresh =    {game_ with
                                    crystals =  [|for i in 0..ESCount-1 -> CrystalInit ()|];
                                    rocks = [|for i in 0..RockCount-1 -> RockInit ()|];
                                    ship = {game_.ship with 
                                                x = 10.0;
                                                y = (H/2.0);
                                                vx = 1.0; vy = 0.0;
                                                r = 10.0; // size radius
                                                energy = 200.0
                                            }
                                    avgScore = game_.ship.fitness1 + game_.avgScore
                                }
            // printfn"GAMEFRESH %A"game_fresh
            return! Update ctx (rc+1)(fc+1) evolution game_fresh 
    else
        do! Async.Sleep(int (1000.0/60.0))
        return! Update ctx rc (fc+1) evolution game_
    }
//------------------------------------------------------------------------------

let MapChromosomeToNetwork (i:int) (chromo:float[])=
    let n = 60 //12
    let n1 = n*n 
    let weights = chromo.[0..(n1-1)] |> Array.map(LinScale -3.0 3.0) 
    let tau = chromo.[n1..(n1+n-1)] |> Array.map(LinScale 1.0 10.0)
    let betaGain_ = 1.0;//LinScale 0.1 0.90 chromo.[n1+n];//sensor angle from midline
    let alphaGain_ = 1.0//LinScale 0.0 0.3 (chromo.[n1+n]);
    let motorGain1_ = LinScale 1.0 25.0 (chromo.[n1+n+0])
    let motorGain2_ = LinScale 1.0 25.0 (chromo.[n1+n+1])
    let sensorGain_ = LinScale 0.1 10.0 (chromo.[n1+n+2])
    let impactSensorGain_ =LinScale 0.1 10.0 (chromo.[n1+n+3])
    let sensorCount = 15 //must be an odd number
    GameInit i sensorCount betaGain_ alphaGain_ motorGain1_ motorGain2_ sensorGain_ impactSensorGain_ weights tau

let InitEvolution popCount maxGenCount pMute pCross=
    let n = 60 //12
    let n1 = n*n;
    let ga_ = InitGA popCount pMute pCross (n1+n+4) maxGenCount;
    // printfn"GAINIT %A" ga_
    {
        games = [|for i in 0..popCount-1 -> MapChromosomeToNetwork i ga_.Population.[i]|];
        measure1 = Array.zeroCreate popCount
        ga = ga_;
        state = EvolutionaryState.Eval;
        i = 0; //index to the Population
        generation = 0; //current generation
        charts =[| 
                    ChartInit (Vector(250.0,10.0))(Vector(200.0,100.0)) ChartType.Linear (GENERATIONS_PER_EPOCH);
                |];
    }

let rec OperateFSM ctx evolution= async{
    if evolution.state = EvolutionaryState.Done then
        return evolution
    else
        let! res =  async{
            match evolution.state with
            |Eval -> 
                    ClearScreen ctx
                    let! fit1 = Update ctx 1 1 evolution evolution.games.[evolution.i]

                    let i_,state_ = if evolution.games.Length > (evolution.i+1) then 
                                        (evolution.i+1),EvolutionaryState.Eval 
                                    else
                                        if (evolution.generation+1) = GENERATIONS_PER_EPOCH then
                                            // printfn"GOING INTO TEST"
                                            0,EvolutionaryState.Test
                                        else
                                            // printfn"GOING INTO NEXTGEN"
                                            0,EvolutionaryState.NextGen

                    evolution.measure1.[evolution.i] <- fit1
                    printfn "game results = %d,%d,%0.2f" evolution.generation evolution.i fit1
                    return 
                        {evolution with
                            measure1 = evolution.measure1;//fit1
                            i = i_;
                            state = state_
                        }

            |NextGen ->
                    // printfn"NEXTGEN %A" evolution.measure1
                    let c1 =  (evolution.measure1|>Array.max|>fun k->printfn "fitness1 = %0.2f"k;k|>ChartUpdate evolution.charts.[0])//fit1
                    let fitness = evolution.measure1
                    let ga_ = Evolve (evolution.ga) (fitness)
                    printfn"NewGen created %d"(evolution.generation + 1)
                    return 
                        {evolution with
                            ga = ga_
                            state = EvolutionaryState.Eval
                            generation = evolution.generation + 1
                            games = [|for i in 0..POPULATION_COUNT-1 ->
                                        MapChromosomeToNetwork i ga_.Population.[i] 
                                    |]
                            charts = [|c1|]
                        }

            |Test -> //take the best individual;
                     // TODO: serialize the winning game to disk
                    let fitness =  evolution.measure1
                     
                    let i,v = fitness |> Array.mapi(fun i v-> i,v) |> Array.maxBy snd
                    let g = evolution.games.[i]
                    let! score = Update ctx 1 1 evolution g
                    console.log("Test Score",score)                            
                    return  {evolution with
                                generation = evolution.generation + 1;
                                state = if evolution.generation < GENERATIONS_PER_EPOCH*2 then
                                            evolution.state
                                        else
                                            EvolutionaryState.Done
                            }

            |Done  ->   return evolution
        }
        return! OperateFSM ctx res
    }

let Main() = async{
    console.log("start")
    console.log("Gen,game,fit1");
    let evolution = InitEvolution POPULATION_COUNT GENERATIONS_PER_EPOCH 0.8 0.5
    let! e = OperateFSM (canvas.getContext_2d()) evolution 
    ()
    }
//go forth
Main() |> Async.StartImmediate
