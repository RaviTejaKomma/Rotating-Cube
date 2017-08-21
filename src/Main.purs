module Main where
-- import statements --
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.DOM (addEventListener, querySelector)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef, modifyRef, Ref)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (Window)
import DOM.HTML.Window (requestAnimationFrame)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, Context2D, getCanvasElementById, getContext2D, setFillStyle, fillRect, moveTo, lineTo, withContext, setStrokeStyle, beginPath, closePath, stroke)
import Math (pi, cos, sin)
import Partial.Unsafe (unsafePartial)
-- import statements --


-- Defining 3D and 2D Point objects --
newtype Point3D = Point3D
  { x :: Number
  , y :: Number
  , z :: Number
  }

newtype Point2D = Point2D
  { x :: Number
  , y :: Number
  }

newtype Angle3D = Angle3D
  { qx :: Number
  , qy :: Number
  , qz :: Number
  }
  -- Defining 3D and 2D Point objects --

-- Defining cube object --
newtype Cube = Cube
  { x :: Number
  , y :: Number
  , z :: Number
  , size :: Number
  , color :: String
  }
-- Defining cube object --


--| Function to project 3D point on 2D coordinate plane
-- This function projects the 3D points onto the 2D coordinate plane --
project :: Point3D -> Angle3D -> Point2D
project (Point3D { x, y, z }) (Angle3D { qx, qy, qz }) =
  let xRotQz = x * (cos qz) + y * (sin qz)
      yRotQz = y * (cos qz) - x * (sin qz)
      yRotQzQx = yRotQz * (cos qx) + z * (sin qx)
      zRotQzQx = z * (cos qx) - yRotQz * (sin qx)
      xRotQzQxQy = xRotQz * (cos qy) + zRotQzQx * (sin qy)
  in
    Point2D { x: 300.0 + xRotQzQxQy, y: 300.0 + yRotQzQx }


withStroke :: forall e.
  Context2D ->
  String ->
  (Context2D -> Eff (canvas :: CANVAS | e) Context2D) ->
  Eff (canvas :: CANVAS | e) Context2D
withStroke ctx color draw = withContext ctx do
  ctx <- setStrokeStyle color ctx
  ctx <- beginPath ctx
  ctx <- draw ctx
  ctx <- closePath ctx
  stroke ctx

-- Function to draw line between two 2D points --
addEdge :: forall e. Context2D -> Point2D -> Point2D -> Eff (canvas :: CANVAS | e) Context2D
addEdge ctx (Point2D from) (Point2D to) = do
  ctx <- moveTo ctx from.x from.y
  lineTo ctx to.x to.y

-- Function to draw the CUBE. It first draws two faces and then joins them by drawing lines between them --
-- v1,v2,v3 ---- v8 are the vertices of the cube --
drawCube :: forall e. Context2D -> Cube -> Angle3D -> Eff (canvas :: CANVAS | e) Context2D
drawCube ctx (Cube { color, x, y, z, size }) (Angle3D { qx, qy, qz })= do
  let half = size / 2.0
  let v1 = project (Point3D { x: x - half, y: y - half, z: z - half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v2 = project (Point3D { x: x - half, y: y + half, z: z - half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v3 = project (Point3D { x: x - half, y: y - half, z: z + half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v4 = project (Point3D { x: x - half, y: y + half, z: z + half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v5 = project (Point3D { x: x + half, y: y - half, z: z - half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v6 = project (Point3D { x: x + half, y: y + half, z: z - half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v7 = project (Point3D { x: x + half, y: y - half, z: z + half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v8 = project (Point3D { x: x + half, y: y + half, z: z + half }) (Angle3D {qx: qx, qy: qy, qz: qz})

  withStroke ctx color \ctx -> do
    -- constructing one face of the cube --
    ctx <- addEdge ctx v1 v5
    ctx <- addEdge ctx v5 v6
    ctx <- addEdge ctx v6 v2
    ctx <- addEdge ctx v2 v1
    -- constructing one face of the cube --
    ctx <- addEdge ctx v3 v7
    ctx <- addEdge ctx v7 v8
    ctx <- addEdge ctx v8 v4
    ctx <- addEdge ctx v4 v3
    -- connecting these two faces --
    ctx <- addEdge ctx v1 v3
    ctx <- addEdge ctx v5 v7
    ctx <- addEdge ctx v6 v8
    addEdge ctx v2 v4

-- this function  is responsible for the animation of CUBE --
loopAnimation :: forall e state.
  Window ->
  Ref state ->
  state ->
  (state -> Eff (ref :: REF, dom :: DOM | e) state) ->
  Eff (ref :: REF, dom :: DOM | e) Unit
loopAnimation window ref state step =
  void $ requestAnimationFrame
    do loopAnimation window ref state step
       state <- readRef ref
       state <- step state
       writeRef ref state
    window


withAnimation :: forall e state.
  state ->
  (state -> Eff (ref :: REF, dom :: DOM | e) state) ->
  Eff (ref :: REF, dom :: DOM | e) Unit
withAnimation state step = do
  window <- window
  ref <- newRef state
  loopAnimation window ref state step


withAnimateContext :: forall e state.
  String ->
  state ->
  (Context2D -> state -> Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) state) ->
  Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) Unit
withAnimateContext name state draw = do
  canvas <- getCanvasElementById name
  case canvas of
    Just canvas -> do
      ctx <- getContext2D canvas
      withAnimation state \state -> do
        draw ctx state
    Nothing -> pure unit

-- This function is used to fill the canvas background with color --
drawBackground :: forall e. Context2D -> Eff (canvas :: CANVAS | e) Context2D
drawBackground ctx = do
  ctx <- setFillStyle "rgb(122,230,232)" ctx
  fillRect ctx { x: 60.0, y: 60.0, w: 500.0, h: 500.0 }

-- state object --
state = { x: 0.0
        , y: 0.0
        , qx: pi / 3.0
        , qy: pi / 5.0
        , qz: pi / 7.0
        , loop : 0.5
        }
-- state object --

-- This function is used to DECELERATE the CUBE when mouse is clicked --
stopcube :: forall e.  Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) Unit
stopcube =
  let
    canvas = getCanvasElementById "mycanvas"
  in
    withAnimateContext "mycanvas" state \ctx state -> do
        ctx <- drawBackground ctx
        void $ drawCube ctx (Cube { x: state.x, y: state.y, z: 0.0, size: 200.0, color: "rgb(0,0,0)" }) (Angle3D { qx: state.qx, qy: state.qy, qz: state.qz})
        pure $ state { x = state.x, y = state.y, qx = state.qx, qy = state.qy, qz = state.qz}

-- This function is used to start the construction and rotation of the CUBE. It also calls the drawBackground function --
startcube1 :: forall e.  Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) Unit
startcube1 =
  let
    -- getting the canvas element from the HTML --
    canvas = getCanvasElementById "mycanvas"
  in
    withAnimateContext "mycanvas" state \ctx state -> do
        ctx <- drawBackground ctx
        void $ drawCube ctx (Cube { x: state.x, y: state.y, z: 0.0, size: 270.0, color: "rgb(0,0,0)" }) (Angle3D { qx: state.qx, qy: state.qy, qz: state.qz})
        pure $ state { x = state.x, y = state.y, qx = state.qx, qy = state.qy, qz = state.qz + state.loop, loop = max (state.loop - 0.004) 0.000}

startcube2 :: forall e.  Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) Unit
startcube2 =
  let
    -- getting the canvas element from the HTML --
    canvas = getCanvasElementById "mycanvas"
  in
    withAnimateContext "mycanvas" state \ctx state -> do
        ctx <- drawBackground ctx
        void $ drawCube ctx (Cube { x: state.x, y: state.y, z: 0.0, size: 270.0, color: "rgb(0,0,0)" }) (Angle3D { qx: state.qx, qy: state.qy, qz: state.qz})
        pure $ state { x = state.x, y = state.y, qx = state.qx, qy = state.qy + state.loop, qz = state.qz, loop = max (state.loop - 0.004) 0.000}

startcube3 :: forall e.  Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) Unit
startcube3 =
  let
    -- getting the canvas element from the HTML --
    canvas = getCanvasElementById "mycanvas"
  in
    withAnimateContext "mycanvas" state \ctx state -> do
        ctx <- drawBackground ctx
        void $ drawCube ctx (Cube { x: state.x, y: state.y, z: 0.0, size: 270.0, color: "rgb(0,0,0)" }) (Angle3D { qx: state.qx, qy: state.qy, qz: state.qz})
        pure $ state { x = state.x, y = state.y, qx = state.qx + state.loop, qy = state.qy, qz = state.qz, loop = max (state.loop - 0.004) 0.000}



-- main function --
main :: Eff ( canvas :: CANVAS
            , ref :: REF
            , dom :: DOM
            , console :: CONSOLE
            ) Unit
main = void $ unsafePartial do

  -- Setting the flag value as 0. It is used to decide whether to call the startcube function or not when mouse is moved--
  myFlag <- newRef 0
  count <- newRef 0
  -- Calling the startcube function --
  startcube1
  -- getting the HTML canvas element by using its ID --
  Just canvas <- getCanvasElementById "mycanvas"
  ctx <- getContext2D canvas
  node <- querySelector "#mycanvas"

  for_ node $ addEventListener "click" $ void do
    -- Increasing the value of the myFlag reference Int --
    modifyRef myFlag \a -> a + 1
    -- reading the value from the myFlag ref Int into a, so that we can compare it with int value(1) --
    a <- readRef myFlag
    if a == 1
      then stopcube
      else log "do nothing when mouse click"


  for_ node $ addEventListener "mousemove" $ void do
    -- reading the value from the myFlag reference object and assigning it to b --
    b <- readRef myFlag
    modifyRef count \a -> mod (a + 1) 3
    c <- readRef count

    if b >= 1 && c == 1
      then do
        modifyRef myFlag \a -> 0
        startcube1
      else log "do nothing when mouse moved"

    b <- readRef myFlag

    if b >= 1 && c == 2
      then do
        modifyRef myFlag \a -> 0
        startcube2
      else log "do nothing when mouse moved"

    b <- readRef myFlag

    if b >= 1 && c == 0
      then do
        modifyRef myFlag \a -> 0
        startcube3
      else log "do nothing when mouse moved"
