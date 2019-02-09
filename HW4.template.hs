module HW4 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--  * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--    functions for generating MiniMiniLogo programs. It contains the type
--    definitions for Mode, Cmd, and Prog.
--  * Render.hs contains code for rendering the output of a MiniMiniLogo
--    program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--   
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen m) (p, (x, y)) = ((m, (x, y)), Nothing)
cmd (Move x2 y2) (m, (x1, y1)) = 
    case m of 
        Down -> ((m, (x2, y2)), Just ((x1, y1), (x2, y2)))
        _ -> ((m, (x2, y2)), Nothing)

-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])

-- [Pen Up, Move x y, Pen Down, Move (x+w) (y+h), Pen Up, Move x (y+h), Pen Down, Move (x+w) y]
-- Cmd 1: ((up, (0, 0)), Nothing)
-- Cmd 2: ((up, (10, 10)), Nothing)
-- Cmd 3: ((down, (10, 10)), Nothing)
-- Cmd 4: ((down, (15, 17)), Just ((10, 10), (15, 17)))
-- Cmd 5: ((up, (15, 17)), Nothing)
-- cmd 6: ((up, (10, 17)), Nothing)
-- Cmd 7: ((down, (10, 17)), Nothing)
-- Cmd 8: ((down, (15, 10)), Just ((10, 17), (15, 10)))

prog :: Prog -> State -> (State, [Line])
prog [] state = (state, [])
prog (x:xs) (state) = case (cmd x state) of
                      ((m, (x1, y1)), Just ((x2, y2), (x3, y3))) -> (appendList (((x2, y2), (x3, y3))) (prog xs (m, (x1, y1))))
                      ((m, (x, y)), Nothing) -> prog xs (m, (x, y))

appendList :: Line -> (State, [Line]) -> (State, [Line])
appendList (line) (state, listLine) = (state, line : listLine)

--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: Prog
amazing = undefined