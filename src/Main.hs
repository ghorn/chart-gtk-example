{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# Language PackageImports #-}

module Main
       ( main
       ) where

import Control.Monad ( forever )
import qualified Control.Concurrent as CC
import "gtk" Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified "gtk" Graphics.UI.Gtk as Gtk
import Control.Lens ( (.~) )
import Data.Default.Class ( def )
import qualified Graphics.Rendering.Chart as Chart
import Graphics.Rendering.Chart.Backend.Cairo ( runBackend, defaultEnv )
import "gtk" Graphics.UI.Gtk.ModelView as Model
import qualified Data.Text as T

animationWaitTime :: Int
animationWaitTime = 33 -- i think this means 1/33 =~= 30.3 Hz

type MyReal = Double

mytable :: [IFGHA]
mytable = [IFGHA i (f i) (g i) (h i) (a i) | i <- [0..30]]
  where
    k,j,q,jp :: MyReal
    k = 1.03**(1/12)
    j = 1
    q = 1
    jp = j + q

    f :: Int -> MyReal
    f 0 = 1
    f i = (f $ i - 1) * k

    g :: Int -> MyReal
    g i = if (mod i 12 == 0) then (f i) else (g $ i - 1)

    h :: Int -> MyReal
    h 0 = 20
    h i = h 0 + (g $ i - 1) * j

    a :: Int -> MyReal
    a i = (f i) * jp + (h i)

--not the safest way to print doubles
tShowSome :: MyReal -> T.Text
tShowSome = T.pack . take 6 . show

data IFGHA = IFGHA {xi :: Int,
                    xf :: MyReal,
                    xg :: MyReal,
                    xh :: MyReal,
                    xa :: MyReal}

main :: IO ()
main = do
  -- have to call this before any other gtk stuff
  _ <- Gtk.initGUI

  -- the main window
  win <- Gtk.windowNew
  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := "wooooooooooo"
                   ]

  -- lets have a little fun, make a little label widget to display a message for us
  msg <- Gtk.labelNew (Just "..... heeeeey macarena ..... lol ..... lol ..... lol ..... lol ")

  -- a little worker to update the message
  let msgUpdater = do
        CC.threadDelay 200000 -- 1/5 second delay
        -- have to call postGUISync because gtk is not thread-safe
        Gtk.postGUISync $ do
          (x0:xs) <- Gtk.labelGetText msg :: IO String
          Gtk.labelSetText msg (xs ++ [x0])

  -- fork that worker thread
  msgThread <- CC.forkIO $ forever msgUpdater

  -- a worker thread to update whatever will be plotted
  -- use an MVar as an abstraction barrier
  -- for now just keep shifting the data in time
  plotData <- CC.newMVar $ unzip [(t, sin(t)) | t <- init [0,0.05..2*pi :: Double]]
  let shiftData (xs,y0:ys) = (map (+0.05) xs, ys ++ [y0])
      shiftData _ = ([],[])
      dataUpdater = do
        CC.threadDelay 50000 -- 20Hz
        CC.modifyMVar_ plotData (return . shiftData)
  -- fork the worker thread
  plotDataThread <- CC.forkIO $ forever dataUpdater

  -- When the window is destroyed, kill the message thread and quit Gtk.
  -- I think this is only important in GHCI or other cases you want to
  -- repeatedly start/stop the gui in a single process.
  _ <- Gtk.onDestroy win $ do
    CC.killThread msgThread
    CC.killThread plotDataThread
    Gtk.mainQuit

  -- create the other widget, the main plotter
  plotArea <- newChartCanvas plotData

  txtfield <- Gtk.entryNew
  _ <- Gtk.onEntryActivate txtfield ((Gtk.entryGetText txtfield) >>= putStrLn)

  -- the table itself
  list <- listStoreNew mytable

  treeview <- Model.treeViewNewWithModel list
  Model.treeViewSetHeadersVisible treeview True

  -- column for i
  coli <- Model.treeViewColumnNew
  Model.treeViewColumnSetTitle coli "i"
  rendereri <- Model.cellRendererTextNew
  Model.cellLayoutPackStart coli rendereri False
  Model.cellLayoutSetAttributes coli rendereri list
          $ \ind -> [Model.cellText := (T.pack . show $ (xi ind))]
  _ <- Model.treeViewAppendColumn treeview coli

  -- column for f
  colf <- Model.treeViewColumnNew
  Model.treeViewColumnSetTitle colf "f"
  rendererf <- Model.cellRendererTextNew
  Model.cellLayoutPackStart colf rendererf False
  Model.cellLayoutSetAttributes colf rendererf list
          $ \ind -> [Model.cellText := (tShowSome $ xf ind)]
  _ <- Model.treeViewAppendColumn treeview colf

-- column for g
  colg <- Model.treeViewColumnNew
  Model.treeViewColumnSetTitle colg "g"
  rendererg <- Model.cellRendererTextNew
  Model.cellLayoutPackStart colg rendererg False
  Model.cellLayoutSetAttributes colg rendererg list
          $ \ind -> [Model.cellText := (tShowSome $ xg ind)]
  _ <- Model.treeViewAppendColumn treeview colg

  -- column for h
  colh <- Model.treeViewColumnNew
  Model.treeViewColumnSetTitle colh "h"
  rendererh <- Model.cellRendererTextNew
  Model.cellLayoutPackStart colh rendererh False
  Model.cellLayoutSetAttributes colh rendererh list
          $ \ind -> [Model.cellText := (tShowSome $ xh ind)]
  _ <- Model.treeViewAppendColumn treeview colh

-- column for a
  cola <- Model.treeViewColumnNew
  Model.treeViewColumnSetTitle cola "a"
  renderera <- Model.cellRendererTextNew
  Model.cellLayoutPackStart cola renderera False
  Model.cellLayoutSetAttributes cola renderera list
          $ \ind -> [Model.cellText := (tShowSome $ xa ind)]
  _ <- Model.treeViewAppendColumn treeview cola

  ---- If only there were a way to select individual boxes instead of whole rows
  --tree <- Model.treeViewGetSelection treeview
  --Model.treeSelectionSetMode tree  SelectionSingle
  --Model.onSelectionChanged tree $ do
  --   sel <- Model.treeSelectionGetSelectedRows tree
  --   let s = head  (head sel)
  --   v <- Model.listStoreGetValue list s
  --   --Model.treeViewColumnSetTitle coli v
  --   putStrLn $ "selected row " ++ (show $ xi v)

  hbox <- Gtk.hBoxNew False 4

  vbox1 <- Gtk.vBoxNew False 4

  Gtk.set vbox1 $
    [ Gtk.containerChild := txtfield
    , Gtk.boxChildPacking txtfield := Gtk.PackNatural
    , Gtk.containerChild := treeview
    , Gtk.boxChildPacking treeview := Gtk.PackNatural
    ]

  -- create a box which will contain the message widget and plot widget
  vbox2 <- Gtk.vBoxNew False 4

  Gtk.set hbox $
    [ Gtk.containerChild := vbox1
    , Gtk.boxChildPacking vbox1 := Gtk.PackNatural
    , Gtk.containerChild := vbox2
    , Gtk.boxChildPacking vbox2 := Gtk.PackNatural
    ]
  -- add the children
  Gtk.set vbox2 $
    [ Gtk.containerChild := msg
    , Gtk.boxChildPacking msg := Gtk.PackNatural
    , Gtk.containerChild := plotArea
    , Gtk.boxChildPacking plotArea := Gtk.PackNatural
    ]

  -- Set the child of the main window
  -- We have to use the vbox because the main window can only have 1 child
  _ <- Gtk.set win [ Gtk.containerChild := hbox ]

  -- show the main window and start the gtk loop
  Gtk.widgetShowAll win
  Gtk.mainGUI

newChartCanvas :: (Chart.PlotValue a, Show a, RealFloat a)
                  => CC.MVar ([a],[a]) -> IO Gtk.DrawingArea
newChartCanvas plotData = do
  -- chart drawing area
  chartCanvas <- Gtk.drawingAreaNew
  _ <- Gtk.widgetSetSizeRequest chartCanvas 250 250
  _ <- Gtk.onExpose chartCanvas $ const (updateCanvas chartCanvas plotData)
  -- this is a delay which makes it periodically redraw
  _ <- Gtk.timeoutAddFull
       (Gtk.widgetQueueDraw chartCanvas >> return True)
       Gtk.priorityDefaultIdle animationWaitTime
  return chartCanvas


-- this reads the data MVar and plots whatever is in it
updateCanvas :: (Chart.PlotValue a, Show a, RealFloat a)
                => Gtk.DrawingArea -> CC.MVar ([a],[a]) -> IO Bool
updateCanvas canvas plotData = do
  points <- CC.readMVar plotData

  let myGraph = displayChart (uncurry zip points)
  chartGtkUpdateCanvas myGraph canvas


-- this is the function which turns a list of points into a Chart
displayChart :: forall a . (Chart.PlotValue a, Show a, RealFloat a)
                => [(a,a)] -> Chart.Renderable ()
displayChart points = Chart.toRenderable layout
  where
    drawOne (name,pc) col
      = Chart.plot_lines_values .~ pc
        $ Chart.plot_lines_style  . Chart.line_color .~ col
--        $ Chart.plot_points_style ~. Chart.filledCircles 2 red
        $ Chart.plot_lines_title .~ name
        $ def
    allLines = zipWith drawOne
               [("hi",[points]),("there", [map (\(x,y) -> (x,y+0.5)) points])]
               Chart.defaultColorSeq

    xlabel = "I am an X label, hear me roar"

    layout = Chart.layout_plots .~ map Chart.toPlot allLines
             $ Chart.layout_title .~ "Wooo, Party Graph!"
             $ Chart.layout_x_axis . Chart.laxis_title .~ xlabel
             $ def

-- this is also available in the Charg-gtk package as "updateCanvas"
chartGtkUpdateCanvas :: Chart.Renderable a -> Gtk.DrawingArea  -> IO Bool
chartGtkUpdateCanvas chart canvas = do
    win <- Gtk.widgetGetDrawWindow canvas
    (width, height) <- Gtk.widgetGetSize canvas
    regio <- Gtk.regionRectangle $ Gtk.Rectangle 0 0 width height
    let sz = (fromIntegral width,fromIntegral height)
    Gtk.drawWindowBeginPaintRegion win regio
    _ <- Gtk.renderWithDrawable win $ runBackend (defaultEnv Chart.bitmapAlignmentFns) (Chart.render chart sz) 
    Gtk.drawWindowEndPaint win
    return True


