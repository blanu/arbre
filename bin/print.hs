import Arbre.Load
import Arbre.Expressions
import Arbre.Print
import Arbre.View

main = do
    views <- loadViews "views.json"
    case views of
        Just v -> do
            printViews v
            putStrLn $ show $ getView v "=="
            printProgramWithViews v
        Nothing -> do
            putStrLn "No views"
            printProgramWithViews (Views [])

printProgramWithViews :: Views -> IO()
printProgramWithViews views = do
    prog <- loadProgram "test.json"
    case prog of
        Just lp -> do printProg views lp
        Nothing -> do return()
