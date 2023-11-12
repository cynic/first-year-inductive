module Main exposing (main)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String.Extra as String
import List.Extra as List
import DefaultData exposing (..)
import DataTypes exposing (..)
import Json.Decode as D
import Json.Encode as E

-- Define the initial model
initialModel : Model
initialModel =
    { problems = defaultProblems
    , solutions = defaultSolutions
    , path = []
    , interactionData = Nothing
    }

-- toggleProblem : Model -> Int -> Int -> Model
-- toggleProblem model problemIdx problemId =
--   { model
--     | problems =
--         List.updateAt index
--           (\problem ->
--             { problem
--             | prerequisites =
--                 if List.member problemId problem.prerequisites then
--                   List.remove id  problem.prerequisites
--                 else
--                   problemId :: problem.prerequisites
--             }
--           )
--           model.problems
--   }

-- toggleSolution : Model -> Int -> Int -> Model
-- toggleSolution model problemIdx solutionId =
--   { model
--     | problems =
--         List.updateAt index
--           (\problem ->
--             { problem
--             | solutions =
--                 if List.member solutionId problem.solutions then
--                   List.remove id problem.solutions
--                 else
--                   solutionId :: problem.solutions
--             }
--           )
--           model.problems
--   }

unselectProblem : Model -> Int -> Int -> Model
unselectProblem model problemIdx problemId =
  { model
    | problems =
        List.updateAt problemIdx
          (\problem ->
            { problem | prerequisites = List.remove problemId problem.prerequisites }
          )
          model.problems
  }

unselectSolution : Model -> Int -> Int -> Model
unselectSolution model problemIdx solutionId =
  { model
    | problems =
        List.updateAt problemIdx
          (\problem ->
            { problem | solutions = List.remove solutionId problem.solutions }
          )
          model.problems
  }

selectProblem : Model -> Int -> Int -> Model
selectProblem model problemIdx problemId =
  { model
    | problems =
        List.updateAt problemIdx
          (\problem ->
            { problem | prerequisites = problemId :: problem.prerequisites }
          )
          model.problems
  }

selectSolution : Model -> Int -> Int -> Model
selectSolution model problemIdx solutionId =
  { model
    | problems =
        List.updateAt problemIdx
          (\problem ->
            { problem | solutions = solutionId :: problem.solutions }
          )
          model.problems
  }

-- Define the Update function
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case Debug.log "MSG" (model.path, msg) of
    ([], AddToPath component) ->
      ({ model | path = component :: model.path }, Cmd.none)
    ([ExpandedFullModal _], GoBack) ->
      ({ model | path = [] }, Cmd.none)
    (ChooseSolutions :: prev, GoBack) ->
      ({ model | path = prev }, Cmd.none)
    ([ExpandedFullModal _], AddToPath ChooseSolutions) ->
      ({ model | path = ChooseSolutions :: model.path, interactionData = Nothing }, Cmd.none)
    (ChooseSolutions :: _, StringInput SearchBox s) ->
      ({ model | interactionData = Just (SearchString s) }, Cmd.none)
    ([ExpandedFullModal index], Unselect (ProblemId id)) ->
      ( unselectProblem model index id, Cmd.none )
    ([ExpandedFullModal index], Unselect (SolutionId id)) ->
      ( unselectSolution model index id, Cmd.none )
    (ChooseSolutions :: ExpandedFullModal index :: _, Select (SolutionId id)) ->
      ( selectSolution model index id, Cmd.none )
    (ChooseSolutions :: ExpandedFullModal index :: _, Unselect (SolutionId id)) ->
      ( unselectSolution model index id, Cmd.none )
    (ChoosePrerequisites :: ExpandedFullModal index :: _, Select (ProblemId id)) ->
      ( selectProblem model index id, Cmd.none )
    (ChoosePrerequisites :: ExpandedFullModal index :: _, Unselect (ProblemId id)) ->
      ( unselectProblem model index id, Cmd.none )
    ([ExpandedFullModal index], SwapListIndices i j) ->
      -- this is swapping the selected solution indices
      ( { model
        | problems =
            List.updateAt index
              (\problem ->
                { problem | solutions = List.swapAt i j problem.solutions }
              )
              model.problems
        }
      , Cmd.none
      )
    x ->
      Debug.log "Unhandled/unexpected message" (x, msg)
      |> (\_ -> (model, Cmd.none))

-- Define the View function for a single problem
viewProblem : Int -> Problem -> Html Msg
viewProblem index problem =
    div
      [ class "square"
      , class "problem"
      , style "backgroundColor" <| categoryToColor problem.category
      , if String.isBlank problem.detail then
          class ""
        else
          title problem.detail
      , onClick (AddToPath <| ExpandedFullModal index)
      ]
      [ text problem.summary
      , div
        [ class "metrics" ]
        [ case problem.prerequisites of
          [] ->
            text ""
          prerequisites ->
            span
              [ class "prerequisites"
              , case List.length prerequisites of
                1 ->
                  title "To solve this, another problem should be solved first"
                n ->
                  title <| "To solve this, " ++ String.fromInt n ++ " other problems must be solved first"
              ]
              [ text <| String.fromInt <| List.length prerequisites ]
        , case problem.solutions of
          [] ->
            span
              [ class "no-solutions"
              , title "This problem has no solutions!"
              ]
              [ text "❌" ]
          solutions ->
            span
              [ class "solutions"
              , case List.length solutions of
                1 ->
                  title "There is 1 possible solution"
                n ->
                  title <| "There are " ++ String.fromInt n ++ " possible solutions"
              ]
              [ text <| String.fromInt <| List.length solutions ]
          ]
      ]

-- For each category, create a list of problems that will be laid out by viewProblem
viewCategory : Category -> List Problem -> Html Msg
viewCategory category problems =
    div
      [ class "flex-updown" ]
      ((div
        [ class "square"
        , class "category"
        , style "color" <| categoryToColor category
        ]
        [ Html.text <| categoryName category ]
      ) ::
      ( List.indexedMap (\i p -> (i, p)) problems
        |> List.filter (\(i, p) -> p.category == category)
        |> List.map (\(i, p) -> viewProblem i p)
      ))

-- Define the View function for the entire model
view : Model -> Html Msg
view model =
  div
    []
    [ div
        [ class "flex-rightleft"
        ]
        (List.map (\c -> viewCategory c model.problems) allCategories)
    , viewInteractive model
    ]

viewScreen : Html Msg -> Html Msg
viewScreen inner =
  div
    []
    [ div
      [ class "screen-overlay" ]
      [ inner ]
    ]

viewChooseSolutions : Model -> (Int, Problem) -> Html Msg
viewChooseSolutions model (problemIdx, problem) =
  -- this is a "modal" display
  -- It has a "search" bar at the top, and a list of solutions underneath that
  -- Each solution has a checkbox before it
  -- There is a "back-button" at the top
  -- At the bottom, there is a button to add new solutions
  div
    [ class "modal" ]
    [ div
        [ class "modal-header" ]
        [ div
          [ class "back-button"
          , onClick GoBack
          ]
          [ text "←" ]
        , h1
          []
          [ text "Choose solutions" ]
        ]
    , h2
      []
      [ text "Problem: "
      , text problem.summary
      ]
    , div
      [ class "search-bar" ]
      [ input
          [ type_ "text"
          , placeholder "Search…"
          , onInput <| \s -> StringInput SearchBox s
          ]
          [ case model.interactionData of
            Nothing ->
              text ""
            Just (SearchString s) ->
              text s
          ]

          ]
    , div
      [ class "solution-list" ]
      ( List.indexedMap (\i s -> (i, s)) model.solutions
        |> List.filterMap
          (\(i, solution) ->
            case model.interactionData of
              Nothing ->
                Just (i, solution)
              Just (SearchString s) ->
                if String.isBlank s then
                  Just (i, solution)
                else
                  if String.contains (String.toLower s) (String.toLower solution.summary) || String.contains (String.toLower s) (String.toLower solution.detail) then
                    Just (i, solution)
                  else
                    Nothing
          )
        |> List.map
            (\(id, solution) ->
              div
                [ class "solution"
                ]
                [ input
                    [ type_ "checkbox"
                    , Html.Attributes.id <| "solution-" ++ String.fromInt id
                    , checked <| List.member id problem.solutions
                    , onClick <|
                        ( if List.member id problem.solutions then
                            Unselect <| SolutionId id
                          else
                            Select <| SolutionId id
                        )
                    ]
                    []
                , label
                    [ for <| "solution-" ++ String.fromInt id ]
                    [ div
                        [ class "solution-text"
                        ]
                        [ text solution.summary ]
                    , if String.isBlank solution.detail then
                        text ""
                      else 
                        div
                          [ class "optional-text" ]
                          [ text solution.detail ]
                    ]
                ]
            )
      )
      , div
        [ class "add-solution-button" ]
        [ button
            [ onClick <| AddToPath <| ChooseSolutions ]
            [ text "Add solution" ]
        ]
      ]

viewFullModal : Model -> Problem -> Html Msg
viewFullModal model problem =
  div
    [ class "modal" ]
    [ div
        [ class "modal-header" ]
        [ div
          [ class "back-button"
          , onClick GoBack
          ]
          [ text "←" ]
        , h1
          []
          [ text problem.summary ]
        ]
      , p
        [ class "optional-text"
        ]
        [ text problem.detail ]
      , case List.filterMap (\i -> List.getAt i model.problems |> Maybe.map (\p -> (i, p))) problem.prerequisites of
        [] ->
          div
            []
            [ text "If this problem cannot be solved without solving another problem first, "
            , button
                [ onClick <| AddToPath ChoosePrerequisites ]
                [ text "click here" ]
            , text " to add a prerequisite."
            ]
        prerequisites ->
          div
            []
            [ h1
                []
                [ text "Impossible / v.difficult to solve without…" ]
            , ul
                []
                ( List.map
                  (\(i, prereq) ->
                    li
                      []
                      [ span
                          [ class "prereq-text"
                          , if String.isBlank prereq.detail then
                              class ""
                            else
                              title prereq.detail
                          ]
                          [ text prereq.summary ]
                      , text " "
                      , button
                          [ onClick <| Unselect <| ProblemId i
                          , class "unselect-x"
                          ]
                          [ text "❌" ]

                      ]
                  )
                  prerequisites
                )
            , div
                []
                [ button
                  [ onClick <| AddToPath ChoosePrerequisites ]
                  [ text "Add prerequisite" ]
                ]
            ]
      , case List.filterMap (\i -> List.getAt i model.solutions |> Maybe.map (\s -> (i, s))) problem.solutions of
        [] ->
          p
            []
            [ text "No solution yet; "
            , button
                [ onClick <| AddToPath ChooseSolutions ]
                [ text "choose some" ]
            , text "."
            ]
        solutions ->
          div
            []
            [ h2
                []
                [ text "Solutions" ]
            , ol
                [ class "solution-list" ]
                ( List.indexedMap
                  (\listIdx (i, solution) ->
                    li
                      []
                      [ if listIdx == 0 then
                          button [] []
                        else
                          button [ onClick <| SwapListIndices (listIdx-1) listIdx ] [ text "▲" ]
                      , if listIdx == List.length solutions - 1 then
                          button [] []
                        else
                          button [ onClick <| SwapListIndices listIdx (listIdx+1) ] [ text "▼" ]
                      ,  span
                          [ class "solution-text"
                          , if String.isBlank solution.detail then
                              class ""
                            else
                              title solution.detail
                          ]
                          [ text solution.summary ]
                      , text " "
                      , button
                          [ onClick <| Unselect <| SolutionId i
                          , class "unselect-x"
                          ]
                          [ text "❌" ]
                      ]
                  )
                  solutions
                )
            , div
                []
                [ button
                  [ onClick <| AddToPath ChooseSolutions ]
                  [ text "Choose solutions" ]
                ]
            ]
      ]
  

-- The interactive parts of the view
viewInteractive : Model -> Html Msg
viewInteractive model =
  case model.path of
    [] ->
      text ""
    [ExpandedFullModal index] ->
      List.getAt index model.problems
      |> Maybe.map (\problem -> viewScreen <| viewFullModal model problem)
      |> Maybe.withDefault (text "")
    ChooseSolutions :: ExpandedFullModal index :: _ ->
      List.getAt index model.problems
      |> Maybe.map (\problem -> viewScreen <| viewChooseSolutions model (index, problem))
      |> Maybe.withDefault (text "")
    x ->
      Debug.log "Unhandled interactive view" x
      |> (\_ -> text "")

-- Define the Subscriptions function
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- Main entry point
main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> (initialModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
