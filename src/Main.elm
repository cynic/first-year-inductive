module Main exposing (main)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String.Extra as String
import List.Extra as List
import DefaultData exposing (..)
import DataTypes exposing (..)
import Time
import Task
import LocalStorage exposing (save)
import Platform.Sub as Sub
import Browser.Events
import Json.Decode

-- Define the initial model
initialModel : Model
initialModel =
    { problems = defaultProblems
    , solutions = defaultSolutions
    , path = []
    , interactionData = Nothing
    }

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

inProblem : Model -> Int -> (Problem -> Problem) -> Model
inProblem model problemIdx f =
  { model
    | problems =
        List.updateAt problemIdx f model.problems
  }

-- Define the Update function
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case Debug.log "MSG" (model.path, msg) of
    (_, NoOp) ->
      (model, Cmd.none)
    (_, Load s) ->
      case decodeModelFromString s of
        Ok m ->
          (m, LocalStorage.loadResult "Data-file loaded successfully")
        Err _ ->
          (model, LocalStorage.loadResult "This data-file is incorrect/corrupt and I couldn't load it.")
    ([], AddToPath component) ->
      ({ model | path = component :: model.path }, Cmd.none)
    ([], AddNewProblem problem) ->
      ( { model
        | problems = model.problems ++ [problem]
        , path = EditProblem (List.length model.problems) :: model.path
        }
      , Cmd.none
      )
    (_, AddToPath (ShowSolution idx)) ->
      ({ model | path = ShowSolution idx :: model.path }, Cmd.none)
    (EditProblem idx :: _, Delete) ->
      let
        newModel =
          { model
          | problems =
              List.removeAt idx model.problems
              |> List.map
                (\problem ->
                  { problem
                  | prerequisites =
                      List.filter (\id -> id /= idx) problem.prerequisites
                      |> List.map (\id -> if id > idx then id - 1 else id)
                  }
                )
          , path = []
          }
      in
        (newModel, save newModel)
    (EditSolution idx :: rest, Delete) ->
      let
        newModel =
          { model
          | solutions =
              List.removeAt idx model.solutions
          , problems =
              List.map
                (\problem ->
                  { problem
                  | solutions =
                      List.filter (\id -> id /= idx) problem.solutions
                      |> List.map (\id -> if id > idx then id - 1 else id)
                  }
                )
                model.problems
          , path = rest
          }
      in
        (newModel, save newModel)
    ([ExpandedFullModal _], AddToPath (EditSolution idx)) ->
      ({ model | path = EditSolution idx :: model.path }, Cmd.none)
    ([ExpandedFullModal _], AddToPath (EditProblem idx)) ->
      ({ model | path = EditProblem idx :: model.path }, Cmd.none)
    (ChooseSolutions :: _, AddToPath (EditSolution idx)) ->
      ({ model | path = EditSolution idx :: model.path }, Cmd.none)
    ([ExpandedFullModal _], GoBack) ->
      ({ model | path = [] }, save model)
    (ChooseSolutions :: prev, GoBack) ->
      ({ model | path = prev }, save model)
    (ChoosePrerequisites :: prev, GoBack) ->
      ({ model | path = prev }, save model)
    (EditProblem _ :: prev, GoBack) ->
      ({ model | path = prev }, save model)
    (EditProblem index :: _, StringInput Summary s) ->
      ( inProblem model index (\problem -> { problem | summary = s })
      , Cmd.none
      )
    (EditProblem index :: _, StringInput Detail s) ->
      ( inProblem model index (\problem -> { problem | detail = s })
      , Cmd.none
      )
    (EditProblem index :: _, StringInput Example s) ->
      ( inProblem model index (\problem -> { problem | example = s })
      , Cmd.none
      )
    (EditSolution _ :: prev, GoBack) ->
      ({ model | path = prev }, save model)
    (ShowSolution _ :: prev, GoBack) ->
      ({ model | path = prev }, Cmd.none)
    (EditSolution index :: _, StringInput Summary s) ->
      ( { model | solutions = List.updateAt index (\solution -> { solution | summary = s }) model.solutions }
      , Cmd.none
      )
    (EditSolution index :: _, StringInput Detail s) ->
      ( { model | solutions = List.updateAt index (\solution -> { solution | detail = s }) model.solutions }
      , Cmd.none
      )
    (EditSolution index :: _, StringInput Implementation s) ->
      ( { model | solutions = List.updateAt index (\solution -> { solution | implementation = s }) model.solutions }
      , Cmd.none
      )
    ([ExpandedFullModal _], AddToPath ChooseSolutions) ->
      ({ model | path = ChooseSolutions :: model.path, interactionData = Nothing }, Cmd.none)
    ([ExpandedFullModal _], AddToPath ChoosePrerequisites) ->
      ({ model | path = ChoosePrerequisites :: model.path, interactionData = Nothing }, Cmd.none)
    (ChooseSolutions :: _, StringInput SearchBox s) ->
      ({ model | interactionData = Just (SearchString s) }, Cmd.none)
    (ChoosePrerequisites :: _, StringInput SearchBox s) ->
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
    (ChooseSolutions :: _, AddNewSolution soln) ->
      ( { model
        | solutions = model.solutions ++ [soln]
        , path = EditSolution (List.length model.solutions) :: model.path
        }
      , Cmd.none
      )
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

viewSolution : Solution -> Html Msg
viewSolution solution =
  -- A solution has a summary, a detail, and an implementation.
  -- All of these are strings.
  -- Here we are just viewing each of them, in a modal display.
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
          [ text solution.summary ]
        ]
    , div
        []
        [ h2
            []
            [ text "Summary" ]
        , text solution.summary
        , h2
            []
            [ text "Details / comments" ]
        , text solution.detail
        , h2
            []
            [ text "Implementation" ]
        , text solution.implementation
        ]
    ]

viewEditSolution : Solution -> Html Msg
viewEditSolution solution =
  -- The "Edit Solution" screen is a modal display
  -- The things to edit are:
  -- - summary (string, via input type text)
  -- - detail (string, via textarea)
  -- implementation (string, via textarea)
  -- And that's all.  There is also a "back" button at the top.
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
          [ text "Edit solution" ]
        ]
    , div
        []
        [ h2
            []
            [ text "Summary" ]
        , input
            [ placeholder "Summary"
            , type_ "text"
            , onInput <| (StringInput Summary)
            , value solution.summary
            ]
            []
        , h2
            []
            [ text "Details / comments" ]
        , textarea
            [ placeholder "Details / comments"
            , onInput <| (StringInput Detail)
            ]
            [ text solution.detail ]
        , h2
            []
            [ text "Implementation" ]
        , textarea
            [ placeholder "Implementation"
            , onInput <| (StringInput Implementation)
            ]
            [ text solution.implementation ]
        ]
        , div
            [ class "modal-footer" ]
            [ button
                [ class "warning-irrevocable"
                , onClick Delete
                ]
                [ text "Delete solution" ]
            , text " ⚠ WARNING: Deletion of solutions is FINAL and IRREVOCABLE!"
            ]
    ]

viewEditProblem : Problem -> Html Msg
viewEditProblem problem =
  -- The "Edit Problem" screen is a modal display
  -- The things to edit are:
  -- - summary (string)
  -- - detail (string)
  -- - example (string)
  -- And that's all.  There is also a "back" button at the top.
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
          [ text "Edit problem" ]
        ]
    , div
        []
        [ h2
            []
            [ text "Summary" ]
        , input
            [ placeholder "Summary"
            , type_ "text"
            , onInput <| (StringInput Summary)
            , value problem.summary
            ]
            []
        , h2
            []
            [ text "Details / comments" ]
        , textarea
            [ placeholder "Details / comments"
            , onInput <| (StringInput Detail)
            ]
            [ text problem.detail ]
        , h2
            []
            [ text "Examples" ]
        , text "To make this clear and unambiguous, put in some examples that you've seen of this at the University"
        , textarea
            [ placeholder "Examples"
            , onInput <| (StringInput Example)
            ]
            [ text problem.example ]
        ]
        , div
            [ class "modal-footer" ]
            [ button
                [ class "warning-irrevocable"
                , onClick Delete
                ]
                [ text "Delete problem" ]
            , text " ⚠ WARNING: Deletion of problems is FINAL and IRREVOCABLE!"
            ]
    ]

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
              [ text "🚫" ]
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
        [ text <| categoryName category
        , text " "
        , button
            [ class "add-new-problem"
            , title "Add a new problem in this category"
            , onClick <| AddNewProblem
                ( Problem
                  "New problem"
                  ""
                  ""
                  category
                  [] []
                )
            ]
            [ text "➕" ]
        ]
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

viewChoosePrerequisites : Model -> (Int, Problem) -> Html Msg
viewChoosePrerequisites model (problemIdx, problem) =
  -- this is largely the same as viewChooseSolutions
  -- except that it is for prerequisites.
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
          [ text "Choose prerequisites" ]
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
      [ class "prereq-list" ]
      ( List.indexedMap (\i p -> (i, p)) model.problems
        |> List.filterMap
          (\(i, prereq) ->
            if i == problemIdx then
              Nothing
            else
              case model.interactionData of
                Nothing ->
                  Just (i, prereq)
                Just (SearchString s) ->
                  if String.isBlank s then
                    Just (i, prereq)
                  else
                    if String.contains (String.toLower s) (String.toLower prereq.summary) || String.contains (String.toLower s) (String.toLower prereq.detail) then
                      Just (i, prereq)
                    else
                      Nothing
          )
        |> List.map
            (\(id, prereq) ->
              div
                [ class "prereq"
                ]
                [ input
                    [ type_ "checkbox"
                    , Html.Attributes.id <| "prereq-" ++ String.fromInt id
                    , checked <| List.member id problem.prerequisites
                    , onClick <|
                        ( if List.member id problem.prerequisites then
                            Unselect <| ProblemId id
                          else
                            Select <| ProblemId id
                        )
                    ]
                    []
                , label
                    [ for <| "prereq-" ++ String.fromInt id ]
                    [ div
                        [ class "prereq-text"
                        ]
                        [ text prereq.summary ]
                    , if String.isBlank prereq.detail then
                        text ""
                      else 
                        div
                          [ class "optional-text" ]
                          [ text prereq.detail ]
                    ]
                ]
            )
      )
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
                , button
                    [ onClick <| AddToPath <| EditSolution id
                    , class "icon-button"
                    , title "Edit this solution"
                    ]
                    [ text "🖊" ]
                ]
            )
      )
      , div
        [ class "add-solution-button" ]
        [ button
            [ onClick <| AddNewSolution
                ( Solution
                  "New solution"
                  ""
                  ""
                )
            ]
            [ text "Add a completely new solution" ]
        ]
      ]

viewFullModal : Model -> (Int, Problem) -> Html Msg
viewFullModal model (problemIdx, problem) =
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
        , span
            []
            [ button
                [ onClick <| AddToPath <| EditProblem problemIdx
                , class "icon-button"
                , class "large"
                , title "Edit this problem"
                ]
                [ text "🖊" ]
            ]
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
            [ h2
                []
                [ text "Impossible / v.difficult to solve without a solution for:" ]
            , ul
                [ class "prereq-list" ]
                ( List.map
                  (\(i, prereq) ->
                    li
                      []
                      [ div
                          [ class "item-content" ]
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
                              , class "unselect-x icon-button small"
                              ]
                              [ text "❌" ]

                          ]
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
                      [ div
                          [ class "item-content" ]
                          [ if listIdx == 0 then
                              button [ class "invisible icon-button small up" ] [ text "▲" ]
                            else
                              button [ class "icon-button small up", onClick <| SwapListIndices (listIdx-1) listIdx ] [ text "▲" ]
                          , if listIdx == List.length solutions - 1 then
                              button [ class "invisible icon-button small down" ] [ text "▼" ]
                            else
                              button [ class "icon-button small down", onClick <| SwapListIndices listIdx (listIdx+1) ] [ text "▼" ]
                          ,  span
                              [ class "solution-text"
                              , if String.isBlank solution.detail then
                                  class ""
                                else
                                  title solution.detail
                              , onClick <| AddToPath <| ShowSolution i
                              , style "cursor" "pointer"
                              ]
                              [ text solution.summary ]
                          , text " "
                          , button
                              [ onClick <| AddToPath <| EditSolution i
                              , class "icon-button small"
                              ]
                              [ text "🖊" ]
                          , button
                              [ onClick <| Unselect <| SolutionId i
                              , class "unselect-x icon-button small"
                              ]
                              [ text "❌" ]
                          ]
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
      |> Maybe.map (\problem -> viewScreen <| viewFullModal model (index, problem))
      |> Maybe.withDefault (text "")
    ChooseSolutions :: ExpandedFullModal index :: _ ->
      List.getAt index model.problems
      |> Maybe.map (\problem -> viewScreen <| viewChooseSolutions model (index, problem))
      |> Maybe.withDefault (text "")
    ChoosePrerequisites :: ExpandedFullModal index :: _ ->
      List.getAt index model.problems
      |> Maybe.map (\problem -> viewScreen <| viewChoosePrerequisites model (index, problem))
      |> Maybe.withDefault (text "")
    EditProblem index :: _ ->
      List.getAt index model.problems
      |> Maybe.map (\problem -> viewScreen <| viewEditProblem problem)
      |> Maybe.withDefault (text "")
    EditSolution index :: _ ->
      List.getAt index model.solutions
      |> Maybe.map (\solution -> viewScreen <| viewEditSolution solution)
      |> Maybe.withDefault (text "")
    ShowSolution index :: _ ->
      List.getAt index model.solutions
      |> Maybe.map (\solution -> viewScreen <| viewSolution solution)
      |> Maybe.withDefault (text "")
    x ->
      Debug.log "Unhandled interactive view" x
      |> (\_ -> text "")

-- Define the Subscriptions function
subscriptions : Model -> Sub Msg
subscriptions model =
  -- A subscription for updating the time every 15 seconds
  Sub.batch
    [ LocalStorage.loadFromUser Load
    , Browser.Events.onKeyDown
        ( Json.Decode.map
            (\k -> if k == "Escape" then GoBack else NoOp)
            (Json.Decode.field "key" Json.Decode.string)
        )
    ]

-- Main entry point
main : Program String Model Msg
main =
    Browser.element
        { init = \s ->
            (decodeModelFromString s |> Result.withDefault initialModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
