module DataTypes exposing (..)
import Json.Decode as D
import Json.Encode as E
import Time
-- data types and various utility functions around them

-- Define the category type
type Category
    = PracticeRelated
    | CourseStructure
    | AcademicLiteracy
    | SubjectSpecificComprehension
    | MentalModel
    | PoorClassBehaviour
    | PoorOutOfClassBehaviour
    | PoorAssessmentBehaviour

-- A list of all the Category values
allCategories : List Category
allCategories =
    [ PracticeRelated
    , CourseStructure
    , AcademicLiteracy
    , SubjectSpecificComprehension
    , MentalModel
    , PoorClassBehaviour
    , PoorOutOfClassBehaviour
    , PoorAssessmentBehaviour
    ]

-- Define the Problem type
type alias Problem =
    { summary : String
    , detail : String -- OR comment
    , example : String
    , category : Category
    , prerequisites : List Int
    , solutions: List Int
    }

type InteractionData
  = SearchString String

type alias Solution =
  { summary : String
  , detail : String -- OR comment
  , implementation : String
  }

type Selection
  = ProblemId Int
  | SolutionId Int

type StateMachine
  = ExpandedFullModal Int -- ✅
  | ChooseSolutions -- ✅
  | ChoosePrerequisites -- ✅
  | EditSolution Int
  | EditProblem Int -- ✅
  | ShowSolution Int

-- Define the Model
type alias Model =
    { problems : List Problem
    , solutions: List Solution
    , path : List StateMachine
    , interactionData : Maybe InteractionData
    }

type StringLocation
  = Summary
  | Detail
  | Example
  | Implementation
  | SearchBox

-- Define Msg type for future updates
type Msg
  = Select Selection
  | Unselect Selection
  | StringInput StringLocation String
  | ChooseCategory Category
  | GoBack
  | SwapListIndices Int Int
  | AddToPath StateMachine
  | AddNewSolution Solution
  | AddNewProblem Problem
  | Load String
  | NoOp

-- Function to map category to color
categoryToColor : Category -> String
categoryToColor category =
  case category of
    PracticeRelated ->
      "#B62045" -- Red
    CourseStructure ->
      "#3CB44B" -- Green
    AcademicLiteracy ->
      "#4363d8" -- Blue
    SubjectSpecificComprehension ->
      "#db5b00" -- Orange
    MentalModel ->
      "#911eb4" -- Purple
    PoorClassBehaviour ->
      "#9A6324" -- Brown
    PoorOutOfClassBehaviour ->
      "#000075" -- Navy
    PoorAssessmentBehaviour ->
      "#F032E6" -- Magenta

-- names for each category
categoryName : Category -> String
categoryName category =
  case category of
    PracticeRelated ->
      "Practice-Related"
    CourseStructure ->
      "Course Structure"
    AcademicLiteracy ->
      "Academic Literacy"
    SubjectSpecificComprehension ->
      "Subject-Specific Comprehension"
    MentalModel ->
      "Mental Model"
    PoorClassBehaviour ->
      "Poor Class Behaviour"
    PoorOutOfClassBehaviour ->
      "Poor Out-of-Class Behaviour"
    PoorAssessmentBehaviour ->
      "Poor Assessment Behaviour"

-- Function to encode a category to JSON
encodeCategory : Category -> E.Value
encodeCategory category =
  case category of
    PracticeRelated ->
      E.string "PracticeRelated"
    CourseStructure ->
      E.string "CourseStructure"
    AcademicLiteracy ->
      E.string "AcademicLiteracy"
    SubjectSpecificComprehension ->
      E.string "SubjectSpecificComprehension"
    MentalModel ->
      E.string "MentalModel"
    PoorClassBehaviour ->
      E.string "PoorClassBehaviour"
    PoorOutOfClassBehaviour ->
      E.string "PoorOutOfClassBehaviour"
    PoorAssessmentBehaviour ->
      E.string "PoorAssessmentBehaviour"

-- Function to decode a category from JSON
decodeCategory : D.Decoder Category
decodeCategory =
  D.string
  |> D.andThen
      (\category ->
          case category of
              "PracticeRelated" ->
                  D.succeed PracticeRelated
              "CourseStructure" ->
                  D.succeed CourseStructure
              "AcademicLiteracy" ->
                  D.succeed AcademicLiteracy
              "SubjectSpecificComprehension" ->
                  D.succeed SubjectSpecificComprehension
              "MentalModel" ->
                  D.succeed MentalModel
              "PoorClassBehaviour" ->
                  D.succeed PoorClassBehaviour
              "PoorOutOfClassBehaviour" ->
                  D.succeed PoorOutOfClassBehaviour
              "PoorAssessmentBehaviour" ->
                  D.succeed PoorAssessmentBehaviour
              _ ->
                  D.fail "Invalid category"
      )

-- Function to encode a problem to JSON
encodeProblem : Problem -> E.Value
encodeProblem problem =
  E.object
    [ ("summary", E.string problem.summary)
    , ("detail", E.string problem.detail)
    , ("example", E.string problem.example)
    , ("category", encodeCategory problem.category)
    , ("prerequisites", E.list E.int problem.prerequisites)
    , ("solutions", E.list E.int problem.solutions)
    ]

-- Function to decode a problem from JSON
decodeProblem : D.Decoder Problem
decodeProblem =
  D.map6 Problem
    (D.field "summary" D.string)
    (D.field "detail" D.string)
    (D.field "example" D.string)
    (D.field "category" decodeCategory)
    (D.field "prerequisites" (D.list D.int))
    (D.field "solutions" (D.list D.int))

-- Function to encode a solution to JSON
encodeSolution : Solution -> E.Value
encodeSolution solution =
  E.object
    [ ("summary", E.string solution.summary)
    , ("detail", E.string solution.detail)
    , ("implementation", E.string solution.implementation)
    ]

-- Function to decode a solution from JSON
decodeSolution : D.Decoder Solution
decodeSolution =
  D.map3 Solution
    (D.field "summary" D.string)
    (D.field "detail" D.string)
    (D.field "implementation" D.string)

-- Function to encode a Model to JSON
encodeModel : Model -> E.Value
encodeModel model =
  E.object
    [ ("problems", E.list encodeProblem model.problems)
    , ("solutions", E.list encodeSolution model.solutions)
    ]

-- Function to decode a Model from JSON
decodeModel : D.Decoder Model
decodeModel =
  D.map2
    (\px sx -> Model px sx [] Nothing)
    (D.field "problems" (D.list decodeProblem))
    (D.field "solutions" (D.list decodeSolution))

-- Function to take a string, parse it as JSON, and return a Model
decodeModelFromString : String -> Result D.Error Model
decodeModelFromString string =
  D.decodeString decodeModel string

-- Function to take a Model and return a string
encodeModelAsString : Model -> String
encodeModelAsString model =
  E.encode 0 (encodeModel model)