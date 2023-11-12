module DataTypes exposing (..)
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
  = ExpandedFullModal Int
  | ChooseSolutions
  | AddNewSolution Solution
  | ChoosePrerequisites
  | AddNewProblem Problem
  | EditSolution Int
  | EditProblem Int
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