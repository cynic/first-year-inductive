module Main exposing (main)

import Browser
import Html exposing (Html, div, text, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String.Extra
import Json.Decode as D
import Json.Encode as E

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

type alias Solution =
  { summary : String
  , detail : String -- OR comment
  , implementation : String
  }

-- Define the Model
type alias Model =
    { problems : List Problem
    , expandedIndex : Maybe Int
    }

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

-- initial problems
defaultProblems : List Problem
defaultProblems =
  [ Problem -- 0 summary, detail, example, category, prerequisites, solutions
    "Cannot move from a concrete/applied context into the disciplinary domain, to solve the problem."
    "Is this a comprehension problem, or a practice problem, or something else?"
    ""
    PracticeRelated
    [] [26, 27]
  , Problem -- 1 summary, detail, example, category, prerequisites, solutions
    "Enormous reliance on tutors, but good tutors (or even qualified tutors) can be tough to come by"
    "If enough people don't pass the course with high marks in previous years, and/or there aren't enough postgraduates, then the tutor pool will be small and likely insufficient."
    ""
    CourseStructure
    [] [1, 9, 2]
  , Problem -- 2 summary, detail, example, category, prerequisites, solutions
    "It's a big class and it's tough to do anything with big classes"
    "Taking attendance, marking, even tutorials/practicals etc are more difficult"
    ""
    CourseStructure
    [0, 1] [21]
  , Problem -- 3 summary, detail, example, category, prerequisites, solutions
    "Inability / unwillingness to read/absorb for comprehension"
    ""
    """Some students have read the notes and some can in fact recite parts of them verbatim,
    but they don't comprehend/absorb what they're reading and cannot (for example)
    explain the same thing in their own words, or identify the same idea when it is
    explained without using the same words as the notes.  The semantics behind the
    words on the page are simply missing.

    Computer Science example: we say that we "assign" a value to a variable.  But
    some students cannot understand what that actually means (i.e. the value of the
    variable will change and then be different after the line is executed).  The same
    students might have no trouble saying that a line means "assign 'moo' the value 5", 
    and will be stumped if you ask "can you tell me what the value of moo is now?".
    """
    AcademicLiteracy
    [] []
  , Problem -- 4 summary, detail, example, category, prerequisites, solutions
    "They don't apply something unless it's clearly stated that they must use that particular thing"
    "They can't infer, based on the context or the problem, WHAT could be done to solve a problem in that context."
    ""
    AcademicLiteracy
    [] [7, 8, 15]
  , Problem -- 5 summary, detail, example, category, prerequisites, solutions
    "Don't understand how to construct a STATEMENT: a full, coherent sentence that expresses a full idea"
    ""
    ""
    AcademicLiteracy
    [] []
  , Problem -- 6 summary, detail, example, category, prerequisites, solutions
    "Students just treat formulae as opaque things, with no interpretation."
    "If the formula doesn't give the result immediately, or there is no obvious formula to apply, then the student is stumped."
    """This appears a lot in Computer Science. If a technique shown in class needs
    even slight modification to work, e.g. nesting loops or breaking out of a loop
    in a different place, then some students just draw a blank.
    """
    SubjectSpecificComprehension
    [] []
  , Problem -- 7 summary, detail, example, category, prerequisites, solutions
    "Students don't differentiate properly between big overarching principles and special ways / shortcuts."
    "Students either apply first-principles inappropriately, OR apply shortcuts inappropriately."
    ""
    SubjectSpecificComprehension
    [] []
  , Problem -- 8 summary, detail, example, category, prerequisites, solutions
    "Students can't construct a logical argument"
    "This is an argument with premises and conclusion(s), based on accepted axioms"
    ""
    SubjectSpecificComprehension
    [] [8]
  , Problem -- 9 summary, detail, example, category, prerequisites, solutions
    "Students don't understand notation and the semantics of notation"
    "Notation includes the argument of a function, equals sign, equivalence, implication, brackets… to them, these are just things that are there with no real rhyme or reason."
    ""
    SubjectSpecificComprehension
    [] [12, 3]
  , Problem -- 10 summary, detail, example, category, prerequisites, solutions
    "Students think that the point is maximising marks."
    "Don't see the point of assignments, or resources put up for them, or feedback, or university.  Don't see the point of an accumulation of knowledge & skill."
    ""
    MentalModel
    [] []
  , Problem -- 11 summary, detail, example, category, prerequisites, solutions
    "Students don't understand overloaded / contextual mathematical or notational dialects."
    "This includes understanding that they exist at all.  Different maths symbols can mean different things in different contexts; it's like a language with dialects AND overloaded symbols.  Students don't understand the differences and don't understand different meanings in their context."
    ""
    MentalModel
    [] []
  , Problem -- 12 summary, detail, example, category, prerequisites, solutions
    "Can't apply something taught in one area into a different area"
    "This includes applying skills/knowledge from a different subject."
    "From Maths, applying knowledge between sets & cartesian products"
    MentalModel
    [] [3, 11]
  , Problem -- 13 summary, detail, example, category, prerequisites, solutions
    "Don't recognize obviously ridiculous answers"
    ""
    ""
    MentalModel
    [] [20]
  , Problem -- 14 summary, detail, example, category, prerequisites, solutions
    "Students don't realize that you have to meet the criteria before applying a theorem to get a conclusion"
    "As a result, they choose the wrong thing to use"
    "In Computer Science, they try to apply integer methods to strings, or list iteration to non-lists, and so on."
    MentalModel
    [] [15, 19]
  , Problem -- 15 summary, detail, example, category, prerequisites, solutions
    "Students make up their own definitions rather than sticking to the definitions they're given"
    "This is especially prevalent when a \"modified\" definition would make the problem more convenient to solve in some way."
    ""
    MentalModel
    [] [23, 26]
  , Problem -- 16 summary, detail, example, category, prerequisites, solutions
    "During lectures, students listen to the voice and don't read the slides"
    "That affects absorption of information, especially detailed information & notation."
    "If you have something e.g. an algorithm up on a slide, and then switch the slide and ask them to tell you what the first step mentioned on the previous slide was, they probably won't be able to do it."
    PoorClassBehaviour
    [] []
  , Problem -- 17 summary, detail, example, category, prerequisites, solutions
    "Students attend class/pracs but are disengaged (even on phone)"
    ""
    ""
    PoorClassBehaviour
    [] [4, 6, 5]
  , Problem -- 18 summary, detail, example, category, prerequisites, solutions
    "Skipping lectures/tutorials/etc"
    ""
    ""
    PoorClassBehaviour
    [] [21, 22, 24, 0, 25]
  , Problem -- 19 summary, detail, example, category, prerequisites, solutions
    "Students rely on external sources (e.g. YouTube or notes) rather than textbooks and \"offical\" sources"
    "Non-official sources can lead them astray (sometimes obviously, sometimes subtly) and it can be difficult to pick this up."
    ""
    PoorOutOfClassBehaviour
    [] []
  , Problem -- 20 summary, detail, example, category, prerequisites, solutions
    "Students ignore / don't engage with feedback on assignments/tests"
    ""
    ""
    PoorOutOfClassBehaviour
    [] [17, 29, 28]
  , Problem -- 21 summary, detail, example, category, prerequisites, solutions
    "Students stick to basics (high-school or earliest-taught) and resist the use of new things when they come in"
    ""
    ""
    PoorAssessmentBehaviour
    [] []
  , Problem -- 22 summary, detail, example, category, prerequisites, solutions
    "Students don't work at the speed required to pass an assessment"
    ""
    ""
    PoorAssessmentBehaviour
    [] [10]
  , Problem -- 23 summary, detail, example, category, prerequisites, solutions
    "For most assignments, students don't care about the assignment, they just get their answer from wherever"
    "Nobody looks up the answer to 1+1. If students could do the assignment, they wouldn't copy etc; and being able to do the assignment is a function of practice."
    "They get answers from friends, the internet, copying textbooks/notes, etc."
    PoorAssessmentBehaviour
    [] [13, 16, 14, 18, 17, 22]
  , Problem -- 24 summary, detail, example, category, prerequisites, solutions
    "Students memorize and reproduce instead of thinking"
    "Students even memorize and reproduce tutorials, even when inapplicable!"
    ""
    AcademicLiteracy
    [] [2]
  -- , Problem -- summary, detail, example, category, prerequisites, solutions
  --   ""
  --   ""
  --   ""
  --   XXX
  --   (Just "")
  ]

-- initial solutions
defaultSolutions : List Solution
defaultSolutions =
  [ Solution -- 0 summary, detail, implementation
    "Use videos for revision"
    ""
    ""
  , Solution -- 1 summary, detail, implementation
    "The when, what, and how of things should be made rigid for students and tutors"
    ""
    ""
  , Solution -- 2 summary, detail, implementation
    "\"Warm-ups\" (step-by-step process) to be done before tutorials"
    "During tutoring, tutors can always  point to a place in the warm-up and say \"I'll explain this in the warm-up, and you tell me how it's done in the tutorial question.\""
    ""
  , Solution -- 3 summary, detail, implementation
    "Pracs/tutorials from Subject X could include basic problems referencing Subject Y"
    "Basic problems should be chosen so that students not doing Subject Y aren't disadvantaged, and so that Subject X doesn't have to spend 15 minutes explaining what the problem is."
    ""
  , Solution -- 4 summary, detail, implementation
    "Give students something (e.g. a task) to hand in for the start of a practical, or a quiz, and make it a DP requirement"
    ""
    ""
  , Solution -- 5 summary, detail, implementation
    "Clicker-equivalent questions for students to vote on"
    ""
    "This can be done using Mentimeter or Wooclap etc… they all do roughly the same thing, but a consistent platform should be used by everyone so that students can get familiar with the same problem quickly everywhere."
  , Solution -- 6 summary, detail, implementation
    "Give students questions in class that they discuss with their neighbours and come up with answer(s)"
    ""
    ""
  , Solution -- 7 summary, detail, implementation
    "\"10 commandments\" sheet for how to approach a problem"
    "This details steps that the student MUST complete when approaching any problem. Tutors/lecturers can ask to see this during pracs/tutorials or expect it in assessments."
    ""
  , Solution -- 8 summary, detail, implementation
    "Tell students to always (a) start with definitions and (b) understand what they need to show"
    "Students who know these things will know the start and end, and just need to fill in gaps."
    ""
  , Solution -- 9 summary, detail, implementation
    "Shared tutor training sessions; and mid-year training; and consistent consequence management for tutors"
    ""
    ""
  , Solution -- 10 summary, detail, implementation
    "Provide advice on expected times to complete tasks, and hold them to those times as much as possible"
    ""
    ""
  , Solution -- 11 summary, detail, implementation
    "Know what other Departments are doing so that the same application of a process/tool can be seen in different contexts"
    ""
    ""
  , Solution -- 12 summary, detail, implementation
    "Consistent mathematical notation in ALL subjects they do"
    "DO NOT accept things like lines below without relations (e.g. \"=\")"
    ""
  , Solution -- 13 summary, detail, implementation
    "Many practicals/tutorials should include at least one discipline-specific academic-skills question"
    "These questions focus on the steps/processes of the skills needed in the discpline, e.g. \"analyse this algorithm\", \"list which steps apply to this physics problem\", etc)"
    ""
  , Solution -- 14 summary, detail, implementation
    "Talk about what your education is FOR and WHY you are at university and what an EDUCATION is rather than being \"training for a job\""
    ""
    ""
  , Solution -- 15 summary, detail, implementation
    "Cheat-sheet of theorems where you must CHOOSE the correct one that is applicable to a context"
    ""
    ""
  , Solution -- 16 summary, detail, implementation
    "Practical/tutorial test at the start of every prac/tut."
    ""
    ""
  , Solution -- 17 summary, detail, implementation
    "Test corrections are always due by the following practical"
    "If you got 100%, you still hand in a blank sheet of paper as your corrections.  If you don't hand in your corrections, you get zero for that practical."
    ""
  , Solution -- 18 summary, detail, implementation
    "Two levels of solutions should be given for pracs/tuts: hints, then actual solutions"
    "Maybe a tutorial on Monday, a hint on Tues/Wed, an assignment that is similar to the tutorial, and then they must submit before the actual solution is uploaded."
    ""
  , Solution -- 19 summary, detail, implementation
    "Teach the process of applying theorems"
    "Can't assume they know it from school."
    ""
  , Solution -- 20 summary, detail, implementation
    "Have a conceptual discussion question at the end of prac/tutorial questions."
    "Example: \"Does your answer match with your experience of the world?  If not, explain why.\""
    ""
  , Solution -- 21 summary, detail, implementation
    "Have a small daily problem set at the end of each lecture."
    ""
    "At the start of the next lecture, select 5 \"random\" students to hand their solution in.  If they aren't there or don't hand in, then that counts against the DP; n strikes and you're out (i.e. must appeal to get your DP back & promise to be good), where n is consistent across our cluster.  OR have everyone hand in, but mark n of them."
  , Solution -- 22 summary, detail, implementation
    "Explain the difference between school and university in terms of working"
    "Explain also what kinds of notes and activities are going to be really important for them to utilize."
    ""
  , Solution -- 23 summary, detail, implementation
    "Have logic puzzles (e.g. Einstein's puzzle) that only are solveable if you read for comprehension AND obey the definitions"
    ""
    ""
  , Solution -- 24 summary, detail, implementation
    "Shared way to monitor/enforce lecture attendance (maybe Acadly?)"
    "Those few who don't have a phone can just sign a manual register."
    ""
  , Solution -- 25 summary, detail, implementation
    "Take registers and assign (small) marks for attendance"
    ""
    ""
  , Solution -- 26 summary, detail, implementation
    "Give more reading assignments that are assessed for comprehension"
    ""
    ""
  , Solution -- 27 summary, detail, implementation
    "Get them to do an identify-task-words / topic-words / limiting-words / etc kind of thing, where we explain the kind of answers we want for particular kinds of questions"
    ""
    ""
  , Solution -- 28 summary, detail, implementation
    "\"I accept failure\" form, for students who reject ADP (or equivalent)"
    ""
    ""
  , Solution -- 29 summary, detail, implementation
    "Tutor-on-call where tutor(s) available in the afternoon, for student consultations"
    ""
    ""
  , Solution -- 30 summary, detail, implementation
    "Identify poor candidates early"
    "Swap notes so that G7, DPs etc are handled early"
    ""
  , Solution -- 31 summary, detail, implementation
    "DPs must be refused early, in first-term, to get attention early."
    ""
    ""
  , Solution -- 32 summary, detail, implementation
    "(Try to) deliver the same themes of academic skills on the same day, so that it's mutually-reinforcing throughout a student's day"
    ""
    ""
  , Solution -- 33 summary, detail, implementation
    "Something must be handed in at the end of each practical/tutorial"
    "This is already largely the case for most of us"
    ""
  , Solution -- 34 summary, detail, implementation
    "Create a database of questions, say \"here are the questions, and the test will be comprised of some of them\""
    ""
    ""
  -- , Solution -- summary, detail, implementation
  --   ""
  --   ""
  --   ""
  ]

-- Define the initial model
initialModel : Model
initialModel =
    { problems = defaultProblems
    , expandedIndex = Nothing
    }

-- Define Msg type for future updates
type Msg
    = ExpandProblem Int

-- Define the Update function
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ExpandProblem index ->
          (model, Cmd.none)
-- Define the View function for a single problem
viewProblem : Int -> Problem -> Html Msg
viewProblem index problem =
    div
      [ class "square"
      , class "problem"
      , style "backgroundColor" <| categoryToColor problem.category
      , if String.Extra.isBlank problem.detail then
          class "no-detail"
        else
          title problem.detail
      , onClick (ExpandProblem index)
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
      ( List.filter (\p -> p.category == category) problems
        |> List.indexedMap viewProblem
      ))

-- Define the View function for the entire model
view : Model -> Html Msg
view model =
    div
      [ class "flex-rightleft"
      ]
      (List.map (\c -> viewCategory c model.problems) allCategories)

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
