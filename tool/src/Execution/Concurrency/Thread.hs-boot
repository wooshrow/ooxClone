module Execution.Concurrency.Thread where
    import           Data.Graph.Inductive.Graph (Node)
    import qualified Data.Stack                 as T
    import qualified Data.Map                   as M
    import           Analysis.CFA.CFG
    import           Language.Syntax
    
    data Thread = Thread 
        { _tid          :: Int
        , _parent       :: Int
        , _pc           :: CFGContext
        , _callStack    :: T.Stack StackFrame
        , _handlerStack :: T.Stack (Node, Int) }

    data StackFrame = StackFrame
        { _returnPoint   :: Node
        , _lhs           :: Maybe Lhs
        , _memory        :: M.Map Identifier Expression
        , _currentMember :: DeclarationMember }
