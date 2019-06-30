module Errors where

data Error = MatchFail deriving (Show, Eq)

message :: Error -> String
message MatchFail = "pattern match failed."
