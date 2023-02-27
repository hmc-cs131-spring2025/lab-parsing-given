{-|
Module       : PicParser
Description  : A parser for a subset of the pic programming language.
Maintainer   : CS 131, Programming Languages (Melissa O'Neill, Chris Stone, Ben Wiedermann)
-}

module PicParser where

import PicAST  -- This is the same abstract syntax as last week's assignment
import ParserCombinators


-- | A picture consists of one or more elements (which can handle leading
--   whitespace), possibly followed by unconsumed trailing whitespace
--   characters at the end of the file.
picture :: Parser Picture
picture = some element <+-> whitespace


-- | An element is either a direction command or a shape command
element :: Parser Element
element =     (direction <+-> sym ';'                  >>=: \d -> Turn d)
          <|> (shape <+> many attr <+-> sym ';'        >>=: \(s,a) -> Draw s a)
          <???> "<element> expected"


-- | An attribute is either a literal string or a direction
attr :: Parser Attribute
attr =        (litstring                                >>=: \s -> Label s)
          <|> (direction                                >>=: \d -> Dir d)
          <???> "<attr> expected"


-- | A direction can be "left", "right", "up", or "down"
direction :: Parser Direction
direction =    (text "left"                            >>: Lt)
           <|> (text "right"                           >>: Rt)
           <|> (text "up"                              >>: Up)
           <|> (text "down"                            >>: Dn)
           <???> "<direction> expected"


-- | A shape can be "box", "circle", "line", "move", or "arrow"
shape :: Parser Shape
shape =        (text "box"                             >>: Box)
           <|> (text "circle"                          >>: Circle)
           <|> (text "line"                            >>: Line)
           <|> (text "move"                            >>: Move)
           <|> (text "arrow"                           >>: Arrow)
           <???> "<shape> expected"

