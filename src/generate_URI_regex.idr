module Main

import System

%default total

data RegexDialect
  = ERE

public
partial
main : IO ()

showHelp       : IO ()
parseDialectId : String -> Maybe RegexDialect
uriRegex       : RegexDialect -> String

repeatString       : Nat -> String -> String
intercalateStrings : String -> List String -> String

main = do
  args <- System.getArgs
  case args of
    [_, dialectId] =>
      case parseDialectId dialectId of
        Just dialect => putStrLn $ uriRegex dialect
        Nothing      => showHelp
    otherwise      => showHelp

showHelp = do
  putStrLn $ "Usage: generate_URI_regex <regex dialect>\n\n"
             ++ "Recognized dialects: `ere`"
  exit 2

parseDialectId d = case d of
  "ere"     => Just ERE
  otherwise => Nothing

parameters (d : RegexDialect)

  -- IETF RFC 3986 rules.
  URI          : String
  hierPart     : String
  scheme       : String
  authority    : String
  userinfo     : String
  host         : String
  port         : String
  IPLiteral    : String
  IPvFuture    : String
  --IPv6address  : String
  h16          : String
  ls32         : String
  IPv4address  : String
  decOctect    : String
  regName      : String
  pathAbempty  : String
  pathAbsolute : String
  pathNoscheme : String
  pathRootless : String
  pathEmpty    : String
  segment      : String
  segmentNz    : String
  segmentNzNc  : String
  pchar        : String
  query        : String
  fragment     : String
  pctEncoded   : String
  unreserved   : String
  reserved     : String
  genDelims    : String
  subDelims    : String

  -- ABNF basic rules.
  ALPHA  : String
  DIGIT  : String
  HEXDIG : String

  -- Literals.
  exclamationMark    : String
  numberSign         : String
  dollarSign         : String
  percentSign        : String
  ampersand          : String
  apostrophe         : String
  leftParenthesis    : String
  rightParenthesis   : String
  asterisk           : String
  plusSign           : String
  comma              : String
  hyphenMinus        : String
  fullStop           : String
  solidus            : String
  colon              : String
  semicolon          : String
  equalsSign         : String
  questionMark       : String
  commercialAt       : String
  leftSquareBracket  : String
  rightSquareBracket : String
  lowLine            : String
  tilde              : String

  squareBracketed : String -> String
  letters         : String -> String
  number          : Nat -> String
  emptyString     : String

  -- Regex primitives.
  -- “P” variants “propagate” the dialect.
  group              : String -> String
  concatP            : List (RegexDialect -> String) -> String
  optional           : String -> String
  anyOf              : List String -> String
  anyOfP             : List (RegexDialect -> String) -> String
  zeroOrMore         : String -> String
  zeroOrMoreP        : (RegexDialect -> String) -> String
  zeroOrMoreOfAnyOfP : List (RegexDialect -> String) -> String
  oneOrMore          : String -> String
  oneOrMoreP         : (RegexDialect -> String) -> String
  oneOrMoreOfAnyOfP  : List (RegexDialect -> String) -> String
  repN               : Nat -> String -> String
  repNP              : Nat -> (RegexDialect -> String) -> String
  repNM              : Nat -> Nat -> String -> String
  repNMP             : Nat -> Nat -> (RegexDialect -> String)
                           -> String
  digitRange         : String -> String -> String

-- IETF RFC 3986 rules.
-- [<https://tools.ietf.org/html/rfc3986#appendix-A>]

URI d =
  scheme d ++ colon d
  ++ hierPart d
  ++ optional d (questionMark d ++ query d)
  ++ optional d (numberSign d ++ fragment d)

hierPart d =
  anyOf d [
    solidus d ++ solidus d ++ authority d ++ pathAbempty d
  , pathAbsolute d
  , pathRootless d
  , pathEmpty d
  ]

scheme d =
  ALPHA d
  ++ zeroOrMore d (anyOfP d
    [ALPHA, DIGIT, plusSign, hyphenMinus, fullStop])

authority d =
  optional d (userinfo d ++ commercialAt d)
  ++ host d
  ++ optional d (colon d ++ port d)

userinfo d =
  zeroOrMoreOfAnyOfP d [unreserved, pctEncoded, subDelims, colon]

host d =
  anyOfP d [IPLiteral, IPv4address, regName]

port d =
  zeroOrMoreP d DIGIT

IPLiteral d =
  squareBracketed d $ anyOfP d [
    --IPv6address,
    IPvFuture
  ]

IPvFuture d =
  letters d "v"
  ++ oneOrMoreP d HEXDIG
  ++ fullStop d
  ++ oneOrMoreOfAnyOfP d [unreserved, subDelims, colon]

-- Not supporting IPv6 addresses yet; too complicated.
--IPv6address d =

h16 d =
  repNMP d 1 4 HEXDIG

ls32 d =
  anyOf d [
    h16 d ++ colon d ++ h16 d,
    IPv4address d
  ]

IPv4address d =
  concatP d [
    decOctect, fullStop,
    decOctect, fullStop,
    decOctect, fullStop,
    decOctect
  ]

decOctect d =
  anyOf d [
    DIGIT d,
    digitRange d "1" "9" ++ DIGIT d,
    number d 1 ++ repNP d 2 DIGIT,
    number d 2 ++ digitRange d "0" "4" ++ DIGIT d,
    number d 25 ++ digitRange d "0" "5"
  ]

regName d =
  zeroOrMoreOfAnyOfP d [unreserved, pctEncoded, subDelims]

pathAbempty d =
  zeroOrMore d (solidus d ++ segment d)

pathAbsolute d =
  solidus d ++ optional d (segmentNz d ++ pathAbempty d)

pathNoscheme d =
  segmentNzNc d ++ pathAbempty d

pathRootless d =
  segmentNz d ++ pathAbempty d

pathEmpty d =
  emptyString d

segment d =
  zeroOrMoreP d pchar

segmentNz d =
  oneOrMoreP d pchar

segmentNzNc d =
  oneOrMoreOfAnyOfP d [
    unreserved, pctEncoded, subDelims, commercialAt
  ]

pchar d =
  anyOfP d [unreserved, pctEncoded, subDelims, colon, commercialAt]

query d =
  zeroOrMoreOfAnyOfP d [pchar, solidus, questionMark]

-- Yes, <fragment> is identical to <query>, and may not include
-- number signs. See also RFC erratum report 3330
-- (<http://www.rfc-editor.org/errata_search.php?eid=3330>).
fragment d =
  zeroOrMoreOfAnyOfP d [pchar, solidus, questionMark]

pctEncoded d =
  percentSign d ++ HEXDIG d ++ HEXDIG d

unreserved d =
  anyOfP d [ALPHA, DIGIT, hyphenMinus, fullStop, lowLine, tilde]

reserved d =
  anyOfP d [genDelims, subDelims]

genDelims d =
  anyOfP d [
    colon, solidus, questionMark, numberSign,
    leftSquareBracket, rightSquareBracket, commercialAt
  ]

subDelims d =
  anyOfP d [
    exclamationMark, dollarSign, ampersand, apostrophe,
    leftParenthesis, rightParenthesis, asterisk, plusSign,
    comma, semicolon, equalsSign
  ]

-- Regex primitives.

group d x = case d of
  ERE => "(" ++ x ++ ")"

concatP d xfs = concatMap (\xf => xf d) xfs

optional d x = case d of
  ERE => group d x ++ "?"

anyOf d xs = case d of
  ERE => group d $ intercalateStrings "|" xs

anyOfP d xfs = anyOf d $ map (\xf => xf d) xfs

zeroOrMore d x = case d of
  ERE => group d x ++ "*"

zeroOrMoreP d xf = zeroOrMore d (xf d)

zeroOrMoreOfAnyOfP d xfs = zeroOrMore d (anyOfP d xfs)

oneOrMore d x = case d of
  ERE => group d x ++ "+"

oneOrMoreP d xf = oneOrMore d (xf d)

oneOrMoreOfAnyOfP d xfs = oneOrMore d (anyOfP d xfs)

repN d qty x = case d of
  ERE => group d (repeatString qty x)

repNP d qty xf = repN d qty (xf d)

repNM d min max x = case d of
  ERE => group d (repeatString min x
                 ++ repeatString (max - min) (optional d x))

repNMP d min max xf = repNM d min max (xf d)

digitRange d start end = case d of
  ERE => "[" ++ start ++ "-" ++ end ++ "]"

-- ABNF basic rules.

ALPHA d = case d of
  ERE => "[[:alpha:]]"

DIGIT d = case d of
  ERE => "[[:digit:]]"

HEXDIG d = case d of
  ERE => "[[:xdigit:]]"

-- Literals.

exclamationMark d = case d of
  ERE => "!"

numberSign d = case d of
  ERE => "#"

dollarSign d = case d of
  ERE => "\\$"

percentSign d = case d of
  ERE => "%"

ampersand d = case d of
  ERE => "&"

apostrophe d = case d of
  ERE => "'"

leftParenthesis d = case d of
  ERE => "\\("

rightParenthesis d = case d of
  ERE => "\\)"

asterisk d = case d of
  ERE => "\\*"

plusSign d = case d of
  ERE => "\\+"

comma d = case d of
  ERE => ","

hyphenMinus d = case d of
  ERE => "-"

fullStop d = case d of
  ERE => "\\."

solidus d = case d of
  ERE => "/"

colon d = case d of
  ERE => ":"

semicolon d = case d of
  ERE => ";"

equalsSign d = case d of
  ERE => "="

questionMark d = case d of
  ERE => "\\?"

commercialAt d = case d of
  ERE => "@"

leftSquareBracket d = case d of
  ERE => "\\["

rightSquareBracket d = case d of
  ERE => "\\]"

lowLine d = case d of
  ERE => "_"

tilde d = case d of
  ERE => "~"

squareBracketed d x =
  leftSquareBracket d ++ x ++ rightSquareBracket d

letters d s = case d of
  ERE => s

number d n = case d of
  ERE => show n

emptyString d = case d of
  ERE => ""

uriRegex d = URI d

-- Utility functions.

repeatString qty str =
  concat $ take qty $ repeat str

intercalateStrings sep strs =
  pack $ intercalate (unpack sep) (map unpack strs)
