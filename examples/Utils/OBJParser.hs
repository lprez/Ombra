{-# LANGUAGE OverloadedStrings #-}

module Utils.OBJParser (
        VertexLocation(..),
        VertexUV(..),
        VertexNormal(..),
        Vertex(..),
        Face(..),
        Command(..),
        parseOBJ
) where

import Control.Applicative
import Data.Text as T
import Data.Attoparsec.Text as P

data VertexLocation = VertexLocation Double Double Double
data VertexUV = VertexUV Double Double
data VertexNormal = VertexNormal Double Double Double
data Vertex = Vertex Int (Maybe Int) (Maybe Int)
data Face = Triangle Vertex Vertex Vertex
          | Quad Vertex Vertex Vertex Vertex

data Command = VertexLocationCommand VertexLocation
             | VertexUVCommand VertexUV
             | VertexNormalCommand VertexNormal
             | FaceCommand Face
             | UnknownCommand Char

parseOBJ :: Text -> Either String [Command]
parseOBJ = parseOnly $ many1 command

command :: Parser Command
command = do skipSpace
             cmd <- choice [ VertexUVCommand <$> uv
                           , VertexNormalCommand <$> normal
                           , VertexLocationCommand <$> location
                           , FaceCommand <$> face
                           , UnknownCommand <$> unknownCommand
                           , fail "invalid command"
                           ]
             endOfLine
             return cmd

unknownCommand :: Parser Char
unknownCommand = (letter <|> char '#') <*
                 (skipMany letter *> sep *> takeTill isEndOfLine)

face :: Parser Face
face = do char 'f'
          sep
          vs <- vertex `sepBy` sep
          case vs of
               [x, y, z] -> pure $ Triangle x y z
               [x, y, z, w] -> pure $ Quad x y z w
               (_ : _ : _ : _ : _ : _) ->
                       fail "only triangles and quads are supported"
               _ -> fail "not enough vertices"

vertex :: Parser Vertex
vertex = do ls <- (option Nothing (Just <$> decimal)) `sepBy` char '/'
            case ls of
                 (Nothing : _) -> fail "no vertex location specified"
                 [Just l] -> pure $ Vertex l Nothing Nothing
                 [Just l, Just u] -> pure $ Vertex l (Just u) Nothing
                 [Just l, u, Just n] -> pure $ Vertex l u (Just n)
                 _ -> fail "invalid vertex/texcoord/normal combination"

location :: Parser VertexLocation
location = do char 'v'
              sep
              xs <- double `sepBy` sep
              case xs of
                   [x, y, z] -> pure $ VertexLocation x y z
                   (_ : _ : _ : _ : _) -> fail "too many vertex components"
                   _ -> fail "not enough vertex components"

uv :: Parser VertexUV
uv = do string "vt"
        sep
        u <- double
        v <- (sep *> double) <|> pure 0
        pure $ VertexUV u v


normal :: Parser VertexNormal
normal = do string "vn"
            sep
            xs <- double `sepBy` sep
            case xs of
                 [x, y, z] -> pure $ VertexNormal x y z
                 (_ : _ : _ : _ : _) -> fail "too many normal components"
                 _ -> fail "not enough normal components"

sep :: Parser ()
sep = skipMany1 $ char ' '
