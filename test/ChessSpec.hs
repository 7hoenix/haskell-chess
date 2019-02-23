module ChessSpec where

import Test.Hspec
import Data.Either

import Chess (legal, place, emptyBoard, remove, get, move
             , knight, monarch, rook, hand, bishop, pawn
             , Team(..)
             , a1, a2, a3, a4, a5, a6, a7, a8
             , b1, b2, b3, b4, b5, b6, b7, b8
             , c1, c2, c3, c4, c5, c6, c7, c8
             , d1, d2, d3, d4, d5, d6, d7, d8
             , e1, e2, e3, e4, e5, e6, e7, e8
             , f1, f2, f3, f4, f5, f6, f7, f8
             , g1, g2, g3, g4, g5, g6, g7, g8
             , h1, h2, h3, h4, h5, h6, h7, h8
             )

spec :: Spec
spec = do
  describe "Chess.place" $
    it "returns a new board with the piece placed on it" $
      Chess.place (knight White) a1 emptyBoard `shouldNotBe` emptyBoard

  describe "Chess.remove" $
    it "returns a new board with the square cleared" $
      (Chess.remove a1 . Chess.place (knight White) a1) emptyBoard `shouldBe` emptyBoard

  describe "Chess.get" $ do
    it "returns the piece from a square if occupied" $
      (Chess.get a1 . Chess.place (knight White) a1) emptyBoard `shouldBe` (Just (knight White))
    it "returns nothing if empty" $
      Chess.get a1 emptyBoard `shouldBe` Nothing

  describe "Chess.move" $ do
     it "no longer has piece in previous square" $
       (Chess.get a1 . Chess.move a1 h8 . Chess.place (knight White) a1) emptyBoard `shouldBe` Nothing
     it "has piece in new square" $
       (Chess.get h8 . Chess.move a1 h8 . Chess.place (knight White) a1) emptyBoard `shouldBe` (Just (knight White))

  describe "Chess.legal" $ do
    describe "knows monarch movement" $ do
      it "bottom left corner" $
        (Chess.legal a1 . Chess.place (monarch White) a1) emptyBoard `shouldMatchList` [b2, b1, a2]
      it "bottom right corner" $
        (Chess.legal h1 . Chess.place (monarch White) h1) emptyBoard `shouldMatchList` [h2, g1, g2]
      it "top right corner" $
        (Chess.legal h8 . Chess.place (monarch White) h8) emptyBoard `shouldMatchList` [g7, h7, g8]
      it "top left corner" $
        (Chess.legal a8 . Chess.place (monarch White) a8) emptyBoard `shouldMatchList` [a7, b7, b8]
      it "center" $
        (Chess.legal b2 . Chess.place (monarch White) b2) emptyBoard `shouldMatchList` [a1, a2, a3, b1, b3, c1, c2, c3]
      it "cannot travel to a square occupied by its own team" $
        (Chess.legal b2
          . Chess.place (monarch White) b2
          . Chess.place (pawn White) b3) emptyBoard `shouldMatchList` [a1, a2, a3, b1, c1, c2, c3]

    describe "knows hand movement" $
      it "bottom left corner" $
        (Chess.legal a1 . Chess.place (hand White) a1) emptyBoard `shouldMatchList` [a2, a3, a4, a5, a6, a7, a8,
                                                                                     b1, c1, d1, e1, f1, g1, h1,
                                                                                     b2, c3, d4, e5, f6, g7, h8]
    describe "knows rook movement" $ do
      it "bottom left corner" $
        (Chess.legal a1 . Chess.place (rook White) a1) emptyBoard `shouldMatchList` [a2, a3, a4, a5, a6, a7, a8,
                                                                                     b1, c1, d1, e1, f1, g1, h1]
      it "top right corner" $
        (Chess.legal h8 . Chess.place (rook White) h8) emptyBoard `shouldMatchList` [h1, h2, h3, h4, h5, h6, h7,
                                                                                     a8, b8, c8, d8, e8, f8, g8]
      it "top right corner" $
        (Chess.legal h8 . Chess.place (rook White) h8) emptyBoard `shouldMatchList` [h1, h2, h3, h4, h5, h6, h7,
                                                                                     a8, b8, c8, d8, e8, f8, g8]
    describe "knows bishop movement" $
      it "bottom left corner" $
        (Chess.legal a1 . Chess.place (bishop White) a1) emptyBoard `shouldMatchList` [b2, c3, d4, e5, f6, g7, h8]

    describe "knows pawn movement" $ do
      it "knows black pawns may only move towards row 1" $
        (Chess.legal a2 . Chess.place (pawn Black) a2) emptyBoard `shouldMatchList` [a1]
      it "knows black pawns may attack diagonally" $
        (Chess.legal b2
          . Chess.place (pawn White) a1
          . Chess.place (pawn White) c1
          . Chess.place (pawn Black) b2) emptyBoard `shouldMatchList` [a1, b1, c1]
      it "knows white pawns may only move towards row 8" $
        (Chess.legal a2 . Chess.place (pawn White) a2) emptyBoard `shouldMatchList` [a3]
      it "knows white pawns may attack diagonally" $
        (Chess.legal b2
          . Chess.place (pawn Black) a3
          . Chess.place (pawn Black) c3
          . Chess.place (pawn White) b2) emptyBoard `shouldMatchList` [a3, b3, c3]

    describe "knows knight movement" $ do
      it "bottom left corner" $
        (Chess.legal a1 . Chess.place (knight Black) a1) emptyBoard `shouldMatchList` [b3, c2]
      it "top right corner" $
        (Chess.legal h8 . Chess.place (knight Black) h8) emptyBoard `shouldMatchList` [f7, g6]
      it "center" $
        (Chess.legal e4 . Chess.place (knight Black) e4) emptyBoard `shouldMatchList` [d2, c3, c5, d6, f6, g5, g3, f2]
      it "cannot travel to a square occupied by its own team" $
        (Chess.legal a1
          . Chess.place (monarch White) b3
          . Chess.place (knight White) a1) emptyBoard `shouldMatchList` [c2]

-- Add support for checks
