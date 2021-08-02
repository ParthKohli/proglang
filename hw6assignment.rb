# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  All_My_Pieces = All_Pieces +
                   [rotations([[0, 0], [1, 0], [0, 1], [1, 1], [-1, 0]]), 
                   [[[-1, 0], [-2, 0], [0, 0], [1, 0], [2, 0]],
                   [[0, -1], [0, -2], [0, 0], [0, 1], [0, 2]]],
                   rotations([[0, 0], [0, 1], [1, 0]])] 

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheating_piece (board)
    MyPiece.new([[[0, 0]]], board)
  end
end

class MyBoard < Board
  attr_accessor :is_cheating, :score

  def initialize (game)
    super
    @current_block = MyPiece.next_piece(self)
    self.is_cheating = false
  end

  def next_piece
    if self.is_cheating
      @current_block = MyPiece.cheating_piece(self)
      @score -= 100
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
    self.is_cheating = false
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size - 1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end    

  def key_bindings
    super # take all the original key-bindings, of course
    # rotating 180 degrees is equivalent to rotating by 90 deg twice
    @root.bind('u', proc {@board.rotate_clockwise; @board.rotate_clockwise})
    @root.bind('c', proc {if @board.score >= 100 and not @board.is_cheating then @board.is_cheating = true end})
  end 
end
