# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece

	All_My_Pieces = Piece::All_Pieces.concat([
		rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2, 1]]),
		rotations([[0, 0], [1, 0], [2, 0], [3, 0], [4, 0]]),
		rotations([[0, 0], [0, 1], [1, 1]])
		])

	Cheat_Piece = [[[0, 0]]]

	def self.next_piece (board)
		if board.cheating
			MyPiece.new(Cheat_Piece, board)
		else
			MyPiece.new(All_My_Pieces.sample, board)
		end
	end

end

class MyBoard < Board

	attr_reader :cheating


	def initialize (game)
		super
		@current_block = MyPiece.next_piece(self)
		@cheating = false
	end

	def rotate_half
		if !game_over? and @game.is_running?
			@current_block.move(0, 0, 2)
		end
		draw
	end

	def next_piece
		@current_block = MyPiece.next_piece(self)
		@cheating = false
		@current_pos = nil
	end

	def store_current
		locations = @current_block.current_rotation
		displacement = @current_block.position
		(0..(locations.size-1)).each{|index| 
			current = locations[index];
			@grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
			@current_pos[index]
		}
		remove_filled
		@delay = [@delay - 2, 80].max
	end

	def cheat_play
		if @score >= 100 and !@cheating
			@score = @score - 100
			@cheating = true
		end
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
		super

		@root.bind('u', proc {@board.rotate_half}) 
		@root.bind('c' , proc {@board.cheat_play})

	end

end


