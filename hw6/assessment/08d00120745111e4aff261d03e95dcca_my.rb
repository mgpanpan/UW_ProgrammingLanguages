# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyTetris < Tetris
    # creates a canvas and the board that interacts with it
    def set_board
        @canvas = TetrisCanvas.new
        @board = MyBoard.new(self)
        @canvas.place(@board.block_size * @board.num_rows + 3,
                      @board.block_size * @board.num_columns + 6, 24, 80)
        @board.draw
    end
    def key_bindings  
        @root.bind('n', proc {self.new_game}) 

        @root.bind('p', proc {self.pause}) 

        @root.bind('q', proc {exitProgram})
        
        @root.bind('a', proc {@board.move_left})
        @root.bind('Left', proc {@board.move_left}) 
        
        @root.bind('d', proc {@board.move_right})
        @root.bind('Right', proc {@board.move_right}) 

        @root.bind('s', proc {@board.rotate_clockwise})
        @root.bind('Down', proc {@board.rotate_clockwise})

        @root.bind('w', proc {@board.rotate_counter_clockwise})
        @root.bind('Up', proc {@board.rotate_counter_clockwise}) 
        
        @root.bind('space' , proc {@board.drop_all_the_way})

        @root.bind('u', proc {@board.rotate_180degrees})
        @root.bind('c', proc {@board.set_cheat})
    end
end

class MyPiece < Piece
    def self.next_piece (board)
        MyPiece.new(All_My_Pieces.sample, board)
    end
    All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                  rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                  [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                   [[0, 0], [0, -1], [0, 1], [0, 2]]],
                  rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                  rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                  rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                  rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
                  rotations([[-1, 0], [0, 0], [1, 0], [-1, 1], [0, 1]]),# add 1
                  [[[0, 0], [-1, 0], [1, 0], [-2, 0], [2, 0]],
                   [[0, 0], [0, -1], [0, 1], [0, -2], [0, 2]]], # add 2
                  rotations([[0, 0], [1, 0], [0, 1]])] # add 3
end

class MyBoard < Board
    def initialize (game)
        @grid = Array.new(num_rows) {Array.new(num_columns)}
        @current_block = MyPiece.next_piece(self)
        @score = 0
        @game = game
        @delay = 500
        @cheat_en = false
    end

    # rotates the current piece 180 degrees
    def rotate_180degrees
        if !game_over? and @game.is_running?
            @current_block.move(0, 0, 1)
            @current_block.move(0, 0, 1)
        end
        draw
    end

    def next_piece
        if @cheat_en
            @current_block = MyPiece.new([[[0,0]]], self)
            @cheat_en = false
        else
            @current_block = MyPiece.next_piece(self)
        end
        @current_pos = nil
    end

    # gets the information from the current piece about where it is and uses this
    # to store the piece on the board itself.  Then calls remove_filled.
    def store_current
        locations = @current_block.current_rotation
        displacement = @current_block.position
        (0..@current_block.current_rotation.size-1).each{|index| # now not all the block size is 4
            current = locations[index];
            @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
            @current_pos[index]
        }
        remove_filled
        @delay = [@delay - 2, 80].max
    end

    def set_cheat
        if !game_over? and @game.is_running? and !@cheat_en
            if @score >= 100
                @cheat_en = true
                @score -= 100
            end
        end
    end
end
