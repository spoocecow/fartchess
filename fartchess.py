# blah!!!!!!!! 2/1/2013
import sys

WHITE=0
BLACK=1

PAWN=1
KNIGHT=2
BISHOP=3
ROOK=4
QUEEN=5
KING=6

xToChr = lambda x: chr(ord('A') + x)
bPos = lambda x,y: xToChr(x) + str(y+1)

class Piece:

    names = {PAWN:   'P',
             KNIGHT: 'N',
             BISHOP: 'B',
             ROOK:   'R',
             QUEEN:  'Q',
             KING:   'K'}

    def __init__(self, color, type, x=0, y=0):
        self.color = color
        self.type = type
        self.x = x
        self.y = y
        self.name = Piece.names[type]
        if self.color == BLACK: self.name = self.name.lower()
        self.captured = False

    def __hash__(self):
        return self.color ^ self.type ^ hash( (self.x, self.y) )

    def __eq__(self, other):
        return self.color == other.color and self.type == other.type and self.x == other.x and self.y == other.y

    def coordinates(self):
        return self.x, self.y

    def typeMoves(self):
        # all conceivable moves, regardless of state
        # (impossible moves due to piece conflicts will be pruned later)
        x,y = self.x, self.y
        if self.type == PAWN:
            if self.color == WHITE: return [(x,y+1), (x-1,y+1), (x+1,y+1)] + ([(x,3)] if y == 1 else [])
            else:                   return [(x,y-1), (x-1,y-1), (x+1,y-1)] + ([(x,4)] if y == 6 else [])
        elif self.type == KNIGHT:
            return [(x+dx, y+dy) for dx in (-2,-1,1,2) for dy in (-2,-1,1,2) if abs(dx)!=abs(dy)]
        elif self.type == BISHOP:
            return [(x+d*dx, y+d*dy) for d in range(8) for dx in (-1,1) for dy in (-1,1)]
        elif self.type == ROOK:
            return [(x+d*dx,y) for d in range(8) for dx in (-1,1)] + [(x,y+d*dy) for d in range(8) for dy in (-1,1)]
        elif self.type == QUEEN:
            return [(x+d*dx, y+d*dy) for d in range(8) for dx in (-1,1) for dy in (-1,1)] +\
                   [(x+d*dx,y) for d in range(8) for dx in (-1,1)] + [(x,y+d*dy) for d in range(8) for dy in (-1,1)]
        elif self.type == KING:
            return [(x+dx, y+dy) for dx in (-1,0,1) for dy in (-1,0,1) if dx!=0 and dy!=0]
            # I think we'll want to handle castling in the state... very state-dependent,
            # and also moves the rook

    def move(self, tox, toy):
        self.x = tox
        self.y = toy

    def isAttacking(self, state):
        # TBD move to state?
        # todo for state eval
        pass

    def isAttackedBy(self, state):
        # TBD move to state?
        # todo for state eval
        pass

    def __str__(self):
        return self.name + bPos(*self.coordinates())

    def __repr__(self):
        return str(self)

    def copy(self):
        return Piece(self.color, self.type, self.x, self.y)

    def asChr(self):
        return self.name
        if self.color == WHITE:
            if   self.type == PAWN:   return 'P'#u'\u2659'
            elif self.type == KNIGHT: return 'N'#u'\u2658'
            elif self.type == BISHOP: return 'B'#u'\u2657'
            elif self.type == ROOK:   return 'R'#u'\u2656'
            elif self.type == QUEEN:  return 'Q'#u'\u2655'
            elif self.type == KING:   return 'K'#u'\u2654'
            else: return '?'
        else:
            if   self.type == PAWN:   return 'p'#u'\u265F'
            elif self.type == KNIGHT: return 'n'#u'\u265E'
            elif self.type == BISHOP: return 'b'#u'\u265D'
            elif self.type == ROOK:   return 'r'#u'\u265C'
            elif self.type == QUEEN:  return 'q'#u'\u265B'
            elif self.type == KING:   return 'k'#u'\u265A'
            else: return '?'


class Pieces:
    # intended to allow various ways of lookup for pieces on the board
    # by location, type, ?
    def __init__(self, color, pieces=()):
        self._all = set()
        self.by_xy = dict()
        self.by_type = {PAWN: set(),
                        KNIGHT: set(),
                        BISHOP: set(),
                        ROOK: set(),
                        QUEEN: set()
        }
        self.color = color
        self.king = None
        for piece in pieces:
            self.addPiece(piece)

    def addPiece(self, piece):
        assert piece.color == self.color, "racism"
        assert piece not in self._all, "Duplicate piece object: " + str(piece)
        assert piece.coordinates() not in self.by_xy, \
            "Duplicate piece at %s: %s <> %s" % (piece.coordinates(), piece, self.by_xy[piece.coordinates()])
        self._all.add(piece)
        self.by_xy[piece.coordinates()] = piece
        if piece.type == KING:
            assert self.king is None, "Adding duplicate king!"
            self.king = piece
        else:
            self.by_type[piece.type].add(piece)

    def rmPiece(self, piece):
        self._all.remove(piece)

    def __iter__(self):
        return iter(self._all)

    def copy(self):
        return Pieces(self.color, [piece.copy() for piece in self._all])

    def byPos(self, x, y):
        return self.by_xy.get((x,y), None)

    def byType(self, type):
        # I guess I don't know when I'd ever use this...?
        return None


class Board:
    def __init__(self):
        self._board = []
        for r in range(8):
            self._board.append( [None] * 8 )

    def __getitem__(self, item):
        return self._board.__getitem__(item)

    def placePieces(self, whites, blacks):
        for piece in whites:
            assert self[piece.x][piece.y] is None, "Conflicting pieces: %s <> %s" % (piece, self[piece.x][piece.y])
            self[piece.x][piece.y] = piece
        for piece in blacks:
            assert self[piece.x][piece.y] is None, "Conflicting pieces: %s <> %s" % (piece, self[piece.x][piece.y])
            self[piece.x][piece.y] = piece

    def display(self):
        for col in range(7,-1,-1):
            sys.stdout.write('+-'*8 +'+\n')
            for row in range(8):
                if self[row][col]:
                    sys.stdout.write('|' + self[row][col].asChr())
                else:
                    sys.stdout.write('| ')
            sys.stdout.write('|\n')
        sys.stdout.write('+-'*8 +'+\n')

class Move:
    def __init__(self, movingpiece, to):
        self.piece = movingpiece
        self._from = movingpiece.coordinates()
        self._to = to
        self.moveno = 0
        self.capturing = None

    def source(self):
        return self._from

    def destination(self):
        return self._to

    def notation(self):
        s = self.piece.asChr()
        if self.capturing:
            s += 'x' + bPos(*self.capturing.coordinates())
        else:
            s += bPos(*self._to)
        return s

class GameState:
    default_white = Pieces(WHITE,
                           [Piece(WHITE, PAWN,   0, 1),
                            Piece(WHITE, PAWN,   1, 1),
                            Piece(WHITE, PAWN,   2, 1),
                            Piece(WHITE, PAWN,   3, 1),
                            Piece(WHITE, PAWN,   4, 1),
                            Piece(WHITE, PAWN,   5, 1),
                            Piece(WHITE, PAWN,   6, 1),
                            Piece(WHITE, PAWN,   7, 1),
                            Piece(WHITE, KNIGHT, 1, 0),
                            Piece(WHITE, KNIGHT, 6, 0),
                            Piece(WHITE, BISHOP, 2, 0),
                            Piece(WHITE, BISHOP, 5, 0),
                            Piece(WHITE, ROOK,   0, 0),
                            Piece(WHITE, ROOK,   7, 0),
                            Piece(WHITE, QUEEN,  3, 0),
                            Piece(WHITE, KING,   4, 0)]
    )
    default_black = Pieces(BLACK,
                           [Piece(BLACK, PAWN,   0, 6),
                            Piece(BLACK, PAWN,   1, 6),
                            Piece(BLACK, PAWN,   2, 6),
                            Piece(BLACK, PAWN,   3, 6),
                            Piece(BLACK, PAWN,   4, 6),
                            Piece(BLACK, PAWN,   5, 6),
                            Piece(BLACK, PAWN,   6, 6),
                            Piece(BLACK, PAWN,   7, 6),
                            Piece(BLACK, KNIGHT, 1, 7),
                            Piece(BLACK, KNIGHT, 6, 7),
                            Piece(BLACK, BISHOP, 2, 7),
                            Piece(BLACK, BISHOP, 5, 7),
                            Piece(BLACK, ROOK,   0, 7),
                            Piece(BLACK, ROOK,   7, 7),
                            Piece(BLACK, QUEEN,  3, 7),
                            Piece(BLACK, KING,   4, 7)]
    )

    def __init__(self, whitepieces=None, blackpieces=None, turn=WHITE):
        self.board = Board()
        if not whitepieces:
            self.whitePieces = GameState.default_white
        else:
            self.whitePieces = whitepieces
        if not blackpieces:
            self.blackPieces = GameState.default_black
        else:
            self.blackPieces = blackpieces
        self.board.placePieces(self.whitePieces, self.blackPieces)
        self.turn = turn
        self.playno = 0

    def possibleMovesByPiece(self, piece):
        allmoves = piece.typeMoves()
        for move in allmoves:
            x,y = move
            # if no move at all, invalid
            if x==piece.x and y == piece.y:
                continue
            # if out of bounds, invalid
            if not (0<=x<=7 and 0<=y<=7):
                continue
            if self.board[x][y]:
                # if same color, invalid
                if self.board[x][y].color == piece.color:
                    continue
            # if pawn diagonal move and not capture or en passant, invalid
            if piece.type == PAWN and piece.x != x and not self.board[x][y]:
                # en passant todo
                continue
            # if king castling and castle not allowed, invalid
            # TODO do this out of this loop
            # if square empty, but move passes through occupied square, and not knight, invalid
            if not self.checkPathValid(piece, x, y):
                #print "Can't move", piece, "to", bPos(x,y)
                continue
            # TODO
            # if causes discovered check on king, invalid
            # TODO
            if False:
                continue
            yield Move(piece, move)

    def checkPathValid(self, piece, tox, toy):
        """make sure the move of `piece` to (tox,toy) doesn't collide with other pieces"""
        sx,sy = piece.coordinates()
        dx,dy = tox-sx, toy-sy
        xstep = 1 if dx > 0 else -1
        ystep = 1 if dy > 0 else -1
        #print "Checking path of", piece, "moving (%d,%d) to %s" % (dx,dy, bPos(tox,toy))
        if piece.type == KNIGHT: return True # knights jump :>
        elif piece.type == PAWN:
            if abs(dy) == 2: # if pawn jumping out two spaces
                return self.board[tox][toy-1] is None if piece.color == WHITE else self.board[tox][toy+1] is None
        elif piece.type == BISHOP:
            for qx,qy in zip( range(sx+xstep,tox,xstep), range(sy+ystep,toy,ystep) ):
                if self.board[qx][qy]: return False
        elif piece.type == ROOK:
            if dx != 0:
                for qx in range(sx+xstep, tox, xstep):
                    print "board[%d][%d] = %s" % (qx, sy, self.board[qx][sy])
                    if self.board[qx][sy]: return False
            elif dy != 0:
                for qy in range(sy+ystep, toy, ystep):
                    print "board[%d][%d] = %s" % (sx, qy, self.board[sx][qy])
                    if self.board[sx][qy]: return False
        elif piece.type == QUEEN:
            for qx,qy in zip( range(sx+xstep,tox,xstep), range(sy+ystep,toy,ystep) ):
                if self.board[qx][qy]: return False
            if dx != 0:
                for qx in range(sx+xstep, tox, xstep):
                    if self.board[qx][sy]: return False
            elif dy != 0:
                for qy in range(sy+ystep, toy, ystep):
                    if self.board[sx][qy]: return False
        return True

    def allPossibleMoves(self):
        if self.turn == WHITE:
            collection = self.whitePieces
        else:
            collection = self.blackPieces
        for piece in collection:
            print "Generating moves for", piece
            for move in self.possibleMovesByPiece(piece):
                yield move

    def applyMove(self, move):
        sx,sy = move.source()
        assert self.board[sx][sy], "Moving piece that isn't on board"
        assert self.board[sx][sy] == move.piece, "board/move consistency"
        movingPiece = move.piece
        assert movingPiece.color == self.turn, "applying move to piece of wrong color: " + str(movingPiece.color) + ' ' + str(self.turn)
        active  = self.whitePieces.copy() if movingPiece.color == WHITE else self.blackPieces.copy()
        passive = self.whitePieces.copy() if movingPiece.color == BLACK else self.blackPieces.copy()

        assert active.byPos(sx,sy) == movingPiece, "piece set consistency: %s v %s" % (active.byPos(sx,sy), movingPiece)
        movingPiece = active.byPos(sx,sy) # switch ptr from Move out for copy
        mx,my = move.destination()
        if self.board[mx][my]:
            # capture!
            capturedPiece = self.board[mx][my]
            assert movingPiece.color != capturedPiece.color, "Capturing own piece: %s capturing %s" % (movingPiece, capturedPiece)
            print movingPiece, "capturing", capturedPiece
            passive.rmPiece(capturedPiece)
            move.capturing = capturedPiece
        print "Moving", movingPiece, "to", bPos(mx,my)
        active.rmPiece(movingPiece)
        movingPiece.move(mx,my)
        active.addPiece(movingPiece)
        if self.turn == WHITE:
            return GameState(active, passive, BLACK)
        else:
            return GameState(passive, active, WHITE)

    def step(self):
        # yield all possible new states resulting from possible moves
        for move in self.allPossibleMoves():
            newstate = self.applyMove(move)
            yield newstate, move


def main():
    initState = GameState()
    initState.board.display()
    for newstate, move in initState.step():
        print move.notation()
        newstate.board.display()

if __name__ == "__main__":
    main()